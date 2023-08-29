

library(tidyverse)
library(openxlsx)

###############################################################
#                     Import ONS Crime Data                   #
###############################################################
# Publication:Crime in England and Wales
# https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/bulletins/crimeinenglandandwales/latest

# Data sets related to publication
# https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/datasets/policeforceareadatatables

# Links to the data from March 2016 - March 2023
y_2023 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/policeforceareadatatables/yearendingmarch2023/pfatableyemar231.xlsx"
y_2022 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/policeforceareadatatables/yearendingmarch2022/pfafinalmar22.xlsx"
y_2021 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/policeforceareadatatables/yearendingmarch2021/pfatablescorrectionmar21.xlsx"
y_2020 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/policeforceareadatatables/yearendingmarch2020/pfatablesyemar20.xlsx"
y_2019 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/policeforceareadatatables/yearendingmarch2019/policeforceareatablesyeendingmarch2019.xlsx"
y_2018 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/policeforceareadatatables/yearendingmarch2018/policeforceareatablesyearendingmarch2018v2.xlsx"
y_2017 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/policeforceareadatatables/yearendingmarch2017/policeforceareadatatablesyearendingmarch2017corrected.xls"
y_2016 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/crimeandjustice/datasets/policeforceareadatatables/yearendingmarch2016/policeforceareatablesyearendingmarch16.xls"

# Use this table to iterate through each data set. There are some idiosyncratic differences between the spreadsheets so this
# table is used to instruct what to bring through when iterating the import of respective data sets in the 'for loop'.  
excel_urls <- tibble(year =  c(2016:2023),
                     file_type = c("xls","xls", rep("xlsx",6)),
                     sheet_name = c("Table P1","Table P1","Table P1","Table P1 ","Table P1","Table P1","Table P1","Table P1"),
                     start_row = c(4,4,5,5,5,5,7,7),
                     url = c(y_2016,y_2017,y_2018,y_2019,y_2020,y_2021,y_2022,y_2023),
                     data = NA) 

excel_urls <- excel_urls %>% group_by(year,file_type, sheet_name,start_row,  url) %>% nest()

#####

# function to read the xls files that are used in 2016 and 2017 spreadsheets
read_xls_fn <- function(url_xls){
  
  temp.file <- tempfile(tmpdir = getwd(), fileext = ".xls")
  download.file(url_xls, temp.file, mode = "wb")
  readxl::read_xls(path = temp.file, sheet = "Table P1", skip = 4,.name_repair = make.names)
  
}

#####

for (i in 1:nrow(excel_urls)) {
  
    excel_urls$data[[i]] <- if (excel_urls[i,"file_type"] == "xls") {
 
      read_xls_fn(url_xls = pull(excel_urls[i,"url"]))
      
   } else {
     
     read.xlsx(xlsxFile = pull(excel_urls[i,"url"]),
               sheet = excel_urls$sheet_name[[i]],
               startRow= excel_urls$start_row[[i]], 
               check.names = T) 
   }
}

cols_robbery <- c("Area.Code", "Area.Name","Robbery")  

excel_urls$data_robbery <- map(.x = excel_urls$data, .f = ~.x[c(1:57),cols_robbery])

excel_urls$data_robbery <- map2(.x = excel_urls$data_robbery, .y = excel_urls$year, .f = ~.x%>%mutate(year = .y))

df <- Reduce(function(x, y) merge(x, y, all=TRUE), excel_urls$data_robbery)  
  
df$Robbery <- as.integer(df$Robbery)


###############################################################
#            Import Police Force Areas Shapefile              #
###############################################################

# Police Force Areas (Dec 2021) EW BUC
# https://geoportal.statistics.gov.uk/datasets/ons::police-force-areas-dec-2021-ew-buc/explore?location=52.545924%2C-1.051175%2C6.84

# Load & rearrange geography data -----------------------------------------
library(tmap)
library(sf)

# This file contains the digital vector boundaries for Police Force Areas in England and Wales as at December 2021.
england <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Police_Force_Areas_Dec_2021_EW_BUC_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=json") %>%
  filter(str_detect(PFA21CD, "^E"))

# Add data from 2016 and 2023
eng_robbery <-left_join(x = england,
          y = df %>% filter(year == 2016) %>% select("Area.Code","Robbery"),
          by = c("PFA21CD" = "Area.Code")) %>% rename(Robbery_2016 = Robbery)

eng_robbery <-left_join(x = eng_robbery,
                        y = df %>% filter(year == 2023) %>% select("Area.Code","Robbery"),
                        by = c("PFA21CD" = "Area.Code")) %>% rename(Robbery_2023 = Robbery)


###############################################################
#           Import Police Force Area Population               #
###############################################################

# rounded mid year population estimates (in 1000s) produced by the ONS
pop <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/116249/population-police-force.csv")
pop <- pop[, c("Police Force", "Mid 2010")]
pop$`Mid 2010` <- pop$`Mid 2010`*1000

# rename so that name is the same for the left join
pop[which(pop$`Police Force` == "Devon and Cornwall"),"Police Force"] <- "Devon & Cornwall"

eng_robbery <- left_join(x = eng_robbery, y = pop, by = c("PFA21NM" = "Police Force"))

# Calculate robberies per 10,000 people
eng_robbery <- eng_robbery %>% mutate(Robbery_2016_ratio = floor(Robbery_2016/`Mid 2010` * 10000),
                       Robbery_2023_ratio = floor(Robbery_2023/`Mid 2010` *10000),
                       diff = Robbery_2023 - Robbery_2016,
                       diff_ratio = Robbery_2023_ratio - Robbery_2016_ratio )

eng_robbery$london <- if_else(condition = eng_robbery$PFA21CD %in% c("E23000001", "E23000034"), true = "London", false = "Rest of England")
# map the robbery data that shows robberies per 10,000 people in England 

# abbreviate area names to put on map
eng_robbery$name_abr <- abbreviate(eng_robbery$PFA21NM  ,named = FALSE)

# Map 2016 robberies ------------------------------------------------------
eng_robbery %>% 
  tm_shape() +
  tm_fill("Robbery_2016_ratio", style = "cont", breaks = c(0,5,20,40,60,80,105),
          labels = c("0","5","20","40","60","80","100+"),
          palette = c("#FFFFFF","#C6DBEF","#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
          title = "Robbery 2016") +
  tm_borders( lwd = 1, lty = "solid", alpha = 0.1)+
  tm_text("name_abr", size = 0.6,col = "black")+
  tm_style("col_blind")+
  tm_layout(frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA)

# Map 2023 robberies ------------------------------------------------------
eng_robbery %>% 
  tm_shape() +
  tm_fill("Robbery_2023_ratio", style = "cont", breaks = c(0,5,20,40,60,80,105),
          labels = c("0","5","20","40","60","80","100+"),
          palette = c("#FFFFFF","#C6DBEF","#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
          title = "Robbery 2023") +
  tm_borders( lwd = 1, lty = "solid", alpha = 0.1)+
  tm_text("name_abr", size = 0.6,col = "black")+
  tm_style("col_blind")+
  tm_layout(frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA)


# Tables difference 2016 Vs 2023 ------------------------------------------------------
eng_robbery %>% 
  st_drop_geometry() %>%
  select("PFA21NM","Mid 2010", "Robbery_2016","Robbery_2023", "diff") %>% 
  arrange(desc(diff)) %>%
  head()

eng_robbery %>% 
  st_drop_geometry() %>%
  select("PFA21NM","Mid 2010", "Robbery_2016_ratio","Robbery_2023_ratio", "diff_ratio") %>% 
  arrange(desc(diff_ratio)) %>%
  head()

###############################################################
#                    Robberies in England                    #
###############################################################

england_ts <- df %>% filter(grepl("^E23", df$Area.Code, ignore.case = TRUE)) %>% group_by(year) %>%
  summarise(total = sum(Robbery, na.rm = T))

ggplot(england_ts, aes(x= year, y = total)) +
  geom_col( fill ="#084594") +
  ggtitle(label = "Number of Robberies in England")+
  labs(y = "Robberies")+
  theme_classic()


###############################################################
#                 Import Inflation data                       #
###############################################################

# Inflation: https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/l55o/mm23

inflation <- read_csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l55o/mm23", skip = 28)

names(inflation) <- c("year", "inflation")

inflation <- inflation %>% filter(year %in% 2015:2022)

# Assume there is a 1 year lag for the impact on robberies 
inflation$year <- as.integer(inflation$year) + 1

england_ts <- left_join(x=england_ts, y=inflation, by= "year")

# spearman correlation robberies Vs inflation
rho <- cor(england_ts$total, england_ts$inflation, method = "spearman")

# Plot that compares robberies and inflation over time 
england_ts %>% reshape2::melt(id.vars= "year") %>%
ggplot(aes(x= year, y = value , fill = variable)) +
  geom_col() +
  facet_wrap(~variable, scales = "free")+
  scale_fill_manual(values = c(  "#084594","#9ECAE1"))+
  labs(y = "", fill = "")+ 
  theme_classic()


###############################################################
#    !DEMO ONLY!       Simple Forecast    !DEMO ONLY!         #
###############################################################

library(fpp3)

ts <- england_ts %>%
  as_tsibble(index = year)

ts <- england_ts %>% mutate(year = lubridate::dmy(paste0("31-03-", year))) %>%
  mutate(year= yearmonth(year)) %>%
  as_tsibble(index = year)

#############

ts |>
  model(
    `Holt's method` = ETS(total ~ error("A") +
                            trend("A") + season("N")),
    `Damped Holt's method` = ETS(total ~ error("A") +
                                   trend("Ad", phi = 0.9) + season("N"))
  ) |>
  forecast(h = 5) |>
  autoplot(ts, level = NULL) +
  labs(title = "Robberies forecast",
       y = "robberies") +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_classic()

###############################################################
#                          END                                #
###############################################################
