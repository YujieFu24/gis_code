### Ineqaulity task - week 4
## data sources:
#HDI data from: https://hdr.undp.org/data-center/documentation-and-downloads
#Shapefile from: https://hub.arcgis.com/datasets/2b93b06dc0dc4e809d3c8db5cb96ba69_0/explore?location=-2.688200%2C0.000000%2C1.41 

#load package
library(here)
library(sf)
library(countrycode)
library(dplyr)
library(janitor)
library(tidyverse)

#read data
World <- st_read("hw4/World_Countries_(Generalized)/World_Countries__Generalized_.shp")
HDI <- read_csv("hw4/HDR21-22_Composite_indices_complete_time_series.csv",
                locale = locale(encoding = "latin1"),
                na = "n/a",
                skip=0)

#select columns(2019,2010)
HDI1<-select(HDI, country, hdi_2019, hdi_2010, iso3)

#calculate difference
HDI1$dif<-HDI1$hdi_2019-HDI1$hdi_2010

#change iso(country name)
#map data iso are two, but HDI are three
HDI2<-mutate(HDI1,iso2=countrycode(iso3, origin = 'iso3c', destination = 'iso2c'))

#join data
final<-World%>%
  clean_names()%>%
  left_join(.,HDI2,by=c("iso"="iso2"))

#show map
library(tmap)
library(tmaptools)

tmap_mode("plot")
qtm(final, 
    fill = "dif")

###lecture code
## Load packages
library(tidyverse)
library(sf)
library(here)
library(janitor)
library(countrycode)

HDI <- read_csv(here::here("hw4", "HDR21-22_Composite_indices_complete_time_series.csv"),locale = locale(encoding = "latin1"), na = " ", skip=0)
World<-st_read(here::here("hw4","World_Countries__Generalized_.shp")

## Column names
HDIcols<- HDI %>%
clean_names()%>%
select(iso3, country, gii_2019, gii_2010)%>%
mutate(difference=gii_2019-gii_2010)%>%

#not needed here as we can now use the country name...but see below
mutate(iso_code=countrycode(country, origin = 'country.name', destination = 'iso2c'))%>%
mutate(iso_code2=countrycode(iso3, origin ='iso3c', destination = 'iso2c'))

#Join the csv to world shape file
Join_HDI <- World %>% 
  clean_names() %>%
  left_join(., 
            HDIcols,
            by = c("iso" = "iso_code"))
# 261 if using "aff_iso", 251 if using "iso". Could filter out the NA values.
Join_HDI_2 <- World %>% 
  clean_names() %>%
  left_join(., 
            HDIcols,
            by = c("country" = "country"))

Join_HDI_FR<-Join_HDI %>%
  filter(aff_iso=="FR")
Join_HDI_2_FR<-Join_HDI_2 %>%
  filter(aff_iso=="FR")

tmap_mode("plot")
giimap <- tm_shape(Join_HDI) +
  tm_polygons("difference",
              style = "jenks",
              palette = "RdBu") +
  tm_layout(main.title = "Change in Gender Inequality Index, 2010 to 2019",
            main.title.position = "center",
  )
giimap

               
