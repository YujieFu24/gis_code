#load package
library(here)
library(sf)
library(countrycode)
library(dplyr)
library(janitor)

#read data
World<-st_read(here::here("hw4","World_Countries__Generalized_.shp")
HDI <- read_csv(here::here("hw4", "HDR21-22_Composite_indices_complete_time_series.csv"),locale = locale(encoding = "latin1"), na = " ", skip=0)

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