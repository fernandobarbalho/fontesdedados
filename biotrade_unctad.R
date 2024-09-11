library(tidyverse)

url_add<- "https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20102011"

https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20122013

https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20142015

https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20162017

https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20182019

https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20202020


url_add<-
"https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20222022"

 https://unctadstat-api.unctad.org/bulkdownload/US.BiotradeMerch/US_BiotradeMerch_20222022


tmp = tempfile(fileext = ".7Z")

download.file(url_add,mode = "wb", destfile = "biotrade.7Z", extra = "-R", method = "wininet")

#C:\\Users\\FERNAN~1.BAR\\AppData\\Local\\Temp\\RtmpGc2nEz\\file34cc27a2fe8.7Z

tmp = tempfile(fileext = ".7Z")

download.file(url_add,mode = "wb", destfile = tmp, extra = "-R", method = "wininet")

library(archive)
arquivo<- archive("biotrade.7z")

archive_extract(archive = arquivo)

system_com<- paste('7z e -ounctad',"biotrade.7z")

system(system_com)

fab<-
read.csv(url_add)

library(jsonlite)


jsonlite::fromJSON(url_add)

unzip("biotrade.zip")

unctad<-
data.table::fread("biotrade/US_BiotradeMerch_20222022_20231120021515.csv")


unctad<- janitor::clean_names(unctad)

summary(unctad)

unique(unctad$product_label)


unctad %>%
  slice_sample(n=10) %>%
  mutate(grupo = product)
  
  
  mutate(grupo = ifelse(str_detect(product_label, "[;]", negate = TRUE), 
                        product_label, 
                        str_extract(product_label, ".+(?=[;])"))) 
  
  
glimpse(unctad)
  

str_extract("Vegetable saps and extracts; of ephedr", ".+(?=[;])")


str_extract("Vegetable saps and extracts; of ephedr", ".+")

str_extract("Glands and other organs; heparin and its salts; other human or animal substances prepared for therapeutic or prophylactic uses, n.e.c. in heading", 
            ".+(?=[;])")


BioTrade_Categories <- read_csv("BioTrade_Categories.csv")


categorias<- BioTrade_Categories$Category

brazil_sample<-
unctad %>%
  filter(product_label %in% categorias,
         economy_label == "Brazil") %>%
  slice_sample(n= 20)


str_detect("Vegetable saps and extracts of ephedr", "[;]")


ifelse(str_detect("Vegetable saps and extracts; of ephedr", "[;]", negate = TRUE), 
       "Vegetable saps and extracts; of ephedr", 
       str_extract("Vegetable saps and extracts; of ephedr", ".+(?=[;])"))
