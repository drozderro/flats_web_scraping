library(tidyverse)
library(skimr)

setwd('/home/pawel/Documents/NAUKA/Projekty/scrapping_olx')
load('oferty_19_03_13.RData')

oferty <- as_tibble(oferty)


oferty %>% skim()

# usun±æ duplikaty, zamieniæ wielkrotne spacje na pojedyncze
oferty <- oferty %>% 
  mutate(opis = str_replace_all(opis, '[:space:]+', ' '))

# regex do wyci±gania ulic 
regex = '(ulica|ul\\.|ul|ulicy|adres|pl\\.|pl|plac|placu)[:space:][:alpha:]+([:punct:]|[:graph:]+ [:graph:]+)'

oferty <- oferty %>% 
  mutate(adres = str_extract(opis, regex))


oferty %>% 
  filter(str_detect(adres, '([:digit:]+)')) %>% 
  select(adres)


  mutate(tmp = str_trim(str_replace(adres,'[:punct:]', ' '))) %>% 
  select(adres, tmp