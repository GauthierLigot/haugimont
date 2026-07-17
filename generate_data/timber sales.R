### explore timber sales
rm(list=ls())
setwd("c:/OneDrive/OneDrive - Universite de Liege/PROJECT/HAUGIMONT/8_PAPER/")

library(readxl)
library(tidyverse)

load(file= "data/1_CalcData_en.Rdata")
load(file= "data/price_list.Rdata")

price_indices <- read_excel(path="generate_data/IPC-Coefficients2024.xlsx",sheet = "Feuil2")%>%
  select(year = Année, price_coef = 'Coefficient 2019') 

dico_block<- tibble( bois = c("Abbaye","Gesves","Haugimont","Maizeroulle","Strud"),
                     block = c(4,5,2,1,3))%>%
                    mutate(block_name = paste("Block",block))%>%
  select(bois,block, block_name)

dico_species_categ <- read_xlsx("generate_data/dico_species_categ_edited.xlsx")%>%
  select(-n)

sales<-read_xlsx(path = "../4_ANALYSE ECONOMIQUE/1_data/fromGDD/ventes.xlsx", sheet = "ventes_détail")%>%
  mutate(Bois = str_to_sentence(Bois))%>%
  select(bois = Bois, year = Année, volume = Vol_m3, nominal_price = prix_courant, species_categ = essence, wood_categ = type_bois, comment = caractéristiques)%>%
  filter(wood_categ != "prime")%>%
  left_join(dico_species_categ, join_by(species_categ))%>%
  mutate(category = if_else(wood_categ == "chauffage","Firewood",species_group_en))%>%
  select(-species_categ, -wood_categ, -species_group_en)%>%
  left_join(price_indices, by = join_by(year))%>%
  mutate(real_price_2019 = nominal_price * price_coef)%>%
  mutate(nominal_price_m3 = nominal_price/volume,
         real_price_2019_m3 = real_price_2019/volume)


save(sales, file = "data/sales.Rdata")

