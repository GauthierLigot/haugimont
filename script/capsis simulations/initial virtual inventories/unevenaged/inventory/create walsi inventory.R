rm(list=ls())

library(tidyverse)
library(sf)

load("2_processing/1_calc_inventories/1_calcData.Rdata")

dico_species <- readxl::read_excel(path ="8_Simulations/unevenaged/inventory/dico_species.xlsx")
# dico_sp<-data.frame(sp1 = unique(structureByBois$species))
# write.table(dico_sp, file="clipboard", sep=";", row.names = F)

# selection
inventoryYear <- 1992
inventoryBois <- "Haugimont"
simulatedArea_ha <- 10


selection<-structureByBois%>%
  as_tibble()%>%
  select(-geom)%>%
  filter(year == inventoryYear & bois == inventoryBois)%>%
  select(bois, year, species, circ, bois_ha, n)%>%
  left_join(dico_species)

area_ha<-as.numeric(selection$bois_ha[1])

walsi_inventory <- selection%>%
  group_by(species_simreg, circ)%>%
  summarise(#count = n(),
            n = sum(n) / area_ha * simulatedArea_ha)%>%
  filter(n>0.49)

walsi_inventory%>%
  ungroup()%>%
  mutate(gha = n * (circ/100)^2 /4/pi/simulatedArea_ha)%>%
  summarise(gha = sum(gha))


### generate also WALSI inventory
keywords<-list()%>%
  bind_rows(tibble(keyword = "standName", value = paste(inventoryBois)))%>%
  bind_rows(tibble(keyword = "inventoryDate", value =  paste(inventoryYear)))%>%
  bind_rows(tibble(keyword = "standArea_ha", value = paste(simulatedArea_ha)))%>%
  bind_rows(tibble(keyword = "simulatedArea_ha", value = paste(simulatedArea_ha)))%>%
  bind_rows(tibble(keyword = "altitude", value = "300"))%>%
  bind_rows(tibble(keyword = "classWidth", value = "10"))%>%
  bind_rows(tibble(keyword = "naturalRegion", value = "5"))%>%
  bind_rows(tibble(keyword = "soilWaterHoldingCapacity_mm", value = "83"))%>%
  bind_rows(tibble(keyword = "polygon", value = "MULTIPOLYGON (((0 0,100 0,100 100,0 100)))"))

fileConn<-file("8_Simulations/unevenaged/inventory/haugimont1992.inv")

# keywords
keywordlines<-c()
for(i in 1:nrow(keywords)){
  line = paste(keywords$keyword[i],"=",keywords$value[i])
  keywordlines<-c(keywordlines,line)
}

# trees
Tree_title <- paste0("#",paste(names(walsi_inventory),collapse="\t"))
Tree_lines <- c()

for(r in 1:nrow(walsi_inventory)){
  line<- paste(walsi_inventory[r,],collapse="\t")
  Tree_lines<-c(Tree_lines,line)
}   

alllines<-c(keywordlines,"\n",Tree_title,Tree_lines)

writeLines(alllines, fileConn)
close(fileConn)
  

