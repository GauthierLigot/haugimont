#clean and translate data (if needed)
rm(list=ls())

library(tidyverse)
library(sf)

# this script call data stored outside the publication directory
# this script is not intended to be shared... just here to ensure tracability
load("c:/OneDrive/OneDrive - Universite de Liege/PROJECT/HAUGIMONT/4_ANALYSE ECONOMIQUE/2_processing/1_calc_inventories/1_CalcData.Rdata") 
load("c:/OneDrive/OneDrive - Universite de Liege/PROJECT/HAUGIMONT/4_ANALYSE ECONOMIQUE/2_processing/1_calc_inventories/3_calcRevenues.Rdata")

### English dico ####
dico_block<-bois.sf%>%
  as_tibble()%>%
  mutate(block = c(4,5,2,1,3))%>%
  mutate(block_name = paste("Block",block))%>%
  select(bois,block, block_name)

dico_species<-structureByBois%>%
  select(species_group)%>%
  distinct()%>%
  mutate(species_group_en = c("Other broadleaved",
                              "Birch",
                              "Hornbeam",
                              "Oak",
                              "Mapple",
                              "Ash",
                              "Beech",
                              "Coniferous"))
dico_fluxes<-fluxes%>%
  select(category)%>%
  distinct()%>%
  mutate(category_en = c("Estate purchase","Fence","Plantation","Vehicle","Road","Staff","Tax","Hunting",
                         "Fishing","Subsidy","Timber sales","Standing value"))%>%
  bind_rows(data.frame(category = "Personnel", category_en = "Staff"))

### convert data - CaclData.Rdata ###
block_sf<-bois.sf%>%
  left_join(dico_block)%>%
  rename(block_area_ha = bois_ha)%>%
  select(block, block_name, block_area_ha, geom)

dendro_block<-dendroByBois%>%
  left_join(dico_block)%>%
  rename(block_area_ha = bois_ha,
         n_sp = n.sp)%>%
  select(block, block_name, block_area_ha, year:n_sp, nha:vcha, geom)
  
dendro_zone <- dendroByZone%>%
  left_join(dico_block)%>%
  rename(zone_area_ha = inventoryZone_ha,
         n_sp = n.sp,
         zone_name = inventoryZone)%>%
  select(block, block_name, zone_name, zone_area_ha, year:n_sp,nha:vcha,geom)

struct_block<-structureByBois%>%
  left_join(dico_block)%>%
  left_join(dico_species)%>%
  rename(block_area_ha = bois_ha)%>%
  select(block, block_name, block_area_ha, year, species_group_en, circ:s, nha:vcha, geom)
  
struct_zone<-structureByZone%>%
  left_join(dico_block)%>%
  left_join(dico_species)%>%
  rename(zone_area_ha = inventoryZone_ha,
         zone_name = inventoryZone)%>%
  select(block, block_name, zone_name, zone_area_ha, year, species_group_en, circ:s, nha:vcha, geom) # s = stumpage value

inventory_year<-inventoryYear%>%
  left_join(dico_block)%>%
  select(block, block_name, year)

zone_sf <- inventoryZone.sf%>%
  select(zone_name = inventoryZone, zone_area_ha = inventoryZone_ha, geom)

price_list<-mercuriale%>%
  left_join(dico_species)%>%
  select(species_group_en, circ, price_2019)

save(block_sf, dendro_block, dendro_zone, struct_block, struct_zone, inventory_year, zone_sf, price_list, file= "c:/OneDrive/OneDrive - Universite de Liege/PROJECT/HAUGIMONT/8_PAPER/data/1_CalcData_en.Rdata")
save(price_list, file= "c:/OneDrive/OneDrive - Universite de Liege/PROJECT/HAUGIMONT/8_PAPER/data/price_list.Rdata")

### convert data - 3_calcRevenues.Rdata ###
cashflows<-fluxes%>%
  left_join(dico_block)%>%
  left_join(dico_fluxes)%>%
  select(-category)%>%
  mutate(produit = if_else(produit == "chauffage","Firewood",
                           if_else(produit == "œuvre","Log",NA)))%>%
  select(block, block_name, block_area_ha = bois_ha, year, category = category_en,
         net_2019 = revenue_2019, 
         net_nominal = revenue_nominal,
         net_2019_ha = revenue_2019_ha,
         net_nominal_ha = revenue_nominal_ha,
         product = produit, volume, volume_ha, period, geom)

estate_purchase<-purchase.property%>%
  left_join(dico_block)%>%
  left_join(dico_fluxes)%>%
  select(-category)%>%
  select(block, block_name, year, category = category_en, net_2019 = revenue_2019, net_nominal = revenue_nominal)

save(cashflows, estate_purchase, file= "c:/OneDrive/OneDrive - Universite de Liege/PROJECT/HAUGIMONT/8_PAPER/data/3_calcRevenues_en.Rdata")

  