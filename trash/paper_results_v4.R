rm(list=ls())

library(tidyverse)
library(sf)
library(mapview)
library(DT)
library(knitr)
library(xlsx)
library(ggpubr)
library(readxl)
library(ggradar)
library(RColorBrewer)
# load("1_data/inventories.rdata")

source("c:/OneDrive/OneDrive - Universite de Liege/TOOLS/R/myGGplotConfigs.R")
load("2_processing/1_calc_inventories/1_CalcData.Rdata") 
load("2_processing/1_calc_inventories/3_calcRevenues.Rdata")
source("2_processing/functions/tirf.R")

#parameters
source("2_processing/3_economicScenario/commonParameters.R")
forest_rent<-fonds ### forest rent. strong assumption !!!
discount_rate<-r<-0.02
inflation_rate<-0.02


achat<-fluxes%>%
  filter(category == "Achat de la propriété")%>% #estate purchase in French
  select(bois, purchaseYear = year, montant = revenue_2019_ha)

datesclefs<-inventoryYear%>%
  filter(year >= 1992)%>%
  group_by(bois)%>%
  filter(year == min(year))%>%
  rename(firstInventory = year)%>%
  left_join(achat[,-3], by=join_by(bois))

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
  mutate(category_en = c("Estate purchase","Fence","Plantation","Vehicle","Road","Management","Tax","Hunting",
                         "Fishing","Subisdy","Timber sales","Standing value"))%>%
  bind_rows(data.frame(category = "Personnel", category_en = "Staff"))

### MAP ####
mapview(bois.sf,zcol = "bois",layer.name = 'forest blocks', label = "bois")
# st_write(bois.sf,dsn = "bois.gpkg",append = F)

### Results ----
#### initial situation ----
t1<-dendroByBois%>%
  as_tibble()%>%
  left_join(dico_block,by=join_by(bois))%>%
  group_by(block)%>%
  filter(year>= 1992)%>%
  filter(year == min(year))%>%
  select(block, year, nha, gha, vha, vcha, ghapc_oak, ghapc_frax, ghapc_conif)%>%
  mutate_at(c("ghapc_oak","ghapc_frax","ghapc_conif"), function(x) {x *100})%>%
  ungroup()%>%
  mutate_if(is.numeric, ~round(.,digits=1))%>%
  as.data.frame()

colnames(t1) <- c("Block", "Year","Density (/ha)","Basal area (m²/ha)", "Standing volume (m³/ha)", 
                  "Standing value (€/ha)", "%Oak", "%Ash", "%Conif.")

write.xlsx(t1,sheetName = "t1_dendro",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F)

#### Final situation ----
t2<-dendroByBois%>%
  as_tibble()%>%
  left_join(dico_block,by=join_by(bois))%>%
  group_by(block)%>%
  filter(year>= 1992)%>%
  filter(year == max(year))%>%
  select(block, year, nha, gha, vha, vcha, ghapc_oak, ghapc_frax, ghapc_conif)%>%
  mutate_at(c("ghapc_oak","ghapc_frax","ghapc_conif"), function(x) {x *100})%>%
  ungroup()%>%
  mutate_if(is.numeric, ~round(.,digits=1))%>%
  as.data.frame()

colnames(t2) <- c("Block", "Year","Density (/ha)","Basal area (m²/ha)", "Standing volume (m³/ha)", 
                  "Standing value (€/ha)", "%Oak", "%Ash", "%Conif.")

write.xlsx(as.data.frame(t2),sheetName = "tf_dendro",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append=T)

#### Proportion of coniferous basal area -----
tt<-dendroByBois%>%
  as_tibble()%>%
  group_by(bois)%>%
  filter(year>= 1992)%>%
  filter(year == min(year))%>%
  select(bois, bois_ha, year, nha, gha, vha, vcha, ghapc_oak, ghapc_frax, ghapc_conif)
weighted.mean(x=tt$ghapc_conif,w=as.numeric(tt$bois_ha))

#### Evolution ----
tx<-dendroByBois%>%
  filter(year >= 1992)%>%
  left_join(dico_block,by=join_by(bois))%>%
  group_by(block)%>%
  arrange(year)%>%
  summarise(vha_i = first(vha),
            vha_f = last(vha),
            vcha_i = first(vcha),
            vcha_f = last(vcha),
            n = last(year) - first(year))%>%
  mutate(tx_vha = (vha_f/vha_i)^(1/n)-1,
         tx_vcha = (vcha_f/vcha_i)^(1/n)-1)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##### Stand structure across inventories ----
g1<-structureByBois%>%
  left_join(dico_block,by=join_by(bois))%>%
  left_join(dico_species, by=join_by(species_group))%>%
  ggplot(aes(y=gha,x=circ/pi,fill=species_group_en))+geom_bar(stat="identity")+
  facet_grid(block_name ~ year)+labs(fill = "Species group")+
  theme(legend.position="top")+xlab("Diameter class (cm)")+ylab("Basal area (m²/ha)")+
  scale_fill_manual(values=cbPalette)

ggsave(file="6_paper/structure.png", plot=g1, width=10, height=8)
ggsave(file="6_paper/structure.svg", plot=g1, width=10, height=8)


##### Harvests ----
periodsDico<-dendroByBois%>%
  as_tibble()%>%
  left_join(dico_block,by=join_by(bois))%>%
  select(block,year)%>%
  distinct()%>%
  group_by(block)%>%
  arrange(year)%>%
  mutate(year_i = year,
         year_f = lead(year),
         period = row_number())%>%
  select(-year)%>%
  filter(!is.na(year_f))%>%
  group_by(block, period, year_i, year_f)%>%
  nest()%>%
  mutate(year = map2(.x = year_i, .y = year_f, ~ .x:(.y-1)))%>%
  ungroup()%>%
  select(block, period, year, year_i, year_f)%>%
  unnest(cols = c(block, year, year_i, year_f))

harvested<-fluxes%>%
  left_join(dico_block,by=join_by(bois))%>%
  filter(year >= 1992)%>%
  filter(category == "Ventes de bois")%>%
  group_by(block,year)%>%
  summarise(volume = sum(volume),
            revenue_2019 = sum(revenue_2019))%>%
  left_join(periodsDico, by = join_by(block, year))%>%
  group_by(block)%>%
  mutate(year_f = ifelse(is.na(year_f),min(year_i,na.rm=T),year_f))%>%
  group_by(block,year_f)%>%
  summarise(volume = sum(volume),
            revenue_2019 = sum(revenue_2019))

t2a<-dendroByBois%>%
  left_join(dico_block,by=join_by(bois))%>%
  as_tibble()%>%
  select(-geom)%>%
  select(block,bois,bois_ha,year,nha,gha,vha)%>%
  left_join(harvested,join_by(block,year==year_f))%>%
  mutate(vha_harvested = volume/bois_ha,
         vha_harvested = ifelse(is.na(vha_harvested),0,vha_harvested))%>%
  group_by(block)%>%
  arrange(year)%>%
  mutate(year_i = lag(year),
         year_f = year,
         vha_i = lag(vha),
         vha_f = vha,
         vha_change = (vha_f - vha_i)/(year_f -year_i),
         vha_harvested_yr = vha_harvested / (year_f - year_i),
         vha_yield = vha_change +  vha_harvested_yr,
         n = row_number())%>%
  ungroup()%>%
  arrange(block,year)%>%
  filter(year_i >= 1992 & n > 1)

t2b<-t2a%>%
  group_by(bois_ha,block)%>%
  summarise(vha_change = mean(vha_change),
            vha_harvested_yr = mean(vha_harvested_yr),
            vha_yield = mean(vha_yield))

# weighted.mean(x=t2b$vha_change,w=as.numeric(t2b$bois_ha))
# weighted.mean(x=t2b$vha_harvested_yr,w=as.numeric(t2b$bois_ha))
# weighted.mean(x=t2b$vha_yield,w=as.numeric(t2b$bois_ha))


t2<-t2a%>%
  select(block, year_i, year_f, vha_i, vha_f, vha_harvested_yr, vha_yield)%>%
  mutate(tx_arvested = vha_harvested_yr/vha_yield)%>%
  mutate_at(c("vha_i","vha_f","vha_harvested_yr","vha_yield"), ~round(.,digits=1))%>%
  mutate(tx_arvested = round(tx_arvested,digits=2))%>%
  as.data.frame()

colnames(t2) <- c("Block","Year_i","Year_f","Vol_i (m³/ha)","Vol_f (m³/ha)","Vol_cut (m³/ha/an)","Yield (m³/ha/an)","Taux Prélèvement")

write.xlsx(t2, sheetName = "t2_vha",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append=T)


  

##### Yield in each block ----
t2a<-dendroByBois%>%
  filter(year >= 1992 )%>%
  as_tibble()%>%
  select(-geom)%>%
  left_join(dico_block,by=join_by(bois))%>%
  select(block,bois,bois_ha,year,nha,gha,vha, vcha)%>%
  left_join(harvested,join_by(block,year==year_f))%>%
  mutate(vha_harvested = volume/bois_ha,
         vha_harvested = ifelse(is.na(vha_harvested),0,vha_harvested),
         vm_harvested = revenue_2019/bois_ha,
         vm_harvested = ifelse(is.na(vm_harvested),0,vm_harvested))%>%
  group_by(block)%>%
  arrange(year)%>%
  summarise(year_i = first(year),
         year_f = last(year),
         vha_i = first(vha),
         vha_f = last(vha),
         vm_i = first(vcha),
         vm_f= last(vcha),
         vha_harvested = sum(vha_harvested),
         vm_harvested = sum(vm_harvested))%>%
  mutate(vha_change = (vha_f - vha_i)/(year_f -year_i),
         vha_harvested_yr = vha_harvested / (year_f - year_i),
         vha_yield = vha_change +  vha_harvested_yr,
         tx_harvested_vol = vha_harvested_yr/vha_yield,
         vm_change = (vm_f - vm_i)/(year_f -year_i),
         vm_harvested_yr = vm_harvested / (year_f - year_i),
         vm_yield = vm_change +  vm_harvested_yr,
         tx_harvested_vm = vm_harvested_yr/vm_yield)%>%
  as.data.frame()


write.xlsx(t2a, sheetName = "t2_vha_global",
           file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append=T)

### turnover period
### volume
t2a%>%
  mutate(harvestToCapital_volume = vha_harvested/vha_i,
         harvestToCapital_value =  vm_harvested/vm_i,
         harvestAndIncrementToCapital_value =  (vm_harvested+(vm_f - vm_i))/vm_i)%>%
  select(block,harvestToCapital_volume,harvestToCapital_value,harvestAndIncrementToCapital_value )


#### Proffitability indicators ------
##### Outflows -----

fpalette<- c("#E69F00", "#999999", "#009E73", "#D55E00", "#CC79A7","#333399")

g2<-fluxes%>%
  left_join(dico_block,by=join_by(bois))%>%
  left_join(dico_fluxes,by=join_by(category))%>%
  filter(!is.na(revenue_2019_ha) & revenue_2019_ha<0)%>%
  filter(category != "Achat de la propriété")%>%
  filter(category != "Capital sur pied")%>%
  ggplot(aes(y=revenue_2019_ha,x=year,fill=category_en))+facet_wrap(~block_name)+
  geom_bar(stat = "identity")+ylab("Expenses (€ 2019/ha)")+
  theme(axis.title.x = element_blank(),legend.position = "top") + labs(fill = element_text("Expense category"))+
  geom_vline(data=datesclefs, aes(xintercept = purchaseYear),  col="darkgrey")+
  geom_vline(data=datesclefs, aes(xintercept = firstInventory) , col="darkgrey",lty=2)+
  scale_fill_manual(values=fpalette)

ggsave(file="6_paper/depense.png", g2, width=8, height=6)

# outflow by block and category
dm<-fluxes%>%
  filter(year >= 1992)%>%
  filter(!is.na(revenue_2019_ha) & revenue_2019_ha<0)%>%
  filter(category != "Achat de la propriété")%>%
  filter(category != "Capital sur pied")%>%
  left_join(achat[,-3], by = join_by(bois))%>%
  mutate(period = 2019 - ifelse(purchaseYear>1992,purchaseYear,1992)  + 1)%>%
  group_by(bois, category, period)%>%
  summarise(
    montant = sum(revenue_2019_ha)/period[1] )%>%
  mutate(isIncome = "Dépense")%>%
  mutate(category = ifelse(category=="Gestion","Personnel",category))

# outflow by block
dm%>%
  left_join(dico_block,by=join_by(bois))%>%
  # filter(category != "Voiries")%>%
  group_by(block)%>%
  summarise(montant = sum(montant))

fpalette<- c("#999999", "#E69F00", "#009E73", "#D55E00", "#CC79A7")
fpalette<- rev(c("#999999", "#009E93", "#CC99A7","#9999FF"))

g3outflows<-dm%>%
  left_join(dico_block,by=join_by(bois))%>%
  left_join(dico_fluxes,by=join_by(category))%>%
  mutate(category_en = factor(category_en, levels = 
                                c("Vehicle","Tax","Road","Staff"),
                              order=T))%>%
  ggplot(aes(y=-montant,x=block_name,fill=category_en))+
  geom_bar(stat = "identity")+ylab("Mean annual cost (€/ha/year)")+
  theme(axis.title.x = element_blank(),legend.position = "top") + 
  labs(fill = element_text(""))+
  scale_fill_manual(values=fpalette)+
  scale_y_continuous(limits = c(0, 550))


ggsave(file="depenseMoyenne.png", g3outflows, width=6, height=4)


dm%>%
  group_by(bois)%>%
  mutate(tot = sum(montant))

dm%>%
  group_by(bois)%>%
  mutate(tot = sum(montant))%>%
  filter(category == "Personnel")

dm%>%
  group_by(bois)%>%
  mutate(tot = sum(montant))%>%
  ungroup()%>%
  mutate(prop = montant/tot)%>%
  filter(category %in% c("Personnel"))%>%
  arrange(category, prop)

dm%>%
  group_by(bois)%>%
  mutate(tot = sum(montant))%>%
  ungroup()%>%
  mutate(prop = montant/tot)%>%
  filter(category %in% c("Voiries"))%>%
  arrange(category, prop)

dm%>%
  group_by(bois)%>%
  mutate(tot = sum(montant))%>%
  ungroup()%>%
  mutate(prop = montant/tot)%>%
  filter(category %in% c("Impôt"))%>%
  arrange(category, prop)

dm%>%
  group_by(bois)%>%
  mutate(tot = sum(montant))%>%
  ungroup()%>%
  mutate(prop = montant/tot)%>%
  filter(category %in% c("Véhicule", "Impôt"))%>%
  arrange(category, prop)

##### Mean outflow at the estate level
areatot<-as.numeric(sum(bois.sf$bois_ha))

mean.expenses<-dm%>%
  left_join(as_tibble(bois.sf),by = join_by(bois))%>%
  group_by(category)%>%
  summarise(montant_estate = sum(montant * as.numeric(bois_ha) / areatot) * -1)

fpalette<- rev(c("#999999", "#009E93", "#CC99A7","#9999FF"))
mean.expenses.plot<-ggplot(data=mean.expenses,aes(x="",y=montant_estate, fill = category))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=fpalette)
  
ggsave(file="6_paper/mean_expenses_plot.svg", mean.expenses.plot, width=4, height=4)
write.xlsx(as.data.frame(mean.expenses), sheetName = "mean_expenses",
           file = "6_paper/tables_brochures.xlsx",
           col.names = T, row.names = F, showNA = F)

##### Inflows ----

fpalette<- c("#CC79A7", "#56B4E9", "#E69F00",  "#D55E00",  "#009E73")

g3<-fluxes%>%
  left_join(dico_block,by=join_by(bois))%>%
  left_join(dico_fluxes,by=join_by(category))%>%
  mutate(category_en2 = ifelse(category_en == "Timber sales",
                            ifelse(produit == "chauffage",
                                   "Firewood","Log"),
                            category_en))%>%
  filter(!is.na(revenue_2019_ha) & revenue_2019_ha>0)%>%
  filter(category != "Capital sur pied")%>%
  mutate(category_en = factor(category_en, levels = 
                                c("Subisdy","Fishing","Hunting","Firewood","Log"),
                              order=T))%>%
  ggplot(aes(y=revenue_2019_ha,x=year,fill=category_en2))+facet_wrap(~block_name)+
  geom_bar(stat = "identity")+ylab("Mean income (€ 2019/ha)")+
  theme(axis.title.x = element_blank(),legend.position = "top") + labs(fill = element_text("Income category"))+
  geom_vline(data=datesclefs, aes(xintercept = purchaseYear), col="darkgrey")+
  geom_vline(data=datesclefs, aes(xintercept = firstInventory), col="darkgrey",lty=2)+
  scale_fill_manual(values=fpalette)

ggsave(file="recette.png", g3, width=8, height=6)

# inflow by block and category
rm<- fluxes%>%
  mutate(category = ifelse(category == "Ventes de bois",
                            ifelse(produit == "chauffage",
                                   "Bois de chauffage","Bois d'oeuvre"),
                            category))%>%
  filter(!is.na(revenue_2019_ha) & revenue_2019_ha>0)%>%
  filter(category != "Capital sur pied")%>%
  left_join(achat[,-3], by = join_by(bois))%>%
  mutate(period = 2019 - ifelse(purchaseYear>1992,purchaseYear,1992)  + 1)%>%
  group_by(bois, category)%>%
  summarise(montant = sum(revenue_2019_ha)/period[1])%>%
  mutate(isIncome = "Recette")

# share of sawwood and firewood sales among inflows
rm%>%
  group_by(bois)%>%
  mutate(tot = sum(montant))%>%
  ungroup()%>%
  mutate(prop = montant/tot)%>%
  filter(category %in% c("Bois de chauffage","Bois d'oeuvre"))%>%
  arrange(category, prop)

# share of total wood sales among inflows
rm%>%
  group_by(bois)%>%
  mutate(tot = sum(montant))%>%
  ungroup()%>%
  mutate(prop = montant/tot)%>%
  filter(category %in% c("Bois de chauffage","Bois d'oeuvre"))%>%
  arrange(category, prop)%>%
  group_by(bois)%>%
  summarise(prop = sum(prop),
            timber_tot = sum(montant))

# Share of firewood and sawwood among total wood sales
rm%>%
  group_by(bois)%>%
  filter(category %in% c("Bois de chauffage","Bois d'oeuvre"))%>%
  mutate(tot = sum(montant))%>%
  ungroup()%>%
  mutate(prop = montant/tot)%>%
  arrange(category, prop)

# total inflow related to wood sales
rm%>%
  filter(category %in% c("Bois de chauffage","Bois d'oeuvre"))%>%
  arrange(category)%>%
  group_by(bois)%>%
  summarise(tot = sum(montant))

# total inflow related to hunting and fishing
rm%>%
  filter(category %in% c("Chasse"))%>%
  arrange(category)%>%
  group_by(bois)%>%
  summarise(tot = sum(montant))

# Share of hunting inflows in total inflows
rm%>%
  group_by(bois)%>%
  mutate(tot = sum(montant))%>%
  ungroup()%>%
  mutate(prop = montant/tot)%>%
  filter(category %in% c("Chasse"))%>%
  arrange(category, prop)

# Recettes de la pêche
rm%>%
  filter(category %in% c("Pêche"))%>%
  arrange(category)%>%
  group_by(bois)%>%
  summarise(tot = sum(montant))

# Proportion de la pêche dans les recettes totales
rm%>%
  group_by(bois)%>%
  mutate(tot = sum(montant))%>%
  ungroup()%>%
  mutate(prop = montant/tot)%>%
  filter(category %in% c("Pêche"))%>%
  arrange(category, prop)

# Subside
rm%>%
  filter(category %in% c("Subsides"))%>%
  arrange(category)%>%
  group_by(bois)%>%
  summarise(tot = sum(montant))

# Proportion des subsides dans les recettes totales
rm%>%
  group_by(bois)%>%
  mutate(tot = sum(montant))%>%
  ungroup()%>%
  mutate(prop = montant/tot)%>%
  filter(category %in% c("Subsides"))%>%
  arrange(category, prop)

t9_bilan<-dm%>%
  bind_rows(rm)%>%
  left_join(dico_block,by=join_by(bois))%>%
  left_join(dico_fluxes,by=join_by(category))%>%
  ungroup()%>%
  mutate(isIncome = ifelse(isIncome == "Dépense","Expense","Income"))%>%
  select(block, category_en, period, value = montant, isIncome)

write.xlsx(as.data.frame(t9_bilan), sheetName = "t9_bilan",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append = T)

t10_bilanTot<-t9_bilan%>%
  group_by(block,isIncome)%>%
  summarise(val = sum(value))%>%
  pivot_wider(id_cols = block, names_from = isIncome, values_from = val)%>%
  mutate(rd = Income/Expense,
         net_revenue = Income + Expense)

write.xlsx(as.data.frame(t10_bilanTot), sheetName = "t10_bilanTot",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append = T)

g4income<-rm%>%
  left_join(dico_block,by=join_by(bois))%>%
  left_join(dico_fluxes,by=join_by(category))%>%
  mutate(category_en = ifelse(category == "Bois d'oeuvre","Log",category_en))%>%
  mutate(category_en = ifelse(category == "Bois de chauffage","Firewood",category_en))%>%
  mutate(category_en = factor(category_en, levels = 
                                c("Subisdy","Fishing","Hunting","Firewood","Log"),
                              order=T))%>%
  ggplot(aes(y=montant,x=block_name,fill=category_en))+
  geom_bar(stat = "identity")+ylab("Mean annual revenue (€/ha/year)")+
  theme(axis.title.x = element_blank(),legend.position = "top") + labs(fill = element_text(""))+
  scale_fill_manual(values=fpalette)+
  scale_y_continuous(limits = c(0, 550))


ggsave(file="6_paper/recetteMoyenne.png", g4income, width=7, height=4)

flows<-ggarrange(g3outflows, g4income, labels = c("A", "B"), ncol = 2, nrow = 1,
                 common.legend = FALSE)

ggsave(flows,file = "6_paper/flows.png", width=10, height = 5)


### mean income

mean.income<-rm%>%
  left_join(as_tibble(bois.sf),by = join_by(bois))%>%
  group_by(category)%>%
  summarise(montant_estate = sum(montant * as.numeric(bois_ha) / areatot))

fpalette<- c("#CC79A7", "#56B4E9", "#E69F00",  "#D55E00",  "#009E73")
mean.income.plot<-ggplot(data=mean.income,aes(x="",y=montant_estate, fill = category))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=fpalette)

ggsave(file="6_paper/mean_income_plot.svg", mean.expenses.plot, width=4, height=4)
write.xlsx(as.data.frame(mean.income), sheetName = "mean_income",
           file = "6_paper/tables_brochures.xlsx",
           col.names = T, row.names = F, showNA = F, append= T)

### Net income -----
tmp<-dm%>%
  ungroup()%>%
  group_by(bois)%>%
  summarise(tot = sum(montant))

net<-rm%>%
  bind_rows(dm)%>%
  group_by(bois)%>%
  summarise(tot = sum(montant))
  



### Valeur actuelle nette
fonds_as_fluxes<- filter(fluxes, category == "Capital sur pied")%>%
  mutate(category = "Fonds",
         revenue_2019_ha  = ifelse(revenue_2019<0,-fonds,fonds),
         revenue_2019 = revenue_2019_ha * as.numeric(bois_ha))

t3<-fluxes%>%
  bind_rows(fonds_as_fluxes)%>%
  left_join(dico_block,by=join_by(bois))%>%
  left_join(dico_fluxes,by=join_by(category))%>%
  mutate(category_en2 = ifelse(category_en == "Timber sales",
                               ifelse(produit == "chauffage",
                                      "Firewood","Log"),
                               category_en))%>%
  mutate(category_en2 = ifelse(category == "Fonds", "Capital opportunity cost", category_en2))%>%
  select(-geom)%>%
  filter(category != "Achat de la propriété")%>%
  left_join(achat[,-3], by = join_by(bois))%>%
  mutate(initialYear = ifelse(purchaseYear>1992,purchaseYear,1992))%>%
  filter((category == "Véhicule" | year >= initialYear) & revenue_2019_ha != 0)%>%
  group_by(bois,block,block_name)%>%
  mutate(year = ifelse(category == "Capital sur pied" & revenue_2019_ha<0,initialYear,year))%>%
  mutate(year2 = year-initialYear)%>% 
  mutate(va = revenue_2019_ha/(1+r)^(year-initialYear))%>%
  group_by(bois, category,block,block_name,category_en2)%>%
  summarise(van_ha = sum(va))%>%
  ungroup()%>%
  select(-bois,-block)%>%
  pivot_wider(names_from = block_name, values_from = van_ha,values_fill = 0)%>%
  arrange(`Block 2`)%>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))%>%
  mutate(across(where(is.numeric), ~round(.x, digits = 0)))

write.xlsx(as.data.frame(t3), sheetName = "t3_npv",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append = T)




### Taux interne de rentabilité
get_tir<-function(df){
  years<-df[["year2"]]
  nets<-df[["revenue_2019_ha"]]
  tir<-TIRF(year = years, net = nets, fonds = fonds, r_step = 0.0005)
  return(tir)
  }

#### il manque l'immobilisation du fonds???? non, il est fixe à 5000 #euro/ha
t4_all<-fluxes%>%
  select(-geom)%>%
  filter(category != "Achat de la propriété")%>%
  left_join(achat[,-3], by = join_by(bois))%>%
  mutate(initialYear = ifelse(purchaseYear>1992,purchaseYear,1992))%>%
  filter((category == "Véhicule" | year >= initialYear) & revenue_2019_ha != 0)%>%
  group_by(bois)%>%
  mutate(year = ifelse(category == "Capital sur pied" & revenue_2019_ha<0,initialYear,year))%>%
  mutate(year2 = year-initialYear)%>% 
  select(bois, year, year2, initialYear, revenue_2019_ha, category)%>%
  nest()%>%
  mutate(tir = map_dbl(data,get_tir),
         n = map_dbl(data, ~ max(.$year2))) %>%
  select(bois, n, tir)%>%
  mutate(TIR_nominal = (tir+1)*(1+inflation_rate)-1)%>%
  left_join(dico_block,by=join_by(bois))

write.xlsx(as.data.frame(t4_all), sheetName = "t4_tir_all",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append = T)

t4_comp<-fluxes%>%
  select(-geom)%>%
  filter(category != "Achat de la propriété")%>%
    filter(!category %in% c("Voiries","Véhicule","Chasse","Pêche","Subsides","Impôt"))%>%
  left_join(achat[,-3], by = join_by(bois))%>%
  mutate(initialYear = ifelse(purchaseYear>1992,purchaseYear,1992))%>%
  filter((category == "Véhicule" | year >= initialYear) & revenue_2019_ha != 0)%>%
  group_by(bois)%>%
  mutate(year = ifelse(category == "Capital sur pied" & revenue_2019_ha<0,initialYear,year))%>%
  mutate(year2 = year-initialYear)%>% 
  select(bois, year, year2, initialYear, revenue_2019_ha, category)%>%
  nest()%>%
  mutate(tir = map_dbl(data,get_tir),
         n = map_dbl(data, ~ max(.$year2))) %>%
  select(bois, n, tir)%>%
  mutate(TIR_nominal = (tir+1)*(1+inflation_rate)-1)%>%
  left_join(dico_block,by=join_by(bois))

write.xlsx(as.data.frame(t4_comp), sheetName = "t4_tir_comp",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append = T)

  
### Summary of all observations to be compared to simulations ------
# outflow that can be compared to simulation results (only staff, major outflow anyway)
tmp_d<-dm%>%
  left_join(dico_block,by=join_by(bois))%>%
  filter(category == "Personnel")%>%
  group_by(block)%>%
  summarise(dm = sum(montant))%>%
  select(block, dm)

tmp_r<-rm%>%
  left_join(dico_block,by=join_by(bois))%>%
  filter(category %in% c("Bois d'oeuvre","Bois de chauffage"))%>%
  group_by(block)%>%
  summarise(rm = sum(montant))%>%
  select(block, rm)

tmp_van1<-t3%>%
  filter(category_en2 %in% c("Management","Firewood","Log"))%>%
  select(-category,-category_en2)%>%
  summarise_all(.funs = sum)%>%
  pivot_longer(everything(), cols = , names_to = "block", values_to = "van1")%>%
  mutate(block = as.numeric(str_sub(block, start = -1, end = -1)))

tmp_van4<-t3%>%
  filter(category_en2 %in% c("Management","Firewood","Log","Capital opportunity cost","Standing value"))%>%
  select(-category,-category_en2)%>%
  summarise_all(.funs = sum)%>%
  pivot_longer(everything(), cols = , names_to = "block", values_to = "van4")%>%
  mutate(block = as.numeric(str_sub(block, start = -1, end = -1)))

tmp_irr<- t4_comp%>%
  ungroup()%>%
  select(block, irr5 = tir)

sumryTable<-t2a%>%
  mutate(nb_year = year_f - year_i,
         vt = vha_harvested,
         vm = vt / nb_year)%>%
  select(block, nb_year, vm, vt, yield = vha_yield)%>%
  left_join(tmp_d, by = join_by(block))%>%
  mutate(dt =dm*nb_year)%>%
  left_join(tmp_r, by = join_by(block))%>%
  mutate(rt = rm*nb_year)%>%
  left_join(tmp_van1, by = join_by(block))%>%
  left_join(tmp_van4, by = join_by(block))%>%
  left_join(tmp_irr, by = join_by(block))%>%
  mutate(npv_inf = van1 * (1+discount_rate)^nb_year/(((1+discount_rate)^nb_year-1)))%>%
  mutate(source = "Field data")
  

### Evaluating simulations ------------------
# load simulation data
load("2_processing/3_economicScenario/DOU.rdata")
load("2_processing/3_economicScenario/SPR.rdata")
load("2_processing/3_economicScenario/OAK.rdata")
load("2_processing/3_economicScenario/MIX.rdata")

# simple functions
totalIncome<-function(net){
  mi<-sum(ifelse(net>0,net,0))
  return(mi)
}
totalExpense<-function(net){
  mi<-sum(ifelse(net<0,net,0))
  return(mi)
}
meanIncome<-function(net, rotation){
  mi<-sum(ifelse(net>0,net,0))/rotation
  return(mi)
  }
meanExpense<-function(net, rotation){
  mi<-sum(ifelse(net<0,net,0))/rotation
  return(mi)
}
volumeProduction<-function(volume, rotation){
  v<-sum(volume, na.rm = T)/rotation
  return(v)
}

dico_hazard<-tibble(label = c(paste0("DOU",1:3),
                              paste0("SPR",1:3),
                              paste0("OAK",1:3),
                              paste0("OAK_SHLTR",1:3),
                              paste0("MIX",1:3)),
                    hazard = c("Hazard-free","Growth decline","Rege. difficulties",
                               "Hazard-free","Bark beetles","Rege. difficulties",
                               "Hazard-free","Growth decline","Rege. difficulties",
                               "Hazard-free","Growth decline","Rege. difficulties",
                               "Hazard-free","Growth decline","Ash dieback"))

t5<-DOU%>%
  bind_rows(SPR)%>%
  bind_rows(OAK)%>%
  bind_rows(MIX)%>%
  mutate(rm = map2_dbl(billbook,rotation, ~ meanIncome(.x$net,.y)))%>%
  mutate(dm = map2_dbl(billbook,rotation, ~ meanExpense(.x$net,.y)))%>%
  mutate(dt = map_dbl(billbook, ~ totalExpense(.x$net)))%>%
  mutate(rt = map_dbl(billbook, ~ totalIncome(.x$net)))%>%
  mutate(vm = map2_dbl(billbook,rotation, ~ volumeProduction(.x$volume, .y)))%>%
  mutate(irr5 = ifelse(is.na(irr4),irr3,irr4))%>%
  mutate(nominal_irr = (irr5+1)*(1+inflation_rate)-1)%>%
  mutate(npv_inf = van1 * (1+discount_rate)^rotation / ((1+discount_rate)^rotation - 1))%>%
  left_join(dico_hazard, by = join_by(label))%>%
  select(scenario = label, species, hazard, nb_year = rotation, 
         rt, dt, rm, dm, vm, van1, van4, npv_inf, irr5, nominal_irr)

write.xlsx(as.data.frame(t5), sheetName = "t5_comparScenarios",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append = T)

t1<-t5%>%
  select(scenario, nb_year, rt, rm, dt, dm,vm,van1,van4,npv_inf,irr5)%>%
  bind_rows(sumryTable)%>%
  mutate(rt=rt/1000,
         rm =rm/1000,
          dt = -dt/1000,
         dm = -dm/1000,
         van1 = van1/1000,
         van4 = van4/1000,
         npv_inf = npv_inf/1000,
         irr5 = irr5 * 100)%>%
  mutate(subscenario = str_sub(scenario, start = -1, end = -1))%>%
  mutate(label = ifelse(is.na(source), str_sub(scenario, start = 1, end=-2), source))%>%
  # mutate_at(vars(-scenario,-subscenario), rescale_gl)%>%
  pivot_longer(cols = nb_year:irr5,names_to = "var")%>%
  mutate(label = factor(label, levels = c("DOU","SPR","OAK","OAK_SHLTR","MIX","Field data"), ordered= T))

t2_errorbar<-t1%>%
  filter(label != "Field data")%>%
  group_by(label,var)%>%
  summarise(min = min(value),
            max = max(value))

# variable = "dm"

mySimulationPlot<-function(variable, variable_name){
  mycolors<-brewer.pal(6, "Dark2")

  g<-ggplot()+
    geom_point(data=filter(t1,(is.na(subscenario)  & var == variable) | (subscenario == "1" & var == variable)), 
               aes(x=label,y=value, col=label),
               size=2)+
    geom_errorbar(data = filter(t2_errorbar,var == variable), 
                  aes(x=label, y = max, ymin=min, ymax=max, col=label),width=0)+
    ylab(variable_name) +
    xlab(NULL) +
    # ggtitle(variable_name)+
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",  # removed here, combined later
      axis.title.y = element_text(size = 8),
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(size = 7,angle = 90,hjust = 1),
      panel.grid.major.x = element_line(color = "grey85"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.4),
      plot.margin = margin(2, 2, 2, 2)  
    )+
    scale_color_manual(values = mycolors)    # theme(axis.title.y = element_blank(), 
    #       legend.position = "none",
    #       panel.grid.major.x = element_line(color = "grey85"))
  
  return(g)
  
}


g1<-mySimulationPlot("nb_year", "Study period (years)")
g2<-mySimulationPlot("vm", "Mean annual harvested volume (m³/ha/year)")
g3<-mySimulationPlot("rt", "Total inflow over the study period (k€/ha)")
g4<-mySimulationPlot("rm", "Mean annual inflow (k€/ha/year)")
g5<-mySimulationPlot("dt", "Total outflow over the study period (k€/ha)")
g6<-mySimulationPlot("dm", "Mean annual outflow (k€/ha/year)")
g7<-mySimulationPlot("van1", expression(NPV[n] ~ " excl. captial opportunity cost (k€/ha)"))
g8<-mySimulationPlot("van4", expression(NPV[n] ~ " incl. captial opportunity cost (k€/ha)"))
g9<-mySimulationPlot("npv_inf", expression(NPV[infinity] ~ " incl. capital opportunity cost (k€/ha)"))
g10<-mySimulationPlot("irr5", "Internal rate of return (%)")

gall<-ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10, ncol = 5, nrow = 2)
ggsave(gall, filename = "6_paper/indicators.png", width = 8, height = 7)




ggplot(filter(t1,subscenario == "1"), 
              aes(x=var,y=value, col=scenario))+
  coord_flip()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey85")
  ) +
  # alternating background stripes by using a rectangle layer
  geom_rect(
    data = data.frame(
      var = unique(t1$var),
      xmin = seq(0.5, length(unique(t1$var)) - 0.5, by = 2),
      xmax = seq(1.5, length(unique(t1$var)) + 0.5, by = 2)
    ),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey92",
    alpha = 0.5
  )+
  geom_point(position=dodge)+
  geom_errorbar(data = t2_errorbar, aes(x=var, y = max, ymin=min, ymax=max, col=scenario),
                position=dodge, width=0)
ggradar(radar_dat0,
        group.point.size = 1)




# VAN of reference scenarios
b1<-douglas_results%>%
  select(rotation,Essence = species,Scénario=label,billbook)%>%
  unnest(cols = c(billbook))

t6<-spruce_results%>%
  select(rotation,Essence = species,Scénario=label,billbook)%>%
  unnest(cols = c(billbook))%>%
  bind_rows(b1)%>%
  mutate(npv = net/(1+r)^age)%>%
  group_by(Essence, Scénario, isIncome)%>%
  summarise(npv  = sum(npv))%>%
  mutate(categ  = ifelse(isIncome, "recette", "depense"))%>%
  select(-isIncome)%>%
  pivot_wider(names_from = c("categ"), values_from  = npv, names_sep="_")

t7<-t6%>%
  left_join(t5,by = join_by(Essence, Scénario))
names(t7)<-c("Essence","Scénario","VAN dépense","VAN recette","Révolution (Nb. années)", "Recettes moyennes", "Dépenses moyennes", "TIR", "TIR nominal")

write.xlsx(as.data.frame(t6), sheetName = "t6_van_ref",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append = T)

write.xlsx(as.data.frame(t7), sheetName = "t7_indic_ref",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append = T)

# Annexe
## Mercuriale des prix
chauffage<-data.frame(circ = unique(mercuriale$circ), species_group = "Bois de chauffage", price_2019 = 15)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","black")
mercuriale_plot<-mercuriale%>%
  bind_rows(chauffage)%>%
  left_join(dico_species, by = join_by(species_group))%>%
  mutate(species_group_en = ifelse(is.na(species_group_en), "Firewood", species_group_en))%>%
  mutate(species_group = factor(species_group_en , levels=c(sort(dico_species$species_group_en),"Firewood"), order=T))%>%
  ggplot(aes(y=price_2019,x=circ/pi, col =  species_group))+geom_line()+
  xlab("Diameter (cm)")+ylab("Price (€/m³)")+
  theme(legend.title = element_blank())+
  scale_color_manual(values=cbPalette)
mercuriale_plot

ggsave(file="6_paper/mercuriale_en.png", mercuriale_plot, width=6, height=4)




## Subsides
subside<-fluxes%>%
  filter(category == "Subsides")%>%
  select(Massif = bois, Année = year, Montant = revenue_2019)%>%
  mutate(Montant = round(Montant,digits=1))

write.xlsx(as.data.frame(subside), sheetName = "t6_subside",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append = T)

## Description des 5 massifs
t7_achat<-achat%>%
  mutate(montant = - montant)%>%
  left_join(bois.sf,by = join_by(bois))%>%
  mutate(bois_ha = as.numeric(bois_ha))%>%
  select(-geom)

write.xlsx(as.data.frame(t7_achat), sheetName = "t7_achat",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append = T)

t8<-dendroByBois%>%
  as_tibble()%>%
  select(bois, year, nha, gha, vha, vcha, ghapc_oak, ghapc_conif)%>%
  mutate_at(c("ghapc_oak","ghapc_conif"), function(x) {x *100})%>%
  mutate_if(is.numeric, ~round(.,digits=1))

names(t8)<-c("Massif", "Année","NHA (/ha)","GHA (m²/ha)", "VHA (m³/ha)", "VC (€/ha)", "%Chênes", "%Résineux")

write.xlsx(as.data.frame(t8), sheetName = "t8_inv",file = "6_paper/tables.xlsx",
           col.names = T, row.names = F, showNA = F, append = T)


#"Evolution de la surface terrière par zone d'inventaire (en gris) et en moyenne à l'échelle des massifs forestiers (en noir)."}
datesclefs<-datesclefs%>%
  left_join(dico_block,by=join_by(bois))
dendroByBois<-dendroByBois%>%
  left_join(dico_block,by=join_by(bois))

evo1<-dendroByZone%>%
  left_join(dico_block,by=join_by(bois))%>%
  ggplot(aes(x=year,y=gha))+geom_line(col="grey", aes(group=inventoryZone))+
  geom_line(data=dendroByBois)+
  facet_grid(~block_name)+xlab("Year")+ylab("Basal area (m²/ha)")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1,vjust = 1))+
  geom_vline(data=datesclefs, aes(xintercept = purchaseYear), col="orange")+
  geom_vline(data=datesclefs, aes(xintercept = firstInventory), col="darkorange",lty=2)

ggsave(evo1,file = "6_paper/gha.png", width=10, height = 4)


# "Evolution du volume bois-fort tige par zone d'inventaire (en gris) et en moyenne à l'échelle des massifs forestiers (en noir)."}
evo2<-dendroByZone%>%
  left_join(dico_block,by=join_by(bois))%>%
  ggplot(aes(x=year,y=vha))+geom_line(col="grey", aes(group=inventoryZone))+
  geom_line(data=dendroByBois)+
  facet_grid(~block_name)+xlab("Year")+ylab("Volume (m³/ha)")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1,vjust = 1))+
  geom_vline(data=datesclefs, aes(xintercept = purchaseYear), col="orange")+
  geom_vline(data=datesclefs, aes(xintercept = firstInventory), col="darkorange",lty=2)

ggsave(evo2,file = "6_paper/vha.png", width=10, height = 4)


# "Evolution de la valeur marchande par zone d'inventaire (en gris) et en moyenne à l'échelle des massifs forestiers (en noir)."}
evo3<-dendroByZone%>%
  left_join(dico_block,by=join_by(bois))%>%
  ggplot(aes(x=year,y=vcha))+geom_line(col="grey", aes(group=inventoryZone))+
  geom_line(data=dendroByBois)+
  facet_grid(~block_name)+xlab("Year")+ylab("Value (€/ha)")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1,vjust = 1))+
  geom_vline(data=datesclefs, aes(xintercept = purchaseYear), col="orange")+
  geom_vline(data=datesclefs, aes(xintercept = firstInventory), col="darkorange",lty=2)

ggsave(evo3,file = "6_paper/vcha.png", width=10, height = 4)


#"Evolution du nombre d'espèce par zone d'inventaire (en gris) et à l'échelle des massifs forestiers (en noir)."}
evo4<-dendroByZone%>%
  left_join(dico_block,by=join_by(bois))%>%
  ggplot(aes(x=year,y=n.sp))+geom_line(col="grey", aes(group=inventoryZone))+
  geom_line(data=dendroByBois)+
  facet_grid(~block_name)+xlab("Year")+ylab("Nb of species")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1,vjust = 1))+
  geom_vline(data=datesclefs, aes(xintercept = purchaseYear), col="orange")+
  geom_vline(data=datesclefs, aes(xintercept = firstInventory), col="darkorange",lty=2)

ggsave(evo4,file = "6_paper/nsp.png", width=10, height = 4)


#
evo5<-dendroByZone%>%
  left_join(dico_block,by=join_by(bois))%>%
  ggplot(aes(x=year,y=ghapc_oak))+geom_line(col="grey", aes(group=inventoryZone))+
  geom_line(data=dendroByBois)+
  facet_grid(~block_name)+xlab("Year")+ylab("%oak basal area")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1,vjust = 1))+
  geom_vline(data=datesclefs, aes(xintercept = purchaseYear), col="orange")+
  geom_vline(data=datesclefs, aes(xintercept = firstInventory), col="darkorange",lty=2)

ggsave(evo5,file = "6_paper/pgha_chene.png", width=10, height = 4)


# "Evolution de la valeur marchande/volume par zone d'inventaire (en gris) et en moyenne à l'échelle des massifs forestiers (en noir)."}
evo6<-dendroByZone%>%
  left_join(dico_block,by=join_by(bois))%>%
  ggplot(aes(x=year,y=vcha/vha))+geom_line(col="grey", aes(group=inventoryZone))+
  geom_line(data=dendroByBois)+
  facet_grid(~block_name)+xlab("Year")+ylab("Value / Volume (€/m³)")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1,vjust = 1))+
  geom_vline(data=datesclefs, aes(xintercept = purchaseYear), col="orange")+
  geom_vline(data=datesclefs, aes(xintercept = firstInventory), col="darkorange",lty=2)

ggsave(evo6,file = "6_paper/vcha_V.png", width=10, height = 4)


# 
evo7a<-dendroByBois%>%
  ggplot(aes(x=year,y=vha))+ geom_line(aes(col=as.factor(block),group=block))+
  geom_point(aes(col=as.factor(block)))+
  xlab("Year")+ylab("Standing volume (m³/ha)")+labs(col="Block")+
  theme(legend.position = "top")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1,vjust = 1))

evo7b<-dendroByBois%>%
  ggplot(aes(x=year,y=vcha/vha))+ geom_line(aes(col=as.factor(block),group=block))+
  geom_point(aes(col=as.factor(block)))+
  xlab("Year")+ylab("Standing value / Standing volume (€/m³)")+
  theme(legend.position = "top")+labs(col="Block")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1,vjust = 1))

evo7<-ggarrange(evo7a, evo7b, labels = c("A", "B"), ncol = 2, nrow = 1,
                common.legend = TRUE, legend = "bottom")

ggsave(evo7,file = "6_paper/mean_vcha_V.png", width=10, height = 5)

## Temps de rotation du capital (en volume et en valeur)

intial_capital<-dendroByBois%>%
  as_tibble()%>%
  group_by(bois)%>%
  filter(year>= 1992)%>%
  filter(year == min(year))%>%
  select(bois, initial_year = year, initial_vha = vha, initial_vm = vcha)

final_capital<-dendroByBois%>%
  as_tibble()%>%
  group_by(bois)%>%
  filter(year == 2019)%>%
  select(bois, final_year = year, final_vha = vha, final_vm = vcha)

fluxes%>%
  filter(category == "Ventes de bois")%>%
  filter(year >= 1992)%>%
  filter(bois == "Haugimont")%>%
  summarise(volume_tot = sum(volume)/unique(bois_ha))

tt<-fluxes%>%
  filter(category == "Ventes de bois")%>%
  filter(year >= 1992)%>%
  mutate(volume_ha = as.numeric(volume/bois_ha))%>%
  left_join(intial_capital, by = join_by(bois))%>%
  left_join(final_capital, by = join_by(bois))%>%
  group_by(bois,year,initial_year,initial_vha,initial_vm,final_year,final_vha,final_vm)%>%
  summarise(volume_ha = sum(volume_ha),
            income_ha = sum(revenue_2019_ha))%>%
  group_by(bois,initial_year,initial_vha,initial_vm,final_year,final_vha,final_vm)%>%
  arrange(year)%>%
  mutate(vol_cum = cumsum(volume_ha),
         val_cum = cumsum(income_ha))

ggplot(data=tt, aes(y=val_cum, x=year, col=bois))+geom_line()+
  geom_hline(aes(yintercept = initial_vm, col=bois))
ggplot(data=tt, aes(y=vol_cum, x=year, col=bois))+geom_line()+
  geom_hline(aes(yintercept = initial_vha, col=bois))

tt%>%filter(year==2018)%>%
  mutate(tx_vol = vol_cum/initial_vha,
         tx_val = val_cum/initial_vm,
          tx_rotation_vol = (vol_cum + (final_vha - initial_vha))/initial_vha,
         tx_rotation_val = (val_cum + (final_vm - initial_vm))/initial_vm) 
         

TempsRotationVolume<-tt%>%
  filter(vol_cum > initial_vha)%>%
  group_by(bois)%>%
  arrange(year)%>%
  slice(1)%>%
  mutate(TR_voume = year - initial_year)
  
TempsRotationValue<-tt%>%
  filter(val_cum > initial_vm)%>%
  group_by(bois)%>%
  arrange(year)%>%
  slice(1)%>%
  mutate(TR_voume = year - initial_year)


## Exporter les échéanciers

scenarios<-douglas_results%>%
  bind_rows(spruce_results)

# scenarios%>%
#   select(billbook)%>%
#   unnest(cols = c(billbook))%>%
#   select(label)%>%
#   as.data.frame()%>%
#   distinct()%>%
#   write.xlsx(sheetName = name,file = "6_paper/dico_label.xlsx",
#              col.names = T, row.names = F, showNA = F, append = T)

dico_label<-read.xlsx(file = "6_paper/dico_label.xlsx",sheetIndex = 1)

for(i in 1:nrow(scenarios)){
  name<-paste(scenarios[i,"species"],"--", scenarios[i,"label"])
  print(name)
  bb<-scenarios[i,]%>%
    select(billbook)%>%
    unnest(cols = c(billbook))%>%
    select(-isIncome)%>%
    mutate_at(c("value","volume","net"), ~round(.,digits=0))%>%
    left_join(dico_label,by = join_by(label))%>%
    filter(label != "Gestion")%>%
    select(label_en, age, value, volume, net)%>%
    as.data.frame()
  names(bb)<-c("Label", "Year","Price (€/ha)","Volume (m³)", "Net (€/ha)")
  
  write.xlsx(bb, sheetName = name,file = "6_paper/tables.xlsx",
             col.names = T, row.names = F, showNA = F, append = T)
  
  }

### unit price for the timber sales
dico_produit <- read_excel(path="2_processing/dico_produit_edited.xlsx")

price_indices <- read_excel(path="1_data/fromGDD/IPC-Coefficients.xlsx",sheet = "Feuil2")%>%
  select(year = Année, price_coef = 'Coefficient 2019')  


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","black")

sales<-read_xlsx(path="1_data/fromGDD/ventes.xlsx",sheet = "ventes_détail")%>%
  select(bois= Bois, year = Année, vol_m3 = Vol_m3,prix_courant, produit = type_bois, produit2 = essence,parcelle)%>%
  left_join(price_indices, by= c("year"))%>%
  filter(produit != "prime")%>%
  mutate(bois = str_to_title(bois))%>%
  mutate(prix_2019 = prix_courant*price_coef)%>%
  mutate(prix_m3 = prix_2019/vol_m3)%>%
  left_join(dico_produit, by = join_by(produit2))%>%
  mutate(species_group_en = ifelse(species_group_en == "Coniferous", "Coniferous", "Broadleaved"))%>%
  mutate(species_group_en = ifelse(produit == "chauffage", "Firewood", species_group_en))%>%
  mutate(species_group_en = factor(species_group_en))%>%
  filter(vol_m3 > 50 & prix_m3 > 0) 

# to make the dico  
# data.frame(produit2 = unique(sales$produit2))%>%
#   write.xlsx(file = "2_processing/dico_produit.xlsx")

get_box_stats <- function(y) {
  return(data.frame(
    y = 1,
    label = paste(
      "Count =", length(y), "\n",
      "Mean =", round(mean(y), 2), "\n",
      "Median =", round(median(y), 2), "\n"
    )
  ))
}

sales_plot<-ggplot(sales, aes(y=prix_m3,x=species_group_en))+
  geom_boxplot(fill = "grey") + 
  ylab("Sale price (€/m³)")+
  theme(legend.position = "none", axis.title.x=element_blank())+
  scale_y_continuous(trans='log',breaks = c(10,50,100,150,200,300,500,1000),limits = c(1, 300)) + 
  stat_summary(fun.data = get_box_stats, geom = "text", hjust = 0.5, vjust = 0.9)
  
mercuriale_plot2 <- mercuriale_plot + 
  scale_y_continuous(trans='log',breaks = c(10,50,100,150,200,300,500,1000),limits = c(1, 300))
  
timber_prices<-ggpubr::ggarrange(mercuriale_plot2, sales_plot,  
          labels = c("A", "B"),
          ncol = 2, nrow = 1,widths = c(3,2))

ggsave(timber_prices,file = "6_paper/timber prices.png", width=10, height = 5)

