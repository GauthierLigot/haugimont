# This script load local yield production table (tdp) and compute economic indicators
# For spruce scenario only

rm(list=ls())

library(tidyverse)
library(readxl)

# parameters and functions
source("script/commonParameters.R")
source("script/tirf.R") 
source("script/process_simulation_fctn.R")

### price list ####
load("data/price_list.Rdata")
mercuriale2<-price_list%>%
  filter(species_group_en=="Coniferous")

### utils ####
cnames<-c("age","hdom","v_20_40","v_40_60","v_60_70","v_70_90","v_90_120","v_120_150","v_150_180","v_180_999",
          "vmv","vma","vcut_20_40","vcut_40_60","vcut_60_70","vcut_70_90","vcut_90_120","vcut_120_150",
          "vcut_150_180","vcut_180_999","vmv2","vma2","vma3")

#### SPR1 : REFERENCE SCENARIO ####
salvage_price <- NA 

costs<-read_xlsx("data/simulations/costs.xlsx",sheet = "SPR1-2")%>%
  mutate(isIncome = F)%>%
  select(label, category, year, isIncome, value = cost)

tdp<-readxl::read_xlsx("data/simulations/Normes Ep 9.0.xlsx", sheet = "Ep2A",range="A26:W37",col_names = cnames)%>%
  select(- vmv, - vma, - vmv2, -vma2, -vma3)%>%
  rename(year = age)

source("script/simulations/runEconomicScenario.R")

ggplot(rotations,aes(x= rotation, y = irr1))+geom_line()
ggplot(rotations,aes(x= rotation, y = irr2))+geom_line()

spruce_reference<-rotations%>%
  filter(irr3 == max(irr3))%>%
  mutate(label="SPR1")%>%
  filter(rotation == min(rotation))
  

#### SPR2 : ALTERNATE SCENARIO - Ips or windthrow ####
salvage_price <- 20 # !!!!
damage_year <- 46

tdp<-read_xlsx("data/simulations/Normes Ep 9.0.xlsx", sheet = "Ep2A",range="A26:W37",col_names = cnames)%>%
  select(- vmv, - vma, - vmv2, -vma2, -vma3)%>%
  rename(year = age)

source("script/simulations/runEconomicScenario.R")

spruce_alternatif1<-rotations%>%
  filter(rotation == damage_year)%>%
  mutate(label="SPR2")

#### SPR3 : ALTERNATE SCENARIO - regeneration problems ####
salvage_price <- NA # !!!!
timelag <- 2

tdp<-read_xlsx("data/simulations/Normes Ep 9.0.xlsx", sheet = "Ep2A",range="A26:W37",col_names = cnames)%>%
  select(- vmv, - vma, - vmv2, -vma2, -vma3)%>%
  mutate(age = age+timelag)%>%
  rename(year = age)

costs<-read_xlsx("data/simulations/costs.xlsx",sheet = "SPR3")%>%
  mutate(isIncome = F)%>%
  select(label, category, year, isIncome, value = cost)

source("script/simulations/runEconomicScenario.R")

spruce_alternatif2<-rotations%>%
  filter(irr3 == max(irr3))%>%
  head(n = 1)%>%
  mutate(label="SPR3")

#### SUMMARY ####
SPR<-spruce_reference%>%
  bind_rows(spruce_alternatif1)%>%
  bind_rows(spruce_alternatif2)%>%
  mutate(species = "Spruce")

save(SPR,file = "data/simulations/SPR.rdata")
