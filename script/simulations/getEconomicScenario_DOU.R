# This script load local yield production table (tdp) and compute economic indicators
# For Douglas-fir scenario only

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

#### DOU1 : REFERENCE SCENARIO ####
salvage_price <- NA 

costs<-read_xlsx("data/simulations/costs.xlsx",sheet = "DOU1-2")%>%
  mutate(isIncome = F)%>%
  select(label, category, year, isIncome, value = cost)

tdp<-read_xlsx("data/simulations/Normes Do 9.0.xlsx", sheet = "Do1A",range="A26:W37",col_names = cnames)%>%
  select(- vmv, - vma, - vmv2, -vma2, -vma3)%>%
  rename(year = age)

source("script/simulations/runEconomicScenario.R")

ggplot(rotations,aes(x= rotation, y = irr3))+geom_line()

# ggplot(rotations,aes(x= rotation, y = tir))+geom_line()

douglas_reference<-rotations%>%
  filter(irr3 == max(irr3))%>%
  mutate(label="DOU1")
 
#### ALTERNATE SCENARIO - GROWTH DECLINE ####
## Perte de croissance -> classe de productivité III
tdp<-read_xlsx("data/simulations//Normes Do 9.0.xlsx", 
               sheet = "Do3A",range="A26:W37",col_names = cnames)%>%
  select(- vmv, - vma, - vmv2, -vma2, -vma3)%>%
  rename(year = age)

source("script/simulations/runEconomicScenario.R")

# ggplot(rotations,aes(x= rotation, y = tir))+geom_line()

douglas_alternatif1<-rotations%>%
  filter(irr3 == max(irr3))%>%
  mutate(label="DOU2")


### ALTERNATE SCENARIO - REGE FAILURE ###
timelag<-3 # !!!

costs<-read_xlsx("data/simulations/costs.xlsx",sheet = "DOU3")%>%
  mutate(isIncome = F)%>%
  select(label, category, year, isIncome, value = cost)

tdp<-read_xlsx("data/simulations/Normes Do 9.0.xlsx", sheet = "Do1A",range="A26:W37",col_names = cnames)%>%
  select(- vmv, - vma, - vmv2, -vma2, -vma3)%>%
  mutate(age=age+timelag)%>%
  rename(year = age)

source("script/simulations/runEconomicScenario.R")

ggplot(rotations,aes(x= rotation, y = irr3))+geom_line()

douglas_alternatif2<-rotations%>%
  filter(irr3 == max(irr3))%>%
  mutate(label="DOU3")%>%
  slice(1) ### take only the first one


#### SUMMARY ####
DOU<-douglas_reference%>%
  bind_rows(douglas_alternatif1)%>%
  bind_rows(douglas_alternatif2)%>%
  mutate(species = "Douglas-fir")

save(DOU,file = "data/simulations/DOU.rdata")

