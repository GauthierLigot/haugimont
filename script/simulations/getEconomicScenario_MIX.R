# This script load simulation results and compute economic indicators
# For mixed-stand scenarios only

rm(list=ls())

library(tidyverse)
library(readxl)

### parameters ####
source("script/commonParameters.R")
source("script/tirf.R") 
source("script/process_simulation_fctn.R")

technical_work <- 77 # !!!
ashdieback_pricereduction <- 0.8 # price = 0.8 * original price

### mercuriale ####
load("data/price_list.Rdata")
mercuriale2<-price_list # !
species_dico<-read_xlsx(path = "data/simulations/MIX_species_dico.xlsx") 

out<-tibble()
out2<-tibble() # use to save stand value as this scenario cannot be considered cyclical

for(scenario in c("MIX1","MIX2","MIX3")){
  
  filename<-paste0("data/simulations/unevenaged/",scenario,"_classExport.txt")
  
  tdp<-read.table (filename, sep="\t", header=T)%>%
    left_join(species_dico, by = join_by(species))%>%
    rowwise()%>%
    mutate(timber_price = getprice(cinf = lowerGirth, csup = lowerGirth + 10, sp = species_group_en))%>%
    ungroup()%>%
    mutate(timber_price = ifelse(scenario == "MIX3" & species == "FR" & date>1992, timber_price * ashdieback_pricereduction,
                                 timber_price))%>%
    group_by(date, status)%>%
    summarise(stemVolume = sum(vc22StemHa),
              stemValue = sum(timber_price*vc22StemHa),
              branchVolume = sum(vc22BranchHa),
              branchValue = sum(firewood_price*vc22BranchHa)
    )%>%
    mutate(totVolume = stemVolume + branchVolume)%>%
    mutate(totValue = stemValue + branchValue)%>%
    select(date,status,stemVolume, stemValue, branchVolume, branchValue, totVolume, totValue)%>%
    mutate(year = date - 1992)%>%
    ungroup()
  
  tmp_out2<-tdp%>%
    mutate(label = scenario)
  out2<-out2%>%
    bind_rows(tmp_out2)
  
  ggplot(filter(tdp,status == "alive"), aes(x=date,y=totVolume))+geom_line()
  ggplot(filter(tdp,status == "alive"), aes(x=date,y=totValue))+geom_line()
  
  # ggplot(filter(tdp,status == "alive"), aes(x=date,y=stemVolume))+geom_line()
  ggplot(filter(tdp,status == "alive"), aes(x=year,y=totVolume))+geom_line()
  
  cuts<-tdp%>%
    filter(status=="cut")
  
  stumpage<-tdp%>%
    filter(status=="alive")
  
  cuts2<-tdp%>%
    filter(status=="cut")%>%
    mutate(label = "Cut", category = "Timber sales", isIncome = T)%>%
    select(label, category, year, isIncome, volume = totVolume, value = totValue)
  
  cycle_length <- max(tdp$year)
  management_tmp<-tibble(label = "Management", 
                         category = "Management", 
                         year = 1:cycle_length, isIncome = FALSE, value = management_cost)
  
  work_tmp<-tibble(label = "Technical work", 
                   category = "Regeneration", 
                   year = 1:cycle_length, isIncome = FALSE, value = technical_work)
  
  # find the internal rate of return (given the forest rent value and timber price list)
  tmp_tir<-data.frame()
  
  for(r_tmp in seq( r_min,r_max,by=r_step)){ # do not confound r and r_tmp ...
    # r_tmp<- 0.02
    
    S0 <- stumpage$totValue[1] #stumpage value at year 0
    SF <- stumpage$totValue[nrow(stumpage)] #stumpage value at year 0
      
    cashflows<-cuts2%>%
      bind_rows(management_tmp)%>%
      bind_rows(work_tmp)%>%
      arrange(year)%>%
      mutate(net = ifelse(isIncome, value, -value))%>%
      mutate(van = net/(1+r_tmp)^year)
    
    van1 = sum(cashflows$van)
    van2 = van1 + soil_expectation_value/(1+r_tmp)^cycle_length - soil_expectation_value
    van3 = van1 + (soil_expectation_value + SF)/(1+r_tmp)^cycle_length - (soil_expectation_value + S0)
    van4 = van3
    
    npv_inf_fr <- NPV_FR(cashflows$year,cashflows$net,rotation = cycle_length, r = r_tmp)
    npv_n_fr <- npv_inf_fr * ((1+r_tmp)^cycle_length-1)/(1+r_tmp)^cycle_length
    
    # write.table(cashflows, file="clipboard", sep=";", row.names=F)
    
    tmp_line <- data.frame(r = r_tmp, van1, van2, van3, van4, npv_inf_fr, npv_n_fr, sev = soil_expectation_value)
    tmp_tir <-tmp_tir%>%
      bind_rows(tmp_line)
    
  }
  # ggplot(tmp_tir, aes(x=r, y=van2))+geom_line()
  
  IRR1<-tmp_tir%>%
    mutate(van1 = abs(van1)) %>%
    arrange(van1)%>%
    slice(1)%>%
    pull(r)
  
  IRR2<-tmp_tir%>%
    mutate(van2 = abs(van2)) %>%
    arrange(van2)%>%
    slice(1)%>%
    pull(r)
  
  IRR3<-tmp_tir%>%
    mutate(van3 = abs(van3)) %>%
    arrange(van3)%>%
    slice(1)%>%
    pull(r)
  
  IRR4<-IRR3
  
  tmp_2PC<-tmp_tir%>%
    filter(r==0.02)
  
  output_line <- tibble(species = "mixed", 
                        scenario = scenario,
                        irr1 = IRR1,
                        irr2 = IRR2,
                        irr3 = IRR3,
                        irr4 = IRR4,
                        npv1=tmp_2PC$van1,
                        npv2=tmp_2PC$van2,
                        npv3=tmp_2PC$van3,
                        npv4=tmp_2PC$van4,
                        npv_inf_fr = tmp_2PC$npv_inf_fr,
                        npv_n_fr = tmp_2PC$npv_n_fr,
                        n = cycle_length,
                        sev = soil_expectation_value,
                        billbook = list(cashflows))
  
  out <- out%>%
    bind_rows(output_line)
}


MIX<-out%>%
  mutate(label = scenario)%>%
  group_by(label)%>%
  arrange(desc(irr3))%>%
  slice(1)%>%
  ungroup()%>%
  select(species, label, rotation = n, irr1, irr2, irr3, irr4, npv1, npv2, npv3, npv4, npv_inf_fr, npv_n_fr, sev, billbook)%>%
  arrange(irr3)

MIX_standevolution<-out2

save(MIX, MIX_standevolution, file = "data/simulations/MIX.rdata")
