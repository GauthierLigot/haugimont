# This script load simulation results and compute economic indicators
# For oak scenarios only

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
  filter(species_group_en=="Oak")

output_cdom<-data.frame()
out2<-tibble() # use to save stand value as this scenario cannot be considered cyclical

# scenario <- "hazardfree"
# rege <- "shelterwood"
for(rege in c("clear-cut","shelterwood")){
  
  # change cost depening on the regeneration system
  rege_abrv<-""
  if(rege=="shelterwood")  rege_abrv<-"_SHLTR"
  
  for(scenario in c("hazardfree","plantation")){
    
    scenario_abrv<-"1-2"
    if(scenario == "plantation") scenario_abrv<-"3"
    
    cost_sheet <- paste0("OAK",rege_abrv,scenario_abrv)
    
    costs<-read_xlsx("data/simulations/costs.xlsx",sheet = cost_sheet)%>%
      mutate(isIncome = F)%>%
      select(label, category, year, isIncome, value = cost)%>%
      mutate(year = year + 1) 
    
    # change cost depening on the regeneration system
    # si = 27
    # cdom = 180
    for(si in c(24,27)){
      for(cdom in c(180, 200)){  #220, 240,260,280)){ #bigger CDOM were never optimum #TODO add cdom = 160
        
        print(paste("SI = ",si," : cdom = ",cdom))
        
        # filename<-paste0("8_Simulations/3_evenagedoaksardin/out/",rege,"_cdom_",cdom,"_SI_",si,".txt")
        filename<-paste0("data/simulations/evenagedoak/",rege,"_cdom_",cdom,"_SI_",si,".txt")
        
        tdp<-read.table (filename, sep="\t", header=T)%>%
          rowwise()%>%
          mutate(timber_price = getprice(cinf = lowerGirth, csup = lowerGirth + 10, sp="Oak"))%>%
          ungroup()%>%
          group_by(date, status)%>%
          summarise(stemVolume = sum(vc22StemHa),
                    stemValue = sum(timber_price*vc22StemHa),
                    branchVolume = sum(vc22BranchHa),
                    branchValue = sum(firewood_price*vc22BranchHa)
          )%>%
          mutate(totVolume = stemVolume + branchVolume)%>%
          mutate(totValue = stemValue + branchValue)%>%
          select(date,status,stemVolume, stemValue, branchVolume, branchValue, totVolume, totValue)%>%
          mutate(year = date - 2000)%>%
          ungroup()
        
        ggplot(filter(tdp,status == "alive"), aes(x=date,y=totVolume))+geom_line()
        # ggplot(filter(tdp,status == "alive"), aes(x=date,y=stemVolume))+geom_line()
        ggplot(filter(tdp,status == "alive"), aes(x=year,y=totVolume))+geom_line()
        
        cuts<-tdp%>%
          filter(status=="cut")
        
        stumpage<-tdp%>%
          filter(status=="alive")
        
        final_cut_date <- tdp%>%
          select(date,year,status,totVolume)%>%
          pivot_wider(names_from = status, values_from = c(totVolume))%>%
          filter(!is.na(cut))%>%
          filter(alive - cut < 10^(-10))%>% # double comparison (clear-cut)
          pull(date)
        
        # cycle2_date<-cuts%>%
        #   filter(date>final_cut_date)%>%
        #   arrange(date)%>%
        #   slice(1)%>%
        #   pull(date)
        
        cycle1_length = final_cut_date - 2000
        
        if(rege=="shelterwood"){
          cycle1_end_date = final_cut_date - 3 * 4 
          cycle1_length = cycle1_end_date - 2000
        }
        
        tdp<-tdp%>%
          filter(date<=final_cut_date)
        
        tdp2<-tdp
        
        S0 <- 0 #stumpage value at year 0
        SF <- 0 #stumpage value at year 0
        
        if(rege=="shelterwood"){
          tdp2<-tdp%>%
            mutate(year = ifelse(year>=cycle1_length, year - cycle1_length, year))%>% ###  
            arrange(year)
          
          # This is only the stumpage value
          S0 <- tdp2%>% # changed tdp to tdp2 (29 Apr 2026)
            filter(status == "alive")%>%
            slice(1)%>%
            pull(totValue) #stumpage value at year 0
          
          SF <- S0 #stumpage value at year 0
      }
        
        # write.table(as.data.frame(tdp2), file = "clipboard", sep=";", row.names = F)
        
        ggplot(filter(tdp2,status == "alive"), aes(x=year,y=totVolume))+geom_line()
        ggplot(filter(tdp2,status == "alive"), aes(x=year,y=totValue))+geom_line()+ylab("Stumpage value")
        
        tmp_out2<-tdp2%>%
          mutate("rege" = rege,
                 "cdom" = cdom,
                 "si" = si,
                 "scenario" = scenario)
        out2<-out2%>%
          bind_rows(tmp_out2)
        
        
        cuts2<-tdp2%>%
          filter(status=="cut")%>%
          mutate(label = "Cut", 
                 category = "Timber sales",
                 isIncome = T)%>%
          select(label, category, year, isIncome, volume= totVolume, value = totValue)
        
        management_tmp<-tibble(label = "Management", category = "Management", 
                               year = 1:cycle1_length, isIncome = FALSE, value = management_cost)
        
        
        
        # find the internal rate of return (given the forest rent value and timber price list)
        tmp_tir<-data.frame()
        
        for(r_tmp in seq(r_min, r_max, by=r_step)){ #look out to avoid confounding r and r_tmp !!!
          # r_tmp <- 0.02 
          V0 <- soil_expectation_value
          
          if(rege=="shelterwood"){
            V0 <- cuts2%>%
              arrange(year)%>%
              slice(1:4)%>%
              mutate(discounted_value = value/(1+r_tmp)^year)%>%
              summarise(va = sum(discounted_value))%>%
              pull(va) 
          }
          
          cashflows<-costs%>%
            bind_rows(cuts2)%>%
            bind_rows(management_tmp)%>%
            arrange(year)%>%
            mutate(net = ifelse(isIncome, value, -value))%>%
            mutate(van = net/(1+r_tmp)^year)
          
          van1 = sum(cashflows$van)
          van2 = van1 + soil_expectation_value/(1+r_tmp)^cycle1_length - soil_expectation_value
          van3 = van1 + (soil_expectation_value + S0)/(1+r_tmp)^cycle1_length - (soil_expectation_value + S0)
          van4 = van1 + (soil_expectation_value + V0)/(1+r_tmp)^cycle1_length - (soil_expectation_value + V0)
          
          npv_inf_fr <- NPV_FR(cashflows$year,cashflows$net,rotation = cycle1_length, r = r_tmp)
          npv_n_fr <- npv_inf_fr * ((1+r_tmp)^cycle1_length-1)/(1+r_tmp)^cycle1_length
          
          # write.table(cashflows, file="clipboard", sep=";", row.names=F)
          
          tmp_line <- data.frame(r = r_tmp, van1, van2, van3, van4, npv_inf_fr, npv_n_fr, fonds = soil_expectation_value)
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
        
        IRR4<-tmp_tir%>%
          mutate(van4 = abs(van4)) %>%
          arrange(van4)%>%
          slice(1)%>%
          pull(r)
        
        tmp_2PC<-tmp_tir%>%
          filter(r==0.02)
        
        output_line <- tibble(species = "oak", 
                              scenario = scenario,
                              rege=rege,
                              si = si,
                              cdom=cdom, 
                              irr1 = IRR1,
                              irr2 = IRR2,
                              irr3 = IRR3, 
                              irr4 = IRR4,
                              npv1 = tmp_2PC$van1,
                              npv2 = tmp_2PC$van2,
                              npv3 = tmp_2PC$van3,
                              npv4 = tmp_2PC$van4,
                              npv_inf_fr = tmp_2PC$npv_inf_fr,
                              npv_n_fr = tmp_2PC$npv_n_fr,
                              n = cycle1_length,
                              sev = soil_expectation_value,
                              billbook = list(cashflows))
        
        output_cdom <- output_cdom%>%
          bind_rows(output_line)
      }
    }
  }
}

unique(output_cdom$rege)

g1<-ggplot(output_cdom, aes(x=cdom, y=irr3, col=scenario, lty=as.factor(si)))+geom_line()+
  facet_wrap(~rege)
ggsave(filename="fig/tmp/oak.png",plot = g1)

OAK<-output_cdom%>%
  filter(!(si==24 & scenario == "plantation"))%>%
  mutate(key2 = ifelse(rege == "clear-cut", "","_SHLTR"))%>%
  mutate(key3 = ifelse(si == 24,"2",
                       ifelse(scenario == "plantation", "3","1")))%>%
  mutate(label = paste0("OAK",key2,key3))%>%
  group_by(scenario,rege,si,label)%>%
  arrange(desc(irr3))%>% # !!
  slice(1)%>%
  ungroup()%>%
  mutate(irr1 = ifelse(irr1==r_max,NA,irr1))%>% ### need to be corrected ? - unreliable anyway
  mutate(irr2 = ifelse(irr2==r_max,NA,irr2))%>% ### need to be corrected ? - unreliable anyway
  # mutate(irr3 = ifelse(key2=="_SHLTR",NA,irr3))%>% ### npv3 had been corrected and is now reliable
  select(species, label, rotation = n, si, cdom, irr1, irr2, irr3, irr4, npv1, npv2, npv3, npv4, npv_inf_fr, npv_n_fr, 
         sev, billbook)%>%
  arrange(irr4)


# write.table(as.data.frame(filter(OAK, label == "OAK_SHLTR1")$billbook), file = "clipboard", sep=";", row.names = F)

### 
# out2%>%
#   filter(rege == "shelterwood" & si == 27 & cdom == 180)%>%
#   filter(status == "alive")%>%
#   filter(year == min(year) | year == max(year))%>%
#   # group_by(label)%>%
#   group_by(si, cdom, rege)%>%
#   arrange(year)%>%
#   summarise(initial_volume = first(totVolume),
#             final_volume = last(totVolume),
#             change_volume = (final_volume-initial_volume)/initial_volume*100,
#             initial_value = first(totValue),
#             final_value = last(totValue),
#             change_value = (final_value-initial_value)/initial_value*100)

OAK_standevolution <- out2%>%
  filter(!(si==24 & scenario == "plantation"))%>%
  mutate(key2 = ifelse(rege == "clear-cut", "","_SHLTR"))%>%
  mutate(key3 = ifelse(si == 24,"3",
                       ifelse(scenario == "plantation", "2","1")))%>%
  mutate(label = paste0("OAK",key2,key3))%>%
  right_join(select(OAK,label, cdom))
  
# ggplot(filter(OAK_standevolution, status=="alive"),aes(x=year,y=totVolume,col=label))+geom_line()

save(OAK, OAK_standevolution, file = "data/simulations/OAK.rdata")
