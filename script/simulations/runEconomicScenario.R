# getprice<-function(cinf,csup,m=mercuriale2){
#   prices<-subset(m, circ > cinf & circ < csup)%>%
#     mutate(t1 = circ > cinf,
#            t2= circ < csup,
#            cinf2 = cinf,
#            cup2 = csup)
#   # print(prices)
#   price<-mean(prices$price_2019)
#   # print(paste0("cinf = ", cinf , " - csup = ",csup," - price = ", price))
#   return(price)
# }

cuts<-tdp%>%
  select(year, vcut_20_40 : vcut_180_999)%>%
  pivot_longer(cols = vcut_20_40 : vcut_180_999,names_to = c("cat","c_inf","c_sup"),
               names_sep="_",values_to = "volume")%>%
  mutate(c_inf = as.numeric(c_inf),
         c_sup = as.numeric(c_sup))%>%
  mutate(cclass = paste(c_inf,ifelse(c_sup==999,"+",c_sup),sep="-"))%>%
  rowwise()%>%
  mutate(mean_price = getprice(cinf = c_inf, csup = c_sup, m=mercuriale2, sp = "Coniferous", average = T))%>%
  ungroup()%>%
  group_by(year)%>%
  summarise(value = max(mean_price * volume),
            volume = sum(volume))

stumpage<-tdp%>%
  select(year, v_20_40 : v_180_999)%>%
  pivot_longer(cols = v_20_40 : v_180_999,names_to = c("cat","c_inf","c_sup"),
               names_sep="_",values_to = "volume")%>%
  mutate(c_inf = as.numeric(c_inf),
         c_sup = as.numeric(c_sup))%>%
  mutate(cclass = paste(c_inf,ifelse(c_sup==999,"+",c_sup),sep="-"))%>%
  rowwise()%>%
  mutate(mean_price = getprice(cinf = c_inf, csup = c_sup, m=mercuriale2, sp = "Coniferous", average = T))%>%
  ungroup()%>%
  group_by(year)%>%
  summarise(value = ifelse(is.na(salvage_price),sum(mean_price*volume),sum(salvage_price*volume)),
            volume = sum(volume))

# getBillBook<-function(rotation,cuts,stumpage,costs,management_cost){
#   
#   cut_tmp<- filter(cuts,year < rotation)%>%
#     mutate(label = "Cut",
#            category = "Timber sales",
#            isIncome = T)
#   
#   harvest_tmp<-filter(stumpage, year == rotation)%>%
#     mutate(label = "Final cut",
#            category = "Timber sales",
#            isIncome = T)
#   
#   management_tmp<-tibble(label = "Management", 
#                          category = "Management", 
#                          year = 1:rotation, isIncome = FALSE, value = management_cost)
#   
#   echeancier <- costs%>%
#     bind_rows(cut_tmp)%>% 
#     bind_rows(harvest_tmp)%>%
#     bind_rows(management_tmp)%>%
#     mutate(net = ifelse(isIncome,value,-value))%>%
#     arrange(year)
#   
#   return(echeancier)
# }
# 
# NPV<-function(year,net,r=0.02){
#   npv<-sum(net/(1+r)^year)
#   return(npv)
# }
# 
# # Forest value for any point of time (within the cutting production cycle)
# FV<-function(year,net,r,a,final_year){
#   tmp<-data.frame(year,net)
#   v <- vector()
#   
#   for(j in 1:nrow(tmp)){
#     i <- tmp$year[j]
#     if(i>=a){
#       v = c(v,tmp$net[j]*(1+r)^(final_year+a-i))
#     }else{
#       v = c(v, tmp$net[j]*(1+r)^(a-i))
#     }
#   }
#   v2<-sum(v) / ((1+r)^final_year-1)
#   return(v2)
# }
# 
# # NPV of a fully regulated forest to account for capital variability through time and space
# # It assumes that the forest is composed of stand of all ages and of equal area 
# # Here, NPVinf is fist estimated from year 1 to year T (rotation). I omitted year "0" as this instant equals instant "n*"...
# NPV_FR<-function(year,net,rotation,r=0.02){
#   
#   # debugging lines
#   # billbook<-rotations$billbook[[1]]
#   # write.table(billbook, file="clipboard", sep=";",col.names = T,row.names = F)
#   # rotation<-rotations$rotation[[1]]
#   # year<-billbook$year
#   # net<-billbook$net
#   # NPV(year,net,r)*(1+0.02)^21/((1+0.02)^21-1)
#   
#   as<-1:(rotation)
#   npvs<-tibble(a=as)%>%
#     mutate(fv = NA)
#     
#   for(a in as){ #a = 0
#     npvs$fv[npvs$a==a] <- FV(year, net, r, a, rotation) # the function is not yet vectorized
#   }
#   npv_fr<-mean(npvs$fv)
#   return(npv_fr)
# }

rotations<-tibble(rotation = cuts$year[-1])%>%
  mutate(billbook = map(rotation, ~ getBillBook(.x,cuts,stumpage,costs,management_cost)))%>%
  mutate(irr1 = map_dbl(billbook, ~ TIRF(year = .$year, net = .$net, fonds=0,r_step = 0.0001,r_max=0.1)))%>%
  mutate(irr2 = map_dbl(billbook, ~ TIRF(year = .$year, net = .$net, fonds=soil_expectation_value,r_step = 0.0001,r_max=0.1)))%>%
  mutate(irr3 = irr2,
         irr4 = irr2)%>%
  mutate(npv1 = map_dbl(billbook, ~ NPV(year = .$year, net = .$net,r = r)))%>%
  mutate(npv2 = npv1 + soil_expectation_value/(1+0.02)^rotation - soil_expectation_value)%>%
  mutate(npv3 = npv2)%>%
  mutate(npv4 = npv2)%>%
  mutate(npv_inf_fr = map2_dbl(billbook,rotation,  ~ NPV_FR(year = .x$year, net = .x$net, rotation = .y, r = r)))%>%
  mutate(npv_n_fr = npv_inf_fr * ((1+r)^rotation-1)/(1+r)^rotation)
  

  
