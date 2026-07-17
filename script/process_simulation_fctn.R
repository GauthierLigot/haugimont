# A function to extract the price of a given girth class and given species 
# In the previous version, there was no "sp" argument (see the commented lines, below)
# Price can be avergae if multiple price class are found and otherwise a warning is thrown
getprice<-function(cinf,csup,m=mercuriale2,sp,average=F){
  prices<-subset(m, circ > cinf & circ < csup & species_group_en == sp) %>%
    mutate(t1 = circ > cinf,
           t2= circ < csup,
           cinf2 = cinf,
           cup2 = csup)
  
  # print(paste0("cinf = ", cinf , " - csup = ",csup," - price = ", price))
  
  price<-NA
  
  if(average){
    price<-mean(prices$price_2019)
  }else if(length(prices$price_2019)!=1){
    print(paste("More than one price for this tree category : species = ",sp," - cinf = ", cinf))
  }else{
    price<-prices$price_2019
  }
  
  return(price)
}
# getprice<-function(cinf,csup,m=mercuriale2){
#   prices<-subset(m, circ > cinf & circ < csup)%>%
#     mutate(t1 = circ > cinf,
#            t2= circ < csup,
#            cinf2 = cinf,
#            cup2 = csup)
#   price<-mean(prices$price_2019)
#   return(price)
# }

# Composute the forest value for any point of time (within a cutting production cycle, that, assumingly, can be reproduced an infinite number of times)
FV<-function(year,net,r,a,final_year){
  tmp<-data.frame(year,net)
  v <- vector()
  
  for(j in 1:nrow(tmp)){
    i <- tmp$year[j]
    if(i>=a){
      v = c(v,tmp$net[j]*(1+r)^(final_year+a-i))
    }else{
      v = c(v, tmp$net[j]*(1+r)^(a-i))
    }
  }
  v2<-sum(v) / ((1+r)^final_year-1)
  return(v2)
}

# NPV of a fully regulated forest to account for capital variability through time and space
# It assumes that the forest is composed of stand of all ages and of equal area 
# Here, NPVinf is fist estimated from year 1 to year T (rotation). I omitted year "0" as this instant equals instant "n*"...
NPV_FR<-function(year,net,rotation,r=0.02){
  as<-1:(rotation)
  npvs<-tibble(a=as)%>%
    mutate(fv = NA)
  
  for(a in as){ #a = 0
    npvs$fv[npvs$a==a] <- FV(year, net, r, a, rotation) # the function is not yet vectorized
  }
  npv_fr<-mean(npvs$fv)
  return(npv_fr)
}

getBillBook<-function(rotation,cuts,stumpage,costs,management_cost){
  
  cut_tmp<- filter(cuts,year < rotation)%>%
    mutate(label = "Cut",
           category = "Timber sales",
           isIncome = T)
  
  harvest_tmp<-filter(stumpage, year == rotation)%>%
    mutate(label = "Final cut",
           category = "Timber sales",
           isIncome = T)
  
  management_tmp<-tibble(label = "Management", 
                         category = "Management", 
                         year = 1:rotation, isIncome = FALSE, value = management_cost)
  
  echeancier <- costs%>%
    bind_rows(cut_tmp)%>% 
    bind_rows(harvest_tmp)%>%
    bind_rows(management_tmp)%>%
    mutate(net = ifelse(isIncome,value,-value))%>%
    arrange(year)
  
  return(echeancier)
}

NPV<-function(year,net,r=0.02){
  npv<-sum(net/(1+r)^year)
  return(npv)
}

