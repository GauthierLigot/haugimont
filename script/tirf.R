BAS<-function(year,net,r){
  if(length(year) == length(net)){
    actualized_fluxes <- net/(1+r)^year
    bas <- sum(actualized_fluxes)
  }else{
    print("ERROR : year and net vectors must of the same length.")
    bas <- NA
  }
  return(bas)
}

TIRF<-function(year,net,r_min=0.005,r_max=3,r_step=0.001,toplot=F,fonds){ #r_step is not used any more
  tmp<-data.frame()
  
  year<-c(0,year,max(year))
  net<-c(-fonds,net,fonds)
  
  
  f<- function(r) BAS(year, net, r)
  tir <- uniroot(f, interval = c(r_min, r_max))$root 
  
  if (toplot) {
    r_seq <- seq(r_min, r_max, length.out = 500)
    tmp <- data.frame(
      r = r_seq,
      bas = sapply(r_seq, f)
    )
    print(
      ggplot(tmp, aes(r, bas)) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = 2)
    )
  }
  
  # for(r in seq( r_min,r_max,by=r_step)){
  #   bas = BAS(year,net,r)
  #   tmp = rbind(tmp,data.frame(r,bas)
  #   )
  # }
  
  # if(toplot)
  #   print(ggplot(tmp,aes(x=r,y=bas))+geom_line()+geom_abline(intercept = 0,slope = 0, lty=2))
  # 
  # tmp<-tmp%>%
  #   mutate(bas0 = abs(bas-0)) %>%
  #   arrange(bas0)
  # 
  # tir = tmp$r[1]
  return(tir)
}
