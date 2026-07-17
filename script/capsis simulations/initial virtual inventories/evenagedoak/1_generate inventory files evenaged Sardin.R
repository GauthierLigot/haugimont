library(tidyverse)

# soit le scénario Chênaie continentale p 199, classe de fertilité 2 (bonne fertilité) 
# output directory
outdir <- "8_Simulations/3_evenagedoaksardin/"

age = 36
nha_post = 1000
nha_pre = 1500 
delta = 0.5 #the first thinning do not modify dmoy if delta = 0
gha = 14.7
sigma = 1.5 # arbitrary

# computations
dg = sqrt(gha/nha_post*4/pi)*100;dg
dmoy = sqrt(dg^2-sigma^2)

# generate a distribution
ds <- truncnorm::rtruncnorm(n=nha_post, mean=dmoy, sd=sigma,a=7) #bounded normal distribution
hist(ds)
gha2 <- sum((ds/200)^2*pi);gha2 # resulting basal area is approximately the same
nha2 <- length(ds);nha2
dg2 <- sqrt(gha2/nha2*4/pi)*100;dg2

ds2 <- truncnorm::rtruncnorm(n=nha_pre-nha_post, mean=dmoy-delta, sd=sigma,a=7) # bounded normal distribution, thinning from below (slightly)
ds.tot <- c(ds,ds2)
hist(ds.tot)
sum((ds.tot/100)^2*pi/4) #looks ok (basal area before thinning)


# build a tree table
TreeTable <- data.frame(Tree_Ids=1:length(ds.tot),SpCode=3,D=ds.tot#X=xy$x,Y=xy$y,Z=0,D=ds,H="NA",
                        #CBH="NA",CR="NA",Age=age,Quality="B",TreMs = "(0,0,0,0,0,0,0,0,0,0,0)",
                        #LifeStage = "alive")%>%
                        )%>%
  mutate(D = round(D, digits=1))

treeClass<-TreeTable%>%
  mutate(girth = D*pi)%>%
  mutate(girthClass = floor(girth/10)*10+5)%>%
  group_by(girthClass)%>%
  summarise(n = n())%>%
  mutate(sp = "CH")%>%
  select(sp,girthClass,n)


### generate also WALSI inventory
keywords<-list()%>%
  bind_rows(tibble(keyword = "standName", value = "evenaged"))%>%
  bind_rows(tibble(keyword = "inventoryDate", value =  paste(2000 + age)))%>%
  bind_rows(tibble(keyword = "standArea_ha", value = "1"))%>%
  bind_rows(tibble(keyword = "altitude", value = "300"))%>%
  bind_rows(tibble(keyword = "classWidth", value = "10"))%>%
  bind_rows(tibble(keyword = "naturalRegion", value = "5"))%>%
  bind_rows(tibble(keyword = "soilWaterHoldingCapacity_mm", value = "83"))%>%
  bind_rows(tibble(keyword = "polygon", value = "MULTIPOLYGON (((0 0,100 0,100 100,0 100)))"))

fileConn<-file(paste0(outdir,"inventory.inv"))

# keywords
keywordlines<-c()
for(i in 1:nrow(keywords)){
  line = paste(keywords$keyword[i],"=",keywords$value[i])
  keywordlines<-c(keywordlines,line)
}

# trees
Tree_title <- paste0("#",paste(names(treeClass),collapse="\t"))
Tree_lines <- c()

for(r in 1:nrow(treeClass)){
  line<- paste(treeClass[r,],collapse="\t")
  Tree_lines<-c(Tree_lines,line)
}   

alllines<-c(keywordlines,"\n",Tree_title,Tree_lines)

writeLines(alllines, fileConn)
close(fileConn)
  
