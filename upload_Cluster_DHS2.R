library(tidyverse)
library(dplyr)
library(tidyr)
library(shiny)

#clusterK <- read.delim("C:/Users/user/Desktop/DHS/pilot/rawF/clusterK.txt")
#structureK <- read.delim("C:/Users/user/Desktop/DHS/pilot/rawF/structureK.txt")
#householdK <- read.delim("C:/Users/user/Desktop/DHS/pilot/rawF/householdK.txt")
getwd()
setwd("C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat")
cluster <- read.delim("C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat - DHS/cluster1a.tab")
#cluster2 <- read.delim("C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat - DHS/cluster2a.tab")
cluster2a <- read.delim("C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat - DHS/cluster2a.tab")
cluster2b <- read.delim("C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat - DHS/cluster2b.tab")

structure <- read.delim("C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat - DHS/structure1a.tab")
household <- read.delim("C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat - DHS/household1a.tab")

cluster2<-cluster2a %>% left_join(cluster2b, by="clu00")

View(cluster)
View(cluster2)
View(structure)
View(household)

cid1<-c(1:10)

cluster$interview__id<-cluster$cc
cluster2$interview__id<-cluster2$cc
structure$interview__id<-structure$cc
household$interview__id<-household$cc

clusterM<-cluster %>% group_by(clu02)%>%
  mutate(cluster__id = row_number())
##cluster part
#View(clusterM)

clp<-clusterM %>% select(clu02,clu00)

clp1<-clp%>% 
  group_by(clu02) %>% 
  mutate(clu00_count = seq(n()) - 1) %>%
  pivot_wider(clu02, names_from =clu00_count, values_from = clu00, names_prefix = "clu00__")%>% as.data.frame()
#View(clp1)
#get the other data from the main cluster file
kk1<-clusterM %>% select(-clu00,-cluster__id) %>% 
  group_by(clu02,svn, cc, interview__id) %>%
  slice(1) %>% as.data.frame()

clp2<-kk1 %>% left_join(clp1, by="clu02") %>% filter(interview__id %in% cid1)
#View(clp2)
##create responsible
##   clp2$'_responsible'<-c("Guid_Nak20","Guid_Nak20")

write.table(clp2, file="C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat - DHS/finDhs/1to10/cl4.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

###---------------------------------------------------------------------------clustering
str2<-structure %>% select(clu00,s02)%>% group_by(clu00,s02)%>%slice(1)
str3<-str2 %>% 
  group_by(clu00) %>% 
  mutate(s02_count = seq(n()) - 1) %>%
  pivot_wider(clu00, names_from =s02_count, values_from = s02, names_prefix = "s02__") %>%as.data.frame()

#str3 
##excract cluster id from main file
clq<-clusterM %>% select(clu00, cluster__id)
str4<-str3 %>% left_join(clq, by="clu00")

cou_clu<-cluster2 %>% select(-s02,-cc) %>% 
  group_by(clu00,chief,chieftelphone,asschief,asschieftelphone,interview__id) %>%
  slice(1) %>% as.data.frame()

str5<-str4 %>% left_join(cou_clu, by="clu00") %>% select(-clu02)%>% filter(interview__id %in% cid1)

write.table(str5, file="C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat - DHS/finDhs/1to10/cluster.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')


############33------------------------------------------------mwitu
#View(d3)
#View(d3)
d3<-str5 %>% 
  pivot_longer(cols = starts_with('s02'), values_to = 's02') %>%    
  group_by(interview__id, cluster__id) %>% 
  mutate(structure__id = row_number())

d4<-d3 %>% filter(!is.na(s02)) %>% select(-name)
#View(d4)
d4$interview__id<-as.integer(d4$interview__id)

household1<-household %>% 
  group_by(clu00) %>% 
  mutate(h07 = row_number())

#View(household1)
#View(hh6)
hh5<-household1 %>% left_join(d4, by=c("clu00","interview__id","s02")) #%>% arrange(interview__id, clusterpart__id,s02)
hh6<-hh5 %>% group_by(clu00,cluster__id,structure__id) %>% mutate(household__id= row_number(structure__id))
hhf<-hh6 %>% select(interview__id,	cluster__id,	structure__id,	household__id,s02,h06b,h08, tel_h) %>% as.data.frame()
hhff<-hhf %>% select(-s02,-clu00)%>% filter(interview__id %in% cid1)
#View(hhff)
write.table(hhff, file="C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat - DHS/finDhs/1to10/household.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')
#-------------------------------end hh tab
#-------------------------------------------------------------------------------------wait
#-----------------------------------------------------------------WAIT
hhQ<-household %>% select(clu00,s02, h06b)
hhQ$uniqw<-paste0(hhQ$clu00,hhQ$s02)
hhR<-hhQ %>% 
  group_by(uniqw) %>% 
  mutate(h06b_count = seq(n()) - 1) %>%
  pivot_wider(uniqw, names_from =h06b_count, values_from = h06b, names_prefix = "h06b__") %>%as.data.frame()
#View(hhR)
hn<-hhQ %>% left_join(hhR, by="uniqw") %>% select(-h06b,-uniqw) %>% group_by(clu00,s02)%>%slice(1)
#View(fFn)



fFn<-d4 %>% left_join(hn, by=c("clu00","s02")) 

ghj<-structure %>% left_join(fFn, by=c("clu00","s02","interview__id")) 
fdd<-ghj %>% select(-clu00,-chief,-chieftelphone,-asschief,-asschieftelphone,-cc,-VE_name,-VE_tele)%>% 
  group_by(interview__id,cluster__id,structure__id)%>%slice(1)%>% filter(interview__id %in% cid1)

#gdf<-fdd %>% group_by(interview__id,cluster__id,structure__id) %>% filter(n()>1)
#View(gdf)
#View(fdd)#-------------------------------------------------------xxxx

#str23<-str22 %>% rename(gps__Latitude=s04,gps__Longitude=s05,gps__Altitude=s06)%>%as.data.frame()
str24<- fdd[order(fdd$interview__id,fdd$cluster__id,fdd$structure__id,fdd$s02),]
#View(str24)
write.table(str24, file="C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat - DHS/finDhs/1to10/structure.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

##creating the protected folder
variable__name<-c("s02","h06b","clu00")
vh<-data.frame(variable__name)

write.table(vh, file="C:/Users/user/Desktop/DHS/DOWNLOADED njesh - Copy-working/rawdat - DHS/finDhs/1to10/protected__variables.tab", na = "",
            row.names = F, col.names = T,   quote = F,   sep = '\t')

