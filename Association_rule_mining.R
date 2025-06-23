library(stringr)
library(readxl)
library(xlsx)
library(openxlsx)
library(arules)
library(arulesViz)
library(dplyr)

# Read gold deposit data
Global_gold_deposits <- read.xlsx("./Global_gold_deposits.xlsx")

# Data preprocessing
Global_gold_deposits <- subset(Global_gold_deposits,Mineral_association != "")
Global_gold_deposits<-Global_gold_deposits[which(str_count(Global_gold_deposits$Mineral_association,",")>3),]
Type_min_merge <- paste(Global_gold_deposits$Deposit_type,Global_gold_deposits$Mineral_association,sep = ',')
associated.mins <- strsplit(as.character(Type_min_merge),',')

# z <- as(associated.mins, "transactions")
# itemFreq <- itemFrequency(z)
# itemFreq
# itemFreq_sort <- sort(itemFreq,decreasing = TRUE)
# itemFreq_sort[1]
# frequency = data.frame(freq=itemFreq_sort)
# summary(itemFreq)
# class(itemFreq)
# itemFrequencyPlot(z,top = 20,col = "lightblue",xlab = "",ylab = "",main = "",cex.names=1)

# Generate mineral association rules
type=c("IOCG","Porphyry","Epithermal","Orogenic","Carlin","VMS")
type_count=count(Global_gold_deposits,Deposit_type)
all_rules1 <- apriori(associated.mins, 
                      parameter = list(supp = 0.02, conf = 0.7,target = "rules",maxtime = 0,minlen = 2,maxlen=20),
                      appearance = list(rhs = type, default = 'lhs'))

# all_rules2 <- apriori(associated.mins, 
#                       parameter = list(supp = 0.04, conf = 0.7,target = "rules",maxtime = 0,minlen = 2,maxlen=20),
#                       appearance = list(rhs = type, default = 'lhs'))
# all_rules1 <- all_rules1[!is.redundant(all_rules1)]

# Obtain a subset of rules for each type of mineral deposit.
rules.sub.porphyry <- subset(all_rules1, subset = (rhs %in% "Porphyry"))
rules.sub.orogeny <- subset(all_rules1, subset = (rhs %in% "Orogenic"))
rules.sub.IOCG <- subset(all_rules1, subset = (rhs %in% "IOCG"))
rules.sub.Epithermal <- subset(all_rules1, subset = (rhs %in% "Epithermal"))
rules.sub.Carlin <- subset(all_rules1, subset = (rhs %in% "Carlin"))
rules.sub.VMS <- subset(all_rules1, subset = (rhs %in% "VMS"))


# Preview some of the generated rules
rules.sub.porphyry <- sort(rules.sub.orogeny, by = "confidence")
rules.sub.orogeny <- sort(rules.sub.orogeny, by = "confidence")
inspect(head(rules.sub.porphyry,n=10))
inspect(rules.sub.IOCG)
inspect(head(rules.sub.porphyry,n=10))
rules.sub.VMS.sort=sort(rules.sub.VMS, decreasing = TRUE,by = "support")
inspect(head(rules.sub.porphyry,n=10, by = "support"))
inspect(rules.sub.porphyry[41:50])
inspect(rules.sub.orogeny[21:40])
rules.sub.porphyry.sub <- subset(rules.sub.porphyry, subset = (lhs %in% "Chlorite"))
rules.sub.orogeny.sub <- subset(rules.sub.orogeny, subset = (lhs %in% "Galena"))

# Statistical sub-rule set Frequency of each mineral
rules.sub.orogeny.list<-as(rules.sub.orogeny@lhs,"list")
rules.sub.porphyry.list<-as(rules.sub.porphyry@lhs,"list")
rules.sub.IOCG.list<-as(rules.sub.IOCG@lhs,"list")
rules.sub.Epithermal.list<-as(rules.sub.Epithermal@lhs,"list")
rules.sub.Carlin.list<-as(rules.sub.Carlin@lhs,"list")
rules.sub.VMS.list<-as(rules.sub.VMS@lhs,"list")

items.orogeny<-data.frame(matrix(ncol = 2, nrow = sum(lengths(rules.sub.orogeny.list))))
items.porphyry<-data.frame(matrix(ncol = 2, nrow = sum(lengths(rules.sub.porphyry.list))))
items.IOCG<-data.frame(matrix(ncol = 2, nrow = sum(lengths(rules.sub.IOCG.list))))
items.Epithermal<-data.frame(matrix(ncol = 2, nrow = sum(lengths(rules.sub.Epithermal.list))))
items.Carlin<-data.frame(matrix(ncol = 2, nrow = sum(lengths(rules.sub.Carlin.list))))
items.VMS<-data.frame(matrix(ncol = 2, nrow = sum(lengths(rules.sub.VMS.list))))

colnames(items.orogeny) <- c('id','mineral')
colnames(items.porphyry) <- c('id','mineral')
colnames(items.IOCG) <- c('id','mineral')
colnames(items.Epithermal) <- c('id','mineral')
colnames(items.Carlin) <- c('id','mineral')
colnames(items.VMS) <- c('id','mineral')

x=0
for ( i in 1:length(rules.sub.VMS.list)){
  for (j in 1:length(rules.sub.VMS.list[[i]])) {
    x=x+1
    items.VMS[x,1]=i
    items.VMS[x,2]=rules.sub.VMS.list[[i]][j]
    if(x%%100000==0){print(x)}
  }
}
x=0
for ( i in 1:length(rules.sub.IOCG.list)){
  for (j in 1:length(rules.sub.IOCG.list[[i]])) {
    x=x+1
    items.IOCG[x,1]=i
    items.IOCG[x,2]=rules.sub.IOCG.list[[i]][j]
    if(x%%100000==0){print(x)}
  }
}
x=0
for ( i in 1:length(rules.sub.Epithermal.list)){
  for (j in 1:length(rules.sub.Epithermal.list[[i]])) {
    x=x+1
    items.Epithermal[x,1]=i
    items.Epithermal[x,2]=rules.sub.Epithermal.list[[i]][j]
    if(x%%100000==0){print(x)}
  }
}
x=0
for ( i in 1:length(rules.sub.Carlin.list)){
  for (j in 1:length(rules.sub.Carlin.list[[i]])) {
    x=x+1
    items.Carlin[x,1]=i
    items.Carlin[x,2]=rules.sub.Carlin.list[[i]][j]
    if(x%%100000==0){print(x)}
  }
}

x=0
for ( i in 1:length(rules.sub.orogeny.list)){
  for (j in 1:length(rules.sub.orogeny.list[[i]])) {
    x=x+1
    items.orogeny[x,1]=i
    items.orogeny[x,2]=rules.sub.orogeny.list[[i]][j]
    if(x%%100000==0){print(x)}
  }
}
x=0
for ( i in 1:length(rules.sub.porphyry.list)){
  for (j in 1:length(rules.sub.porphyry.list[[i]])) {
    x=x+1
    items.porphyry[x,1]=i
    items.porphyry[x,2]=rules.sub.porphyry.list[[i]][j]
    if(x%%100000==0){print(x)}
  }
}
# Obtain the characteristic mineral associations for each type of ore deposit.
count.VMS=count(items.VMS,mineral)
count.IOCG=count(items.IOCG,mineral)
count.porphyry=count(items.porphyry,mineral)
count.orogeny=count(items.orogeny,mineral)
count.Carlin=count(items.Carlin,mineral)
count.Epithermal=count(items.Epithermal,mineral)


# Obtain characteristic mineral visualization network graph edge data
mineral_links_VMS=data.frame(matrix(ncol = 2, nrow = 0))
colnames(mineral_links_VMS) <- c('source','target')
y=0
for (i in 1:384) {
  y=y+1
  if(y%%5000==0){
    print(y)
  }
  deposit=subset(items.VMS,items.VMS$id==i)
  
  MinCombo=combn(x=deposit$mineral,m = 2)
  mineral_links1=data.frame(source=MinCombo[1,],target=MinCombo[2,])
  mineral_links_VMS=rbind(mineral_links_VMS,mineral_links1)

}
mineral_links_IOCG=data.frame(matrix(ncol = 2, nrow = 0))
colnames(mineral_links_IOCG) <- c('source','target')
y=0
for (i in 1:268) {
  y=y+1
  if(y%%5000==0){
    print(y)
  }
  deposit=subset(items.IOCG,items.IOCG$id==i)
  if(dim(deposit)[1]>=2){  
   MinCombo=combn(x=deposit$mineral,m = 2)
   mineral_links1=data.frame(source=MinCombo[1,],target=MinCombo[2,])
   mineral_links_IOCG=rbind(mineral_links_IOCG,mineral_links1)
  }
}
mineral_links_Epithermal=data.frame(matrix(ncol = 2, nrow = 0))
colnames(mineral_links_Epithermal) <- c('source','target')
y=0
for (i in 1:17211) {
  y=y+1
  if(y%%5000==0){
    print(y)
  }
  deposit=subset(items.Epithermal,items.Epithermal$id==i)
  if(dim(deposit)[1]>=2){  
   MinCombo=combn(x=deposit$mineral,m = 2)
   mineral_links1=data.frame(source=MinCombo[1,],target=MinCombo[2,])
   mineral_links_Epithermal=rbind(mineral_links_Epithermal,mineral_links1)
  }
}
mineral_links_Carlin=data.frame(matrix(ncol = 2, nrow = 0))
colnames(mineral_links_Carlin) <- c('source','target')
y=0
for (i in 1:172813) {
  y=y+1
  if(y%%10000==0){
    print(y)
  }
  deposit=subset(items.Carlin,items.Carlin$id==i)
  if(dim(deposit)[1]>=2){  
   MinCombo=combn(x=deposit$mineral,m = 2)
   mineral_links1=data.frame(source=MinCombo[1,],target=MinCombo[2,])
   mineral_links_Carlin=rbind(mineral_links_Carlin,mineral_links1)
  }
}
mineral_links_orogeny=data.frame(matrix(ncol = 2, nrow = 0))
colnames(mineral_links_orogeny) <- c('source','target')
y=0
for (i in 2:154964) {
  y=y+1
  if(y%%10000==0){
    print(y)
  }
  deposit=subset(items.orogeny,items.orogeny$id==i)

   MinCombo=combn(x=deposit$mineral,m = 2)
   mineral_links1=data.frame(source=MinCombo[1,],target=MinCombo[2,])
   mineral_links_orogeny=rbind(mineral_links_orogeny,mineral_links1)

}
mineral_links_porphyry=data.frame(matrix(ncol = 2, nrow = 0))
colnames(mineral_links_porphyry) <- c('source','target')
y=0
for (i in 1:361317) {
  y=y+1
  if(y%%10000==0){
    print(y)
  }
  deposit=subset(items.porphyry,items.porphyry$id==i)

   MinCombo=combn(x=deposit$mineral,m = 2)
   mineral_links1=data.frame(source=MinCombo[1,],target=MinCombo[2,])
   mineral_links_porphyry=rbind(mineral_links_porphyry,mineral_links1)
   
}
mineral_Links_VMS=mineral_links_VMS %>% group_by_all() %>% count
mineral_Links_IOCG=mineral_links_IOCG %>% group_by_all() %>% count
mineral_Links_Epithermal=mineral_links_Epithermal %>% group_by_all() %>% count
mineral_Links_Carlin=mineral_links_Carlin %>% group_by_all() %>% count
mineral_Links_Orogeny=mineral_links_orogeny %>% group_by_all() %>% count
mineral_Links_Porphyry=mineral_links_porphyry %>% group_by_all() %>% count

write.xlsx(mineral_Links_VMS,file = 'mineral_Links_VMS.xlsx')
write.xlsx(mineral_Links_IOCG,file = 'mineral_Links_IOCG.xlsx')
write.xlsx(mineral_Links_Epithermal,file = 'mineral_Links_Epithermal.xlsx')
write.xlsx(mineral_Links_Carlin,file = 'mineral_Links_Carlin.xlsx')
write.xlsx(mineral_Links_Orogeny,file = 'mineral_Links_Orogeny.xlsx')
write.xlsx(mineral_Links_Porphyry,file = 'mineral_Links_Porphyry.xlsx')


