#complete is a database where every observation is georeferenced with lat long
#use complete, it has been cleaned from NA's in localities
library("knitr")
library("gdata")
library("vegan")
library("bipartite")
library("betapart")
library("reshape")
library("labdsv")
library("car")
source("~/Dropbox/R resources/codes/pois_aov.R")
complete = read.csv(file="data/complete.csv", header=TRUE)
dim(complete)
names(complete)
complete = complete[!is.na(complete$Regist),] #removes 77 unused lat long NA rows
dim(complete)
summary(complete$lon)
summary(complete$lat) #use the min and max values for map making

# Species rank curve
table(complete$spp)
allsp = sort(table(complete$spp), decreasing = TRUE)
#make graph on paper
barplot(allsp[allsp>5], #xlab = "Species", 
        ylab = "Number of captures", cex.names=0.34,las=2) #Species Rank curve for all captures

all.caps <- summary(complete$general.habitat)


#remove edge, so we only consider forest species
complete$general.habitat <- factor(complete$general.habitat)
completeF = subset(complete, general.habitat !="edge")
complete$NetlineYEAR <- factor(complete$NetlineYEAR)
completeF = subset(completeF, NetlineYEAR !="2ndary_2011")
completeF = subset(completeF, NetlineYEAR !="Lek 1_2009")
completeF$general.habitat <- drop.levels(completeF$general.habitat, reorder=TRUE)
completeF$NetlineYEAR <- drop.levels(completeF$NetlineYEAR, reorder=TRUE)
forest.caps <- summary(completeF$general.habitat)
forest.net <- summary(completeF$NetlineYEAR)   ###
### Calculate the average number of captures, mean,stdev of forest.net
write.table(forest.net, col.names=TRUE)
capmeans <- mean(forest.net)
capsd <- sd(forest.net)
##completeF is forest captures, always use this summarized for 500 net hours
# print out chamizal and terra firme with date, summarized per species

#new captures subset, basically removing all repeated captures
norecap = completeF[ which(completeF$Codigo != "R"),]
norecap <- norecap[which(!is.na(norecap$general.habitat)),] #delete NA rows
norecap$Codigo <- drop.levels(norecap$Codigo, reorder=TRUE)
summary(norecap$general.habitat)

#individuals recapatured between years
re.cap = completeF[ which(completeF$Codigo =="A"),]
re.cap <- re.cap[which(!is.na(re.cap$general.habitat)),] #delete NA rows
recaps <- summary(re.cap$general.habitat)
## calculating % recaptures


#individuals recapatured between years on other netlines
rother.cap = completeF[ which(completeF$Codigo =="AO"),]
rother.cap <- rother.cap[which(!is.na(rother.cap$general.habitat)),] #delete NA rows
moves <- summary(rother.cap$general.habitat)
## calculating % recaptures elsewhere. only 92 movements across 

#new individuals subset
newcap = completeF[ which(completeF$Codigo == "N"),]
newcap <- newcap[which(!is.na(newcap$general.habitat)),] #delete NA rows
newcap$spp <- drop.levels(newcap$spp, reorder=TRUE)
newcap$FAMILY <- drop.levels(newcap$FAMILY, reorder=TRUE)
newcap$Genus <- drop.levels(newcap$Genus, reorder=TRUE)
no.individuals <- summary(newcap$general.habitat)
indivs.species <- summary(newcap$spp)


#Habitat specific - tierra firme
tf <- completeF[which(completeF$general.habitat =="clay"),] #clay]
tfcap = newcap[ which(newcap$general.habitat =="clay"),] #clay
tf$general.habitat <- drop.levels(tf$general.habitat, reorder=TRUE)
tf$spp <- drop.levels(tf$spp, reorder=TRUE)
tf$NetlineYEAR <- drop.levels(tf$NetlineYEAR, reorder=TRUE)
tfsp.table <- table(tfcap$spp)
tfsp = sort(table(tfcap$spp), decreasing = TRUE)
summary(tfcap)
#make graph 
barplot(tfsp[tfsp>5], #xlab = "Species", 
        ylab = "Number of captures", 
        main="Clay",
        cex.names=0.6,las=2) #Species Rank curve for all captures)
tfforest.caps <- summary(tf$general.habitat)
tfforest.net <- summary(tf$NetlineYEAR)  ###
### Calculate the average number of captures, mean,stdev of forest.net
tfcapmeans <- mean(tfforest.net)
tfcapsd <- sd(tfforest.net)

#Habitat specific - all white sands
ws <- completeF[which(completeF$general.habitat =="wsf"),]
wscap = newcap[ which(newcap$general.habitat =="wsf"),] #wsf
ws$general.habitat <- drop.levels(ws$general.habitat, reorder=TRUE)
ws$NetlineYEAR <- drop.levels(ws$NetlineYEAR, reorder=TRUE)
ws$spp <- drop.levels(ws$spp, reorder=TRUE)
wsfsp = sort(table(wscap$spp), decreasing = TRUE)
#make graph for birds captured in white sand forest
barplot(wsfsp[wsfsp>5],#xlab = "Species", 
        main="WSF",
        ylab = "Number of captures", cex.names=0.6,las=2) #Species Rank curve for all captures
wsforest.caps <- summary(ws$general.habitat)
wsforest.net <- summary(ws$NetlineYEAR)  ###
### Calculate the average number of captures, mean,stdev of forest.net
wscapmeans <- mean(wsforest.net)
wscapsd <- sd(wsforest.net)


#Habitat specific - chamizal

chamizal = newcap[ which(newcap$Specific.Habitat =="chamizal"),]
chamizal$spp <- drop.levels(chamizal$spp, reorder=TRUE)
chamizal$Specific.Habitat <- drop.levels(chamizal$Specific.Habitat, reorder=TRUE)
chamsp = sort(table(chamizal$spp), decreasing = TRUE)
barplot(chamsp[chamsp>2], ylab = "Number of captures", cex.names=0.41,las=2,
        main="Chamizal") #Species Rank curve for all captures)
#Habitat specific - varillal seco
vs = newcap[ which(newcap$Specific.Habitat =="varillal seco"),] #varillal seco
vs$Specific.Habitat <- drop.levels(vs$Specific.Habitat, reorder=TRUE)
vs$spp <- drop.levels(vs$spp, reorder=TRUE)
vsspp = sort(table(vs$spp), decreasing = TRUE)
#make graph for birds captured in white sand forest
barplot(vsspp[vsspp>5],#xlab = "Species", 
        main="Varillal Seco",
        ylab = "Number of captures", cex.names=0.5,las=2) #Species Rank curve for all captures

#Habitat specific - varillal humedo 
vhum = newcap[ which(newcap$Specific.Habitat =="varillal humedo"),]
vhum$Specific.Habitat <- drop.levels(vhum$Specific.Habitat, reorder=TRUE)
vhum$spp <- drop.levels(vhum$spp, reorder=TRUE)
vhsp = sort(table(vhum$spp), decreasing = TRUE)
vhsp
barplot(vhsp[vhsp>5], #xlab = "Species", 
        main="varillal humedo",
        ylab = "Number of captures", cex.names=0.4,las=2) #Species Rank curve for all captures


#to calculate diversity indices

norecap$general.habitat <- factor(norecap$general.habitat)
norecap$general.habitat <- drop.levels(norecap$general.habitat, reorder=TRUE)
norecap$spp <- drop.levels(norecap$spp, reorder=TRUE)
norecap$NetlineYEAR <- drop.levels(norecap$NetlineYEAR, reorder=TRUE)

###data.c is based on 500 hour netlines

data.m <- melt(norecap)
head(data.m)
data.c <- cast(data.m, NetlineYEAR ~ spp)
data.c <- drop.levels(data.c, reorder=TRUE)
str(data.c)
head(data.c)
rownames(data.c) <- data.c[,1]
data.c <- data.c[,-1] #removing rownames
data.c[data.c>0] <- 1 #presence/absence matrix

##Number of sites where species were detected during sampling
sites.species <- colSums(data.c)
sort(sites.species)
freq.hist.sites.species <- hist(sites.species,
                                breaks=pretty(0:93, n=15), 
                                xlim=c(1,100),
                                main=NULL,
                                xlab="Number of sites detected", 
                                ylab="Number of Species",
                                col="black")


###data.hab compares the two habitat types
data.m.hab <- melt(norecap)
head(data.m.hab)
data.hab <- cast(data.m.hab, general.habitat ~ spp)
data.hab <- drop.levels(data.hab, reorder=TRUE)
str(data.hab)
head(data.hab)
rownames(data.hab) <- data.c[,1]
#data.hab <- data.hab[,-1] #removing rownames
data.hab[data.hab>0] <- 1 #presence/absence matrix



#USING BETAPART

#creating data matrix for clay only
clay <- norecap[ which(norecap$general.habitat == "clay"),]
data.clay <- cast(clay, NetlineYEAR ~ spp)
data.clay <- drop.levels(data.clay, reorder=TRUE)
str(data.clay)
head(data.clay)
rownames(data.clay) <- data.clay[,1]
data.clay <- data.clay[,-1] #removing rownames
data.clay[data.clay>0] <- 1

## NUMBER OF SPECIES CAPUTRED ON CLAY NETLINES
clayspp <- rowSums(data.clay)

#creating data matrix for sand only
sand <- norecap[ which(norecap$general.habitat == "wsf"),]
data.sand <- cast(sand, NetlineYEAR ~ spp)
data.sand <- drop.levels(data.sand, reorder=TRUE)
str(data.sand)
head(data.sand)
rownames(data.sand) <- data.sand[,1]
#data.sand <- data.sand[,-1] #removing rownames
data.sand[data.sand>0] <- 1 #presence/absence matrix
write.csv(data.sand, file="sandbirds.csv", row.names=TRUE, col.names=TRUE)
## NUMBER OF SPECIES CAPUTRED ON WSF NETLINES
sandspp <- rowSums(data.sand)

### COMPARING WSF AND CLAY BY CAPTURE RATES
captable <- data.frame(c(tfforest.net,wsforest.net), Habitat=TRUE, Species=c(clayspp,sandspp))
names(captable)[1] <- "Captures"
captable$Habitat[1:19] <- "clay"
captable$Habitat[20:93] <- "wsf"
#captable <- captable 
rm(Captures,Habitat, Species)
attach(captable)
captable[,1] <- as.numeric(captable[,1])
hist((captable$Captures), xlab="number of captures",
     main="")
hist((captable$Captures), prob=TRUE, xlab="number of captures",
     main="") 
curve(dnorm(x, mean = mean(captable$Captures), sd = sd(captable$Captures)), col="red",add = TRUE)
shapiro.test(Captures)
compare.t.caps <- t.test(Captures~Habitat, data=captable)
by(Captures,Habitat,mean)
by(Captures,Habitat,sd)
boxplot(Captures~Habitat, main="Captures / 500 net hours", xlab="Habitat", ylab="Number of Captures")
mtext("n=19", side=1, line=2, at=1)
mtext("n=74", side=1, line=2, at=2)


hist(log(captable$Species), prob=TRUE, xlab="log(number of species)",
     main="")
curve(dnorm(x, mean = mean(log(captable$Species)), sd = sd(log(captable$Species))), col="red",add = TRUE)
shapiro.test(Species) ##test for normality
shapiro.test(log(Species))
compare.t.spps <- t.test(log(Species)~Habitat, data=captable)
by(Species,Habitat,mean)
by(Species,Habitat,sd)
boxplot(log(Species)~Habitat, main="Species / 500 net hours", xlab="Habitat", ylab="Log(Number of Species)")
mtext("n=19", side=1, line=2, at=1)
mtext("n=74", side=1, line=2, at=2)
legend("topright", c("Welch Two Sample t-test", 
                 "p-value = 0.01018"), cex=0.55)
###pois.aov(captable$Captures, as.factor(captable$Habitat))### No need, comparing with Poisson distr
##pois.aov(captable$Species, as.factor(captable$Habitat))### comparing with Poisson distr




###data.hab #Habitat specific - all white sands
ws <- completeF[which(completeF$general.habitat =="wsf"),]
wscap = newcap[ which(newcap$general.habitat =="wsf"),] #wsf
ws$general.habitat <- drop.levels(wscap$general.habitat, reorder=TRUE)
ws$NetlineYEAR <- drop.levels(wscap$Netline, reorder=TRUE)
ws$spp <- drop.levels(wscap$spp, reorder=TRUE)
wsfsp = sort(table(wscap$spp), decreasing = TRUE)
data.sand.islands <- melt(wscap)
head(data.sand.islands.v)
data.sand.islands.v <- cast(data.sand.islands, Netline ~ spp)
data.sand.islands <- drop.levels(data.sand.islands.v, reorder=TRUE)
str(data.sand.islands.v)
head(data.sand.islands.v)
rownames(data.sand.islands.v) <- data.c[,1]
#data.hab <- data.hab[,-1] #removing rownames
data.sand.islands.v[data.sand.islands.v>0] <- 1 #presence/absence matrix
write.csv(data.sand.islands.v, file="sandnetbirds.csv")
sizes=read.csv(file="data.env.csv")
write.csv(merge(data.sand.islands.v,sizes, by="Netline"), file="species.islandsize.csv")
dist=read.csv(file="newdis.csv")

birds <- read.csv('species.islandsize.csv')
birds  <- write.csv(merge(birds,dist, by="Netline"), file="species.islandsize.csv")

birds <- read.csv("species.islands.csv")
scatterplot(log(area)~log(MEAN)|Accipiter.superciliosus,data=birds,smoother=FALSE,
            reg.line=FALSE,xlab="isolation",ylab="Log Area")


pdf('presence.islands.pdf')
par(mfrow=c(1,1))
for(i in 14:length(birds)){
  scatterplot(log(area)~MEAN|birds[,i],data=birds,smoother=FALSE,
              reg.line=FALSE,xlab="isolation",ylab="Log Area",main=names(birds[i]))
}
dev.off()

head(birds)
species.islands <- rowSums(birds[,15:ncol(birds)])
plot(log(birds$area),species.islands, pch=19, xlab="Log Area", ylab="number of species")
properties <- data.frame(birds[,1:14], species=species.islands)
tmp <- ddply(properties, .(REGION), summarize, area=mean(area), isolation=mean(MEAN), spec.num=mean(species), spec.sd=sd(species))
plot(log(tmp$area),tmp$spec.num, pch=19, xlab="Log Area", ylab="number of species")
source("C:/Users/jungvari.UFAD/Dropbox/R resources/codes/error.barfunc.R")
error.bar(log(tmp$area), tmp$spec.num, upper=tmp$spec.sd)
tmp$spec.sd+tmp$spec.num
table(birds$REGION)
tmp <- data.frame(tmp, N=as.vector(table(birds$REGION)))
plot(log(tmp$area),tmp$spec.num, pch=19, xlab="Log Area", ylab="number of species")
source("C:/Users/jungvari.UFAD/Dropbox/R resources/codes/error.barfunc.R")
error.bar(log(tmp$area), tmp$spec.num, upper=1.96*(tmp$spec.sd/sqrt(tmp$N)))
