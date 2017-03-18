library(readr)
library(dplyr)
library(lubridate)
library(networkDynamic)

# 1. get graph membership for each year -----
setwd("~/Dropbox/Dissertation/Analyses/Alliance_Network_Chapter/isamw16_analysis/")

cov <- read.csv("old/syria_covariates.csv", stringsAsFactors = F)

nodes <- cov[,1:4]

nodes$end[is.na(nodes$end)] <- 2017

#create id nums & merge
nums <- data.frame(name=nodes$name, num=seq(1:length(nodes$name)))
nodes <- left_join(nodes, nums)

# 2. get edges ---------

edges <- read.csv("old/syria_alliances.csv", na.strings = "", stringsAsFactors = F)
affil <- read.csv("old/syria_affilitations.csv", na.strings = "", stringsAsFactors = F)

#merge alliances & affiliations
edges <- rbind(edges[,-5], affil)

#fix dates
edges <- edges %>%  mutate(start=year(mdy(start)),end=year(mdy(end)))
edges$end[is.na(edges$end)] <- 2017

#aggregate to fix a few duplicates
edges <- edges %>%
  group_by(groupA, groupB) %>%
  summarize(start=min(start),end=max(end))

rm(affil)

#merge in numbers
colnames(nums) <- c("groupA","numA")
edges <- left_join(edges, nums)
colnames(nums) <- c("groupB","numB")
edges <- left_join(edges, nums)

# 3. convert to networkdynamic object -------
syrnet <- network.initialize(29, directed=F)
syrnet <- networkDynamic(vertex.spells = data.frame(nodes[,3:5]), edge.spells = data.frame(edges[,3:6]), base.net = syrnet, verbose = F)

rm(nodes, edges)

# 4. set attributes ------

#import condensed cov data
cov2 <- read.csv("covariates_umbrellas.csv", stringsAsFactors = F)
colnames(nums) <- c("name", "num")
cov2 <- right_join(cov2,nums)

#set covariates
set.vertex.attribute(syrnet, 'ltroops.hi', log(cov2$troop.high+1))
set.vertex.attribute(syrnet, 'ltroops.low', log(cov2$troop.low+1))
set.vertex.attribute(syrnet, 'rtroops.hi', rank(cov2$troop.high))
set.vertex.attribute(syrnet, 'rtroops.low', rank(cov2$troop.low))
set.vertex.attribute(syrnet, 'troops.hi', cov2$troop.high+1)
set.vertex.attribute(syrnet, 'troops.low', cov2$troop.low+1)
set.vertex.attribute(syrnet, 'sunni', cov2$sunni)
set.vertex.attribute(syrnet, 'shia', cov2$shia)
set.vertex.attribute(syrnet, 'jihadist', cov2$jihadist)
set.vertex.attribute(syrnet, 'islamist', cov2$islamist)
set.vertex.attribute(syrnet, 'caliphate', cov2$caliphate)
set.vertex.attribute(syrnet, 'shia.interests', cov2$shia.interests)
set.vertex.attribute(syrnet, 'moderate', cov2$moderate)
set.vertex.attribute(syrnet, 'territory', cov2$territorial.aims)
set.vertex.attribute(syrnet, 'polwing', cov2$polwing)
set.vertex.attribute(syrnet, 'main.objective', as.character(cov$main.objective))
set.vertex.attribute(syrnet, 'origin', as.character(cov$origin))
set.vertex.attribute(syrnet, 'abbrev', as.character(cov2$abbrev))
set.vertex.attribute(syrnet, 'resources', cov2$resources)
set.vertex.attribute(syrnet, 'foreign.fighter', cov2$foreign.fighters)
set.vertex.attribute(syrnet, 'civilian.targeting', cov2$civilian.targeting)
set.vertex.attribute(syrnet, 'us', cov2$us)
set.vertex.attribute(syrnet, 'iran', cov2$iran)
set.vertex.attribute(syrnet, 'gcc', cov2$gcc)
set.vertex.attribute(syrnet, 'iran', cov2$iran)

#recode religion
cov$religion[cov$religion=="Christian"] <- "Other"
cov$religion[cov$religion=="Mixed"] <- "Other"
set.vertex.attribute(syrnet, 'religion',as.character( cov$religion))

network.vertex.names(syrnet) <- as.character(cov$abbrev)

rm(cov)

# 5. convert to Siena object ------

#extract networks for each year
s11 <- as.network(network.extract(syrnet, at=2011))
s12 <- as.network(network.extract(syrnet, at=2012))
s13 <- as.network(network.extract(syrnet, at=2013))
s14 <- as.network(network.extract(syrnet, at=2014))
s15 <- as.network(network.extract(syrnet, at=2015))
s16 <- as.network(network.extract(syrnet, at=2016))

syria <- list(s11,s12,s13,s14,s15,s16)

s1 <- as.network(network.extract(syrnet, 2011, 2012))
s2 <- as.network(network.extract(syrnet, 2013, 2014))
s3 <- as.network(network.extract(syrnet, 2015, 2016))

syria.condensed <- list(s1,s2,s3)

#remove shia
delete.vertices(syrnet, 1:7)

s11 <- as.network(network.extract(syrnet, at=2011))
s12 <- as.network(network.extract(syrnet, at=2012))
s13 <- as.network(network.extract(syrnet, at=2013))
s14 <- as.network(network.extract(syrnet, at=2014))
s15 <- as.network(network.extract(syrnet, at=2015))
s16 <- as.network(network.extract(syrnet, at=2016))

syria.nonshia <- list(s11,s12,s13,s14,s15,s16)

rm(s11,s12,s13,s14,s15,s16,s1,s2,s3)

# 6. Create dyadic covariates  ---------
# http://mjh4.blogspot.fr/2012/09/ergm-edgecov-and-dyadcov-specifications.html

# create a list of all possible edges
full <- data.frame(head=numeric(),tail=numeric())

# for(i in nums$num){
#   head = rep(i, length(nums$num)-1)
#   tail = nums[-i, "num"]
#   x = cbind(head, tail)
#   full = rbind(full, x)
# }

for(i in nums$num){
  head = rep(i, length(nums$num))
  tail = nums[, "num"]
  x = cbind(head, tail)
  full = rbind(full, x)
}

rm(head,tail,i,x,nums)

# merge covariates
cov2 <- cov2[,c(13:17,20:23,25)]

colnames(cov2)[10] <- "head"

full <- left_join(full, cov2)

colnames(cov2)[10] <- "tail"

full <- left_join(full, cov2, by="tail", suffix=c(".head",".tail"))


#recode variables
full$same.loc <- ifelse((full$west.head==1 & full$west.tail==1) | (full$east.head==1 & full$east.tail==1) | (full$south.head==1 & full$south.tail==1) | (full$iraq.head==1 & full$iraq.tail==1), 1, 0)

full$same.support <- ifelse((full$us.head==1 & full$us.tail==1) | (full$iran.head==1 & full$iran.tail==1) | (full$gcc.head==1 & full$gcc.tail==1) | (full$turkey.head==1 & full$turkey.tail==1), 1, 0)

full$same.support.exact <- ifelse((full$us.head==full$us.tail) & (full$iran.head==full$iran.tail) & (full$gcc.head==full$gcc.tail) & (full$turkey.head==full$turkey.tail) & rowSums(full[,4:7]) > 0, 1, 0)

full <- select(full, head, tail, same.loc, same.support, same.support.exact)

#convert to network
syria.full <- network(full[,1:2], directed = F)

set.edge.attribute(syria.full, 'same.loc', full$same.loc)
set.edge.attribute(syria.full, 'same.support', full$same.support)
set.edge.attribute(syria.full, 'same.support.exact', full$same.support.exact)

network.vertex.names(syria.full) <- cov2$abbrev

rm(cov2)

# Prep covariates
library(xergm)
