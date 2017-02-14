library(dplyr)
library(lubridate)
library(networkDynamic)

# 1. get graph membership for each year -----

setwd("~/Box Sync/ISAMW16/isamw16_analysis/")

cov <- read.csv("old/syria_covariates.csv")

nodes <- cov[,1:4]

nodes$end[is.na(nodes$end)] <- 2017

#create id nums & merge
nums <- data.frame(name=nodes$name, num=seq(1:length(nodes$name)))
nodes <- left_join(nodes, nums)

# 2. get edges ---------

edges <- read.csv("old/syria_alliances.csv", na.strings = "")
affil <- read.csv("old/syria_affilitations.csv", na.strings = "")

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
edges$start <- as.numeric(edges$start)

syrnet <- network.initialize(29, directed=F)
syrnet <- networkDynamic(vertex.spells = nodes[,c(3,4,5)], edge.spells = data.frame(edges[,3:6]), base.net = syrnet, verbose = F)

rm(nodes, edges)

# 4. set attributes ------

#import condensed cov data
cov2 <- read.csv("covariates_umbrellas.csv")
colnames(nums) <- c("name", "num")
cov2 <- right_join(cov2, nums)

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
network.vertex.names(syrnet) <- as.character(cov$abbrev)

#recode religion
cov$religion[cov$religion=="Christian"] <- "Other"
cov$religion[cov$religion=="Mixed"] <- "Other"
set.vertex.attribute(syrnet, 'religion',as.character( cov$religion))


rm(nums, cov, cov2)
