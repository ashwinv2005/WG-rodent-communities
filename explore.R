library(tidyverse)
library(vegan)

data = read.csv("kdmcapture.csv", stringsAsFactors=F)
envvar = read.csv("envvar.csv", stringsAsFactors=F)
datavegan = read.csv("kdmcapturevegan.csv", stringsAsFactors=F, row.names = 1)
names(data)[1] = "grid"

head(datavegan)
head(envvar)
head(data)

envvar$shannon = diversity(datavegan,"shannon")
envvar[,(length(names(envvar))+1):(length(names(envvar))+length(names(datavegan)))] = datavegan

envvar = cbind(envvar,datavegan)

head(data)

with(envvar, plot(shannon~rock))

total = data %>%
  filter(!is.na(weight)) %>%
  group_by(grid) %>% summarize(wt = sum(weight), n = n())

mm = data %>%
  filter(!is.na(weight), species == "Mus musculus") %>%
  group_by(grid) %>% summarize(mmbiomass = sum(weight), mmn = n())

mb = data %>%
  filter(!is.na(weight), species == "Mus booduga") %>%
  group_by(grid) %>% summarize(mbbiomass = sum(weight), mbn = n())

rs = data %>%
  filter(!is.na(weight), species == "Rattus satarae") %>%
  group_by(grid) %>% summarize(rsbiomass = sum(weight), rsn = n())

envvar = left_join(envvar,total)
envvar = left_join(envvar,mm)
envvar = left_join(envvar,mb)
envvar = left_join(envvar,rs)

envvar

envvar$cat = "F"
envvar[envvar$grid == "GL1" | envvar$grid == "GL2" | envvar$grid == "TP1" | envvar$grid == "BT1",]$cat = "O"
