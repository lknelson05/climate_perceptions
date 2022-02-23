#fishermen survey demographics
#install.packages("tidyverse")
library(tidyverse)
library(devtools)
devtools::install_github("jakelawlor/PNWColors")
library(PNWColors)
#install.packages("gmodels")
library(gmodels)

#summary stats of responses

#numbers in each fishery
fishery_participation<-data.frame(responses[,8:18])
colSums(fishery_participation)
#histogram of fishery responses
jpeg("fishery_participation.jpeg")
barplot(colSums(fishery_participation), 
        ylim = c(0,120),
          main = "Responses by Fishery", las=2,
        col = pnw_palette(name = "Starfish", n=11, type = "continuous"))
dev.off()

#main fisheries
main_fisheries<-colSums(demographics[,14:24])

#fishing region
fishing_region<-data.frame(responses[,32:38])
jpeg("fishing_region.jpeg")
barplot(colSums(fishing_region),
        names.arg = c("Puget Sound", "WA Coast", "Columbia River", "OR Coast", "Northern CA", "Central CA", "Southern CA"),
        cex.names = .7, ylim = c(0,60),
        main = "Responses by Fishing Area", las=2,
        col = pnw_palette(name = "Starfish", n=7, type = "continuous"))
dev.off()

#industry role
jpeg("industry role.jpeg")
industry_role<-data.frame(responses[,45:48])
barplot(colSums(industry_role), main="Role in Industry", 
        ylim = c(0, 140),
        col = pnw_palette(name = "Starfish", n=4, type = "continuous"))
dev.off()

#vessel length
jpeg("vessel length.jpeg")
demographics$length<-recode(demographics$length, "1"="25 or less", "2"="26 to 35", "3"="36 to 45", "4"="46 to 55", "5"="56 to 65", "6"="65 or greater")
barplot(table(demographics$length), main = "Vessel Length", col = pnw_palette(name = "Starfish"))

#gear types
gear_types<-colSums(demographics[,45:52])
barplot(gear_types, ylim = c(0,50), main = "Gear Types", las = 2, col = pnw_palette(name = "Starfish"))
#age
demographics$age<-recode(demographics$age, "1"="under 30", "2"="30-40", "3"="40-50", "4"="50-60", "5"="60-70", "6"="over 70")
barplot(table(demographics$age), main = "Ages", col = pnw_palette(name = "Starfish"))
#gender
demographics$gender<-recode(demographics$gender, "1"="female", "2"="male", "3"="prefer not to say", "4"="other")
table(demographics$gender)
#tribal
demographics$tribal<-recode(demographics$tribal, "1"="Y", "2"="N")
table(demographics$tribal)
geartype_byfishery<-(demographics[,1:12, 45:52])
temp = cbind(demographics[,2:12], demographics[,45:52])
library(dplyr)
temp = as.tibble(temp)
summary(temp)
table(temp)
#q20plot<-barplot(oceanconditionobs, main= "Observations of change", cex.main = .75,
                 #names.arg = c("ocean temperature", "severe weather", "species availability"), cex.names = .75,
                 #col = pnw_palette(name="Cascades", n=3, type="discrete"),
                 #legend = row.names(oceanconditionobs), beside = T)
#add record locater to tables from above to subset vulnerability
#geography with ID number
fishingarea<-data.frame(record=demographics$record, fishing_region)
