survey = read.csv("Surveydata_4.2020.csv", header = TRUE)

#ask laura about code that means you must do this ahead of time

survey$exposure = exposure$score
survey$sensitivity = sensitivity$score
survey$adaptive = acscore$rank
survey$vuln = percent_rank(survey$exposure + survey$sensitivity - survey$adaptive)


### Example 1: subset by a fishery

#index essentially placeholder
#change q1r2 for whatever fishery you want to look at
index = which(survey$q1r2 == 1) #q1r2 is fishing for dungeness crab
# if you changed column names, used the appropriate column name
dungycrab = survey[index,]


### Example 2: or make a new column and specify fishery(ies) for each person
# this will be better for averaging across a fishery

survey$listoffisheries = rep(NA, length =  nrow(survey))
# loop through each person and get what they are fishing for, put in new column
# should list each fishery they fish for
# not beautiful code but works
for(i in 1: nrow(survey)) {
  fisheriesname = ""
  if(survey$q1r1[i] == 1) {
    fisheriesname = paste(fisheriesname, "CPS", sep = "") 
  }
  if(survey$q1r2[i] == 1) {
    if(fisheriesname == "") { # have to do extra to not put a , at the beginning
      # if this is the first fishery 
      fisheriesname = paste(fisheriesname, "Dungeness crab", sep = "")
    } else {
      fisheriesname = paste(fisheriesname, "Dungeness crab", sep = ", ")
    }
  }
  if(survey$q1r3[i] == 1) {
    if(fisheriesname == "") {
      fisheriesname = paste(fisheriesname, "Groundfish", sep = "")
    } else {
      fisheriesname = paste(fisheriesname, "Groundfish", sep = ", ")
    }
  }
  if(survey$q1r4[i] == 1) {
    if(fisheriesname == "") {
    fisheriesname = paste(fisheriesname, "Hake", sep = "")
    } else {
      fisheriesname = paste(fisheriesname, "Hake", sep = ", ")
    }
  }
  if(survey$q1r5[i] == 1) {
    if(fisheriesname == "") {
      fisheriesname = paste(fisheriesname, "HMS", sep = "")
    } else {
      fisheriesname = paste(fisheriesname, "HMS", sep = ", ")
    }
  }
  if(survey$q1r6[i] == 1) {
    if(fisheriesname == "") {
      fisheriesname = paste(fisheriesname, "Salmon", sep = "")
    } else {
      fisheriesname = paste(fisheriesname, "Salmon", sep = ", ")
    }
  }
  if(survey$q1r7[i] == 1) {
    if(fisheriesname == "") {
      fisheriesname = paste(fisheriesname, "Scallops", sep = "")
    } else {
      fisheriesname = paste(fisheriesname, "Scallops", sep = ", ")
    }
  }
  if(survey$q1r8[i] == 1) {
    if(fisheriesname == "") {
      fisheriesname = paste(fisheriesname, "Sea urchin", sep = "")
    } else {
      fisheriesname = paste(fisheriesname, "Sea urchin", sep = ", ")
    }
  }
  if(survey$q1r9[i] == 1) {
    if(fisheriesname == "") {
      fisheriesname = paste(fisheriesname, "Shrimp", sep = "")
    } else {
      fisheriesname = paste(fisheriesname, "Shrimp", sep = ", ")
    }
  }
  if(survey$q1r10[i] == 1) {
    if(fisheriesname == "") {
      fisheriesname = paste(fisheriesname, "Squid", sep = "")
    } else {
      fisheriesname = paste(fisheriesname, "Squid", sep = ", ")
    }
  }
  if(survey$q1r11[i] == 1) {
    if(fisheriesname == "") { 
    fisheriesname = paste(fisheriesname, survey$q1r11oe[i], sep = "")
    #paste in what the person put for "other"
    }else {
      fisheriesname = paste(fisheriesname, survey$q1r11oe[i], sep = ", ")
    }
  }
  survey$listoffisheries[i] = fisheriesname
}

### Average across each UNIQUE "fishery" - so if they fish for salmon AND HMS, gets
# treated different than just fishing for salmon
#could use if fuction to pull out whatever "fishery" is desired with this method

library(plyr)
fisheryavg = ddply(survey, c("listoffisheries"), summarise,
                   exposure_avg = mean(exposure),
                   sensitivity_avg    = mean(sensitivity),
                   adaptive_avg = mean(adaptive),
                   vuln_avg = mean(vuln)
                   
)

# OR if want average across any that contain a single one - 
fisheryavgmatrix = matrix(NA, nrow = 10, ncol = 2)
fisheryavgmatrix[,1] = c("CPS", "Dungeness crab", "Groundfish", "Hake", "HMS",
                         "Salmon", "Scallops","Sea urchin", "Shrimp", "Squid")

#subset to the fishery and avg vulnerability by people who fish for that fishery
for(i in 1:nrow(fisheryavgmatrix)) {
  index = which(grepl(fisheryavgmatrix[i,1], survey$listoffisheries) == TRUE) 
  #which contain the fishery specified at i, 1 in the matrix
  # so for i = 1, this gives you which rows (which fishermen) fish for CPS
  fisheryavgmatrix[i,2] = mean(survey$vuln[index])
  # average vulnerability across any fisherman that fishes that fishery -
  # would need to add columns to the matrix and then calculate avg exposure, avg 
  # sensitivity, etc. 
}

fisheryavgmatrix #see what it looks like - avg vuln looks like its getting 
# treated as a character
as.numeric(fisheryavgmatrix[,2]) # gives you values


####### County #######
# find county and average vulnerability by county
county = read.csv("CommunityCounty2.csv", header = TRUE)
which(tolower(as.character(survey$q4[-91])) %in% tolower(county$City))
survey$county = rep(NA, length = nrow(survey))

for(i in 1:nrow(survey)) {
  if(i == 91) { #weird entry in 91
    name = ""
    tempindex = ""
  } else {
    name = tolower(as.character(survey$q4[i]))
    name = substr(name, start = 1, stop = 6) # hopefully first 6 characters are unique
    # if you correct names and only have the city name (no state abbreviation),
    # could get rid of this second line
    #grepl looks name in your whole list of counties
    tempindex = which(grepl(name, tolower(county$City)))
    if(length(tempindex) == 0) {
      survey$county[i] = NA
    } else if(length(tempindex) == 1) { #since cities are named the same in multiple 
      # states could have a length > 1 
      survey$county[i] = as.character(county$County[tempindex])
    }
  }
}

index = which(is.na(survey$county))
index = which(grepl("aberdeen", tolower(as.character(survey$q4[-91]))) == TRUE) 
survey$county[index] = "Grays Harbor"

#once cleaned up, and county for each, can then average by county
countyavg = ddply(survey, c("county"), summarise,
                   exposure_avg = mean(exposure),
                   sensitivity_avg    = mean(sensitivity),
                   adaptive_avg = mean(adaptive),
                   vuln_avg = mean(vuln)
                   
) # right now is finding an average across all that have county as "NA"
#look for those to spell right or figure out what county that city is in and add to list

