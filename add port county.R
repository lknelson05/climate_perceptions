####### County #######
# find county and average vulnerability by county
county = read.csv("CommunityCounty2.csv", header = TRUE)
which(tolower(as.character(responses$homeport)) %in% tolower(county$City))
responses$county = rep(NA, length = nrow(responses))

for(i in 1:nrow(responses)) {
  if(i == 40) { #weird entry in 91
    name = ""
    tempindex = ""
  } else {
    name = tolower(as.character(responses$homeport[i]))
    name = substr(name, start = 1, stop = 6) # hopefully first 6 characters are unique
    # if you correct names and only have the city name (no state abbreviation),
    # could get rid of this second line
    #grepl looks name in your whole list of counties
    tempindex = which(grepl(name, tolower(county$City)))
    if(length(tempindex) == 0) {
      responses$county[i] = NA
    } else if(length(tempindex) == 1) { #since cities are named the same in multiple 
      # states could have a length > 1 
      responses$county[i] = as.character(county$County[tempindex])
    }
  }
}

index = which(is.na(responses$county))
index = which(grepl("aberdeen", tolower(as.character(responses$homeport[-40]))) == TRUE) 
responses$county[index] = "Grays Harbor"

#once cleaned up, and county for each, can then average by county
countyavg = ddply(survey, c("county"), summarise,
                  exposure_avg = mean(exposure),
                  sensitivity_avg    = mean(sensitivity),
                  adaptive_avg = mean(adaptive),
                  vuln_avg = mean(vuln)
                  
) # right now is finding an average across all that have county as "NA"
#look for those to spell right or figure out what county that city is in and add to list