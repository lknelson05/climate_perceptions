#OR if want average across any that contain a single one - 

phisheryavgmatrix = matrix(NA, nrow = 13, ncol = 10)
colnames(phisheryavgmatrix) <- c("fishery", "avg_exposure", "avg_sensitivity", "avg_ac", "ac_sd", "avg_risk", "risk_sd", "avg_risk_euc", "avg_vulnerability", "avg_vulnerabilty_euc")
phisheryavgmatrix[,1] = c("CPS", "dungeness crab", "groundfish", "hake", "HMS",
                         "salmon", "scallops","sea urchin", "shrimp", "squid", "geoduck", "sea cucumber", "razor clams")

#subset to the fishery and avg vulnerability by people who fish for that fishery
for(i in 1:nrow(phisheryavgmatrix)) {
  index = which(grepl(phisheryavgmatrix[i,1], responses$listoffisheries) == TRUE) 
  #which contain the fishery specified at i, 1 in the matrix
  #so for i = 1, this gives you which rows (which fishermen) fish for CPS
  phisheryavgmatrix[i,2] = mean(responses$indv_exposure[index], na.rm = TRUE)
  phisheryavgmatrix[i,3] = mean(responses$indv_sensitivity[index], na.rm = TRUE)
  phisheryavgmatrix[i,4] = mean(responses$indv_ac[index], na.rm = TRUE)
  phisheryavgmatrix[i,5] = sd(responses$indv_ac[index], na.rm = TRUE)
  phisheryavgmatrix[i,6] = mean(responses$indv_risk[index], na.rm = TRUE)
  phisheryavgmatrix[i,7] = sd(responses$indv_risk[index], na.rm = TRUE)
  phisheryavgmatrix[i,8] = mean(responses$risk_euc[index], na.rm = TRUE)
  phisheryavgmatrix[i,9] = mean(responses$indv_vulnerability[index], na.rm = TRUE)
  phisheryavgmatrix[i,10] = mean(responses$indv_vulnerability_euc[index], na.rm = TRUE)
  #average vulnerability across any fisherman that fishes that fishery -
  #would need to add columns to the matrix and then calculate avg exposure, avg 
  #sensitivity, etc. 
}

