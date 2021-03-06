#################################################################################################################################
################################################### AssertLife Lapse Analysis ###################################################
#################################################################################################################################

#####################################################################################################
# Initialize #
##############

# Clears Memory
rm(list = ls())
gc()

Path <- getwd()

source(paste(Path, "/R_Code/Initialize.R", sep = ""))

#####################################################################################################
# Load Data #
#############

source(paste(Path, "/R_Code/Load_Data.R", sep = ""))

#####################################################################################################
# Cleans Data #
###############

source(paste(Path, "/R_Code/Append_Sort.R", sep = ""))

#####################################################################################################
# Appending by Month #
######################

source(paste(Path, "/R_Code/Appender.R", sep = ""))

#####################################################################################################
# Prepare Model Data #
######################

AgentDat <- Low5LAP[2] # Low5LAP ; Top5LAP ; Low5NTU ; Top5NTU
AgentDat
Dat_Loop <- LAPData[LAPData$AGENTNAME == AgentDat, ]
Dat_Loop <- subset(Dat_Loop, select = -c(AGENTNAME, Year))
to_drop <- c()

for (i in 1:ncol(Dat_Loop)) {
  if (sum(Dat_Loop[[i]] == Dat_Loop[[i]][1]) == nrow(Dat_Loop)) {
    to_drop <- c(to_drop, i)
  }
}

Dat_Loop <- Dat_Loop[, -to_drop]

prop.table(table(Dat_Loop$STATUS))

source(paste(Path, "/R_Code/Prep_Model_Data.R", sep = ""))

#####################################################################################################
# Build Model #
###############

source(paste(Path, "/R_Code/Build_Model.R", sep = ""))

#####################################################################################################
# Test Model #
##############

source(paste(Path, "/R_Code/Testing.R", sep = ""))

impdf
print(auc$auc)











