####################################################################################################################
# Preparing the model All_lap_Data #
####################################

print("Observed NTU's")
prop.table(table(NTUData$STATUS))

NTUDataDummy <- dummyVars(formula   =  "~.", 
                          data      =  NTUData, 
                          fullRank  =  FALSE)

NTUData      <- as.data.frame(predict(NTUDataDummy, NTUData))

rm(NTUDataDummy)

outcomeName     <- "STATUS"
predictorsNames <- names(NTUData)[names(NTUData) != outcomeName]

splitIndex <- createDataPartition(y      =  NTUData[, outcomeName], 
                                  p      =  0.75, 
                                  list   =  FALSE, 
                                  times  =  1)

trainDF <- NTUData[ splitIndex, ]
testDF  <- NTUData[-splitIndex, ]






