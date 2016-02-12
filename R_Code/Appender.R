#########################

### Subset Data (to reflect lapse status and also for additional testing)  ###
All_lap_Data  <- All_lap_Data[All_lap_Data$VOICELOGGED <= seq(as.Date(fileXLSDate), length = 2, by = "-6 months")[2], ]

All_lap_Data <- All_lap_Data[All_lap_Data$AGENTNAME != "", ]

All_lap_Data$Year <- format(All_lap_Data$VOICELOGGED, format = "%Y")

NTU_Data <- All_lap_Data
NTU_Data$LapStat <- 0
NTU_Data$LapStat[NTU_Data$STATUS == "NTU"] <- 1

LAP_Data <- All_lap_Data[All_lap_Data$STATUS %in% c("LAP", "ACT"), ]
LAP_Data$LapStat <- 0
LAP_Data$LapStat[LAP_Data$STATUS == "LAP"] <- 1

AgentsNTU <- NTU_Data %>% 
  group_by(AGENTNAME,Year) %>%
  summarise(Tot_Pols = n(), NTUs = sum(LapStat))
AgentsNTU$NTU_Rate    <-  AgentsNTU$NTUs/AgentsNTU$Tot_Pols

AgentsNTU_RecentYear  <-  AgentsNTU[AgentsNTU$Year == max(AgentsNTU$Year),]
AgentsNTU_RecentYear  <-  AgentsNTU_RecentYear[with(AgentsNTU_RecentYear, order(NTU_Rate)), ]

AgentsLAP <- LAP_Data %>% 
  group_by(AGENTNAME, Year) %>%
  summarise(Tot_Pols = n(), LAPs = sum(LapStat), Dur = mean(DURATION))
AgentsLAP$LAP_Rate    <-  AgentsLAP$LAPs/AgentsLAP$Tot_Pols

AgentsLAP_RecentYear  <-  AgentsLAP[AgentsLAP$Year == max(AgentsLAP$Year),]
AgentsLAP_RecentYear  <-  AgentsLAP_RecentYear[with(AgentsLAP_RecentYear, order(LAP_Rate)), ]

Top5NTU <- AgentsNTU_RecentYear$AGENTNAME[((nrow(AgentsNTU_RecentYear)-5):nrow(AgentsNTU_RecentYear))]
Low5NTU <- AgentsNTU_RecentYear$AGENTNAME[1:5]
Top5LAP <- AgentsLAP_RecentYear$AGENTNAME[((nrow(AgentsLAP_RecentYear)-5):nrow(AgentsLAP_RecentYear))]
Low5LAP <- AgentsLAP_RecentYear$AGENTNAME[1:5]

Agent_Dat <- subset(Agent_Dat, select = c(STATUS, AFFINITYGROUP, PROVINCE, RACE))
Agent_Dat$STATUS <- as.factor(Agent_Dat$STATUS)
Agent_Dat$AFFINITYGROUP <- as.factor(Agent_Dat$AFFINITYGROUP)
Agent_Dat$PROVINCE <- as.character(Agent_Dat$PROVINCE)
Agent_Dat$RACE[is.na(Agent_Dat$RACE)] <- "UNKOWN"
Agent_Dat$RACE <- as.factor(Agent_Dat$RACE)
Agent_Dat$PROVINCE[Agent_Dat$PROVINCE == "" ] <- "UNKOWN"
Agent_Dat$PROVINCE <- as.factor(Agent_Dat$PROVINCE)

Agent_Dat$STATUS <- as.character(Agent_Dat$STATUS)
Agent_Dat$STAT <- 0
Agent_Dat$STAT[Agent_Dat$STATUS == "NTU"] <- 1

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(STATUS) ~ AFFINITYGROUP + PROVINCE + RACE, 
                          data = Agent_Dat, 
                          importance = TRUE, 
                          ntree = 1000)
varImpPlot(my_forest)
plot(varImp(my_forest, scale = TRUE))
for (i in 1:5) {
  
  Agent <- Top5NTU[i] # Top5NTU
  Agent
  
  Agent_Dat     <- All_lap_Data[All_lap_Data$AGENTNAME == Agent, ]
  Agent_Dat     <- All_lap_Data[All_lap_Data$AGENTNAME == Agent & All_lap_Data$STATUS %in% c("NTU", "ACT"), ]
  Agent_ACT_Dat <- All_lap_Data[All_lap_Data$STATUS == "NTU" & All_lap_Data$AGENTNAME == Agent, ]
  
  aff <- merge(data.frame(table(Agent_Dat$AFFINITYGROUP)), data.frame(table(Agent_NTU_Dat$AFFINITYGROUP)), by = "Var1")
  aff$Rank <- aff$Freq.y / aff$Freq.x
  
  MeanVals <- Agent_NTU_Dat[1,]
  
  MeanVals$AFFINITYGROUP <- 1
  
  MeanVals$NUMBEROFBENEFICIARIES <- mean(Agent_NTU_Dat$NUMBEROFBENEFICIARIES, na.rm = T)
  MeanVals$NOCREDITPROVIDERS <- mean(Agent_NTU_Dat$NOCREDITPROVIDERS, na.rm = T)
  
}









