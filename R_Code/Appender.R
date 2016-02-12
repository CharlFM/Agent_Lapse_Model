#########################

### Subset Data (to reflect lapse status and also for additional testing)  ###
AgentsNTU <- NTUData %>% 
               group_by(AGENTNAME, Year) %>%
               summarise(Tot_Pols  =  n(), 
                         NTUs      =  sum(STATUS))

AgentsNTU$NTU_Rate <- AgentsNTU$NTUs / AgentsNTU$Tot_Pols

AgentsNTU_RecentYear  <-  AgentsNTU[AgentsNTU$Year == max(AgentsNTU$Year), ]
AgentsNTU_RecentYear  <-  AgentsNTU_RecentYear[with(AgentsNTU_RecentYear, order(NTU_Rate)), ]

AgentsLAP <- LAPData %>% 
                group_by(AGENTNAME, Year) %>%
                summarise(Tot_Pols  =  n(), 
                          LAPs      =  sum(STATUS), 
                          Dur       =  mean(DURATION))

NTUData <- subset(NTUData, select = -c(DURATION))
LAPData <- subset(LAPData, select = -c(DURATION))

AgentsLAP$LAP_Rate    <-  AgentsLAP$LAPs/AgentsLAP$Tot_Pols

AgentsLAP_RecentYear  <-  AgentsLAP[AgentsLAP$Year == max(AgentsLAP$Year), ]
AgentsLAP_RecentYear  <-  AgentsLAP_RecentYear[with(AgentsLAP_RecentYear, order(LAP_Rate)), ]

Top5NTU <- AgentsNTU_RecentYear$AGENTNAME[((nrow(AgentsNTU_RecentYear) - 4):nrow(AgentsNTU_RecentYear))]
Low5NTU <- AgentsNTU_RecentYear$AGENTNAME[1:5]
Top5LAP <- AgentsLAP_RecentYear$AGENTNAME[((nrow(AgentsLAP_RecentYear) - 4):nrow(AgentsLAP_RecentYear))]
Low5LAP <- AgentsLAP_RecentYear$AGENTNAME[1:5]

rm(AgentsNTU_RecentYear, AgentsLAP_RecentYear, AgentsLAP, AgentsNTU)









