#install.packages("dplyr")
#install.packages("lubridate")
library(data.table)
library(dplyr)
library(lubridate)
library(lfe)
#install.packages("lfe")

# #Diego Directory
setwd("S:/rothert/Soccer Project/Diego")

#Prof Kaplan Directory
#setwd("S:/rothert/Soccer Project/Diego")

Data <- fread("soccer_data_final.csv")

##First, just outputting this .csv data file to an .RData file
saveRDS(Data, "soccer_data_final.Rdata")

##Need to create a season variable. Each league should have 5 seasons. 
Data$Match_Date <- ymd(Data$Match_Date)
Data <- mutate(Data, Month_Year = format(Match_Date, "%m/%y"))

##Renaming Bundesliga
Data$League[Data$League == "FuÃŸball-Bundesliga"] <- "Bundesliga"

##Premier League##
Data_League1 <- filter(Data, League == "Premier League")


Data_League1 <- mutate(Data_League1, Season = ifelse(Match_Date >= "2017-08-11" & Match_Date <= "2018-05-13", "Premier League Season 1",
                                              ifelse(Match_Date >= "2018-08-10" & Match_Date <= "2019-05-12", "Premier League Season 2",
                                              ifelse(Match_Date >= "2019-08-09" & Match_Date <= "2020-07-26", "Premier League Season 3",
                                              ifelse(Match_Date >= "2020-09-12" & Match_Date <= "2021-05-23", "Premier League Season 4",
                                              ifelse(Match_Date >= "2021-08-13" & Match_Date <= "2022-05-22", "Premier League Season 5", 0))))))

##La Liga##
Data_League2 <- filter(Data, League == "La Liga")

 
Data_League2 <- mutate(Data_League2, Season = ifelse(Match_Date >= "2017-08-18" & Match_Date <= "2018-05-20", "La Liga Season 1",
                                              ifelse(Match_Date >= "2018-08-17" & Match_Date <= "2019-05-19", "La Liga Season 2",
                                              ifelse(Match_Date >= "2019-08-16" & Match_Date <= "2020-07-19", "La Liga Season 3",
                                              ifelse(Match_Date >= "2020-09-12" & Match_Date <= "2021-05-23", "La Liga Season 4",
                                              ifelse(Match_Date >= "2021-08-13" & Match_Date <= "2022-05-22", "La Liga Season 5", 0))))))
##Serie A##
Data_League3 <- filter(Data, League == "Serie A")
                                              

Data_League3 <- mutate(Data_League3, Season = ifelse(Match_Date >= "2017-08-19" & Match_Date <= "2018-05-20", "Serie A Season 1",
                                              ifelse(Match_Date >= "2018-08-18" & Match_Date <= "2019-05-26", "Serie A Season 2",
                                              ifelse(Match_Date >= "2019-08-24" & Match_Date <= "2020-08-02", "Serie A Season 3",
                                              ifelse(Match_Date >= "2020-09-19" & Match_Date <= "2021-05-23", "Serie A Season 4",
                                              ifelse(Match_Date >= "2021-08-21" & Match_Date <= "2022-05-22", "Serie A Season 5", 0))))))
                                              
                                              
##Ligue 1##
Data_League4 <- filter(Data, League == "Ligue 1")
                                              

Data_League4 <- mutate(Data_League4, Season = ifelse(Match_Date >= "2017-08-04" & Match_Date <= "2018-05-19", "Ligue 1 Season 1",
                                              ifelse(Match_Date >= "2018-08-10" & Match_Date <= "2019-05-24", "Ligue 1 Season 2",
                                              ifelse(Match_Date >= "2019-08-09" & Match_Date <= "2020-04-28", "Ligue 1 Season 3",
                                              ifelse(Match_Date >= "2020-08-21" & Match_Date <= "2021-05-23", "Ligue 1 Season 4",
                                              ifelse(Match_Date >= "2021-08-06" & Match_Date <= "2022-05-21", "Ligue 1 Season 5", 0))))))
                                              
                                              
##Bundelsiga##
Data_League5 <- filter(Data, League == "Bundesliga")
                                              

Data_League5 <- mutate(Data_League5, Season = ifelse(Match_Date >= "2017-08-18" & Match_Date <= "2018-05-12", "Bundesliga Season 1",
                                              ifelse(Match_Date >= "2018-08-24" & Match_Date <= "2019-05-18", "Bundesliga Season 2",
                                              ifelse(Match_Date >= "2019-08-16" & Match_Date <= "2020-07-27", "Bundesliga Season 3",
                                              ifelse(Match_Date >= "2020-09-18" & Match_Date <= "2021-05-22", "Bundesliga Season 4",
                                              ifelse(Match_Date >= "2021-08-13" & Match_Date <= "2022-05-14", "Bundesliga Season 5", 0))))))


                                              
#After season determined for each league
BigData <- rbind(Data_League1, Data_League2, Data_League3, Data_League4, Data_League5)


##Create distinct rows for each team by match observation (remove within-match player observations)
  
  #Group by season, home team, and away team, match-date, then use distinct
  UniqueMatchData <- distinct(BigData, Season, Home_Team, Away_Team, Match_Date)
  
  UniqueMatchData <- melt(UniqueMatchData, id.vars = c("Match_Date", "Season"), measure.vars = c("Home_Team", "Away_Team"))
  UniqueMatchData <- group_by(UniqueMatchData, Season, value) %>% arrange(Match_Date) %>% mutate(GameNumber = row_number())
  
  
  #
  BigData_subset <- dplyr::select(BigData, c(Match_Date, Home_Team, Home_Yellow_Cards, Home_Red_Cards,
                                      Away_Team, Away_Yellow_Cards, Away_Red_Cards, Season))
  
  
  #Line below is the same as this tu just added home and away red cards#
  #BigData_subset_melted <- melt(BigData_subset, id.vars = c("Match_Date", "Season", "Home_Yellow_Cards", "Away_Yellow_Cards"), 
                                #measure.vars = c("Home_Team", "Away_Team")) %>% distinct()
  
  BigData_subset_melted <- melt(BigData_subset, id.vars = c("Match_Date", "Season", "Home_Yellow_Cards", "Away_Yellow_Cards", "Home_Red_Cards", "Away_Red_Cards"), 
                                measure.vars = c("Home_Team", "Away_Team")) %>% distinct()
                    
  FinalData <- left_join(UniqueMatchData, BigData_subset_melted,
                         by = c("Match_Date" = "Match_Date",
                                "Season" = "Season",
                                "variable" = "variable",
                                "value" = "value"))                  

  #First, do an ifelse to create a single yellow card (and red card) column based on if variable = home_team or away_team
  
  #FinalData_test <- mutate(FinalData, Y_Card = ifelse(variable == "Home_Team", FinalData$Home_Yellow_Cards, FinalData$Away_Yellow_Cards))
  #FinalData_test <- mutate(FinalData_test, R_Card = ifelse(variable == "Home_Team", FinalData_test$Home_Red_Cards, FinalData_test$Away_Red_Cards))
  #for some reason when I ran these codes the numbers it was putting in wasn't changing, 3 yellow cards for away team would carry on for the entire season
  
  FinalData$Yellow <- ifelse(FinalData$variable == "Home_Team", FinalData$Home_Yellow_Cards, FinalData$Away_Yellow_Cards)
  FinalData$Red <- ifelse(FinalData$variable == "Home_Team", FinalData$Home_Red_Cards, FinalData$Away_Red_Cards)
  
  #it looks like from 2020-08-21 to 2020-09-20 the season got put in as 0 for Ligue 1 (between season 3-4)#
  
  #Then, group_by value (team name), arrange by game number, cumsum each card column
  
  FinalData <- FinalData %>%
              group_by(value, Season) %>%
              mutate(Season_Yellows = cumsum(Yellow))
  FinalData <- FinalData %>%
    group_by(value, Season) %>%
    mutate(Season_Reds = cumsum(Red))
  
  fwrite(FinalData, "team_cumulative_cards.csv")
  
  #Doing the same but for players
 

  Player_Data1 <- distinct(BigData, Season, Home_Team, Away_Team, Match_Date, Player)
  
  UniquePlayerMatchData <- melt(Player_Data1, id.vars = c("Match_Date", "Season"), measure.vars = c("Player"))
  UniquePlayerMatchData <- group_by(UniquePlayerMatchData, Season, value) %>% arrange(Match_Date) %>% mutate(GameNumber = row_number())
  
  BigData_subset_Player <- dplyr::select(BigData, c(Match_Date, Player, CrdY, CrdR, Fls, Fld, Season))
  
  BigData_subset_Player_melted <- melt(BigData_subset_Player, id.vars = c("Match_Date", "Season", "CrdY", "CrdR", "Fls", "Fld" ), 
                                measure.vars = c("Player")) %>% distinct()
  
  Final_Player_Data <- left_join(UniquePlayerMatchData, BigData_subset_Player_melted,
                                 by = c("Match_Date" = "Match_Date",
                                        "Season" = "Season",
                                        "variable" = "variable",
                                        "value" = "value"))
  
  Final_Player_Datatest <- Final_Player_Data %>%
    group_by(value, Season) %>%
    mutate(Season_Yellows = cumsum(CrdY),
           Season_Reds = cumsum(CrdR))
  
  #Final_Player_Datatest <- Final_Player_Data %>%
   # group_by(value, Season) %>%
  #  mutate(Season_Reds = cumsum(CrdR))
  
  fwrite(Final_Player_Datatest, "player_cumulative_cards.csv")
  
  
  
  
  
  
  
  #Just premier league player data, the data we want
  
  Prem_Player_Data <- distinct(Data_League1, Season, Home_Team, Away_Team, Match_Date, Player)
  Unique_Prem_Player <- melt(Prem_Player_Data, id.vars = c("Match_Date", "Season", "Home_Team", "Away_Team"), measure.vars = c("Player"))
  Unique_Prem_Player <- group_by(Unique_Prem_Player, Season, value) %>% arrange(Match_Date) %>% mutate(GameNumber = row_number())
  
  Prem_Player_Subset <- dplyr::select(Data_League1, c(Match_Date, Player, CrdY, CrdR, Fls, Fld, Season, Home_Team, Away_Team, Min, Tkl, Team))
  
  Prem_Player_melt <- melt(Prem_Player_Subset, id.vars = c("Match_Date", "Season", "CrdY", "CrdR", "Fls", "Fld", "Min", "Tkl", "Team" ), 
                           measure.vars = c("Player")) %>% distinct()
  Final_Prem_Player_Data <- left_join(Unique_Prem_Player, Prem_Player_melt,
                                      by = c("Match_Date" = "Match_Date",
                                             "Season" = "Season",
                                             "variable" = "variable",
                                             "value" = "value"))
  
  Prem_Player_Test <- Final_Prem_Player_Data %>%
    group_by(value, Season) %>%
    mutate(Season_Yellows = cumsum(CrdY),
           Season_Reds = cumsum(CrdR))
  
  #Adding a lag to the yellows
  Prem_Player_Test$Begin_Yellow <- Prem_Player_Test$Season_Yellows - Prem_Player_Test$CrdY
  
  Prem_Player_Test <- mutate(Prem_Player_Test, Game1_19 = ifelse(GameNumber <= 19, 1, 0))
  Prem_Player_Test <- mutate(Prem_Player_Test, Game20_32 = ifelse(GameNumber <= 32 & GameNumber >= 20, 1, 0))
  Prem_Player_Test <- mutate(Prem_Player_Test, Game33_38 = ifelse(GameNumber <= 38 & GameNumber >= 33, 1, 0))
  
  
  
  Prem_Player_Test$Cutoff_Tally <- ifelse(Prem_Player_Test$Game1_19 == 1, 5 - Prem_Player_Test$Season_Yellows, 
                                   ifelse(Prem_Player_Test$Game20_32 == 1, 10 - Prem_Player_Test$Season_Yellows,
                                   ifelse(Prem_Player_Test$Game33_38 == 1, 15 - Prem_Player_Test$Season_Yellows, 0)))
  
 # Prem_Player_Test <- mutate(Prem_Player_Test, Cutoff_2 = ifelse(Cutoff_Tally == 2, 1, 0),
                     #        Cutoff_1 = ifelse(Cutoff_Tally == 1, 1, 0))
  
  ##Creating GameID variable
  Prem_Player_Test <- mutate(Prem_Player_Test, GameID = paste0(Match_Date, Away_Team, Home_Team, collapse = NULL))
  Prem_Player_Test$GameID <- as.factor(Prem_Player_Test$GameID)
  
  ##Adding better rule specification after meeting with Rothert
  
  #This Y2Give/Susop_Dur Specification is off of Season_Yellows, or the accumulated yellows AFTER each game
  #Prem_Player_Test$Y2Give <- ifelse(Prem_Player_Test$GameNumber < 19 & Prem_Player_Test$Season_Yellows < 5, 5 - Prem_Player_Test$Season_Yellows,
                #           ifelse(Prem_Player_Test$GameNumber < 19 & Prem_Player_Test$Season_Yellows >= 5 & Prem_Player_Test$Season_Yellows < 10, 10 - Prem_Player_Test$Season_Yellows,
               #           ifelse(Prem_Player_Test$GameNumber < 19 & Prem_Player_Test$Season_Yellows >= 10 & Prem_Player_Test$Season_Yellows < 15, 15 - Prem_Player_Test$Season_Yellows,
              #           ifelse(Prem_Player_Test$GameNumber >= 19 & Prem_Player_Test$GameNumber < 32 & Prem_Player_Test$Season_Yellows < 10, 10 - Prem_Player_Test$Season_Yellows,
             #            ifelse(Prem_Player_Test$GameNumber >= 19 & Prem_Player_Test$GameNumber < 32 & Prem_Player_Test$Season_Yellows >= 10 & Prem_Player_Test$Season_Yellows < 15, 15 - Prem_Player_Test$Season_Yellows,
            #            ifelse(Prem_Player_Test$GameNumber >= 32 & Prem_Player_Test$Season_Yellows < 15, 15 - Prem_Player_Test$Season_Yellows, 0))))))
  
  
  #Prem_Player_Test$Susp_Dur <- ifelse(Prem_Player_Test$GameNumber <= 19 & Prem_Player_Test$Season_Yellows < 5, 1,
     #                          ifelse(Prem_Player_Test$GameNumber <= 19 & Prem_Player_Test$Season_Yellows >= 5 & Prem_Player_Test$Season_Yellows < 10, 2,
      #                         ifelse(Prem_Player_Test$GameNumber <= 19 & Prem_Player_Test$Season_Yellows >= 10 & Prem_Player_Test$Season_Yellows < 15, 3,
       #                        ifelse(Prem_Player_Test$GameNumber >= 19 & Prem_Player_Test$GameNumber < 32 & Prem_Player_Test$Season_Yellows < 10, 2,
        #                       ifelse(Prem_Player_Test$GameNumber >= 19 & Prem_Player_Test$GameNumber < 32 & Prem_Player_Test$Season_Yellows >= 10 & Prem_Player_Test$Season_Yellows < 15, 3,
         #                      ifelse(Prem_Player_Test$GameNumber >= 32 & Prem_Player_Test$Season_Yellows < 15, 3, 0))))))
  
  ##This Y2Give/Susp_Dur is using the accumulated yellows BEFORE each game
  
  Prem_Player_Test$Y2Give <- ifelse(Prem_Player_Test$GameNumber < 19 & Prem_Player_Test$Begin_Yellow < 5, 5 - Prem_Player_Test$Begin_Yellow,
             ifelse(Prem_Player_Test$GameNumber < 19 & Prem_Player_Test$Begin_Yellow >= 5 & Prem_Player_Test$Begin_Yellow < 10, 10 - Prem_Player_Test$Begin_Yellow,
             ifelse(Prem_Player_Test$GameNumber < 19 & Prem_Player_Test$Begin_Yellow >= 10 & Prem_Player_Test$Begin_Yellow < 15, 15 - Prem_Player_Test$Begin_Yellow,
             ifelse(Prem_Player_Test$GameNumber >= 19 & Prem_Player_Test$GameNumber < 32 & Prem_Player_Test$Begin_Yellow < 10, 10 - Prem_Player_Test$Begin_Yellow,
             ifelse(Prem_Player_Test$GameNumber >= 19 & Prem_Player_Test$GameNumber < 32 & Prem_Player_Test$Begin_Yellow >= 10 & Prem_Player_Test$Begin_Yellow < 15, 15 - Prem_Player_Test$Begin_Yellow,
             ifelse(Prem_Player_Test$GameNumber >= 32 & Prem_Player_Test$Begin_Yellow < 15, 15 - Prem_Player_Test$Begin_Yellow, 0))))))
  
  Prem_Player_Test$Susp_Dur <- ifelse(Prem_Player_Test$GameNumber <= 19 & Prem_Player_Test$Begin_Yellow < 5, 1,
                             ifelse(Prem_Player_Test$GameNumber <= 19 & Prem_Player_Test$Begin_Yellow >= 5 & Prem_Player_Test$Begin_Yellow < 10, 2,
                             ifelse(Prem_Player_Test$GameNumber <= 19 & Prem_Player_Test$Begin_Yellow >= 10 & Prem_Player_Test$Begin_Yellow < 15, 3,
                             ifelse(Prem_Player_Test$GameNumber >= 19 & Prem_Player_Test$GameNumber < 32 & Prem_Player_Test$Begin_Yellow < 10, 2,
                             ifelse(Prem_Player_Test$GameNumber >= 19 & Prem_Player_Test$GameNumber < 32 & Prem_Player_Test$Begin_Yellow >= 10 & Prem_Player_Test$Begin_Yellow < 15, 3,
                             ifelse(Prem_Player_Test$GameNumber >= 32 & Prem_Player_Test$Begin_Yellow < 15, 3, 0))))))
  
  #Making it so we have dummy variables for when Y2Give is 1 or 2
  
  Prem_Player_Test <- mutate(Prem_Player_Test, Cutoff_2 = ifelse(Y2Give == 2, 1, 0),
                             Cutoff_1 = ifelse(Y2Give == 1, 1, 0))
  
 #Adding dummy variables for when suspension duration is specific value
  
  Prem_Player_Test$SD1 <- ifelse(Prem_Player_Test$Susp_Dur == 1, 1, 0)
  Prem_Player_Test$SD2 <- ifelse(Prem_Player_Test$Susp_Dur == 2, 1, 0)
  Prem_Player_Test$SD3 <- ifelse(Prem_Player_Test$Susp_Dur == 3, 1, 0)
  
#Regression <- felm(Fls ~ Cutoff_Tally + Cutoff_2 + Cutoff_1 | value + Season, data = Prem_Player_Test)
#summary(Regression)

#DroppedNegatives <- filter(Prem_Player_Test, Cutoff_Tally >= 0)
#Regression_DroppedNegatives <- felm(Fls ~ Cutoff_Tally + Cutoff_2 + Cutoff_1 | GameID + value + Season, data = DroppedNegatives)
#summary(Regression_DroppedNegatives)


Prem_Player_Test$s1c1 <- Prem_Player_Test$Cutoff_1*Prem_Player_Test$SD1
Prem_Player_Test$s1c2 <- Prem_Player_Test$Cutoff_2*Prem_Player_Test$SD1
Prem_Player_Test$s2c1 <- Prem_Player_Test$Cutoff_1*Prem_Player_Test$SD2
Prem_Player_Test$s2c2 <- Prem_Player_Test$Cutoff_2*Prem_Player_Test$SD2
Prem_Player_Test$s3c1 <- Prem_Player_Test$Cutoff_1*Prem_Player_Test$SD3
Prem_Player_Test$s3c2 <- Prem_Player_Test$Cutoff_2*Prem_Player_Test$SD3



#Trying to filter to find player that has 2Y in a game, 5Y before 19 game
#There is a problem with how we have defined GameNumber, it is defined by how many times a certain row has shown up in a game for each season
#GameNumber should be defined by the matchweek
UnderstandingPt1 <- filter(Prem_Player_Test, CrdY == 2 & Game1_19 == 1)

#Ã‰rik Lamela Season 4

#Lamela <- filter(Prem_Player_Test, value == "Ã‰rik Lamela" & Season == "Premier League Season 4")
Lamela <- filter(Data_League1, Player == "Ã‰rik Lamela" & Season == "Premier League Season 4")

#Aaron Mooy, Season 3

#Mooy <- filter(Prem_Player_Test, value == "Aaron Mooy" & Season == "Premier League Season 3")
Mooy <- filter(Data_League1, Player == "Aaron Mooy" & Season == "Premier League Season 3")

#Mohammed Salisu, Season 5
Salisu <- filter(Data_League1, Player == "Mohammed Salisu" & Season == "Premier League Season 5")


#Regression with GameID fixed effects
Regression_withGameID <- felm(log(Fls+1) ~ log(Y2Give) + log(Tkl+1) + log(Min) + s1c1 + s1c2 + s2c1 + s2c2 + s3c1 + s3c2 | GameID + value + Season, data = Prem_Player_Test)
summary(Regression_withGameID)

 
 #Create cutoff-tally with if-else statement for each rule, then add fouls or another aggression thing
  #fixed effects for game, season, player
  #Beta2 (C-t ==2), 

#ask about negatives, and the specification of the regression#
#minutes played, tackles, on the right side of the regression

  #Dummy variables for each section of season cutoffs for rules (1 or 0 depending on what game you're in)
# interact these dumy variables each with the C-t, c-t ==2, c-t ==1



Team_by_Season <- distinct(Data_League1, Match_Date, Team, Season)
Team_by_Season <- group_by(Team_by_Season, Season, Team) %>% arrange(Match_Date) %>% mutate(GameNumber = row_number())

Players_by_Team <- distinct(Data_League1, Team, Player, Season)
New <- left_join(Team_by_Season, Players_by_Team,
                 by = c("Team" = "Team",
                        "Season" = "Season"))

#Dropping the incorrect GameNumber, this has everything we need but wrong game specifications
Prem_Player_Final <- subset(Prem_Player_Test, select = -c(GameNumber))
colnames(Prem_Player_Final)[6] = "Player"

#Left joining the correct game specifications with the dataset that has all the stats but incorrect Gamenumber
Final <- left_join(New, Prem_Player_Final,
                   by = c("Match_Date" = "Match_Date",
                          "Team" = "Team",
                          "Player" = "Player"))
#adding dummy variables for game numbers
Final <- mutate(Final, Game1_19 = ifelse(GameNumber <= 19, 1, 0))
Final <- mutate(Final, Game20_32 = ifelse(GameNumber <= 32 & GameNumber >= 20, 1, 0))
Final <- mutate(Final, Game33_38 = ifelse(GameNumber <= 38 & GameNumber >= 33, 1, 0))


#Updating Y2Give based off of the new game number
Final$Y2Give <- ifelse(Final$GameNumber < 19 & Final$Begin_Yellow < 5, 5 - Final$Begin_Yellow,
                           ifelse(Final$GameNumber < 19 & Final$Begin_Yellow >= 5 & Final$Begin_Yellow < 10, 10 - Final$Begin_Yellow,
                           ifelse(Final$GameNumber < 19 & Final$Begin_Yellow >= 10 & Final$Begin_Yellow < 15, 15 - Final$Begin_Yellow,
                           ifelse(Final$GameNumber >= 19 & Final$GameNumber < 32 & Final$Begin_Yellow < 10, 10 - Final$Begin_Yellow,
                           ifelse(Final$GameNumber >= 19 & Final$GameNumber < 32 & Final$Begin_Yellow >= 10 & Final$Begin_Yellow < 15, 15 - Final$Begin_Yellow,
                           ifelse(Final$GameNumber >= 32 & Final$Begin_Yellow < 15, 15 - Final$Begin_Yellow, 0))))))

#Updating Susp_Dur off of new game number
Final$Susp_Dur <- ifelse(Final$GameNumber <= 19 & Final$Begin_Yellow < 5, 1,
                             ifelse(Final$GameNumber <= 19 & Final$Begin_Yellow >= 5 & Final$Begin_Yellow < 10, 2,
                             ifelse(Final$GameNumber <= 19 & Final$Begin_Yellow >= 10 & Final$Begin_Yellow < 15, 3,
                             ifelse(Final$GameNumber >= 19 & Final$GameNumber < 32 & Final$Begin_Yellow < 10, 2,
                             ifelse(Final$GameNumber >= 19 & Final$GameNumber < 32 & Final$Begin_Yellow >= 10 & Final$Begin_Yellow < 15, 3,
                             ifelse(Final$GameNumber >= 32 & Final$Begin_Yellow < 15, 3, 0))))))

#Making it so we have dummy variables for when Y2Give is 1 or 2

Final <- mutate(Final, Cutoff_2 = ifelse(Y2Give == 2, 1, 0),
                           Cutoff_1 = ifelse(Y2Give == 1, 1, 0))

#Adding dummy variables for when suspension duration is specific value

Final$SD1 <- ifelse(Final$Susp_Dur == 1, 1, 0)
Final$SD2 <- ifelse(Final$Susp_Dur == 2, 1, 0)
Final$SD3 <- ifelse(Final$Susp_Dur == 3, 1, 0)

#adding dummy variables for when conditions are met
Final$s1c1 <- Final$Cutoff_1*Final$SD1
Final$s1c2 <- Final$Cutoff_2*Final$SD1
Final$s2c1 <- Final$Cutoff_1*Final$SD2
Final$s2c2 <- Final$Cutoff_2*Final$SD2
Final$s3c1 <- Final$Cutoff_1*Final$SD3
Final$s3c2 <- Final$Cutoff_2*Final$SD3

#Dropping the NAs
Final_Final <- na.omit(Final)

#Outputting the Data
fwrite(Final_Final, "Outputted Data/FOFS_Data.csv")

Regression_withGameID <- felm(log(Fls+1) ~ log(Y2Give) + log(Tkl+1) + log(Min) + s1c1 + s1c2 + s2c1 + s2c2 + s3c1 + s3c2 | GameID + Player + Season.x, data = Final_Final)
summary(Regression_withGameID)

Regression_2 <- felm(Fls ~ Y2Give + Tkl + Min + s1c1 + s1c2 + s2c1 + s2c2 + s3c1 + s3c2 | GameID + Player + Season.x, data = Final_Final)
summary(Regression_2)

Regression_3 <- felm(log(Fls+1) ~ Y2Give + Tkl + Min + s1c1 + s1c2 + s2c1 + s2c2 + s3c1 + s3c2 | GameID + Player + Season.x | 0 | GameID + Player, data = Final_Final)
summary(Regression_3)

