NFL_Data<-read.csv("~/Desktop/data101/spreadspoke_scores.csv")
head(NFL_Data)
NFL_Data

#Show modern nfl data using subset (modern nfl is from 2010 onwards)
NFL_modern<-subset(NFL_Data, schedule_season >= 2010)
#Creates a new column that consolidates team name
NFL_modern$team_home_clean<-NFL_modern$team_home

#Consolidates Name Change Variations of Washington NFL team, to make data more accurate
NFL_modern$team_home_clean[NFL_modern$team_home == "Washington Redskins"]<-"Washington Commanders" #replace all redskins data with commanders data (same home location, makes data more accurate because includes data from when name was different)
NFL_modern$team_home_clean[NFL_modern$team_home == "Washington Football Team"]<-"Washington Commanders" #same as above, consolidates to most recent NFL name
NFL_modern$team_away_clean[NFL_modern$team_away == "Washington Redskins"]<-"Washington Commanders" #repeats process for away games
NFL_modern$team_away_clean[NFL_modern$team_away == "Washington Football Team"]<-"Washington Commanders"

#use tapply to group means by team and calculate the mean for each, while using na.rm to remove null values to make the mean more accurate
avg_wind_clean<-tapply(NFL_modern$weather_wind_mph, NFL_modern$team_home_clean, mean, na.rm = TRUE)
#Avg Wind Speed Click bait Bar Plot, rotates x-axis teams and resizes them so they fit in frame, makes y axis increment by 2 using las, some cities were slightly cut off but still perfectly legible with team name, needed cex.names = 0.4 to have no team cities cut off on x-axis but then all names would be too small and illegible.
barplot(avg_wind_clean, xlab = "", ylab = "Avg Wind Speed (mph)", main = "Home Stadium vs Average Wind Speed (mph)", col = "blue", las = 2, cex.names = 0.7) #used chatgpt to try to format most effectively, but still could not make small enough for x axis to not be cut off but large enough to see in article.

#Cleans the score and wind data
avg_wind_clean<-tapply(NFL_modern$weather_wind_mph, NFL_modern$team_home_clean, mean, na.rm = TRUE)
avg_score_away<-tapply(NFL_modern$score_away, NFL_modern$team_home_clean, mean, na.rm = TRUE)
plot(avg_wind_clean, avg_score_away, xlab = "Avg Wind Speed (mph)", ylab = "Avg Score", col = "red") #tried this scatterplot but was not helpful, then later got a successful scatterplot of both home and away scores (I took the average) vs wind speed, used in my article


#********Bar plot of Cleveland wind avg compared to rest of league setup
cleveland_wind<-NFL_modern$weather_wind_mph[NFL_modern$team_home_clean == "Cleveland Browns"] #filters out all other teams but Cleveland
other_teams_wind<-NFL_modern$weather_wind_mph[NFL_modern$team_home_clean != "Cleveland Browns"] #filters out Cleveland from the rest of the league

#Calculate avgs
cleveland_avg<-mean(cleveland_wind, na.rm = TRUE) #na.rm removes null values to ensure accurate mean when dividing by number of values. Without removing null values, mean can be underrepresented (too small) since we are dividing by an inaccurately larger n
other_teams_avg<-mean(other_teams_wind, na.rm = TRUE)
averages<-c(cleveland_avg, other_teams_avg)
names(averages) #chatgpt helped me with this syntax, I was having troube debugging the x-axis labels for the barplot
barplot(averages, main = "Modern NFL Average Wind Speed Comparison", xlab = "NFL Team", ylab = "Wind Speed (mph)", col = c("green", "black"),) #

cleveland_games<-subset(NFL_Data, team_home == "Cleveland Browns") #makes a subset of games played in Cleveland
cleveland_games$win<-cleveland_games$score_home > cleveland_games$score_away #creates a win column for Cleveland when they score more points than the away team
windy_game_clev<-subset(cleveland_games, weather_wind_mph >= 10.8) #creates a subset of windy games if they are above or equal to Cleveland's average of 10.8
non_windy_game_clev<-subset(cleveland_games, weather_wind_mph < 10.8) #creates a subset of non windy games if they are below Cleveland's average of 10.8


#Wins rates in high wind vs low wind for Cleveland
windy_win_rate<-mean(windy_game_clev$win) #checked, doesn't need na.rm since same without it
non_windy_win_rate<-mean(non_windy_game_clev$win) #checked, doesn't need na.rm since same without it

win_rates<-c(windy_win_rate, non_windy_win_rate) #define different measurements that I am comparing
names(win_rates) <- c("High Wind (>= 10.8)", "Low Wind (< 10.8)") #used same syntax as chatgpt assisted barplot above
barplot(win_rates, main = "Cleveland Win Rate: Windy vs Non-Windy Games", ylab = "Win Rate", col = c("green","black"))


#other team variables, for win rate graph


other_teams_games<-subset(NFL_Data, team_home != "Cleveland Browns")

#Creates win column for other teams
other_teams_games$win<-other_teams_games$score_home > other_teams_games$score_away

#Creates subset for windy/non-windy conditions (above/below Cleveland wing avg)
windy_game_other<-subset(other_teams_games, weather_wind_mph >= 10.8)
non_windy_game_other<-subset(other_teams_games, weather_wind_mph < 10.8)

#Calculates other teams win rates
windy_win_rate_other<-mean(windy_game_other$win)
non_windy_win_rate_other<-mean(non_windy_game_other$win)
other_teams_improvement<-(windy_win_rate_other - non_windy_win_rate_other)

cleveland_improvement<-(windy_win_rate - non_windy_win_rate) #calculates difference to show improvement in wind

improvements<-c(cleveland_improvement, other_teams_improvement) 
names(improvements)<-c("Cleveland", "Other Teams") #used same syntax as chatgpt assisted barplot above
barplot(improvements, main = "Win Rate Improvement in Windy Games (>= 10.8 mph)", ylab = "Win Rate Increase", col = c("green", "black"), ylim = c(0, 0.10)) #bar plot visualizing increase in win rate due to high wind



#Calculates average points of home and away
NFL_modern$points<-((NFL_modern$score_home + NFL_modern$score_away)/2)

#Scatter plot Wind Speed vs Total Points
#Use nfl league wide data to show the trend on a large scale. Saw clear negative corelation, so useful in my article
plot(NFL_modern$weather_wind_mph, NFL_modern$points, main = "The Death of Offense: How Wind Kills Scoring", xlab = "Wind Speed (mph)",ylab = "Points Scored", col = "darkblue", pch = 16)




#Create a wind category for the entire league dataset
NFL_modern$WindCategory<-"Calm (< 10.8 mph)" #Use 10.8 mph to separate low/high wind
NFL_modern$WindCategory[NFL_modern$weather_wind_mph >= 10.8]<-"Windy (>= 10.8 mph)"

#box plot comparing league wide nfl points scored
boxplot(points ~ WindCategory, data = NFL_modern, main = "Modern NFL Scoring Distribution, Calm vs Windy", xlab = "Weather Condition",  ylab = "Points Scored",  col = c("skyblue", "lightgrey"))

#checks on sample size using table function effectively
table(NFL_modern$schedule_season >= 2010) #get how many games were played since 2010 for article
table(NFL_modern$WindCategory) #makes sure we have enough data to make an argument, enough data entrees in both low and high winds so argument valid! 
table(cleveland_wind >= 10.8) #same as above, makes sure we have enough windy data and non windy data, since we have 30+ of both (large enough sample size) we are good!
#getting median data to use in article
league_scoring_medians<-tapply(NFL_modern$points, NFL_modern$WindCategory, median, na.rm = TRUE)
league_scoring_medians
