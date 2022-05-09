#Group Project
#Group 20: Owen Koury, Noah Prasad, Victoria Markuson, Delaney Collis
#4/20/22

rm(list=ls())

setwd("/Users/owenkoury/Desktop/Wrangling Project")

library(xml2)
library(readxl)
library(scales)
library(ggplot2)
library(dplyr)

Team_Stats <- read_excel("Team Stats.xlsx")
Advanced_Stats<-read_excel("Advanced Stats.xlsx")

Heat<-read.csv("Heat.csv")
Heat$Team<-"Heat"
Celtics<-read.csv("Celtics.csv")
Celtics$Team<-"Celtics"
Bucks<-read.csv("Bucks.csv")
Bucks$Team<-"Bucks"
Sixers<-read.csv("76ers.csv")
Sixers$Team<-"Sixers"
Raptors<-read.csv("Raptors.csv")
Raptors$Team<-"Raptors"
Bulls<-read.csv("Bulls.csv")
Bulls$Team<-"Bulls"
Nets<-read.csv("Nets.csv")
Nets$Team<-"Nets"
Hawks<-read.csv("Hawks.csv")
Hawks$Team<-"Hawks"
Pistons<-read.csv("Pistons.csv")
Pistons$Team<-"Pistons"
Magic<-read.csv("Magic.csv")
Magic$Team<-"Magic"
Pacers<-read.csv("Pacers.csv")
Pacers$Team<-"Pacers"
Wizards<-read.csv("Wizards.csv")
Wizards$Team<-"Wizards"
Knicks<-read.csv("Knicks.csv")
Knicks$Team<-"Knicks"
Hornets<-read.csv("Hornets.csv")
Hornets$Team<-"Hornets"
Cavaliers<-read.csv("Cavaliers.csv")
Cavaliers$Team<-"Cavaliers"

Suns<-read.csv("Suns.csv")
Suns$Team<-"Suns"
Grizzlies<-read.csv("Grizzlies.csv")
Grizzlies$Team<-"Grizzlies"
Warriors<-read.csv("Warriors.csv")
Warriors$Team<-"Warriors"
Mavericks<-read.csv("Mavericks.csv")
Mavericks$Team<-"Mavericks"
Jazz<-read.csv("Jazz.csv")
Jazz$Team<-"Jazz"
Nuggets<-read.csv("Nuggets.csv")
Nuggets$Team<-"Nuggets"
Timberwolves<-read.csv("Timberwolves.csv")
Timberwolves$Team<-"Timberwolves"
Pelicans<-read.csv("Pelicans.csv")
Pelicans$Team<-"Pelicans"
Clippers<-read.csv("Clippers.csv")
Clippers$Team<-"Clippers"
Spurs<-read.csv("Spurs.csv")
Spurs$Team<-"Spurs"
Lakers<-read.csv("Lakers.csv")
Lakers$Team<-"Lakers"
Kings<-read.csv("Kings.csv")
Kings$Team<-"Kings"
Trailblazers<-read.csv("Trailblazers.csv")
Trailblazers$Team<-"Trailblazers"
Thunder<-read.csv("Thunder.csv")
Thunder$Team<-"Thunder"
Rockets<-read.csv("Rockets.csv")
Rockets$Team<-"Rockets"


East<-rbind(Heat,Celtics,Bucks,Sixers,Raptors,Bulls,Nets,Hawks,Pistons,Magic,Pacers,Wizards,Knicks,Hornets,Cavaliers)
West<-rbind(Suns,Grizzlies,Warriors,Mavericks,Jazz,Nuggets,Timberwolves,Pelicans,Clippers,Spurs,Lakers,Kings,Trailblazers,Thunder,Rockets)

NBA<-rbind(East,West)

teams <- c("mia", "bos", "mil", "phi", "tor", "chi", 'bkn', 'cle', 'atl', 'cha', 'nyk', 'was', 'ind', 'det', 'orl', 'pho', 'mem', 'gsw', 'dal', 'utah', 'den', 'min', 'lac', 'no', 'sas', 'lal', 'sac', 'por', 'okc', 'hou')

team_sce <- character(0)
team_she <- character(0)
team_name <- character(0)

for (team in teams){
  link <- paste('https://www.espn.com/nba/team/stats/_/name/', team, sep = "")
  page <- read_html(link)
  
  sce2 <- xml_text(xml_find_all(page, '//span[@class = "Stats__TotalRow fw-bold"]'))[29]
  team_sce <- c(team_sce, sce2)
  
  she2 <- xml_text(xml_find_all(page, '//span[@class = "Stats__TotalRow fw-bold"]'))[30]
  team_she <- c(team_she, she2)
  
  city <- xml_text(xml_find_all(page, '//span[@class="db pr3 nowrap"]'))
  name <- xml_text(xml_find_all(page, '//span[@class="db fw-bold"]'))
  full_name <- paste(city, name, sep = ' ')
  team_name <- c(team_name, full_name)
}

efficiency <- data.frame(team_name, team_sce, team_she)
colnames(efficiency)[1] <- "TEAM"

efficiency$TEAM[efficiency$TEAM=="Miami Heat"]<-"Heat"
efficiency$TEAM[efficiency$TEAM=="Boston Celtics"]<-"Celtics"
efficiency$TEAM[efficiency$TEAM=="Portland Trail Blazers"]<-"Trail Blazers"
efficiency$TEAM[efficiency$TEAM=="Milwaukee Bucks"]<-"Bucks"
efficiency$TEAM[efficiency$TEAM=="Philadelphia 76ers"]<-"76ers"
efficiency$TEAM[efficiency$TEAM=="Toronto Raptors"]<-"Raptors"
efficiency$TEAM[efficiency$TEAM=="Chicago Bulls"]<-"Bulls"
efficiency$TEAM[efficiency$TEAM=="Brooklyn Nets"]<-"Nets"
efficiency$TEAM[efficiency$TEAM=="Cleveland Cavaliers"]<-"Cavaliers"
efficiency$TEAM[efficiency$TEAM=="Atlanta Hawks"]<-"Hawks"
efficiency$TEAM[efficiency$TEAM=="Charlotte Hornets"]<-"Heat"
efficiency$TEAM[efficiency$TEAM=="New York Knicks"]<-"Knicks"
efficiency$TEAM[efficiency$TEAM=="Washington Wizards"]<-"Wizards"
efficiency$TEAM[efficiency$TEAM=="Indiana Pacers"]<-"Pacers"
efficiency$TEAM[efficiency$TEAM=="Detroit Pistons"]<-"Pistons"
efficiency$TEAM[efficiency$TEAM=="Orlando Magic"]<-"Magic"
efficiency$TEAM[efficiency$TEAM=="Phoenix Suns"]<-"Suns"
efficiency$TEAM[efficiency$TEAM=="Memphis Grizzlies"]<-"Grizzlies"
efficiency$TEAM[efficiency$TEAM=="Dallas Mavericks"]<-"Mavericks"
efficiency$TEAM[efficiency$TEAM=="Denver Nuggets"]<-"Nuggets"
efficiency$TEAM[efficiency$TEAM=="Utah Jazz"]<-"Jazz"
efficiency$TEAM[efficiency$TEAM=="Golden State Warriors"]<-"Warriors"
efficiency$TEAM[efficiency$TEAM=="Minnesota Timberwolves"]<-"Timberwolves"
efficiency$TEAM[efficiency$TEAM=="LA CLippers"]<-"Clippers"
efficiency$TEAM[efficiency$TEAM=="New Orleans Pelicans"]<-"Pelicans"
efficiency$TEAM[efficiency$TEAM=="San Antonio Spurs"]<-"Spurs"
efficiency$TEAM[efficiency$TEAM=="Los Angeles Lakers"]<-"Lakers"
efficiency$TEAM[efficiency$TEAM=="Sacramento Kings"]<-"Kings"
efficiency$TEAM[efficiency$TEAM=="Oklahoma City Thunder"]<-"Thunder"
efficiency$TEAM[efficiency$TEAM=="Houston Rockets"]<-"Rockets"



NBA_Teams<-merge(Team_Stats,efficiency, by.x = "TEAM")
NBA_Teams<-merge(NBA_Teams, Advanced_Stats, by.x = "TEAM")
#####################################
#For all Hypothesis Test
#H0: Correlation between "stat" and winning percentage is 0
#H1: Correlation between "stat" and winning percentage is NOT 0
Points<-cor.test(NBA_Teams$`WIN%`,
                 NBA_Teams$PTS)
Points
#The P-value is 0.0005563 which is < 0.05 so  reject the null Hypothesis that the correlation is 0

ThreePoint<-cor.test(NBA_Teams$`WIN%`,
              NBA_Teams$`3P%`)
ThreePoint

#The P-value is  2.207e-05 which is < 0.05 meaning we reject the null hypothesis that the correlation is 0

FieldGoal<-cor.test(NBA_Teams$`WIN%`,
                    NBA_Teams$`FG%`)
FieldGoal
#The P-value is 0.0002269 which is < 0.05 meaning we reject the null hypothesis that the correlation is 0

Rebound<-cor.test(NBA_Teams$`WIN%`,
                  NBA_Teams$REB)
Rebound
#The P-value is 0.03149 which is < 0.05 meaning we reject the null hypothesis that the correlation is 0
Assist<-cor.test(NBA_Teams$`WIN%`,
                 NBA_Teams$AST)
Assist
#The p-value is 0.05578 which is > 0.05 meaning we fail to reject the null hypothesis that the correlation is 0
NBA_Teams$team_sce<-as.numeric(NBA_Teams$team_sce)

ScoringEfficiency<-cor.test(NBA_Teams$`WIN%`,
                            NBA_Teams$team_sce)
ScoringEfficiency
#The p-value is  0.0142 which is < 0.05 meaning we reject the null hypothesis that the correlation is 0
NBA_Teams$team_she<-as.numeric(NBA_Teams$team_she)
ShootingEfficiency<-cor.test(NBA_Teams$`WIN%`,
                             NBA_Teams$team_she)
ShootingEfficiency
#The P-Value is 0.04514 which is < 0.05 meaning that we reject the null hypothesis that the correlation is 0 
  
FTAttempted<-cor.test(NBA_Teams$`WIN%`,
                      NBA_Teams$FTA)
FTAttempted
#The P-value is 0.6861 which is > 0.05 meaning that we fail to reject the null hypothesis that the correlation is 0

TOV<-cor.test(NBA_Teams$`WIN%`,
              NBA_Teams$TOV)
TOV
#The P-value us 0.01152 which is < 0.05 meaning that we reject the null hypothesus that the correlation is 0
#Negative Correlation

FTPercentage<-cor.test(NBA_Teams$`FT%`,
                       NBA_Teams$`WIN%`)
FTPercentage

#The P-value is 0.03965 which is < 0.05 meaning that we reject the null hypothesis that the correlation is 0

Steals<-cor.test(NBA_Teams$`WIN%`,
                 NBA_Teams$STL)
Steals
#The P-Value is 0.05341 which is > 0.05 meaning that we fail to reject the nul hypothesis that the correlation is 0

Blocks<-cor.test(NBA_Teams$`WIN%`,
                 NBA_Teams$BLK)
Blocks
#The P-Value is 0.7822 which is > 0.05 meaning that we fail to reject the null hypothesis that the correlation is 0

DEF_RTG<-cor.test(NBA_Teams$`WIN%`,
                  NBA_Teams$DEFRTG)
DEF_RTG
#The P-Value is  6.23e-07 < 0.05 meaning that we reject the Null hypothesis that the correlation is 0

OFF_RTG<-cor.test(NBA_Teams$`WIN%`,
                  NBA_Teams$OFFRTG)
OFF_RTG

#The P-Value is 9.773e-08 which is < 0.05 meaning we reject the Null Hypothesis that the correlation is 0


AST_Percent<-cor.test(NBA_Teams$`WIN%`,
                      NBA_Teams$`AST%`)
AST_Percent
#The P-Value is 0.5934 which is > 0.05 meaning we fail to reject the null hypothesis that the correlation is 0

TOV_Percent<-cor.test(NBA_Teams$`WIN%`,
                      NBA_Teams$`TOV%`)
TOV_Percent
#The P-Value is 0.02658 which is < 0.05 meaning we reject the null hypothesis that the correlation is 0

Net_RTG<-cor.test(NBA_Teams$NETRTG,
                  NBA_Teams$`WIN%`)
Net_RTG
#The P-value is 3.065e-15 which is < 0.05 meaning we reject the null hypothesus that the correlation is 0

#SIGNIFICANT STATS 
#1 Net RTG - 3.065e-15 - Correlation = 0.9461084 
#2 OFF RTG -  9.773e-08 - Correlation =  0.8022309
#3 DEF RTG - 6.23e-07 - Correlation = (-0.7708436) 
#4 Three PT% - 2.207e-05 - Correlation = 0.692796 
#5 FG% - 0.0002269 - Correlation = 0.6242964 
#6 Points - 0.0005563 - Correlation = 0.5928154
#7 TOVs - 0.01152 - Correlation = (-0.4550405) 
#8 Scoring Efficiency - 0.0142 - Correlation = 0.4430579 
#9 TOV% - 0.02658 - Correlation = (-0.4045843)
#10 Rebounding - 0.03149 - Correlation = 0.3934082
#11 FT_Percentage - 0.03965 - Correlation = 0.3776403
#12 Shooting Efficiency - 0.04514 - Correlation = 0.3684337 

qplot(TEAM, NETRTG,
      data=NBA_Teams, geom = 'col',
      xlab = "Team Name", ylab = "Net Rating")+ theme(axis.text.x = element_text(angle = 90))+ geom_text(aes(label=NETRTG), position = position_stack(vjust = 0.5), size = 4)+
  ggtitle("A Plot of Net Rating by Team")


#Plot of Net Rating and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y= NETRTG, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)

#Plot of Offensive Rating and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y= OFFRTG, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)

#Plot of Defensive Rating and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y= DEFRTG, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)

#Plot of Three Point Percentage and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y=`3P%`, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)

#Plot of Field Goal Percentage and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y= `FG%`, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)

#Plot of Points and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y= PTS, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)

#Plot of Scoring Efficiency and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y= team_sce, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)

#Plot of Shooting Efficiency and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y= team_she, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)

#Plot of Turnover Percentage and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y= `TOV%`, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)

#Plot of Turnovers and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y= TOV, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)

#Plot of Rebounding and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y= REB, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)

#Plot of Free Throw Percentage and Winning Percentage
ggplot(NBA_Teams, aes(x= `WIN%`, y= `FT%`, label=TEAM))+geom_point() +geom_text(hjust=0, vjust=0)+geom_smooth(method=lm, se=FALSE, col='black', size=0.5)



#Impactful players on the most successful team 
BeastMode<-subset(NBA,NBA$Team == "Suns" & NBA$X3P.>= .35 & NBA$PTS.G >= 10 & NBA$TOV <= 3 & NBA$FG.>=.46)

#This is a list of players that had stats that greatest correlated with winning. They all had low turnover numbers
#They had high three point percentages, high field goal percentages, high amounts of points scored.
#Based on this data I can confidently say that Devin Booker is the most impactful player on the Suns when it comes to winning
#He had by far the most amount of points scored per game, second highest 3pt percentage, and second highest rebound numbers among the group.
#When you factor in his three point scoring efficiency and rebounding, with how far ahead he was in points.. It is clear he was the most impactful


#Do individual players who dominate in these statistics belong to teams that lead the league in these statistics?
playerThrees <- NBA %>% arrange(desc(X3P.))
playerThrees <- subset(playerThrees, playerThrees$G >= 50 & playerThrees$X3PA >= 5)[1:10,]
#6/10 Belong to playoff teams^

playerFGs <- NBA %>% arrange(desc(FG.))
playerFGs <- subset(playerFGs, playerFGs$G >= 50 & playerFGs$FGA >= 11)[1:10,]
#8/10 belong to playoff teams^

playerPoints <- NBA %>% arrange(desc(PTS.G))
playerPoints <- subset(playerPoints, playerPoints$G >= 50)[1:10,]
#9/10 belong to playoff teams^

playerTOVs <- NBA %>% arrange(TOV)
playerTOVs<- subset(playerTOVs, playerTOVs$G >= 50 & playerTOVs$AST >= 4)[1:10,]
#7/10 belong to playoff teams^

playerRebounds <- NBA %>% arrange(desc(TRB))
playerRebounds<- subset(playerRebounds, playerRebounds$G >= 50)[1:10,]
#7/10 belong to playoff teams

playerFTs <- NBA %>% arrange(desc(FT.))
playerFTs<- subset(playerFTs, playerFTs$G >= 50 & playerFTs$FTA >= 5)[1:10,]
#9/10 belong to playoff teams



