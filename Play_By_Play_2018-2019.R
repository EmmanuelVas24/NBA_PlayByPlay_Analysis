library(grid)
library(jpeg)
library(ggplot)
library(ggplot2)
library(e1071)
library(RCurl)
library(dplyr)
library(corrplot)
library(caret)
library(caTools)
library(lubridate)

#setwd("C:\\python\\NBA\\PbP_logs\\2018-2019")

file_names <- dir()
df <- do.call(rbind,lapply(file_names,read.csv))


# Half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

#x: distance from x axis
#y: distance from y axis

# values of x>470 represent shots taken in the other half
# df$x will project all x's in the same half
df$x1 <- ifelse(!is.na(df$original_x) ,250+as.numeric(df$original_x),df$original_x)
df$y1 <- ifelse(!is.na(df$original_y) ,53+as.numeric(df$original_y),df$original_y)

#create a numeric data frame
df2 <- cbind(df)

#remove some columns
df2[,c("ï..game_id","data_set","a1","a2","a3","a4","a5","h1","h2","h3","h4","h5","description")] <- list(NULL)


#convert H:M:S to seconds
df2$remaining_time <- period_to_seconds(hms(df2$remaining_time))
df2$elapsed <- period_to_seconds(hms(df2$elapsed))
df2$play_length <- period_to_seconds(hms(df2$play_length))

#remove more columns
df2[,c("play_id","away","home","block","entered","left","num","opponent","outof","possession","reason","steal")] <- list(NULL)



#---------------------------------------------------------------------------------
# data cleaning
df2[df2==""]<-NA           #Make blank values NA

df4 <- df2[!is.na(df2$result), ]
df4 <- df4[!is.na(df4$shot_distance), ]


colSums(is.na(df4))       #Check NAs

df4$original_x <- as.numeric(df4$original_x)
df4$original_y <- as.numeric(df4$original_y)



# values of x>470 represent shots taken in the other half
# df$x will project all x's in the same half
df4$x1 <- ifelse(!is.na(df4$original_x) ,250+df4$original_x,df4$original_x)
df4$y1 <- ifelse(!is.na(df4$original_y) ,53+df4$original_y,df4$original_y)


# Shot chart
ggplot(df4, aes(x=x1, y=y1)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart \n",color = "SHOT\n") +
  scale_color_manual(labels = c("Missed", "Scored"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse(df4$result == 'made','tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)


#________________________________________________________________________________
#Left corner 3s

ggplot(df4, aes(x=x1, y=y1)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart \n",color = "SHOT\n") +
  scale_color_manual(labels = c("Other", "Left Corner 3s"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse(df4$y1<=150 & df4$x1<35,'tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)

df5$left_corner <- as.numeric(df5$y1<=150 & df5$x1<35)



#Right corner 3s

ggplot(df4, aes(x=x1, y=y1)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart \n",color = "SHOT\n") +
  scale_color_manual(labels = c("Other", "Right Corner 3s"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse(df4$y1<=150 & df4$x1>465,'tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)

df5$right_corner <- as.numeric(df5$y1<=150 & df5$x1>465)


#Both corner 3s

ggplot(df4, aes(x=x1, y=y1)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart \n",color = "SHOT\n") +
  scale_color_manual(labels = c("Other", "Corner 3s"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse((df4$y1<=150 & df4$x1<35) | (df4$y1<=150 & df4$x1>465),'tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)


#Wings

ggplot(df4, aes(x=x1, y=y1)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart \n",color = "SHOT\n") +
  scale_color_manual(labels = c("Other", "Wing 3s"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse((df4$y1>150 & df4$x1<200 & df4$shot_distance<30 & df4$shot_distance>23) | (df4$y1>150 & df4$x1>300 & df4$shot_distance<30& df4$shot_distance>23),'tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)

df5$left_wing <- as.numeric(df5$y1>150 & df5$x1<200 & df5$shot_distance<30 & df5$shot_distance>23)
df5$right_wing <- as.numeric(df5$y1>150 & df5$x1>300 & df5$shot_distance<30& df5$shot_distance>23)

df5$wings <- as.numeric((df5$y1>150 & df5$x1<200 & df5$shot_distance<30 & df5$shot_distance>23) | (df5$y1>150 & df5$x1>300 & df5$shot_distance<30& df5$shot_distance>23))

#Top of the key 3s

ggplot(df4, aes(x=x1, y=y1)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart \n",color = "SHOT\n") +
  scale_color_manual(labels = c("Other", "Top 3s"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse((df4$y1>150 & df4$x1>=200 & df4$x1<=300 & df4$shot_distance<30 & df4$shot_distance>23),'tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)

df5$top <- as.numeric(df5$y1>150 & df5$x1>=200 & df5$x1<=300 & df5$shot_distance<30 & df5$shot_distance>23)

#Left Midrange

ggplot(df4, aes(x=x1, y=y1)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart \n",color = "SHOT\n") +
  scale_color_manual(labels = c("Other", "Left Mid-range"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse(df4$x1>35 & df4$x1<180 & df4$shot_distance<23,'tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)

df5$left_mid <- as.numeric(df5$x1>35 & df5$x1<180 & df5$shot_distance<23)

#Right Midrange

ggplot(df4, aes(x=x1, y=y1)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart \n",color = "SHOT\n") +
  scale_color_manual(labels = c("Other", "Right Mid-range"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse(df4$x1>320 & df4$x1<465 & df4$shot_distance<23,'tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)

df5$right_mid <- as.numeric(df5$x1>320 & df5$x1<465 & df5$shot_distance<23)


#Top Midrange

ggplot(df4, aes(x=x1, y=y1)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart \n",color = "SHOT\n") +
  scale_color_manual(labels = c("Other", "Top Mid-range"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse(df5$x1>=180 & df5$x1<=320 & df5$shot_distance<23 & df5$y1>150,'tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)

df5$top_mid <- as.numeric(df5$x1>=180 & df5$x1<=320 & df5$shot_distance<23 & df5$y1>150)

#points per shot ??

#All mid-range
ggplot(df4, aes(x=x1, y=y1)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart \n",color = "SHOT\n") +
  scale_color_manual(labels = c("Other", "Top Mid-range"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse((df5$x1>=180 & df5$x1<=320 & df5$shot_distance<23 & df5$y1>150) | (df4$x1>320 & df4$x1<465 & df4$shot_distance<23) | (df5$x1>35 & df5$x1<180 & df5$shot_distance<23),'tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)

df5$midrange <- as.numeric((df5$x1>=180 & df5$x1<=320 & df5$shot_distance<23 & df5$y1>150) | (df4$x1>320 & df4$x1<465 & df4$shot_distance<23) | (df5$x1>35 & df5$x1<180 & df5$shot_distance<23))


  
table(df4$shot_distance)
boxplot(df4$shot_distance)

((sum(df4$shot_distance < 28))/nrow(df4))*100

#-----------------------------------------------------------------------------------


#LEFT CORNER = 0.38%, POINTS PER SHOT = 0.38*3 = 1.14
aggregate(df5$result ~ df5$catch_shoot + df5$left_corner, FUN = length)
aggregate(df5$result ~ df5$catch_shoot + df5$left_corner, FUN = sum)


#RIGHT CORNER = 0.37%, POINTS PER SHOT = 0.37*3 = 1.11
aggregate(df5$result ~ df5$catch_shoot + df5$right_corner, FUN = length)
aggregate(df5$result ~ df5$catch_shoot + df5$right_corner, FUN = sum)


#LEFT WING = 0.34%, POINTS PER SHOT = 0.34*3 = 1.02
aggregate(df5$result ~ df5$catch_shoot + df5$left_wing, FUN = length)
aggregate(df5$result ~ df5$catch_shoot + df5$left_wing, FUN = sum)

#RIGHT WING = 0.34%, POINTS PER SHOT = 0.34*3 = 1.02
aggregate(df5$result ~ df5$catch_shoot + df5$right_wing, FUN = length)
aggregate(df5$result ~ df5$catch_shoot + df5$right_wing, FUN = sum)

#TOP = 0.33%, POINTS PER SHOT = 0.33*3 = 0.99
aggregate(df5$result ~ df5$catch_shoot + df5$top, FUN = length)
aggregate(df5$result ~ df5$catch_shoot + df5$top, FUN = sum)


#LAYUPS = 0.63%, POINTS PER SHOT = 0.63*2 = 1.26
aggregate(df5$result ~ df5$layup, FUN = length)
aggregate(df5$result ~ df5$layup, FUN = sum)

#------Mid-Range-------# = 0.81 Points per shot

#Top Midrange = 0.42%, POINTS PER SHOT = 0.42*2 = 0.84
aggregate(df5$result ~ df5$top_mid, FUN = length)
aggregate(df5$result ~ df5$top_mid, FUN = sum)

#Left Midrange = 0.39%, POINTS PER SHOT = 0.39*2 = 0.8
aggregate(df5$result ~ df5$left_mid, FUN = length)
aggregate(df5$result ~ df5$left_mid, FUN = sum)

#Right Midrange = 0.39%, POINTS PER SHOT = 0.39*2 = 0.8
aggregate(df5$result ~ df5$right_mid, FUN = length)
aggregate(df5$result ~ df5$right_mid, FUN = sum)

#---------------------------------------------------------------------------------

#Convert result and type into numeric in df5
#df4$type[df4$type == "B"] <- "b"

df5<-df4

df5$type <- as.character(df4$type)
df5$result <- as.integer(df5$result)

df5$result[df5$result == 3] <- 0
df5$result[df5$result == 2] <- 1

df5$type[df5$type == "Driving Dunk"] <- 100 #Driving Dunk
df5$type[df5$type == "Driving Layup"] <- 101 #Driving Layup
df5$type[df5$type == "Layup"] <- 102 #Layup
df5$type[df5$type == "Reverse Layup"] <- 103 #Reverse Layup
df5$type[df5$type == "Finger Roll Layup"] <- 104 #Finger Roll Layup
df5$type[df5$type == "Putback Layup"] <- 105 #Putback Layup
df5$type[df5$type == "Alley Oop Layup"] <- 106 #Alley Oop Layup
df5$type[df5$type == "Running Layup"] <- 107 #Running Layup
df5$type[df5$type == "Running Dunk"] <- 108 #Running Dunk
df5$type[df5$type == "Driving Reverse Layup"] <- 109 #Driving Reverse Layup
df5$type[df5$type == "Dunk"] <- 110 #Dunk
df5$type[df5$type == "Driving Finger Roll Layup"] <- 111 ##Driving Finger Roll Layup
df5$type[df5$type == "Running Reverse Layup"] <- 112 #Running Reverse Layup
df5$type[df5$type == "Running Finger Roll Layup"] <- 113 #Running Finger Roll Layup
df5$type[df5$type == "Putback Dunk"] <- 114 #Putback Dunk
df5$type[df5$type == "Alley Oop Dunk"] <- 115 #Alley Oop Dunk
df5$type[df5$type == "Reverse Dunk"] <- 116 #Reverse Dunk

df5$type[df5$type == "Hook Shot"] <- 125 #Hook Shot
df5$type[df5$type == "Turnaround Hook Shot"] <- 126 #Turnaround Hook Shot
df5$type[df5$type == "Floating Jump Shot"] <- 127 #Floating Jump Shot
df5$type[df5$type == "Running Jump Shot"] <- 128 #Running Jump Shot
df5$type[df5$type == "Jump Bank Shot"] <- 129 #Jump Bank Shot
df5$type[df5$type == "Turnaround Jump Shot"] <- 130 #Turnaround Jump Shot
df5$type[df5$type == "Turnaround Fadeaway"] <- 131 #Turnaround Fadeaway
df5$type[df5$type == "Turnaround Fadeaway "] <- 131 #Turnaround Fadeaway
df5$type[df5$type == "Driving Hook Shot"] <- 132 #Driving Hook Shot
df5$type[df5$type == "Turnaround Bank Hook Shot"] <- 133 #Turnaround Bank Hook Shot
df5$type[df5$type == "Hook Bank Shot"] <- 134 #Hook Bank Shot
df5$type[df5$type == "Driving Bank Hook Shot"] <- 135 #Driving Bank Hook Shot

df5$type[df5$type == "Fadeaway Jumper"] <- 140 #Fadeaway Jumper

df5$type[df5$type == "Step Back Jump Shot"] <- 150 #Step Back Jump Shot
df5$type[df5$type == "Pullup Jump Shot"] <- 151 #Pullup Jump Shot

df5$type[df5$type == "Jump Shot"] <- 160 #Jump Shot

df5$type[df5$type == "Unknown"] <- 200 #Unknown      (Noise)
df5$type[df5$type == "unknown"] <- 201 #Unknown

df5$type <- as.numeric(as.character(df5$type))    # convert type col,umn to numerical
#-------------------------------------------------------------------------------------------------------------------

# Classifying unknown shots


agg <- aggregate(df5$result ~ df5$shot_distance + df5$type, FUN = length)
agg


df5$type[df5$type == 201 & df5$shot_distance %in% c(0,1,2)] <- 120    #Unknown Layup

df5$type[df5$type == 201 & df5$shot_distance %in% c(3,4,5,6,7,8,9)] <- 138    #Unknown 3-9 ft shot

df5$type[df5$type == 201 & df5$shot_distance %in% c(10,11,12,13,14,15,16)] <- 145    #Unknown mid range shot

df5$type[df5$type == 201 & df5$shot_distance %in% c(17,18,19,20,21,22)] <- 155    #Unknown long mid range shot

df5$type[df5$type == 201 & df5$shot_distance %in% c(23,24,25,26,27)] <- 170    #Unknown 3pt shot

df5$type[df5$type == 201 & df5$shot_distance > 27] <- 180    #Heave

#---------------------------------------------------------------------------------------------------------------------

# 1. Numeric vs Categorical
plot(df5$shot_distance ~ df5$result)

tapply(df4$shot_distance,df4$result,mean,na.rm = TRUE)

tapply(df4$shot_distance,df4$result,sd,na.rm = TRUE)

# 2. Numeric vs Numeric
aggregate(df4$result ~ df4$player + df4$type, FUN = length)




pairs(df5[,c("period","play_length","points","shot_distance","result","type",'away_score',"home_score","remaining_time","converted_x","converted_y","layup","skill_shot","fadaway_stepback","catch_shoot","pullup")])

res <- cor(df5[,c("period","play_length","points","result","skill_shot","fadeway_stepback","pullup","layup","catch_shoot","shot_distance")])
corrplot(res, method = "circle")

df6 <- df5[,c("period","play_length","points","result","skill_shot","fadeway_stepback","pullup","layup","catch_shoot","shot_distance")]

corrgram(df6, order=TRUE)

str(df5)

table(df5$result,df5$shot_distance) / length(df5$result)


#--------------------------------------------------------------------------------------------------
#           Assign player_IDs

#create a new df with players and player_IDs
df3 <- data.frame(matrix(ncol=2,nrow=length(as.factor(unique(df5$player))), dimnames=list(NULL, c("player", "player_id"))))
df3$player <- as.factor(unique(df5$player))
#df3$player_id <- as.integer(runif(nrow(df3), min = 0, max = 5000))   #duplicating

df3$player_id <- sample(2000, size = length(as.factor(unique(df5$player))), replace = FALSE)

#Check if each player_id is unique
length(unique(df3$player_id)) == length(as.factor(unique(df5$player)))

df3[duplicated(df3$player_id),]


#Get all rows with player
#subset(df5,player %in% "Alex Caruso")

df5[["player_id"]] <- df3[ match(df5[['player']], df3[['player']] ) , 'player_id']

#Check if each player_id is unique
length(unique(df5$player_id)) == length(as.factor(unique(df5$player)))


#----------------------------------------------------------------------------------------

#Convert Shot Type into 6 types, which will be in dummy columns

df5$layup <- as.numeric(df5$type %in% c(100:116))
df5$skill_shot <- as.numeric(df5$type %in% c(125:135))
df5$fadeway_stepback <- as.numeric(df5$type %in% c(140,150))
df5$catch_shoot <- as.numeric(df5$type %in% c(160))
df5$pullup <- as.numeric(df5$type %in% c(151))
df5$unknown <- as.numeric(df5$type %in% c(120,138,145,155,170,180,200,201))


df5 <- within(df5, rm("unknown"))

#Examine unknown column


#------------------------------------------------------------------------------------------------------



'

difference between catch and shoot and non-catch and shoot?

shot locations heat map

deep 3s: 4 feet beyond the arc

decrease in mid-range and increase in 3s over the years

% of shots taken from each location

'

agg1 <- aggregate(df5$result ~ df5$team + df5$layup, FUN = length)
agg2 <- aggregate(df5$result ~ df5$team + df5$layup, FUN = sum)

agg1 <- agg1[order(agg1$`df5$result`),]
agg2 <- agg2[order(agg2$`df5$result`),]


df5$close_shot <- as.numeric(df5$shot_distance<5)

#------------------------

#layups
aggregate(df5$result ~ df5$layup, FUN = length)  #0 = other shots, 1 = layups
aggregate(df5$result ~ df5$layup, FUN = sum)     #0 = other shots, 1 = made layups


df5$corners <- as.numeric((df5$y1<=150 & df5$x1<35) | (df5$y1<=150 & df5$x1>465))

#corners
agg3 <- aggregate(df5$result ~ df5$team + df5$corners + df5$catch_shoot, FUN = length)
agg4 <- aggregate(df5$result ~ df5$team + df5$corners + df5$catch_shoot, FUN = sum)

agg3 <- agg3[order(agg3$`df5$result`),]
agg4 <- agg4[order(agg4$`df5$result`),]


#-------------------------------

layup_fg <- subset(df5, team %in% c("GSW", "TOR", "NYK", "CLE"), select=c(game_id, team, layup,result))
corner_fg <- subset(df5, team %in% c("GSW", "TOR", "NYK", "CLE"), select=c(game_id, team, corners,result))

#remove rows where layup = 0
layup_fg<-layup_fg[!(layup_fg$layup == 0),]  
#remove rows where corner = 0
corner_fg<-corner_fg[!(corner_fg$corners == 0),]  

aggregate(cbind(layup_fg$layup, layup_fg$result), by=list(game=layup_fg$game_id, team = layup_fg$team), FUN=sum)

layup_fg_pct <- aggregate(cbind(layup_fg$layup, layup_fg$result), by=list(game=layup_fg$game_id, team = layup_fg$team), FUN=sum)
layup_fg_pct <- transform(layup_fg_pct, fg_pct = (V2/V1)*100)

layup_model = lm(fg_pct ~ team, data = layup_fg_pct)
anova(layup_model)

TukeyHSD(aov(layup_model))

#----------------------------------------------------------------------------

corner_fg_pct <- aggregate(cbind(corner_fg$corners, corner_fg$result), by=list(game=corner_fg$game_id, team = corner_fg$team), FUN=sum)
corner_fg_pct <- transform(corner_fg_pct, fg_pct = (V2 / V1)*100)

corner_model = lm(fg_pct ~ team, data = corner_fg_pct)
anova(corner_model)

TukeyHSD(aov(corner_model))

#-----------------------------------------------------------------------------

#Test for normality
x1_GSW <- subset(layup_fg_pct, team %in% c("GSW"), select=c(fg_pct))
x2_TOR <- subset(layup_fg_pct, team %in% c("TOR"), select=c(fg_pct))
x3_NYK <- subset(layup_fg_pct, team %in% c("NYK"), select=c(fg_pct))
x4_CLE <- subset(layup_fg_pct, team %in% c("CLE"), select=c(fg_pct))

qqnorm(x1_GSW$fg_pct)
qqline(x1_GSW$fg_pct)
shapiro.test(x1_GSW$fg_pct)

qqnorm(x2_TOR$fg_pct)
qqline(x2_TOR$fg_pct)
shapiro.test(x2_TOR$fg_pct) #p>0.05

qqnorm(x3_NYK$fg_pct)
qqline(x3_NYK$fg_pct)
shapiro.test(x3_NYK$fg_pct) #p>0.05

qqnorm(x4_CLE$fg_pct)
qqline(x4_CLE$fg_pct)
shapiro.test(x4_CLE$fg_pct) #p>0.05

y1_GSW <- subset(corner_fg_pct, team %in% c("GSW"), select=c(fg_pct))
y2_TOR <- subset(corner_fg_pct, team %in% c("TOR"), select=c(fg_pct))
y3_NYK <- subset(corner_fg_pct, team %in% c("NYK"), select=c(fg_pct))
y4_CLE <- subset(corner_fg_pct, team %in% c("CLE"), select=c(fg_pct))

qqnorm(y1_GSW$fg_pct)
qqline(y1_GSW$fg_pct)
shapiro.test(y1_GSW$fg_pct) #p>0.05

qqnorm(y2_TOR$fg_pct)
qqline(y2_TOR$fg_pct)
shapiro.test(y2_TOR$fg_pct) #p>0.05

qqnorm(y3_NYK$fg_pct)
qqline(y3_NYK$fg_pct)
shapiro.test(y3_NYK$fg_pct) #p>0.05

qqnorm(y4_CLE$fg_pct)
qqline(y4_CLE$fg_pct)
shapiro.test(y4_CLE$fg_pct) #p>0.05

#----------------------------------------------------------------------------

#remove playoff games
games <- subset(df, (team %in% c("GSW", "TOR", "NYK", "CLE") & data_set != '2018-2019 Regular Season'), select=c(game_id, team))
layup_fg_pct<-layup_fg_pct[!(layup_fg_pct$game %in% c(unique(games$game_id))),]
corner_fg_pct<-corner_fg_pct[!(corner_fg_pct$game %in% c(unique(games$game_id))),]




#layups boxplot comparison
ggplot(layup_fg_pct, aes(x = team, y = fg_pct)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Team") +
  ylab("Layup_Shot%")


#remove outlier games      (UNDONE), use only if needed
df[df$game_id == "=\"0021800291\"", ] #NYK 80%

#layup_fg_pct<-layup_fg_pct[!(layup_fg_pct$game == "=\"0021800291\""),]        #NYK top 4 games
#layup_fg_pct<-layup_fg_pct[!(layup_fg_pct$game == "=\"0021800186\""),] 
#layup_fg_pct<-layup_fg_pct[!(layup_fg_pct$game == "=\"0021800605\""),] 
#layup_fg_pct<-layup_fg_pct[!(layup_fg_pct$game == "=\"0021800105\""),] 



corner_fg_pct[corner_fg_pct$game == "=\"0021800291\"", ] 


#corner 3 boxplot comparison
ggplot(corner_fg_pct, aes(x = team, y = fg_pct)) +
  geom_boxplot(fill = "grey80", colour = "black") +
  scale_x_discrete() + xlab("Team") +
  ylab("Corner_Shot%")




sum(corner_fg_pct[corner_fg_pct$team == "GSW",]$V1)
sum(corner_fg_pct[corner_fg_pct$team == "GSW",]$V2)



#------------------------------------------------------------------------------
df5$long_range <- as.numeric(df5$shot_distance>=30 & df5$shot_distance<=35)
df5$bomb <- as.numeric(df5$shot_distance>=36)

# long range PPS = 0.87
aggregate(df5$result ~ df5$long_range, FUN = length)
aggregate(df5$result ~ df5$long_range, FUN = sum)

# bomb PPS = 0.15
aggregate(df5$result ~ df5$bomb, FUN = length)
aggregate(df5$result ~ df5$bomb, FUN = sum)

# close shot = 1.24
aggregate(df5$result ~ df5$close_shot, FUN = length)
aggregate(df5$result ~ df5$close_shot, FUN = sum)

# wings = 1.06
aggregate(df5$result ~ df5$wings, FUN = length)
aggregate(df5$result ~ df5$wings, FUN = sum)

# top shot = 1.03
aggregate(df5$result ~ df5$top, FUN = length)
aggregate(df5$result ~ df5$top, FUN = sum)

# corners = 1.14
aggregate(df5$result ~ df5$corners, FUN = length)
aggregate(df5$result ~ df5$corners, FUN = sum)

# midrange = 0.82
aggregate(df5$result ~ df5$midrange, FUN = length)
aggregate(df5$result ~ df5$midrange, FUN = sum)


df5$points_per_shot[df5$close_shot == 1]<-1.24
df5$points_per_shot[df5$corners == 1]<- 1.14
df5$points_per_shot[df5$midrange == 1]<- 0.82
df5$points_per_shot[df5$top == 1]<- 1.03
df5$points_per_shot[df5$wings == 1]<-1.06
df5$points_per_shot[df5$long_range == 1]<-0.87
df5$points_per_shot[df5$bomb == 1]<-0.15

df5$fg_pct[df5$close_shot == 1]<-0.62
df5$fg_pct[df5$corners == 1]<- 0.38
df5$fg_pct[df5$midrange == 1]<- 0.41
df5$fg_pct[df5$top == 1]<- 0.33
df5$fg_pct[df5$wings == 1]<-0.34
df5$fg_pct[df5$long_range == 1]<-0.29
df5$fg_pct[df5$bomb == 1]<-0.05


colSums(is.na(df6))       #Check NAs

df6 <- df5[!is.na(df5$points_per_shot), ]



x <- cor(df6[,c("period","play_length","result","layup","catch_shoot","shot_distance","points_per_shot","fg_pct","points")])
corrplot(x, method = "circle")
cor(df6[,c("play_length","points","result","layup","catch_shoot","shot_distance","pullup","fadeway_stepback","skill_shot","points_per_shot","fg_pct")])


train_201819 <- df6[1:167454,]
train_201819 <- subset(train_201819, select=c("play_length","points","layup","catch_shoot","shot_distance","pullup","fadeway_stepback","skill_shot","points_per_shot","fg_pct"))

test_201819 <- df6[167455:209317,]
test_201819 <- subset(test_201819, select=c("play_length","points","layup","catch_shoot","shot_distance","pullup","fadeway_stepback","skill_shot","points_per_shot","fg_pct"))



# Multiple Linear Regression
multiple_regression_model <- lm(formula = points  ~ shot_distance  + play_length  + layup + catch_shoot + points_per_shot + fg_pct , data = train_201819)
summary(multiple_regression_model)

a <- predict(multiple_regression_model, test_201819)

actuals_preds <- data.frame(cbind(actuals=test_201819$points, predicteds=a))
actuals_preds <- actuals_preds[!is.na(actuals_preds$predicteds), ]


correlation_accuracy <- cor(actuals_preds)
correlation_accuracy



df7 <- df6[1:100000,]
df7 = subset(df7, select = c(game_id,date,play_length,team,event_type,assist,player,shot_distance,points_per_shot,fg_pct) )
write.csv(df7,'C:\\python\\NBA\\PbP_logs\\NBA_shot_logs.csv')


str(df7)



#---------------------------------------------------------------------------------------------------------------------------------------------

colSums(is.na(df_HOU_opp_stats)) 

df_7 <- df5
df_7$assist <- as.numeric(df_7$assist)
df_7$assist[is.na(df_7$assist)] <- 0
df_7$assist[!(df_7$assist == 0)] <- 1

subset(df5,player %in% "Alex Caruso")


#All Houston games
unique(df_7$game_id[(df_7$team == 'HOU')])

#keep only HOU games
df_7 <- subset(df_7,game_id %in% c(unique(df_7$game_id[(df_7$team == 'HOU')])))

#-----------------------------------------------------------------------------------------------------------

#group by game id and team
layups_made <- aggregate(df_7$result ~ df_7$game_id + df_7$team + df_7$layup, FUN=sum)
layups_attempted <-aggregate(df_7$result ~ df_7$game_id + df_7$team + df_7$layup, FUN=length)


#filter only layup attempts (filter out other shot attempts)
layups_made <- layups_made[layups_made$`df_7$layup` == 1,]
layups_attempted <- layups_attempted[layups_attempted$`df_7$layup` == 1,]

#drop identifier column, now that all values are 1
layups_made$`df_7$layup` <- NULL
layups_attempted$`df_7$layup` <- NULL


#Rename columns
colnames(layups_made) <- c("game_id", "team","layups_made")
colnames(layups_attempted) <- c("game_id", "team","layups_attempted")


df_HOU <- layups_made

# Joining layups made and attempted dfs
df_HOU$layups_attempted <- layups_attempted$layups_attempted[match(df_HOU$game_id, layups_attempted$game_id)]

#add fg_pct column
df_HOU$layup_pct <- df_HOU$layups_made/df_HOU$layups_attempted

#------------------------------------------------------------------------------------------------------------------------------

#group by game id and team
catch_shoot_makes <- aggregate(df_7$result ~ df_7$game_id + df_7$team + df_7$catch_shoot, FUN=sum)
catch_shoot_attempts <-aggregate(df_7$result ~ df_7$game_id + df_7$team + df_7$catch_shoot, FUN=length)


#filter only layup attempts (filter out other shot attempts)
catch_shoot_makes <- catch_shoot_makes[catch_shoot_makes$`df_7$catch_shoot` == 1,]
catch_shoot_attempts <- catch_shoot_attempts[catch_shoot_attempts$`df_7$catch_shoot` == 1,]

#drop identifier column, now that all values are 1
catch_shoot_makes$`df_7$catch_shoot` <- NULL
catch_shoot_attempts$`df_7$catch_shoot` <- NULL


#Rename columns
colnames(catch_shoot_makes) <- c("game_id", "team","off_catch_made")
colnames(catch_shoot_attempts) <- c("game_id", "team","off_catch_attempted")


# Joining layups made and attempted dfs
df_HOU$off_catch_makes <- catch_shoot_makes$off_catch_made[match(df_HOU$game_id, catch_shoot_makes$game_id)]

df_HOU$off_catch_attempts <- catch_shoot_attempts$off_catch_attempted[match(df_HOU$game_id, catch_shoot_attempts$game_id)]


#add fg_pct column
df_HOU$catch_shoot_pct <- df_HOU$off_catch_makes/df_HOU$off_catch_attempts

#------------------------------------------------------------------------------------------------------------------------------

#group by game id and team
pullup_makes <- aggregate(df_7$result ~ df_7$game_id + df_7$team + df_7$pullup, FUN=sum)
pullup_attempts <-aggregate(df_7$result ~ df_7$game_id + df_7$team + df_7$pullup, FUN=length)


#filter only layup attempts (filter out other shot attempts)
pullup_makes <- pullup_makes[pullup_makes$`df_7$pullup` == 1,]
pullup_attempts <- pullup_attempts[pullup_attempts$`df_7$pullup` == 1,]

#drop identifier column, now that all values are 1
pullup_makes$`df_7$pullup` <- NULL
pullup_attempts$`df_7$pullup` <- NULL


#Rename columns
colnames(pullup_makes) <- c("game_id", "team","pullups_made")
colnames(pullup_attempts) <- c("game_id", "team","pullups_attempted")


# Joining layups made and attempted dfs
df_HOU$pullup_makes <- pullup_makes$pullups_made[match(df_HOU$game_id, pullup_makes$game_id)]

df_HOU$pullup_attempts <- pullup_attempts$pullups_attempted[match(df_HOU$game_id, pullup_attempts$game_id)]


#add fg_pct column
df_HOU$pullup_pct <- df_HOU$pullup_makes/df_HOU$pullup_attempts

#--------------------------------------------------------------------------------------------------------------------


#group by game id and team
corner_makes <- aggregate(df_7$result ~ df_7$game_id + df_7$team + df_7$corners, FUN=sum)
corner_attempts <-aggregate(df_7$result ~ df_7$game_id + df_7$team + df_7$corners, FUN=length)


#filter only layup attempts (filter out other shot attempts)
corner_makes <- corner_makes[corner_makes$`df_7$corners` == 1,]
corner_attempts <- corner_attempts[corner_attempts$`df_7$corners` == 1,]

#drop identifier column, now that all values are 1
corner_makes$`df_7$corners` <- NULL
corner_attempts$`df_7$corners` <- NULL


#Rename columns
colnames(corner_makes) <- c("game_id", "team","corner_makes")
colnames(corner_attempts) <- c("game_id", "team","corner_attempts")


# Joining layups made and attempted dfs
df_HOU$corner_makes <- corner_makes$corner_makes[match(df_HOU$game_id, corner_makes$game_id)]

df_HOU$corner_attempts <- corner_attempts$corner_attempts[match(df_HOU$game_id, corner_attempts$game_id)]


#add fg_pct column
df_HOU$corner_pct <- df_HOU$corner_makes/df_HOU$corner_attempts

#------------------------------------------------------------------------------------------------------------------


#group by game id and team
midrange_makes <- aggregate(df_7$result ~ df_7$game_id + df_7$team + df_7$midrange, FUN=sum)
midrange_attempts <-aggregate(df_7$result ~ df_7$game_id + df_7$team + df_7$midrange, FUN=length)


#filter only layup attempts (filter out other shot attempts)
midrange_makes <- midrange_makes[midrange_makes$`df_7$midrange` == 1,]
midrange_attempts <- midrange_attempts[midrange_attempts$`df_7$midrange` == 1,]

#drop identifier column, now that all values are 1
midrange_makes$`df_7$midrange` <- NULL
midrange_attempts$`df_7$midrange` <- NULL


#Rename columns
colnames(midrange_makes) <- c("game_id", "team","midrange_makes")
colnames(midrange_attempts) <- c("game_id", "team","midrange_attempts")


# Joining layups made and attempted dfs
df_HOU$midrange_makes <- midrange_makes$midrange_makes[match(df_HOU$game_id, midrange_makes$game_id)]

df_HOU$midrange_attempts <- midrange_attempts$midrange_attempts[match(df_HOU$game_id, midrange_attempts$game_id)]


#add fg_pct column
df_HOU$midrange_pct <- df_HOU$midrange_makes/df_HOU$midrange_attempts

#-------------------------------------------------------------------------------------------------------------------------

#group by game id and team
fg_makes <- aggregate(df_7$result ~ df_7$game_id + df_7$team, FUN=sum)
fg_attempts <-aggregate(df_7$result ~ df_7$game_id + df_7$team, FUN=length)


#Rename columns
colnames(fg_makes) <- c("game_id", "team","fg_makes")
colnames(fg_attempts) <- c("game_id","team", "fg_attempts")


# Joining layups made and attempted dfs
df_HOU$fg_makes <- fg_makes$fg_makes[match(df_HOU$game_id, fg_makes$game_id)]

df_HOU$fg_attempts <- fg_attempts$fg_attempts[match(df_HOU$game_id, fg_attempts$game_id)]


#add fg_pct column
df_HOU$fg_pct <- df_HOU$fg_makes/df_HOU$fg_attempts

#--------------------------------------------------------------------------------------------------------------------

#group by game id and team
assists <- aggregate(df_7$assist ~ df_7$game_id + df_7$team, FUN=sum)


#Rename columns
colnames(assists) <- c("game_id", "team","assists")

# Joining layups made and attempted dfs
df_HOU$assists <- assists$assists[match(df_HOU$game_id, assists$game_id)]


#add fg_pct column
df_HOU$fg_pct <- df_HOU$fg_makes/df_HOU$fg_attempts

#---------------------------------------------------------------------------------------------------------------------

#Get all free throw attempts
df_ft <-  subset(df,game_id %in% c(unique(df$game_id[(df$team == 'HOU')])))
df_ft <- subset(df_ft,event_type == 'free throw')

#--------------------------------------------------------------------------------------------------------------------

#group by game id and team
ft_makes <- aggregate(df_ft$points ~ df_ft$game_id + df_ft$team, FUN=sum)
ft_attempts <-aggregate(df_ft$points ~ df_ft$game_id + df_ft$team, FUN=length)


#Rename columns
colnames(ft_makes) <- c("game_id", "team","ft_makes")
colnames(ft_attempts) <- c("game_id","team", "ft_attempts")


# Joining layups made and attempted dfs
df_HOU$ft_makes <- ft_makes$ft_makes[match(df_HOU$game_id, ft_makes$game_id)]

df_HOU$ft_attempts <- ft_attempts$ft_attempts[match(df_HOU$game_id, ft_attempts$game_id)]


#add fg_pct column
df_HOU$ft_pct <- df_HOU$ft_makes/df_HOU$ft_attempts

#---------------------------------------------------------------------------------------------------------------------

# Get win/loss column for HOU

df_wl <-  subset(df,game_id %in% c(unique(df$game_id[(df$team == 'HOU')])))


df_wl$home_game[(df_wl$home %in% c("Clint Capela","Chris Paul","PJ Tucker","Isaiah Hartenstein","Nene","Danuel House Jr.","Gary Clark","James Harden","Kenneth Faried","James Ennis III","Austin Rivers","Eric Gordon")) & (df_wl$event_type == 'jump ball') & (df_wl$home_score == 0) & (df_wl$away_score == 0)]<-1



jump <- unique(df_wl$home)
jump <- as.data.frame(jump)



df_wl$home_game[is.na(df_wl$home_game)] <- 0


sum(df_wl$home_game)

unique(df_wl$game_id)

aggregate(df_wl$home_game ~ df_wl$game_id, FUN=max)


#group by game id and team
home_score <- aggregate(df_wl$home_score ~ df_wl$game_id, FUN=max)
away_score <- aggregate(df_wl$away_score ~ df_wl$game_id, FUN=max)
home_game  <- aggregate(df_wl$home_game ~ df_wl$game_id, FUN=max)

sum(home_game$`df_wl$home_game`)


#Rename columns
colnames(home_score) <- c("game_id", "home_score")
colnames(home_game) <- c("game_id","home_game")
colnames(away_score) <- c("game_id","away_score")

# making the win/loss column

df_winloss <- home_score
df_winloss$away_score <- away_score$away_score[match(df_winloss$game_id, away_score$game_id)]
df_winloss$home_game <- home_game$home_game[match(df_winloss$game_id, home_game$game_id)]

df_winloss$win[(df_winloss$home_score > df_winloss$away_score) & df_winloss$home_game == 1] <- 1
df_winloss$win[(df_winloss$away_score > df_winloss$home_score) & df_winloss$home_game == 0] <- 1


df_winloss$win[is.na(df_winloss$win)] <- 0


df_HOU$win <- df_winloss$win[match(df_HOU$game_id, df_winloss$game_id)]


#----------------------------------------------------------------------------------------------------------------

#LOGISTIC MODEL

df_HOU_stats <- subset(df_HOU,team == 'HOU')


df_HOU_stats$layup_pct <- df_HOU_stats$layup_pct*100
df_HOU_stats$catch_shoot_pct <- df_HOU_stats$catch_shoot_pct*100
df_HOU_stats$pullup_pct <- df_HOU_stats$pullup_pct*100
df_HOU_stats$corner_pct <- df_HOU_stats$corner_pct*100
df_HOU_stats$midrange_pct <- df_HOU_stats$midrange_pct*100
df_HOU_stats$fg_pct <- df_HOU_stats$fg_pct*100
df_HOU_stats$ft_pct <- df_HOU_stats$ft_pct*100


res <- cor(df_HOU_stats[,c("layups_made","layups_attempted","layup_pct","off_catch_makes","off_catch_attempts","catch_shoot_pct","pullup_makes","pullup_attempts","pullup_pct","corner_makes","corner_attempts","corner_pct","midrange_makes","midrange_attempts","midrange_pct","fg_makes","fg_attempts","fg_pct","assists","ft_makes","ft_attempts","ft_pct","win")])
corrplot(res, method = "circle")


logistic_win_predictor <- glm(win ~ layups_made + layups_attempted + layup_pct + off_catch_makes + off_catch_attempts + catch_shoot_pct + pullup_makes + pullup_attempts + pullup_pct + corner_makes + corner_attempts + corner_pct + midrange_makes + midrange_attempts + midrange_pct + fg_makes + fg_attempts + fg_pct + assists + ft_makes + ft_attempts + ft_pct, data = df_HOU_stats, family = "binomial")

summary(logistic_win_predictor)

#shuffle rows
df_model <- df_HOU_stats[sample(nrow(df_HOU_stats)),]

train_201819 <- df_HOU_stats[1:74,]
train_201819 <- subset(train_201819, select=c("layups_made","layups_attempted","layup_pct","off_catch_makes","off_catch_attempts","catch_shoot_pct","pullup_makes","pullup_attempts","pullup_pct","corner_makes","corner_attempts","corner_pct","midrange_makes","midrange_attempts","midrange_pct","fg_makes","fg_attempts","fg_pct","assists","ft_makes","ft_attempts","ft_pct","win"))

test_201819 <- df_HOU_stats[75:93,]
test_201819 <- subset(test_201819, select=c("layups_made","layups_attempted","layup_pct","off_catch_makes","off_catch_attempts","catch_shoot_pct","pullup_makes","pullup_attempts","pullup_pct","corner_makes","corner_attempts","corner_pct","midrange_makes","midrange_attempts","midrange_pct","fg_makes","fg_attempts","fg_pct","assists","ft_makes","ft_attempts","ft_pct","win"))


predictions = predict(logistic_win_predictor, test_201819, type="response")
#a <- predict(logistic_win_predictor, test_201819)

actuals_preds <- data.frame(cbind(actuals=test_201819$win, predicteds=predictions))
actuals_preds <- actuals_preds[!is.na(actuals_preds$predicteds), ]

actuals_preds$predicteds[actuals_preds$predicteds<0.5] <- 0
actuals_preds$predicteds[actuals_preds$predicteds>0.5] <- 1

correlation_accuracy <- cor(actuals_preds)
correlation_accuracy


#Make opponent columns

df_opp_stats <- subset(df_HOU,team != 'HOU')


df_HOU_opp_stats <- df_HOU_stats





df_HOU_opp_stats$opp_layups_made <- df_opp_stats$layups_made[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_layups_attempted <- df_opp_stats$layups_attempted[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_layup_pct <- df_opp_stats$layup_pct[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]

df_HOU_opp_stats$opp_off_catch_makes <- df_opp_stats$off_catch_makes[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_off_catch_attempts <- df_opp_stats$off_catch_attempts[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_catch_shoot_pct <- df_opp_stats$catch_shoot_pct[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]

df_HOU_opp_stats$opp_pullup_makes <- df_opp_stats$pullup_makes[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_pullup_attempts <- df_opp_stats$pullup_attempts[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_pullup_pct <- df_opp_stats$pullup_pct[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]

df_HOU_opp_stats$opp_corner_makes <- df_opp_stats$corner_makes[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_corner_attempts <- df_opp_stats$corner_attempts[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_corner_pct <- df_opp_stats$corner_pct[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]

df_HOU_opp_stats$opp_midrange_makes <- df_opp_stats$midrange_makes[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_midrange_attempts <- df_opp_stats$midrange_attempts[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_midrange_pct <- df_opp_stats$midrange_pct[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]

df_HOU_opp_stats$opp_fg_makes <- df_opp_stats$fg_makes[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_fg_attempts <- df_opp_stats$fg_attempts[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_fg_pct <- df_opp_stats$fg_pct[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]

df_HOU_opp_stats$opp_ft_makes <- df_opp_stats$ft_makes[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_ft_attempts <- df_opp_stats$ft_attempts[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]
df_HOU_opp_stats$opp_ft_pct <- df_opp_stats$ft_pct[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]

df_HOU_opp_stats$opp_assists <- df_opp_stats$assists[match(df_HOU_opp_stats$game_id, df_opp_stats$game_id)]

df_HOU_opp_stats$opp_layup_pct <- df_HOU_opp_stats$opp_layup_pct*100
df_HOU_opp_stats$opp_catch_shoot_pct <- df_HOU_opp_stats$opp_catch_shoot_pct*100
df_HOU_opp_stats$opp_pullup_pct <- df_HOU_opp_stats$opp_pullup_pct*100
df_HOU_opp_stats$opp_corner_pct <- df_HOU_opp_stats$opp_corner_pct*100
df_HOU_opp_stats$opp_midrange_pct <- df_HOU_opp_stats$opp_midrange_pct*100
df_HOU_opp_stats$opp_fg_pct <- df_HOU_opp_stats$opp_fg_pct*100
df_HOU_opp_stats$opp_ft_pct <- df_HOU_opp_stats$opp_ft_pct*100
#-------------------------------------------------------------------------

res_2 <- cor(df_HOU_opp_stats[,c("layups_made","layups_attempted","layup_pct","off_catch_makes","off_catch_attempts","catch_shoot_pct","pullup_makes","pullup_attempts","pullup_pct","corner_makes","corner_attempts","corner_pct","midrange_makes","midrange_attempts","midrange_pct","fg_makes","fg_attempts","fg_pct","assists","ft_makes","ft_attempts","ft_pct","opp_layups_made","opp_layups_attempted","opp_layup_pct","opp_off_catch_makes","opp_off_catch_attempts","opp_catch_shoot_pct","opp_pullup_makes","opp_pullup_attempts","opp_pullup_pct","opp_corner_makes","opp_corner_attempts","opp_corner_pct","opp_midrange_makes","opp_midrange_attempts","opp_midrange_pct","opp_fg_makes","opp_fg_attempts","opp_fg_pct","opp_assists","opp_ft_makes","opp_ft_attempts","opp_ft_pct","win")])
corrplot(res_2, method = "circle")


logistic_win_predictor_2 <- glm(win ~ layups_made + layups_attempted + layup_pct + off_catch_makes + off_catch_attempts + catch_shoot_pct + pullup_makes + pullup_attempts + pullup_pct + corner_makes + corner_attempts + corner_pct + midrange_makes + midrange_attempts + midrange_pct + fg_makes + fg_attempts + fg_pct + assists + ft_makes + ft_attempts + ft_pct + opp_layups_made + opp_layups_attempted + opp_layup_pct + opp_off_catch_makes + opp_off_catch_attempts + opp_catch_shoot_pct + opp_pullup_makes + opp_pullup_attempts + opp_pullup_pct + opp_corner_makes + opp_corner_attempts + opp_corner_pct + opp_midrange_makes + opp_midrange_attempts + opp_midrange_pct + opp_fg_makes + opp_fg_attempts + opp_fg_pct + opp_assists + opp_ft_makes + opp_ft_attempts + opp_ft_pct, data = df_HOU_opp_stats, family = "binomial")

summary(logistic_win_predictor_2)









