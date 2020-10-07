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

dfs<- read.csv('D:\\MY-DOC\\Documents\\NBA_Data\\NBA-PbP-Sample-Dataset.csv')


# Half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

#x: distance from x axis
#y: distance from y axis

# values of x>470 represent shots taken in the other half
# df$x will project all x's in the same half
dfs$x1 <- ifelse(!is.na(dfs$original_x) ,250+dfs$original_x,dfs$original_x)
dfs$y1 <- ifelse(!is.na(dfs$original_y) ,53+dfs$original_y,dfs$original_y)

# Shot chart of Cleveland
ggplot(dfs, aes(x=x1, y=y1)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart \n",color = "SHOT\n") +
  scale_color_manual(labels = c("Missed", "Scored"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse(dfs$y1<=150 & dfs$x1<35,'tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)


#create a numeric data frame
df2 <- cbind(df)

#remove some columns
df2[,c("ï..game_id","data_set","date","a1","a2","a3","a4","a5","h1","h2","h3","h4","h5","description")] <- list(NULL)


#convert H:M:S to seconds
df2$remaining_time <- period_to_seconds(hms(df2$remaining_time))
df2$elapsed <- period_to_seconds(hms(df2$elapsed))
df2$play_length <- period_to_seconds(hms(df2$play_length))

#remove more columns
df2[,c("play_id","away","home","block","entered","left","num","opponent","outof","possession","reason","steal")] <- list(NULL)


#create a new df with players and player_IDs
df3 <- data.frame(matrix(ncol=2,nrow=length(as.factor(unique(df2$player))), dimnames=list(NULL, c("player", "player_id"))))
df3$player <- as.factor(unique(df2$player))
df3$player_id <- as.integer(runif(nrow(df3), min = 0, max = 5000))

#Check if each player_id is unique
length(unique(df3$player_id)) == length(as.factor(unique(df2$player)))


#Get all rows with player
#subset(df2,player %in% "LeBron James")

#table(df2$result)
table(df2$result) / length(df2$result)       #%data for each factor
#boxplot,hist

#Bivariate Analysis

# 1. Numeric vs Categorical
plot(df2$shot_distance ~ df2$result)

tapply(df2$shot_distance,df2$result,mean,na.rm = TRUE)

tapply(df2$shot_distance,df2$result,sd,na.rm = TRUE)

# 2. Numeric vs Numeric
aggregate(df2$result ~ df2$player + df2$type + df2$shot_distance, FUN = length)

pairs(df2[,c("period","play_length","points","shot_distance")])

cor(df2[,c("period","play_length","points","shot_distance")])


table(df2$result,df2$shot_distance) / length(df2$result)

m <- lm(y~x)
summary(m)

predict(m,data.frame("x"=1000))

#--------------------------------------------------------------------------------------------------


# data cleaning
df2[df2==""]<-NA           #Make blank values NA

df4 <- df2[!is.na(df2$result), ]
df4 <- df2[!is.na(df2$shot_distance), ]



#---------------------------------------------------------------------------------------------------------------------

#Convert result and type into numeric in df5

df5<-df4

df5$result <- as.integer(df5$result)

df5$result[df5$result == 3] <- 0
df5$result[df5$result == 2] <- 1

levels(df5$type) <- 1:50
df5$type <- as.integer(df5$type)

df5$type[df5$type == 6] <- 100 #Driving Dunk
df5$type[df5$type == 7] <- 101 #Driving Layup
df5$type[df5$type == 24] <- 102 #Layup
df5$type[df5$type == 33] <- 103 #Reverse Layup
df5$type[df5$type == 12] <- 104 #Finger Roll Layup
df5$type[df5$type == 30] <- 105 #Putback Layup
df5$type[df5$type == 4] <- 106 #Alley Oop Layup
df5$type[df5$type == 36] <- 107 #Running Layup
df5$type[df5$type == 34] <- 108 #Running Dunk
df5$type[df5$type == 8] <- 109 #Driving Reverse Layup
df5$type[df5$type == 9] <- 110 #Dunk


df5$type[df5$type == 19] <- 125 #Hook Shot
df5$type[df5$type == 45] <- 126 #Turnaround Hook Shot
df5$type[df5$type == 13] <- 127 #Floating Jump Shot


df5$type[df5$type == 35] <- 128 #Running Jump Shot
df5$type[df5$type == 21] <- 129 #Jump Bank Shot
df5$type[df5$type == 46] <- 130 #Turnaround Jump Shot

df5$type[df5$type == 11] <- 140 #Fadeaway Jumper

df5$type[df5$type == 40] <- 150 #Step Back Jump Shot
df5$type[df5$type == 29] <- 151 #Pullup Jump Shot


df5$type[df5$type == 22] <- 160 #Jump Shot

df5$type[df5$type == 49] <- 200 #Unknown      (Noise)


#-------------------------------------------------------------------------------------------------------------------

# 1. Numeric vs Categorical
plot(df5$shot_distance ~ df5$result)

tapply(df4$shot_distance,df4$result,mean,na.rm = TRUE)

tapply(df4$shot_distance,df4$result,sd,na.rm = TRUE)

# 2. Numeric vs Numeric
aggregate(df4$result ~ df4$player + df4$type, FUN = length)




pairs(df5[,c("period","play_length","points","shot_distance","result","type")])

cor(df5[,c("period","play_length","points","shot_distance","result","type")])


table(df5$result,df5$shot_distance) / length(df5$result)

# Linear Regression
m <- lm(formula = df5$type~df5$shot_distance)
summary(m)

a <- predict(m,data.frame("shot_distance"=26))

#unknowns are causing noise

aggregate(df5$result ~ df5$shot_distance + df5$type, FUN = length)

#dist   type         count
#0      200          3
#1      200          3
#7      200          1
#8      200          1

#18/8 = 2 (shot_dist), so let unknown = 117


df5$type[df5$type == 200] <- 117 #Unknown
















