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

df<- read.csv('D:\\MY-DOC\\Documents\\2016\\NBA shot log 16-17-regular season\\NBA shot log 16-17-regular season\\Shot data\\shot log LAL.csv')
stats<- read.csv('D:\\MY-DOC\\Documents\\2016\\NBA shot log 16-17-regular season\\NBA shot log 16-17-regular season\\Player Regular 16-17 Stats.csv')

# Half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

#x: distance from x axis
#y: distance from y axis

# values of x>470 represent shots taken in the other half
# df$x will project all x's in the same half
df$x <- ifelse(df$location.x >470,940-df$location.x,df$location.x)


# Shot chart of Lakers 2016-2017 season 
ggplot(df, aes(x=location.y, y=x)) + 
  annotation_custom(court, 0, 500, 0, 470) +
  labs(title = "Shot Chart Lakers 2016-2017\n",color = "SHOT\n") +
  scale_color_manual(labels = c("Missed", "Scored"), values = c("tomato", "green")) +
  geom_point(aes(colour = ifelse(df$current.shot.outcome == 'SCORED','tomato','green'))) +
  xlim(0, 500) +
  ylim(0, 470)






