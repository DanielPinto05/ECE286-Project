#lets us use %>%
library(dplyr)
library(tidyr)
# library(ggplot2)
library(stats)


# clear variables each time
rm(list = ls())
# ensure code is running in the right directory
setwd("~/Desktop/School/Skule/Winter-25/ECE286/Project")
# reading in and converting to 
df<-read.csv('data.csv')
n <-nrow(df) # number of participants


# organizing Music and no Music Scores and Times
df$M.Score <- ifelse(df$Music == 'A', df$A.Score, df$B.Score)
df$NM.Score <-ifelse(df$Music=='A', df$B.Score, df$A.Score)

df$M.Time <- ifelse(df$Music == 'A', df$A.Time, df$B.Time)
df$NM.Time <-ifelse(df$Music =='A', df$B.Time, df$A.Time)

df$Score.Diff <- df$NM.Score - df$M.Score # if 


# removing anyone whose data isn't entered yet
df <- df[!is.na(df$A.Score), ]




#look at test A vs test B

boxplot(df$A.Score, df$B.Score,  ylab = "Score [%]", names = c("Test A", "Test B"), main = "Scores on Tests A and B - was one test 'harder' than the other?")
boxplot(df$A.Time, df$B.Time,  ylab = "Time [s]", names = c("Test A", "Test B"), main = "Time taken on Tests A and B - was one test 'harder' than the other?")
hist(df$A.Score, breaks= 5, main = "Score Distribution on Quiz A", xlab = "Score")
hist(df$B.Score, breaks= 5, main = "Score Distribution on Quiz B", xlab = "Score")
hist(df$A.Time, breaks= 10)
hist(df$B.Time, breaks= 10)


# look at correlation of scores
# png(file = "Music-No Music Score Scatterplot.png")
plot(df$M.Score, df$NM.Score, 
     xlab = "Score WITH Music", 
     ylab = "Score WITHOUT Music")

res <- lm(df$M.Score ~df$NM.Score)
abline(res$coefficients[1], res$coefficients[2])
x<-round(cor(df$M.Score, df$NM.Score), 3)
legend("topleft", as.expression(x))
#dev.off()

plot(df$A.Time, df$A.Score)
cor(df$A.Time, df$A.Score)
plot(df$B.Time, df$B.Score)
cor(df$B.Time, df$B.Score)




# Basic facts about the dataset
mean(df$Score.Diff)


mean(df$M.Score)
sd(df$M.Score)/sqrt(n)
mean(df$NM.Score)
sd(df$NM.Score)/sqrt(n)
mean(df$M.Time)
sd(df$M.Time)/sqrt(n)
mean(df$NM.ime)
sd(df$NM.Time)/sqrt(n)

# is A harder than B?
mean(df$A.Score)
mean(df$B.Score)


" Things I want to get
1. Boxplot of the music and non music groups"


"2. Time vs score"
all_tests <- df%>% 
  select(Subject.ID, M.Time, NM.Time, M.Score, NM.Score) %>%
  pivot_longer(cols = c(df$M.Score, df$NM.Score), names_to = "Score") %>%
  select(Score, Time)








"3. Doing the test first or second - did this have a strong effect? 


4. A test vs B test - was one easier than the other? " 


    
             
            





