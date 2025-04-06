#lets us use %>%
library(dplyr)
library(tidyr)
# library(ggplot2)
library(stats)


# BRINGING IN DATA
# clear variables each time
rm(list = ls())
# ensure code is running in the right directory
setwd("~/Desktop/School/Skule/Winter-25/ECE286/Project")
# reading in and converting to 
df<-read.csv('data.csv')
n <-nrow(df) # number of participants


# NECESSARY DATA CLEANING
# organizing Music and no Music Scores and Times
df$M.Score <- ifelse(df$Music == 'A', df$A.Score, df$B.Score)
df$NM.Score <-ifelse(df$Music=='A', df$B.Score, df$A.Score)

df$M.Time <- ifelse(df$Music == 'A', df$A.Time, df$B.Time)
df$NM.Time <-ifelse(df$Music =='A', df$B.Time, df$A.Time)

df$First.Score<-ifelse(df$First == 'A', df$A.Score, df$B.Score)
df$Second.Score<-ifelse(df$First == 'A', df$B.Score, df$A.Score)


df$First.Time<-ifelse(df$First == 'A', df$A.Time, df$B.Time)
df$Second.Time<-ifelse(df$First == 'A', df$B.Time, df$A.Time)

df$Score.Diff <- df$NM.Score - df$M.Score # POSITIVE is better NO MUSIC
df$Time.Diff <-  df$M.Time - df$NM.Time # POSITIVE is 'better' (smaler) NO MUSIC

df$Score.Order.Diff <- df$Second.Score -df$First.Score # positive means better on second
df$Time.Order.Diff <- df$First.Time - df$Second.Time # positive means better on second


df$Mean.Score <- (df$A.Score+df$B.Score)/2
df$Mean.Time <- (df$A.Time+df$B.Time)/2

df$Bio <- ifelse(df$Major == 'bio', 1, 0)



# removing anyone whose data isn't entered yet
df <- df[!is.na(df$A.Score), ]


hist(df$Score.Diff, xlab = "Score Diff = Non-music score - Music score", main = "Histogram of Score Diffs", xlim =c(-50, 50))
hist(df$Time.Diff, xlab = "Time Diff =Music time - Non-music time", main = "Histogram of Time Diffs")


hist(df$Score.Order.Diff)
mean(df$Score.Order.Diff)
hist(df$Time.Order.Diff)
mean(df$Time.Order.Diff)








# MAIN ANALYSIS


# MUSIC VS NO MUSIC

# do a t-test. Paired = TRUE tell it to do a 'paired' test (what we did, where everyone writes both quizzes). 
t.test(df$NM.Score, df$M.Score, paired = TRUE)
t.test(df$M.Time, df$NM.Time, paired = TRUE)




# boxplot of Music vs No music
boxplot(df$M.Score, df$NM.Score, ylab = "Score", names = c("Music", "No Music"), main = "Music vs. No Music Scores") 
boxplot(df$M.Time, df$NM.Time, ylab = "Time [s]", names = c("Music", "No Music"), main = "Music v No Music Times") 




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














# IS A OR B HARDER?

boxplot(df$A.Score, df$B.Score,  ylab = "Score [%]", names = c("Test A", "Test B"), main = "Scores on Tests A and B - was one test 'harder' than the other?")
boxplot(df$A.Time, df$B.Time,  ylab = "Time [s]", names = c("Test A", "Test B"), main = "Time taken on Tests A and B - was one test 'harder' than the other?")
hist(df$A.Score, breaks= 5, main = "Score Distribution on Quiz A", xlab = "Score")
hist(df$B.Score, breaks= 5, main = "Score Distribution on Quiz B", xlab = "Score")
hist(df$A.Time, breaks= 10)
hist(df$B.Time, breaks= 10)




#DOES ORDER MATTER?

boxplot(df$First.Score, df$Second.Score, ylab = "Score", names = c("First Quiz Score", "Second Quiz Score"), main = "Scores on the first and second quizzes")
boxplot(df$First.Time, df$Second.Time, ylab = "Time", names = c("First Quiz Score", "Second Quiz Score"), main = "Times on the first and second quizzes")

# CORRELATION OF SCORES FOR INDIVIDUALS
# png(file = "Music-No Music Score Scatterplot.png")
plot(df$M.Score, df$NM.Score, 
     xlab = "Score WITH Music", 
     ylab = "Score WITHOUT Music")

res <- lm(df$M.Score ~df$NM.Score)
abline(res$coefficients[1], res$coefficients[2])
x<-round(cor(df$M.Score, df$NM.Score), 3)
legend("topleft", as.expression(x))
#dev.off()


# TIME TAKEN VS SCORE


plot(df$Mean.Time, df$Mean.Score)
summary(lm(df$Mean.Time ~ df$Mean.Score))




# TIME TAKE ON ONE TEST VS ANOTHER
plot(df$A.Time, df$B.Time, main = "Time taken on Quiz A and B was highly correlated/predictable.")
res <- lm(df$M.Score ~df$NM.Score)
abline(res$coefficients[1], res$coefficients[2])
res$coefficients[1]
res$coefficients[2]



# BIO vs NON- BIO students

t.test(df$Score.Diff ~ df$Bio)
t.test(df$Time.Diff ~ df$Bio ) 

bio <- df[df$Major == "bio", ]
nonbio <-df[df$Major != "Bio", ]
boxplot(bio$Time.Diff, nonbio$Time.Diff, main = "Time Diffs for Biology Major and Non-Biology Major Groups", names = c("Biology Major", "Non-Biology Major"), ylab = "Score Diff = Music - non-music")

# GENDER?

t.test(df$Score.Diff ~ df$Gender)
t.test(df$Time.Diff ~ df$Gender) 

# OVER/UNDER 25?

t.test(df$Score.Diff ~ df$Under.25)
t.test(df$Time.Diff ~ df$Under.25)

u25 <- df[df$Under.25==TRUE, ]
o25 <- df[df$Under.25 == FALSE, ]

boxplot(u25$Score.Diff, o25$Score.Diff, main = "Score Diffs for Under 25 and Over 25 Groups", names = c("Under 25", "Over 25"), ylab = "Score Diff = Non-music - music")
boxplot(u25$Time.Diff, o25$Time.Diff, main = "Time Diffs for Under 25 and Over 25 Groups", names = c("Under 25", "Over 25"), ylab = "time Diff = Music - non-music")

# MULTIPLE LINEAR REGRESSION - gender, major, age on scoe
regression_score<-lm(df$Score.Diff ~ df$Gender + df$Under.25 + df$Bio)
summary(regression_score)
residuals_score<-resid(regression_score)
hist(residuals_score)
# evaluating goodness of full regression model - are all the men or women above or below the line?
plot(factor(df$Gender), residuals_score)
plot(df$Under.25, residuals_score)
plot(df$Bio, residuals_score) 










## MULTIPLE LINEAR REGRESSION - gender, major, age on time

regression_time<-lm(df$Time.Diff ~ df$Gender + df$Under.25 + df$Bio)
summary(regression_time)
residuals_time<-resid(regression_time)
hist(residuals_time)




