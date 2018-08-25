#Install and Load Packages
install.packages("corrplot")
install.packages("data.table")
library(devtools)
install_github("raivokolde/pheatmap")
install.packages("RColorBrewer")
install.packages("ggstatsplot")
install.packages("later")
install.packages("stringdist")
install.packages("sjstats")
install.packages("tidytext")
install.packages("dplyr")
install.packages("stringr")
require(devtools)
install_github("lchiffon/wordcloud2")
install.packages("tidyr")
install.packages("ggrepel")
install.packages("formattable")
install.packages("skimr")
install.packages("DT")


library(pheatmap)
library(data.table)
library(corrplot)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(later)
library(stringdist)
library(sjstats)
library(ggstatsplot)
library(tidytext)
library(dplyr)
library(stringr)
library(rtweet)
library(wordcloud2)
library(tidyr)
library(ggrepel)
library(skimr)
library(formattable)
library(DT)

#Bring in the data set
df= fread('https://raw.githubusercontent.com/lgellis/STEM/master/DATA-ART-1/Data/FinalData.csv')
attach(df)

#Bring in the data with normalized job descr
dfNormal <- fread('https://raw.githubusercontent.com/lgellis/STEM/master/DATA-ART-1/Data/FinalDataNormalizedCareer.csv')
head(dfNormal)

# Create color palettes
bluePalette <- colorRampPalette(brewer.pal(15,"Blues"))(8)
bluePalette <-bluePalette[-c(1,2)] # remove first 2 b/c they are too light to plot
bluePalette

bluePalette2 <- c("#C5E2F7", "#92BAD2", "#53789E", "#395877")

##Set the blue color scale since non gender specific
pinkPalette <- c("#FDAB9F", "#FE7F9C", "#DF5286", "#F5C3C2", "#FE5BAC", "#FF69B4", "#FB9AAC")

#set the male, female color scale
genderNeutralCol <- "#C6AE74"
girlCol <- "#E07478"
boyCol <- "#588CA8"
  
mfPalette <- c(genderNeutralCol, girlCol, boyCol)
mfPalette

#######################   CORRELATION ##############################


# Create the Correlation Plots

corDf <- df[ , 8:11]
corDf <-sapply(corDf, FUN = as.numeric) 
corDf <-corDf[complete.cases(corDf), ]

#Make the correlation matrix
corMatrix <-cor(corDf, use="complete.obs", method="pearson")
corMatrix

#Plot numerous formats of the correlation matrix
corrplot(corMatrix, type = "upper", order = "hclust", col = c("black", "white"), bg = "lightblue", tl.col = "black")
corrplot(corMatrix, method="circle", tl.col = "black")
corrplot(corMatrix, method="square", tl.col = "black")
corrplot(corMatrix, method="number", tl.col = "black")
corrplot(corMatrix, method="shade", tl.col = "black")
corrplot.mixed(corMatrix, tl.col = "black")


## The male-female scatterplots

dim(df)
head(df)
summary(df)
attach(df)

#######################   SCATTERPLOTS ##############################

#Gender Based Graphs
# please ignore all of the lazy naming :) 

#Screen, Sleep and Gender
scat2 <-ggplot(df, aes(x=Sleep, y=ScreenTime, colour=Gender))
scat2 <- scat2 + scale_color_manual(values= mfPalette) + geom_count() 
scat2 +  theme_bw() + theme_minimal() +  
  facet_wrap( ~ Gender, ncol=2) +
  labs(title = "Time Spent on Screen vs Sleep Considering Gender",
       x = "Hours of Sleep per Night", y = "Hours of Screen Time Per Day")


#Screen, Sleep, Physical Activity and Gender

scat2 <-ggplot(df, aes(x=Sleep, y=ScreenTime, colour=Gender, size=PhysActive))
scat2 <- scat2 + scale_color_manual(values= mfPalette) + geom_point() 
scat2 +  theme_bw() + theme_minimal() +
  facet_wrap( ~ Gender, ncol=2) +
  labs(title = "Time Spent on Screen vs Sleep Considering Gender and Physical Activity",
       x = "Hours of Sleep per Night", y = "Hours of Screen Time Per Day")


#Homework, Sleep, Physical Activity and Gender
scat2 <-ggplot(df, aes(x=Sleep, y=HrsHomework, colour=Gender, size=PhysActive))
scat2 <- scat2 + scale_color_manual(values= mfPalette) + geom_point() 
scat2 +  theme_bw() + theme_minimal() +
  facet_wrap( ~ Gender, ncol=2) +
  labs(title = "Time Spent on Homwork vs Sleep Considering Gender and Physical Activity",
       x = "Hours of Sleep per Night", y = "Hours of Homework Time Per Week")

# Numerical Relations

#Sleep and Screentime - view 1
scat2 <-ggplot(df, aes(x=Sleep, y=ScreenTime))
scat2 <- scat2  + geom_count() 
scat2 +  stat_smooth(method=lm, fill="#BAD6EB", colour="#08306B", size=2) +
  theme_bw() + theme_minimal() +
  labs(title = "Time Spent on Sleep vs Screen Time",
       x = "Hours of Sleep per Night", y = "Hours of Screen Time Per Day")


#Sleep and Screentime - view 2

scat2 <-ggplot(df, aes(x=Sleep, y=ScreenTime))
scat2 <- scat2 + geom_bin2d()
scat2 +  theme_bw() + theme_minimal() +
  stat_smooth(method=lm, fill="#BAD6EB", colour="#08306B", size=2) +
  labs(title = "Time Spent on Screen vs Sleep",
       x = "Hours of Sleep per Night", y = "Hours of Screen Time Per Day")

#Sleep and Screentime - Facet Wrap by Grade to see if it's a common trend without age 
# as confounding factor

scat2 <-ggplot(df, aes(x=Sleep, y=ScreenTime))
scat2 <- scat2 + geom_count()
scat2 +  theme_bw() + theme_minimal() +
  stat_smooth(method=lm, fill="#BAD6EB", colour="#08306B", size=2) +
  labs(title = "Time Spent on Screen vs Sleep by Grade",
       x = "Hours of Sleep per Night", y = "Hours of Screen Time Per Day") +
  facet_wrap( ~ Grade, ncol=2)

#Chart 6

violin <-ggplot(df, aes(y=Sleep, x=IntExt)) 
violin + geom_violin(trim=FALSE, fill="lightblue")  +  
  theme_bw() + theme_minimal() +
  labs(title = "Time Spent on Sleep by Social Factors",
       x = "Hours of Sleep per Night")



#Sleep, Homework and Subject
scat3 <-ggplot(df, aes(x=HrsHomework, y=Sleep, colour=Subject))
scat3<- scat3 + geom_point()
scat3 + facet_wrap( ~ Subject, ncol=2) + 
  scale_color_manual(values= bluePalette)  +
  theme_bw() + theme_minimal() +
  labs(title = "Time Spent on Homwork vs Sleep Considering Students Favorite Subject",
       x = "Hours of Sleep per Night", y = "Hours of Homework Per Week")



# Age differences

# Sleep by Age

sleepTime <-ggplot(df, aes(y=Sleep, x=Grade)) 
sleepTime + geom_count(trim=FALSE, fill="lightblue")  +  
  theme_bw() + theme_minimal() +
  stat_smooth(method=lm, fill="#BAD6EB", colour="#08306B", size=2) +
  labs(title = "Time Spent on Sleep by Grade",
       y = "Hours of Sleep per Night", x="Grade")

# Homework by Age

homeworkTime <-ggplot(df, aes(y=HrsHomework, x=Grade)) 
homeworkTime + geom_count(trim=FALSE, fill="lightblue")  +  
  theme_bw() + theme_minimal() +
  stat_smooth(method=lm, fill="#BAD6EB", colour="#08306B", size=2) +
  labs(title = "Time Spent on Homework by Grade",
       y = "Hours of Homework per Week", x="Grade")


# Physical Activity by Age

physTime <-ggplot(df, aes(y=PhysActive, x=Grade)) 
physTime + geom_count(trim=FALSE, fill="lightblue")  +  
  theme_bw() + theme_minimal() +
  stat_smooth(method=lm, fill="#BAD6EB", colour="#08306B", size=2) +
  labs(title = "Time Spent on Physical Activity by Grade",
       y = "Hours of Physical Activity per Week", x="Grade")

# Screen Time by Age

screenTime <-ggplot(df, aes(y=ScreenTime, x=Grade)) 
screenTime + geom_count(trim=FALSE, fill="lightblue")  +  
  theme_bw() + theme_minimal() +
  stat_smooth(method=lm, fill="#BAD6EB", colour="#08306B", size=2) +
  labs(title = "Time Spent on Screens by Grade",
       y = "Hours of Screen Time per Day", x="Grade")


# Summary by grade

head(df)

#Summary of grade - average screentime, sleep, physical activity etc

gradeSummary <- df %>%
  group_by(Grade) %>%
  summarize(ScreenTimeDay = mean(ScreenTime, na.rm = TRUE), SleepDay = mean(Sleep, na.rm = TRUE), 
            PhysActiveDay = mean(PhysActive, na.rm = TRUE), HomeworkDay = mean(HrsHomework, na.rm = TRUE))  %>%
  gather(measurement, average, ScreenTimeDay:HomeworkDay)


ggplot(gradeSummary, aes(x=Grade, y=average, color=measurement) )+ 
  geom_line(size=1) +
  theme_bw() + theme_minimal() +
  theme(legend.position="bottom",legend.text=element_text(size=7),
        legend.title = element_blank()) +
  labs(title = "Time on Activities by Grade",
       y = "Hours Spent", x="Grade") + 
  scale_color_manual(values=bluePalette2)


#######################   GGSTATSPLOT ##############################

# ggstatsplot

ggstatsplot::ggscatterstats(
  data = df, 
  x = ScreenTime, 
  y = Grade,
  title = "Screen Time by Grade",
  messages = FALSE, 
  marginal.type = "histogram", 
  line.color = "#08306B", 
  xfill = "#BAD6EB",                              
  yfill = "#539ECC",  
)

# plot
ggstatsplot::ggbetweenstats(
  data = df, 
  x = Gender, 
  y = Grade,
  messages = FALSE
)

#Histogram
ggstatsplot::gghistostats(
  data = df,
  x = PhysActive,
  title = "Distribution of Physically Active Hours",
  type = "parametric",                           # one sample t-test
  test.value = 3,                                # default value is 0
  centrality.para = "mean",                      # which measure of central tendency is to be plotted
  centrality.color = "darkred",                  # decides color of vertical line representing central tendency
  binwidth = 0.10,                               # binwidth value (experiment until you find the best one)
  messages = FALSE                               # turn off the messages
) 


#######################   WORD CLOUDS ##############################

#Data set is dfNormal
head(dfNormal)


## Career is first

#Process Data

#Unnest the words - code via Tidy Text

table2 <- dfNormal %>% 
  unnest_tokens(word, Career)

#remove stop words - aka typically very common words such as "the", "of" etc
head(table2)
data(stop_words)

career <- table2 %>%
  anti_join(stop_words) %>%
  filter(! word=='player')


#Girl View

careerGirl <- table2 %>%
  anti_join(stop_words) %>%
  filter(! word=='player') %>%
  filter(Gender == "female")


careerBoy <- table2 %>%
  anti_join(stop_words) %>%
  filter(! word=='player') %>%
  filter(Gender == "male")


#do a word count
careerBoy <- careerBoy %>%
  count(word, sort = TRUE) 
careerBoy


careerGirl <- careerGirl %>%
  count(word, sort = TRUE) 
careerGirl


#Make wordcloud
wordcloud2(careerGirl, size=1, color=girlCol)
wordcloud2(careerBoy, size=1,  color=boyCol)


#Career Bar Charts
careerGirl$gender <- 'female'
careerBoy$gender <- 'male'
careerFull <-rbind(careerBoy, careerGirl)
careerFull

careerFull %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = gender)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gender, scales = "free_y") +
  scale_fill_manual(values=c(girlCol, boyCol)) + 
  labs(y = "Male vs Female Careers",
       x = NULL) +
  coord_flip()  +
  theme_bw() + theme_minimal() 


########################
### Hobbies ###

head(dfNormal)
#Unnest the words - code via Tidy Text

#Combine the hobbies column
dfNormal$hb <- paste(dfNormal$SpendTime1, dfNormal$SpendTime2, sep= " ")
dfNormal$hb

#Unnest the words - code via Tidy Text

table2 <- dfNormal %>% 
  unnest_tokens(word, hb)

#remove stop words - aka typically very common words such as "the", "of" etc

head(table2)
data(stop_words)

hobbies <- table2 %>%
  anti_join(stop_words) %>%
  filter(! word=='player')

hobbiesGirl <- table2 %>%
  anti_join(stop_words) %>%
  filter(Gender == "female")

hobbiesBoy <- table2 %>%
  anti_join(stop_words) %>%
  filter(Gender == "male")

#do a word count

hobbies <- hobbies %>%
  count(word, sort = TRUE) 
hobbies 

hobbiesGirl <- hobbiesGirl %>%
  count(word, sort = TRUE) 
hobbiesGirl

hobbiesBoy <- hobbiesBoy %>%
  count(word, sort = TRUE) 
hobbiesBoy


#download symbol

url <- "https://github.com/lgellis/STEM/blob/master/DATA-ART-1/Symbols/soccer1.png?raw=true"
img <- "soccer1.png"
download.file(url, img) # download file

url <- "https://github.com/lgellis/STEM/blob/master/DATA-ART-1/Symbols/girl3.png?raw=true"
imgGirl <- "girl3.png"
download.file(url, imgGirl) # download file

url <- "https://github.com/lgellis/STEM/blob/master/DATA-ART-1/Symbols/kid.png?raw=true"
imgBoy <- "kid.png"
download.file(url, imgBoy) # download file

#Make wordcloud
wordcloud2(hobbies)
wordcloud2(hobbiesBoy)

wordcloud2(hobbies, size=3, figPath = img)

## girl hobbies boy hobbies

wordcloud2(hobbiesGirl, size=1, figPath = imgGirl, color=girlCol )
wordcloud2(hobbiesBoy, size=3, figPath = imgBoy, color=boyCol) 

# Hobbies bar charts
hobbiesGirl$gender <- 'female'
hobbiesBoy$gender <- 'male'
fullHobbies <-rbind(hobbiesBoy, hobbiesGirl)
fullHobbies

fullHobbies %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = gender)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gender, scales = "free_y") +
  scale_fill_manual(values=c(girlCol, boyCol)) + 
  labs(y = "Male vs Female Hobbies",
       x = NULL) +
  coord_flip()  +
  theme_bw() + theme_minimal() 


########################
### Self View ###

head(dfNormal)
#Unnest the words - code via Tidy Text

#Combine the hobbies column
dfNormal$self <- paste(dfNormal$Self1, dfNormal$Self2, sep= " ")
dfNormal$self

#Unnest the words - code via Tidy Text

table2 <- dfNormal %>% 
  unnest_tokens(word, self)


#remove stop words - aka typically very common words such as "the", "of" etc

self <- table2 %>%
  anti_join(stop_words) 
self

selfGirl <- table2 %>%
  anti_join(stop_words) %>%
  filter(Gender == "female")

selfBoy <- table2 %>%
  anti_join(stop_words) %>%
  filter(Gender == "male")

#do a word count

self <- self %>%
  count(word, sort = TRUE) 
self 

selfGirl <- selfGirl %>%
  count(word, sort = TRUE) 
selfGirl

selfBoy <- selfBoy %>%
  count(word, sort = TRUE) 
selfBoy


#download symbol

url <- "https://github.com/lgellis/STEM/blob/master/DATA-ART-1/Symbols/heart.png?raw=true"
heartimg <- "heart.png"
download.file(url, heartimg) # download file

#Make wordcloud
wordcloud2(self, size=1)
wordcloud2(self, size=3, figPath = heartimg )

wordcloud2(selfBoy, size=1.5, figPath = heartimg, color=boyCol )
wordcloud2(selfGirl, size=1.5, figPath = heartimg, color=girlCol )

# Self bar charts
selfGirl$gender <- 'female'
selfBoy$gender <- 'male'
fullSelf <-rbind(selfBoy, selfGirl)
fullSelf

fullSelf %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = gender)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gender, scales = "free_y") +
  scale_fill_manual(values=c(girlCol, boyCol)) +
  labs(y = "Male vs Female Self Description",
       x = NULL) +
  coord_flip()  +
  theme_bw() + theme_minimal() 


# Sentiment Analysis - do on the full data

?get_sentiments
#afinn is numeric scale, bing is +ve/-ve, 
#nrc is more detailed (ie trust, fear etc), loughran is +ve/-ve
sentiments <- get_sentiments("afinn") 
sentiments

## Get raw word and gender (1 row per instance of assignment)

head(dfNormal)
self1 <- as.data.frame(dfNormal[,c("Gender", "Self1")])
self2 <- as.data.frame(dfNormal[,c("Gender", "Self2")])

head(self2)
#TO DO rename self1 and self2 

names(self1)[2]<-"word"
names(self2)[2]<-"word"
fullStackSelf <-rbind(self1, self2)
head(fullStackSelf)

#add sentiment

sentiments <- get_sentiments("afinn") 
sentiments

fullStackSelfSentiment <- fullStackSelf %>%
  inner_join(sentiments)

fullStackSelfSentiment
skim(fullStackSelfSentiment)
#graph and get the averages


#Averages
summaryGenderSentiment <- fullStackSelfSentiment %>% 
  group_by(Gender) %>%
  summarize(avgSentimentScore = mean(score, na.rm = TRUE), 
            medSentimentScore = median(score, na.rm = TRUE), 
            countWords = length(word))

names(summaryGenderSentiment) <- c("Gender", "Avg Sentiment Score", "Median Sentiment Score", "Word Count")


formattable(summaryGenderSentiment, 
            list("Avg Sentiment Score" = color_tile("transparent", "#00A86B")))

as.datatable(formattable(summaryGenderSentiment))

#box plot

ggplot(fullStackSelfSentiment, aes(x=Gender, y=score, fill=Gender)) + 
  geom_boxplot(notch=TRUE) +
  scale_fill_manual(values=c(genderNeutralCol, girlCol, boyCol)) +
  labs(y = "Sentiment Score for Male vs Female Self Description",
       x = NULL) +
  coord_flip()  +
  theme_bw() + theme_minimal() 






