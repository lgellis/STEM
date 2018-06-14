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

#Bring in the data set
df= fread('https://raw.githubusercontent.com/lgellis/STEM/master/DATA-ART-1/Data/FinalData.csv')
attach(df)

# Create color palettes
##Set the blue color scale since non gender specific
bluePalette <- colorRampPalette(brewer.pal(15,"Blues"))(8)
bluePalette <-bluePalette[-c(1,2)] # remove first 2 b/c they are too light to plot
bluePalette

#set the male, female color scale
mfPalette <- c("#C6AE74", "#E07478", "#588CA8")
mfPalette



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


# ggstatsplot

?ggscatterstats

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

#density
ggstatsplot::ggscatterstats(
  data = df, 
  x = ScreenTime, 
  y = Sleep,
  line.color = "#08306B", 
  title = "Sleep vs Screen Time",
  messages = FALSE,
  marginal.type = "density", 
  xfill = "#BAD6EB",                              
  yfill = "#539ECC",  
  xlab = "Sleep",      
  ylab = "Screen Time",   
)


