library(ggplot2)
library(dplyr)
library(randomForest)
library(caTools)
library(openxlsx)
library(corrgram)
library(tidyr)
library("PerformanceAnalytics")
library(ranger)
library(MASS)
#--------------------------------- 1. Get Data --------------------------------#
#--- create data directory and add it to .gitignore ---#
if(!file.exists("./data")) {
      dir.create("./data")
      fileConn<-file(".gitignore")
      writeLines(c("data/"), fileConn)
      close(fileConn)
}

#--- download file ---#
url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/00242/ENB2012_data.xlsx'
if(!file.exists('./data/ENB2012_data.xlsx')) {
      download.file(url, destfile = './data/ENB2012_data.xlsx',method='curl')
}

#--- read file ---#
energy.raw <- read.xlsx('data/ENB2012_data.xlsx')
names(energy.raw) <- c('Compactness','SurfaceArea', 'WallArea','RoofArea',
                       'Height','Orientation', 'GlazingArea','GlazingDist', 
                       'HeatingLoad','CoolingLoad')

#------------------------------------ 2. EDA ----------------------------------#
#--- First Looks ---#
head(energy.raw)
str(energy.raw)

# --- facet of histograms --- #
ggplot(gather(energy.raw[,1:8]), aes(value)) + 
      geom_histogram(bins = 10, color='darkblue', fill = 'lightblue') + 
      facet_wrap(~key, scales = 'free_x')

ggplot(energy.raw, aes(HeatingLoad)) + 
      geom_histogram(bins = 10, color='darkblue', fill = 'lightblue')

ggplot(energy.raw, aes(CoolingLoad)) + 
      geom_histogram(bins = 10, color='darkblue', fill = 'lightblue')

#--- correlation using Spearman ---#
corrgram(energy.raw,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt, method='spearman')


chart.Correlation(energy.raw, histogram=TRUE, pch=19)


#-------------------------------- 3. Split Data --------------------------------#
set.seed(1234)
sample <- sample.split(energy.raw, .7)
train <- subset(energy.raw,sample == T)
test <- subset(energy.raw, sample == F)

#---------------------------------- 4. Models ----------------------------------#

heat.train <- as.data.frame(train[,1:9])
heat.test <- as.data.frame(test[,1:9])
summary(rr.huber.h <- rlm(HeatingLoad~ .-RoofArea, data = heat.train))


cool.train <- as.data.frame(train[,c(1:8,10)])
cool.test <- as.data.frame(test[,c(1:8,10)]) 
summary(rr.huber.c <- rlm(CoolingLoad~ .-RoofArea, data = cool.train))

# -- linear -- #
lmod1.h <- lm(HeatingLoad ~ ., data=heat.train)
summary(lmod1.h)

lmod2.h <- lm(HeatingLoad ~ . - RoofArea, heat.train)
summary(lmod2.h)

lmod3.h <- lm(HeatingLoad ~ . - RoofArea - Orientation, heat.train)
summary(lmod3.h)

lmod4.h <- lm(HeatingLoad ~ . - RoofArea -GlazingDist - Orientation, heat.train)
summary(lmod4.h)

lmod5.h <- lm(HeatingLoad ~ . - RoofArea -GlazingDist - Orientation -Compactness, heat.train)
summary(lmod5.h)

lmod6.h <- lm(HeatingLoad ~ . - RoofArea -GlazingDist - Orientation - SurfaceArea, heat.train)
summary(lmod6.h)

lmod1.c <- lm(CoolingLoad ~ ., data=cool.train)
summary(lmod1.c)

lmod2.c <- lm(CoolingLoad ~ . - RoofArea, data=cool.train)
summary(lmod2.c)

lmod3.c <- lm(CoolingLoad ~ . - Orientation - RoofArea, data=cool.train)
summary(lmod3.c)

lmod4.c <- lm(CoolingLoad ~ . - Orientation - RoofArea - GlazingDist, data=cool.train)
summary(lmod4.c)

# -- resid -- #
plot(lmod4.h)
plot(lmod4.c)

# mse #
mse <- function(sm) mean(sm$residuals^2)
mse(summary(lmod3.h))
mse(summary(lmod4.c))

# -- predicting -- #
l.pred.h <- predict(lmod3.h, heat.test)
plot(l.pred.h, test$HeatingLoad)

l.pred.c <- predict(lmod4.c, cool.test)
plot(l.pred.c, test$CoolingLoad)

# --- Random Forests --- #
set.seed(101)
rf.model.h <- randomForest(HeatingLoad ~ . , data = heat.train, importance = T)
print(rf.model.h)
rf.pred.h <- predict(rf.model.h, test[,1:9])
plot(rf.pred.h, test$HeatingLoad)
plot(rf.model.h)
which.min(rf.model.h$mse)

rf.model.c <- randomForest(CoolingLoad ~ . , data = cool.train, importance = T)
print(rf.model.c)
rf.pred.c <- predict(rf.model.c, cool.test)
plot(rf.pred.c, cool.test$CoolingLoad)
plot(rf.model.c)
which.min(rf.model.c$mse)

# -- variable importance -- #
DF <- DF %>% mutate(IncMSE = X.IncMSE/100)

ggplot(DF, aes(x=reorder(Variable,IncMSE), y=IncMSE,fill=IncMSE))+ 
      geom_bar(stat="identity", position="dodge")+ coord_flip()+
      ylab("Variable Importance")+
      xlab("")+
      ggtitle("Information Value Summary")+
      guides(fill=F)+
      scale_fill_gradient(low="lightblue", high="darkblue")

