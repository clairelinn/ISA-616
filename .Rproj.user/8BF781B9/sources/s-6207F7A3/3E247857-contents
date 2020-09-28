### Read in Happiness Data ###


happy<- read.csv("HappyData.csv")

### Explore and Introduce Data ###

install.packages("DataExplorer")
library(DataExplorer)
library(dplyr)
library(DataExplorer)
introduce(happy)
plot_intro(happy)
plot_missing(happy)
class(happy)
str(happy)
plot_str(happy)

### remove columns that have > 50% missing ###

happy<- select (happy, -"Most.people.can.be.trusted..WVS.round.2010.2014",
                -"Most.people.can.be.trusted..WVS.round.2005.2009",
                -"Most.people.can.be.trusted..WVS.round.1999.2004",
                -"Most.people.can.be.trusted..WVS.round.1994.1998",
                -"Most.people.can.be.trusted..WVS.round.1989.1993",
                -"Most.people.can.be.trusted..WVS.round.1981.1984",
                -"Most.people.can.be.trusted..Gallup",
                -"gini.of.household.income.reported.in.Gallup..by.wp5.year",
                -"GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel",
                -"GINI.index..World.Bank.estimate.")
plot_missing(happy)

head(happy)

# impute Confidence.in.national.government
summary(happy$Confidence.in.national.government)
happy$Confidence.in.national.government_M<-as.factor(ifelse(is.na(happy$Confidence.in.national.government), 1, 0))
summary(happy$Confidence.in.national.government_M)
happy$Confidence.in.national.government[is.na(happy$Confidence.in.national.government)]<-mean(happy$Confidence.in.national.government, na.rm=TRUE)
summary(happy$Confidence.in.national.government)
happy<- select(happy, -"Confidence.in.national.government_M")

# impute Democratic.Quality
summary(happy$Democratic.Quality)
happy$Democratic.Quality_M<-as.factor(ifelse(is.na(happy$Democratic.Quality), 1, 0))
summary(happy$Democratic.Quality_M)
happy$Democratic.Quality[is.na(happy$Democratic.Quality)]<-mean(happy$Democratic.Quality, na.rm=TRUE)
summary(happy$Democratic.Quality)
happy<- select(happy, -"Democratic.Quality_M")

# impute Delivery.Quality
summary(happy$Delivery.Quality)
happy$Delivery.Quality_M<-as.factor(ifelse(is.na(happy$Delivery.Quality), 1, 0))
summary(happy$Delivery.Quality_M)
happy$Delivery.Quality[is.na(happy$Delivery.Quality)]<-mean(happy$Delivery.Quality, na.rm=TRUE)
summary(happy$Delivery.Quality)
happy<- select(happy, -"Delivery.Quality_M")

# impute Perceptions.if.corruption
summary(happy$Perceptions.of.corruption)
happy$Perceptions.of.corruption_M<-as.factor(ifelse(is.na(happy$Perceptions.of.corruption), 1, 0))
summary(happy$Perceptions.of.corruption_M)
happy$Perceptions.of.corruption[is.na(happy$Perceptions.of.corruption)]<-mean(happy$Perceptions.of.corruption, na.rm=TRUE)
summary(happy$Perceptions.of.corruption)
happy<- select(happy, -"Perceptions.of.corruption_M")

# impute Generousity
summary(happy$Generosity)
happy$Generousity_M<-as.factor(ifelse(is.na(happy$Generosity), 1, 0))
summary(happy$Generousity_M)
happy$Generosity[is.na(happy$Generosity)]<-mean(happy$Generosity, na.rm=TRUE)
summary(happy$Generosity)
happy<- select(happy, -"Generousity_M")

# impute Healthy.life.expectancy.at.birth
summary(happy$Healthy.life.expectancy.at.birth)
happy$healthy_M<-as.factor(ifelse(is.na(happy$Healthy.life.expectancy.at.birth), 1, 0))
summary(happy$healthy_M)
happy$Healthy.life.expectancy.at.birth[is.na(happy$Healthy.life.expectancy.at.birth)]<-mean(happy$Healthy.life.expectancy.at.birth, na.rm=TRUE)
summary(happy$Healthy.life.expectancy.at.birth)
happy<- select(happy, -"healthy_M")

# impute Freedom.to.make.life.choices
summary(happy$Freedom.to.make.life.choices)
happy$freedom_M<-as.factor(ifelse(is.na(happy$Freedom.to.make.life.choices), 1, 0))
summary(happy$freedom_M)
happy$Freedom.to.make.life.choices[is.na(happy$Freedom.to.make.life.choices)]<-mean(happy$Freedom.to.make.life.choices, na.rm=TRUE)
summary(happy$Freedom.to.make.life.choices)
happy<- select(happy, -"freedom_M")

# impute Log.GPD.per.capita
summary(happy$Log.GDP.per.capita)
happy$log_M<-as.factor(ifelse(is.na(happy$Log.GDP.per.capita), 1, 0))
summary(happy$log_M)
happy$Log.GDP.per.capita[is.na(happy$Log.GDP.per.capita)]<-mean(happy$Log.GDP.per.capita, na.rm=TRUE)
summary(happy$Log.GDP.per.capita)
happy<- select(happy, -"log_M")

# impute Positvie.affect
summary(happy$Positive.affect)
happy$positive_M<-as.factor(ifelse(is.na(happy$Positive.affect), 1, 0))
summary(happy$positive_M)
happy$Positive.affect[is.na(happy$Positive.affect)]<-mean(happy$Positive.affect, na.rm=TRUE)
summary(happy$Positive.affect)
happy<- select(happy, -"positive_M")

# impute Negative.affect
summary(happy$Negative.affect)
happy$negative_M<-as.factor(ifelse(is.na(happy$Negative.affect), 1, 0))
summary(happy$negative_M)
happy$Negative.affect[is.na(happy$Negative.affect)]<-mean(happy$Negative.affect, na.rm=TRUE)
summary(happy$Negative.affect)
happy<- select(happy, -"negative_M")

# impute Social.support
summary(happy$Social.support)
happy$social_M<-as.factor(ifelse(is.na(happy$Social.support), 1, 0))
summary(happy$social_M)
happy$Social.support[is.na(happy$Social.support)]<-mean(happy$Social.support, na.rm=TRUE)
summary(happy$Social.support)
happy<- select(happy, -"social_M")

plot_missing(happy)

###training and validation split

set.seed(13)
trainIndex<-sample(1:nrow(happy), size = round(0.7*nrow(happy)), replace = F)
happy.train<-happy[trainIndex,]
happy.valid<-happy[-trainIndex,]
nrow(happy.train)
nrow(happy.valid)
dim(happy.train)
dim(happy.valid)

happy <- happy[, sapply(happy.train, function(col) length(unique(col))) > 1]
set.seed(13)
trainIndex<-sample(1:nrow(happy), size = round(0.7*nrow(happy)), replace = F)
happy.train<-happy[trainIndex,]
happy.valid<-happy[-trainIndex,]

happy.train.preds<-select(happy.train,-"Country.name")
### create model

set.seed(13)
lm.model <- lm(Life.Ladder~. , data=happy.train.preds)
summary(lm.model)


library(ggplot2)

### test model

preds<- predict(lm.model, happy.valid)
AIC(lm.model)

actuals_preds <- data.frame(cbind(actuals=happy.valid$Life.Ladder, predicteds=preds))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy
head(actuals_preds)


### create visuals
library(caret)
bar.data<- data.frame(varImp(lm.model))

bar.data

ggplot(bar.data, aes(x=Overall)) + geom_()

### connect infrormation about significant variables