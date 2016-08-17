rm(list = ls())
# import data from a csv file
data <- read.csv(file.choose(), header = T, sep = ",")
# attach data
attach(data)

# Analyzing data 

# Looking at the histogrm (outcome of the following codes), it seems that 
# '3 clicks before display' plays an important role on the outcome
data$ClicksBeforeThisDisplay <- as.factor(data$ClicksBeforeThisDisplay)
ggplot(data, aes(x = ClicksBeforeThisDisplay, fill = factor(Contact))) +
    geom_bar() +
    xlab("ClicksBeforeThisDisplay") +
    ylab("Total Count") +
    labs(fill = "Accepted offer") 



#data$PreviousDisplaysThisSession <- as.factor(data$PreviousDisplaysThisSession)
sum(Contact[PreviousDisplaysThisSession == 2])/nrow(data)
ggplot(data, aes(x = PreviousDisplaysThisSession, fill = factor(Contact))) +
  geom_bar() +
  xlab("PreviousDisplaysThisSession") +
  ylab("Total Count") +
  labs(fill = "Accepted offer") 

# Outliers should be omitted


# logistic regerssion model using genetalized linear model (glm) 
# Outcome: 0/1-level Contact and features (predictor): are TotalVisits, ClickBeforeThisDisplay and PreviousDisplaysThisSession
mylogit <- glm(Contact ~ TotalVisits + ClicksBeforeThisDisplay + PreviousDisplaysThisSession, data = data, family = "binomial")
# summary of the model
summary(mylogit)
# confInterval = exp(confint(mylogit))
# Odds ratio
odds = exp(coef(mylogit))

# Random forest model
library(randomForest)
rf.train.1 <- as.data.frame(data[, "TotalVisits"])
rf.label <- as.factor(Contact)
set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)



