rm(list = ls())
# import data from a csv file
data <- read.csv(file.choose(), header = T, sep = ",")

# Trim original data
data.trimed <- data.frame(data$TotalVisits, data$ClicksBeforeThisDisplay, data$PreviousDisplaysThisSession, data$Contact)
colnames(data.trimed) <- c("visits", "clicks", "displays", "contact")

# attach and trim data
attach(data.trimed)
data.trimed <- data.trimed[visits < 10 & clicks < 10 & displays < 10,]




# Analyzing data 
library(ggplot2)
# Looking at the histogrm (outcome of the following codes), it seems that 
# '3 clicks before display' plays an important role on the outcome
#data.trimed$ClicksBeforeThisDisplay <- as.factor(data.trimed$ClicksBeforeThisDisplay)
ggplot(data.trimed, aes(x = clicks, fill = factor(contact))) +
    geom_bar() +
    xlab("ClicksBeforeThisDisplay") +
    ylab("Total Count") +
    labs(fill = "Accepted offer") 



#data$PreviousDisplaysThisSession <- as.factor(data$PreviousDisplaysThisSession)
sum(contact[displays == 1])/nrow(data.trimed)
ggplot(data.trimed, aes(x = displays, fill = factor(contact))) +
  geom_bar() +
  xlab("PreviousDisplaysThisSession") +
  ylab("Total Count") +
  labs(fill = "Accepted offer") 


#data$PreviousDisplaysThisSession <- as.factor(data$PreviousDisplaysThisSession)
sum(data.trimed$Contact[data.trimed$TotalVisits == 50])/nrow(data.trimed)
ggplot(data.trimed, aes(x = visits, fill = factor(contact))) +
  geom_bar() +
  xlab("Total Visits") +
  ylab("Total Count") +
  labs(fill = "Accepted offer") 

# Outliers should be omitted


# logistic regerssion model using genetalized linear model (glm) 
# Outcome: 0/1-level Contact and features (predictor): are TotalVisits, ClickBeforeThisDisplay and PreviousDisplaysThisSession
mylogit <- glm(contact ~ visits + displays + clicks, data = data.trimed, family = "binomial")
# summary of the model
summary(mylogit)
# confInterval = exp(confint(mylogit))
# Odds ratio
odds = exp(coef(mylogit))


# Random forest model
library(randomForest)
rf.train.1 <- as.data.frame(data.trimed[,c("visits","clicks","displays")])
rf.label <- as.factor(data.trimed$contact)
set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 500)
rf.1
varImpPlot(rf.1)



