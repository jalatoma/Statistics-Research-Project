# Research Chicago Schools

# Response: Graduation Rate

# multiple regression method
cpsbad <- read.csv("~/Downloads/CPS.csv", header=TRUE)
CPS <- read.csv("~/Downloads/cpsmod1.csv", header=TRUE)
CPS$Median.income <- as.integer(CPS$Median.income)
CPS$Track.Schedule <- as.factor(CPS$Track.Schedule)
CPS$CPS.Performance.Policy.Status <- as.factor(CPS$CPS.Performance.Policy.Status)
CPS$CPS.Performance.Policy.Level <- as.factor(CPS$CPS.Performance.Policy.Level)
CPS$Safety.Score <- as.integer(CPS$Safety.Score)
CPS$Family.Involvement.Score <- as.integer(CPS$Family.Involvement.Score)
CPS$Leaders.Score <- as.integer(CPS$Leaders.Score)
CPS$Teachers.Score <- as.integer(CPS$Teachers.Score)
CPS$Parent.Engagement.Score <- as.integer(CPS$Parent.Engagement.Score)
CPS$Parent.Environment.Score <- as.integer(CPS$Parent.Environment.Score)
CPS$Average.Student.Attendance <- as.integer(CPS$Average.Student.Attendance)
CPS$Net.Change.EXPLORE.and.PLAN <- as.integer(CPS$Net.Change.EXPLORE.and.PLAN)
CPS$Net.Change.PLAN.and.ACT <- as.integer(CPS$Net.Change.PLAN.and.ACT)

# good scatterplots, summaries, etc.
boxplot(CPS$Graduation.Rate.., main = "Boxplot of Graduation Rate", ylab = "Graduation Rate (percentage)")
summary(CPS$Graduation.Rate..)

# MICE imputing missing data
library(mice)
set.seed(4893)
tempData <- mice(CPS,m=5,maxit=50,meth=c("pmm"))

CPS <- complete(tempData, 1)

# remove influential point
outliers <- boxplot(CPS$Graduation.Rate.., plot=FALSE)$out
CPS<- CPS[-which(CPS$Graduation.Rate.. %in% outliers),]

# linear regression model
library(car)
library(DAAG)
mod <- lm(Graduation.Rate..~., data = CPS)

DAAG::vif(mod) # ASK ABOUT THIS remove highly correlated values VIF>=10
CPSnew = CPS[, -c(1,2)]


modNew <- lm(Graduation.Rate..~., data = train)
DAAG::vif(modNew)
par(mfrow=c(1,1))
plot(modNew) 
car::boxCox(modNew) # no new transformation needed


# stepwise regression
library(MASS)
step.model <- stepAIC(modNew, direction = "both", trace = F)
stepmodsum = summary(step.model)

# split the data
split <- sample.split(CPSnew$Graduation.Rate.., SplitRatio = 0.7)

train <- subset(CPSnew, split == "TRUE")
test <- subset(CPSnew, split == "FALSE")

train.model = lm(Graduation.Rate..~College.Eligibility..+Average.Student.Attendance+
                   Rate.of.Misconducts..per.100.students., data = train)
summ = summary(train.model)
par(mfrow=c(1,1))
plot(train.model)
car::boxCox(train.model)
anova(train.model)


train.model = lm(Graduation.Rate..~College.Eligibility..+Average.Student.Attendance+
                   Rate.of.Misconducts..per.100.students., data = train)
# predicting
predreg = predict(train.model, newdata = test)

# MSE regression
mean((predreg - test$Graduation.Rate..)^2)

# coefficients
summ

# random forest regression - REALLY LOW %VAR EXPLAINED
library(randomForest)
library(dplyr)
library(caTools)
library(tree)
set.seed(4893)
tree.rf <- randomForest(Graduation.Rate..~., mtry = 4, ntree = 400, data = train, importance = TRUE)
pred = predict(tree.rf, newdata = test)

tree.rf
# MSE Random Forest
mean((pred-test$Graduation.Rate..)^2)

plot(tree.rf, main = "Random Forest Error vs. Number of Trees")
importance(tree.rf)
varImpPlot(tree.rf, type = 1, main = "Random Forest Tree")

mod1 = tree(Graduation.Rate.. ~ Teachers.Score+Average.Student.Attendance+Parent.Environment.Score+Leaders.Score+Safety.Score, 
               data = train)

plot(mod1)
text(mod1, pretty = 0)

# comparing predictions
Mass_pred_rf <- predict(tree.rf, CPSnew, predict.all = F)
Mass_pred_reg <- predict(step.model, CPSnew, predict.all = F)

tree2 <- as.data.frame(cbind(CPSnew,  Mass_pred_rf, Mass_pred_reg))

tree2 %>%
  summarize(CPSnew$Graduation.Rate.., Mass_pred_rf, Mass_pred_reg)



