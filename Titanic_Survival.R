# Setting the working directory
getwd()
setwd("C:/Users/DELL/Desktop/R_projects/Logistic Regression/Titanic")

# Required libraries
library(Amelia)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(Matrix)

# Loading data
df.train <- read.csv('train.csv')
df.test <- read.csv('test.csv')

# Checking the dimension of the data
dim(df.train)
dim(df.test)

#looking top 5 rows
head(df.train)
head(df.test)

#looking bottom 5 rows
tail(df.train)
tail(df.test)

# Checking the structure and the summary of the data
str(df.train)
str(df.test)

summary(df.train)
summary(df.test)

# Checking for the columns with missing values 
names(which(sapply(df.train, anyNA)))
names(which(sapply(df.test, anyNA)))

# Converting the variables into factors 
df.train$Pclass <- factor(df.train$Pclass)
df.train$Survived <- factor(df.train$Survived)

# Exploratory Data Analysis (EDA)

# Exploring how much missing data we have

missmap(df.train, main = "Titanic Training Data - Missings Map", 
        col = c('yellow', 'black'),legend = FALSE)

# Data Visualization with ggplot2

# Survival Count
ggplot(df.train,aes(Survived)) +
  geom_bar() +
  theme_bw()

# Passengers in each class
ggplot(df.train, aes(Pclass)) +
  geom_bar(aes(fill= factor(Pclass))) +
  theme_bw()

# Count of Males and females aboard
ggplot(df.train, aes(Sex)) + 
  geom_bar(aes(fill= factor(Sex))) +
  theme_bw()

# Age
ggplot(df.train, aes(Age)) + 
  geom_histogram(bins = 20, alpha= 0.5, fill ='blue') +
  theme_bw()                                       

# Number of siblings and spouses
ggplot(df.train, aes(SibSp)) +
  geom_bar() +
  theme_bw()

# Fare
ggplot(df.train, aes(Fare)) +
  geom_histogram(fill = 'green', color = 'black', alpha= 0.5) +
  theme_bw()

# Number of passengers suvrvived based on Age
ggplot(df.train, aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  ggtitle("Age vs Survived") +
  theme_bw()

# Number of passengers suvrvived based on their Sex
ggplot(df.train,aes(Sex)) +
  geom_bar(aes(fill= factor(Survived)), position = 'dodge') +
  ggtitle("Sex vs Survived") +
  theme_bw()

# Number of passengers suvrvived based on Age and Sex
ggplot(df.train, aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  facet_grid(. ~ Sex) +
  ggtitle("Age vs Sex vs Survived") +
  theme_bw()

# Number of passengers suvrvived based on Pclass
ggplot(df.train,aes(Pclass)) +
  geom_bar(aes(fill= factor(Survived)), position = 'dodge') +
  ggtitle("Pclass vs Survived") +
  theme_bw()
  
# Number of passengers suvrvived based on Pclass and Sex
ggplot(df.train,aes(Pclass)) +
  geom_bar(aes(fill= factor(Survived)), position = 'dodge') +
  facet_grid(. ~ Sex) +
  ggtitle("Pclass vs Sex vs Survived") +
  theme_bw()

# Number of passengers suvrvived based on Age
ggplot(df.train, aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  facet_grid(. ~ Sex) +
  ggtitle("Age vs Sex vs Survived") +
  theme_bw()
  
# Number of passengers suvrvived based on Fare
ggplot(df.train, aes(Fare, fill = factor(Survived))) + 
  geom_histogram(bins=30, position = 'dodge') + 
  ggtitle("Fare vs Survived") +
  theme_bw()

# Data Pre-processing On Training data
# Treating the Name variable

# Creating a title column
df.train$Title <- gsub('(.*, )|(\\..*)', '', df.train$Name)

# Titles by Sex
table(df.train$Sex, df.train$Title)

# Reassigning the rare titles
officer <- c( "Capt", "Col", "Don", "Dr", "Major", "Rev")
royalty <- c("Dona", "Lady", "the Countess", "Sir", "Jonkheer")

# Reassign mlle, ms, and mme, and others
df.train$Title[df.train$Title == 'Mlle'] <- 'Miss' 
df.train$Title[df.train$Title == 'Ms'] <- 'Miss'
df.train$Title[df.train$Title == 'Mme'] <- 'Mrs' 
df.train$Title[df.train$Title %in% royalty]  <- 'Royalty'
df.train$Title[df.train$Title %in% officer]  <- 'Officer'

# Number of passengers suvrvived based on Title
ggplot(df.train, aes(Title,fill = factor(Survived))) +
  geom_bar(stat = "count")+
  ggtitle("Title vs Survived") +
  theme_bw()

# Getting the size of the family aboard

# Creating a new variable Family Size using Name, SibSp and Parch
df.train$Family_Members <- df.train$SibSp + df.train$Parch + 1
summary(df.train$Family_Members)

# Family Members vs Survived
ggplot(df.train, aes(Family_Members,fill = factor(Survived))) +
  geom_bar(stat = "count", position = "dodge")+
  scale_x_continuous(breaks=c(1:11)) +
  ggtitle("Family Members vs Survived") +
  theme_bw()

# Classifying Family_Size into Factors such as Big, Small and Alone
df.train$Family_Size[df.train$Family_Members == 1] <- "Alone"
df.train$Family_Size[df.train$Family_Members < 5 & df.train$Family_Members > 1] <- "Small"
df.train$Family_Size[df.train$Family_Members > 4] <- "Big"

# Family Size vs Survived
ggplot(df.train, aes(Family_Size,fill = factor(Survived))) +
  geom_bar(stat = "count", position = "dodge")+
  ggtitle("Family Size vs Survived") +
  theme_bw()

# Embarked
summary(df.train$Embarked)
levels(df.train$Embarked)

#2 missing datas 
which(df.train$Embarked == "")

# Imputing the missing data with S
df.train$Embarked[c(62, 830)] <- 'S'

df.train$Embarked <- factor(df.train$Embarked,
                            levels = c("C", "Q", "S"))

# Number of passengers suvrvived based on Pclass and Embarked
ggplot(df.train,aes(Pclass)) +
  geom_bar(aes(fill= factor(Survived))) +
  facet_wrap(. ~ Embarked) + 
  ggtitle("Embarked vs Pclass vs Survived") +
  theme_bw()

# Imputing the na values
ggplot(df.train,aes(Pclass,Age)) +
  geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4)) +
  scale_y_continuous(breaks = seq(min(0), max(80), by = 2)) +
  theme_bw()

# Imputation based on class
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.ages

# Final na Check
missmap(df.train, main = "  Imputation Check", 
        col = c('yellow', 'black'),legend = FALSE)

# Finding the number of Children aboard based on their Ages
df.train$Child <- ifelse(df.train$Age < 18, "Child", "Adult")

# Child vs Survived
ggplot(df.train,aes(Child)) +
  geom_bar(aes(fill= factor(Survived))) +
  ggtitle("Child vs Survived") +
  theme_bw()

# Child vs Survived
ggplot(df.train,aes(Child)) +
  geom_bar(aes(fill= factor(Survived)), position = "dodge") +
  facet_wrap(. ~ Pclass) +
  ggtitle("Child vs Survived") +
    theme_bw()

df.train$Child <- ifelse(df.train$Child == "Child", 1, 0)

# Converting into Factors
df.train$Child  <- factor(df.train$Child)
df.train$Sex  <- factor(df.train$Sex)
df.train$Embarked  <- factor(df.train$Embarked)
df.train$Title  <- factor(df.train$Title)
df.train$Family_Size  <- factor(df.train$Family_Size)

#############################
## TEST Data

# Converting the Pclass variables into factors 
df.test$Pclass <- factor(df.test$Pclass)

# Exploring how much missing data we have
missmap(df.test, main = "Titanic Test Data - Missings Map", 
        col = c('yellow', 'black'),legend = FALSE)


# Data Pre-processing on Test data
# Treating the Name variable

# Creating a title column
df.test$Title <- gsub('(.*, )|(\\..*)', '', df.test$Name)

# Titles by Sex
table(df.test$Sex, df.test$Title)

# Reassigning the rare titles
officer_test <- c( "Col", "Dona", "Dr", "Rev")

df.test$Title[df.test$Title %in% officer_test]  <- 'Officer'
df.test$Title[df.test$Title == 'Ms']          <- 'Miss'

# Getting the size of the family aboard

# Creating a new variable Family Size using Name, SibSp and Parch
df.test$Family_Members <- df.test$SibSp + df.test$Parch + 1
summary(df.test$Family_Members)

# Classifying Family_Size into Factors such as Big, Small and Alone
df.test$Family_Size[df.test$Family_Members == 1] <- "Alone"
df.test$Family_Size[df.test$Family_Members < 5 & df.test$Family_Members > 1] <- "Small"
df.test$Family_Size[df.test$Family_Members > 4] <- "Big"

# Imputing the na values
ggplot(df.test,aes(Pclass,Age)) +
  geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4)) +
  scale_y_continuous(breaks = seq(min(0), max(80), by = 2)) +
  theme_bw()

ggplot(df.test,aes(Pclass,Fare)) +
  geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4)) +
  scale_y_continuous(breaks = seq(min(0), max(550), by = 50))

# Imputation based on class
impute_age_test <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 42
        
      }else if (class[i] == 2){
        out[i] <- 27
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

fixed.ages_test <- impute_age_test(df.test$Age,df.test$Pclass)
df.test$Age <- fixed.ages_test

# Fare
which(is.na(df.test$Fare))
df.test[153,]

Pclass_3 <- df.test %>% 
  select(Fare, Pclass) %>% 
  filter(Pclass == '3')

df.test$Fare[is.na(df.test$Fare)] <- median(Pclass_3$Fare, na.rm =  TRUE) 

# Final na check
missmap(df.test, main = "  Imputation Check", 
        col = c('yellow', 'black'),legend = FALSE)

# Finding the number of Children aboard based on their Ages
df.test$Child <- ifelse(df.test$Age < 18, "Child", "Adult")

df.test$Child <- ifelse(df.test$Child == "Child", 1, 0)

# Converting into Factors
df.test$Child  <- factor(df.test$Child)
df.test$Sex  <- factor(df.test$Sex)
df.test$Embarked  <- factor(df.test$Embarked)
df.test$Title  <- factor(df.test$Title)
df.test$Family_Size  <- factor(df.test$Family_Size)

# Data without Cabin & Ticket
df.train1 <- df.train[,-c(9,11)]
df.test1 <- df.test[,-c(8,10)]

# Data without PassengerId & Name
df.train1 <- select(df.train1,-PassengerId,-Name,-Family_Members)
df.test1 <- select(df.test1,-PassengerId,-Name,-Family_Members)

# Checking the structure and summary of the new data frames
str(df.train1)
summary(df.train1)

str(df.test1)
summary(df.test1)

names(df.test1)
names(df.train1)

## Model Builing

# Logistic Regression

Logistic_model <- glm(Survived ~ ., 
                      family = binomial(link='logit'),
                      data = df.train1)

Logistic_model
summary(Logistic_model)

# Making Predictions
fitted.probabilities <- predict(Logistic_model, newdata = df.test1, type = 'response')
fitted.results <- ifelse(fitted.probabilities > 0.5, 1, 0)

# Solution
solution <- data.frame(Survived = fitted.results, PassengerID = df.test$PassengerId)

#  Creatin the solution csv file
write.csv(solution, file = 'Logistic_model.csv', row.names = F)

# Decision Tree
set.seed(100)
DTree_model <- rpart(Survived ~.,
                     data = df.train1,
                     method = 'class')
summary(DTree_model)
rpart.plot(DTree_model)

# Making Predictions
DT_predictions <- predict(DTree_model, newdata = df.test1, type = 'class')

# Solution
solution_1 <- data.frame(Survived = DT_predictions, PassengerID = df.test$PassengerId)

#  Creatin the solution csv file
write.csv(solution_1, file = 'DTree_model.csv', row.names = F)

# random forest
levels(df.test1$Title) = levels(df.train1$Title)

set.seed(100)
RF_model <- randomForest(Survived ~ ., 
             data = df.train1, 
             ntree = 1000,
             mtry = 6, 
             importance = TRUE)
RF_model
summary(RF_model)
plot(RF_model)

# Var importantes
importance    <- importance(RF_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# var imp
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Graph var importantes
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank))+
  labs(x = 'Variables') +
  coord_flip() + 
  theme_bw()
  
# Making Predictions
RF_predictions <- predict(RF_model, newdata = df.test1)

# Solution
solution_2 <- data.frame(Survived = RF_predictions, PassengerID = df.test$PassengerId)

#  Creatin the solution csv file
write.csv(solution_2, file = 'RF_model.csv', row.names = F)


