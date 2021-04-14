getwd()

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

tail(titanic.train$IsTrainSet)
tail(titanic.test$IsTrainSet)

titanic.test$Survived <- NA

ncol(titanic.test)
ncol(titanic.train)

names(titanic.test)
names(titanic.train)

titanic.full <- rbind(titanic.train, titanic.test)

#to check if they align
table(titanic.full$IsTrainSet, titanic.full$Sex)

table(titanic.full$Embarked)
#some empty cells, do a flter for the empty cells and replace with 'S'

titanic.full[titanic.full$Embarked=="", "Embarked"] <- "S"

is.na(titanic.full$Age)
table(is.na(titanic.full$Age))

age.median <- median(titanic.full$Age, na.rm = TRUE)

titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

table(is.na(titanic.full$Age))
table(is.na(titanic.full$Fare))

# a fare entry is missing, replace with median as above
fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median
table(is.na(titanic.full$Fare))

#categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

#split the data again
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)
table(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survive.formula <- as.formula(survived.equation)

library(randomForest)
titanic.model <- randomForest(formula = survive.formula, data = titanic.train, ntree = 500 , mtry = 3, nodesize = 0.01 * nrow(titanic.test))
features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare  + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file="kaggle_submission.csv", row.names = FALSE)
