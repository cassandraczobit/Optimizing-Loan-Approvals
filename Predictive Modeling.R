# CIND 119 Final Project 
# Partners: Cassandra Czobit; Jossa Soto

# PREDICTIVE MODELING
# Load the clean dataset:
library(readr)
Clean_CreditCard <- read_csv("./Clean_CreditCard.csv")
Columns <- c(1,2,4,6:7,9,11:13)
Clean_CreditCard[,Columns] <- lapply(Clean_CreditCard[,Columns], 
                                     as.factor)

# Create the test and training sets:
library(caret)
set.seed(10)
train_index <- createDataPartition(Clean_CreditCard$Creditability, 
                                   p = .7, list = FALSE)
train.set <- Clean_CreditCard[train_index,]
test.set <- Clean_CreditCard[-train_index,]

# Create a logistic regression model:
model <- glm(Creditability ~. , data = Clean_CreditCard, 
             family = "binomial")

# Naive Bayes Model:
library(e1071)
NBmodel <- naiveBayes(Creditability ~., data = train.set)
class(NBmodel) 
nb_train_predict <- predict(NBmodel, test.set[ , names(test.set) != "Creditability"])

# Confusion Matrix:
cfm <- confusionMatrix(nb_train_predict, test.set$Creditability, 
                       mode = "prec_recall", positive = '1')
cfm

# Decision Tree:
library(rpart)
library(rpart.plot)

tree <- rpart(Creditability ~ ., data = Clean_CreditCard, cp=0.00001)
rpart.plot(tree)

fit <- rpart(Creditability ~ .,
             method="class", data=train.set)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# Plot tree
rpart.plot(fit, yesno = TRUE)

# Prune the tree
pfit <- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# Plot the pruned tree
rpart.plot(pfit, yesno = TRUE,
           box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)


pred <- predict(pfit, test.set, type = "class")
confusionMatrix(test.set$Creditability, pred, mode = "prec_recall", 
                positive = '1')


# Comparing to a baseline model:
set.seed(15)
train_index_base <- createDataPartition(CreditCard$Creditability, 
                                   p = .7, list = FALSE)
train.set_base <- CreditCard[train_index,]
test.set_base <- CreditCard[-train_index,]

# Naive Bayes 
library(e1071)
NBmodel_base <- naiveBayes(Creditability ~., data = train.set_base)
class(NBmodel_base) 
nb_train_pred_base <- predict(NBmodel_base, test.set_base[ , names(test.set_base) != "Creditability"])

# Confusion Matrix:
cfm_base <- confusionMatrix(nb_train_pred_base, test.set_base$Creditability,
                            mode = "prec_recall", positive = '1')
cfm_base
