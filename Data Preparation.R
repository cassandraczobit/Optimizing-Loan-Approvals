# CIND 119 Final Project 
# Partners: Cassandra Czobit; Jossa Soto

# DATA PREPARATION
# Load the dataset:
library(readr)
CreditCard <- read_csv("./german_credit_card/german_credit.csv")

# Check the attributes: 
str(CreditCard)

# The following attributes should be corrected to reflect a qualitative 
# value (refer to project outline):

# Creditability
# Account balance; 
# Payment status of previous credit; 
# Purpose; 
# Value savings/stocks; 
# Length of current employment; 
# Sex and martial status; 
# Guarantors; 
# Duration in current address;
# Most valuable available asset;
# Concurrent credits;
# Type of apartment;
# Occupation;
# Telephone
# Foreign worker

# Convert the above attributes to factors: 
columns <- c(1,2,4,5,7,8,10:13,15,16,18,20,21)
CreditCard[,columns] <- lapply(CreditCard[,columns], as.factor)

str(CreditCard)

# Check for any missing values: 
sum(is.na(CreditCard))

# There are no missing values.

# Measures of central tendency and max + min values:
mean(CreditCard$`Age (years)`)
max(CreditCard$`Age (years)`)
min(CreditCard$`Age (years)`)
sd(CreditCard$`Age (years)`)

mean(CreditCard$`No of Credits at this Bank`)
max(CreditCard$`No of Credits at this Bank`)
min(CreditCard$`No of Credits at this Bank`)
sd(CreditCard$`No of Credits at this Bank`)

mean(CreditCard$`Duration of Credit (month)`)
max(CreditCard$`Duration of Credit (month)`)
min(CreditCard$`Duration of Credit (month)`)
sd(CreditCard$`Duration of Credit (month)`)

mean(CreditCard$`Credit Amount`)
max(CreditCard$`Credit Amount`)
min(CreditCard$`Credit Amount`)
sd(CreditCard$`Credit Amount`)

mean(CreditCard$`Instalment per cent`)
max(CreditCard$`Instalment per cent`)
min(CreditCard$`Instalment per cent`)
sd(CreditCard$`Instalment per cent`)

mean(CreditCard$`No of dependents`)
max(CreditCard$`No of dependents`)
min(CreditCard$`No of dependents`)
sd(CreditCard$`No of dependents`)

# Determine any outlier values:
boxplot(CreditCard$`Duration of Credit (month)`, 
        CreditCard$`Credit Amount`, 
        CreditCard$`Instalment per cent`,
        CreditCard$`Age (years)`,
        CreditCard$`No of Credits at this Bank`,
        CreditCard$`No of dependents`)

par(mfrow = c(2,3))
boxplot(CreditCard$`Duration of Credit (month)`, main = "Duration of
        Credit (month)")
# Several outliers present.

boxplot(CreditCard$`Credit Amount`, main = "Credit Amount")
# Several outliers present, may not be necessary to remove.

boxplot(CreditCard$`Instalment per cent`, main = "Instalment per cent")
# No visible outliers.

boxplot(CreditCard$`Age (years)`, main = "Age (years)")
# Several outliers present, but may not be necessary to remove. 

boxplot(CreditCard$`No of Credits at this Bank`, main = "Number of 
        Credits at this Bank")
# One outlier present, not necessary to remove.

boxplot(CreditCard$`No of dependents`, main = "Number of Dependents")
# One outlier present, not necessary to remove. 

# Remove outliers:
# It is not necessary to remove any outliers in this dataset. 

# Plot histograms for attributes of concern:
par(mfrow = c(1,2))
hist(CreditCard$`Duration of Credit (month)`, xlab = "Duration of 
     Credit (months)", main = "Distribution of Credit Duration")
hist(CreditCard$`Credit Amount`, xlab = "Credit Amount", main = 
             "Distribution of Credit Amount")

# Determine whether the dataset has an imbalanced class distribution:
sum(CreditCard$Creditability == 0)
sum(CreditCard$Creditability == 1)

# Which attributes seem to be correlated? Which attributes seem to be 
# most linked to the class attribute?:
library(corrplot)
x <- cor(CreditCard[c(3,6,9,14,17,19)])
x
corrplot(x, type = "upper", order = "hclust")

# Which attributes do you think can be eliminated or included in the 
# analysis?:
# Attributes to be included:
# Account balance
# Duration of credit
# Payment status of previous credit
# Credit amount
# Value Savings/Stocks
# Length of current employment
# Instalment percent
# Sex & Marital Status
# Age
# Concurrent credit
# Occupation 
# Foreign worker

# Write clean dataset to a new csv file:
CreditCard_clean <- CreditCard[,c(1:4,6:10,14:15,18,21)]
write.csv(CreditCard_clean, file = "./Clean_CreditCard.csv", 
          row.names = FALSE)
