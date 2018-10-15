install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
##install.packages("datasets") #not working#
##library(datasets)
data("Groceries")
class(Groceries)
Groceries
View(Groceries)
itemFrequencyPlot(Groceries,topN=20,type="absolute")
##Read CSV file
mydata <- read.csv("C:/Users/vishw/OneDrive/Documents/Cafe Great Transaction Data updated.csv", header = TRUE)
View(mydata)

##itemFrequencyPlot(mydata,topN=20,type="absolute")
##size(head(mydata))

head(mydata, n=10) ##first 10 observations
##mydata$X <- NULL ##removing unwanted column from the table

##mydata1 <- data.frame(mydata, Quantity = factor('Quantity'), Rate = factor('Rate'), Tax = factor('Tax'), Discount = factor('Discount'), Total = factor('Total'))
##View(mydata1)

##class(mydata$Quantity) ##check the class of Quantity column
##mydata1 = discretizeDF(mydata, method = NULL, default = NULL, )

##converting continuous data to factor
##mydata$Quantity = discretize(mydata$Quantity, method = "frequency", 1)
##mydata$Rate = discretize(mydata$Rate, method = "frequency", 1)
##mydata$Tax = discretize(mydata$Tax, method = "frequency", 1)
##mydata$Discount = discretize(mydata$Discount, method = "frequency", 1)
##mydata$Total = discretize(mydata$Total, method = "frequency", 1)

mydata1 <- split(mydata$Item.Desc, mydata$Bill.Number)

##converting data to transactions
tData <- as (mydata1, "transactions")
summary(tData)
View(tData)
##inspect(tData) ##do not run as we have massive dataset

##Most frequent items
itemFrequency(tData, type = "relative")
itemFrequencyPlot(tData,topN = 10)

# aggregated data
rules = apriori(tData, parameter=list(support=0.001, confidence=0.1))
##rules = apriori(tData, parameter=list(support=0.005, confidence=0.8, minlen = 3))
##rules = apriori(tData, parameter=list(support=0.005, confidence=0.8, maxlen = 4))

#Convert rules into data frame 
rules3 = as(rules, "data.frame") 
View(rules3)
write(rules,file="Market_Basket_analysis.csv",sep=",",row.names = FALSE)

# Show only particular product rules 
##inspect( subset( rules, subset = rhs %pin% "Product H" )) 
inspect(rules)

# Show the top 10 rules
options(digits=2)
inspect(rules[1:10])

# Get Summary Information
summary(rules)

# Sort by Lift
rules<-sort(rules, by="confidence", decreasing=TRUE)

# Remove Unnecessary Rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

#Visualization
library(arulesViz)
plot(rules,method="graph")
plot(rules, method = "graph", shading = NA)
