# loading raw data


bank <- read.csv("bankdata_csv_all.csv", header = TRUE)
#View(bank) # view the table

# using library dplyr for removing the ID
library(dplyr)
bank_data_view <- bank

boxplot(bank$age)
boxplot(bank$income)
boxplot(bank$children)
# remove null values
nrow(bank)
nrow(bank[!duplicated(bank), ]) 

# remove the ID column from the bank_csv_all.csv
bank_data_view = select(bank_data_view, -c(id))
#View(bank_data_view)

# as the association rule mining can only be applied on the data that are been ranged or categrical
# the main attributes that need to be discretized are "age", "childern" and "salary"
# first attribute is children - 0,1,2 or 3
library(arules)
install.packages("arules")
install.packages("arc")

library(arc)
#---------discretize----------

# for age
bank_data_view$age <- discretize(bank_data_view$age, method = "interval", breaks = 3, 
                             labels = c("18-34","34-50","50-67")) # try ndgis = 2 to round off

# for income
bank_data_view$income <- discretize(bank_data_view$income, method = "interval", breaks = 3, 
                                labels = c("5014-24386", "24386-43758", "43758-max"))



View(bank_data_view)
library(arules)
library(arulesViz)
#install.packages("arulesViz")
str(bank_data_view)

records <- apriori(bank_data_view[, sapply(bank_data_view, is.factor)],
                   parameter = list(support =0.1, confidence = 0.5, minlen = 3)) # threshold
inspect(head(records, 600))

records <- apriori(bank_data_view[, sapply(bank_data_view, is.factor)],
                   parameter = list(support =0.2, confidence = 0.8, minlen = 2))
inspect(head(records, 30))



# method 2 for the same
# converting the recod dataset to transactional dataset--------------------------------------------

fac <- sapply(bank_data_view, is.factor)
bank_datset <- as(bank_data_view[, fac], "transactions")
rules_transactions <- apriori(bank_datset, parameter = list(support = 0.2, confidence = 0.8, minlen = 5))
inspect(head(rules_transactions, 10))

rules <- apriori(data = bank_datset, parameter = list(support = 0.1, confidence = 0.1, minlen =4),
                 appearance = list(default = "lhs", rhs = c("pep=YES")),control = list (verbose=F) )
summary(bank_datset)
inspect(head(sort(rules,by = "lift", decreasing = T), 50))





