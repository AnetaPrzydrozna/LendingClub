library(stats)
library(rpart)

loan<-read.csv("loan.csv") 

colnames(loan)
dim(loan)
head(loan)
str(loan)
table(loan$loan_status,loan$inq_last_6mths)
loan<-loan[,c(3,6,7,8,9,10,12,14,17,21,25,28,33,34)]

library(tidyr)
library(dplyr)

#data
loan<-loan %>% drop_na()

loan<-subset(loan,loan_status!="Current")
loan<-subset(loan,emp_length!="n/a")
dim(loan)
loan$loan_status<-ifelse(loan$loan_status=="Fully Paid",1,0) ## 1= Paid,  0=Unpaid

table(loan$loan_status)
library(ROSE)
loan<- ovun.sample(loan_status ~ ., data = loan, method = "under")$data
table(loan$loan_status,loan$term)[2,]/(colMeans(table(loan$loan_status,loan$term))*2) #percentage

# plots
table(loan$loan_status,loan$emp_length)
barplot(table(loan$loan_status,loan$emp_length)[,1:11],main="Employment length influence on repaid",
        names=c("< 1",1,"10+",2,3,4,5,6,7,8,9),las=2) #without n/a
barplot(table(loan$loan_status,loan$term),main="Loan repayment date")
barplot(table(loan$loan_status, loan$grade),main="Loan grades",las=2)

status<-loan$loan_status
interaction.plot(loan$grade, status ,loan$loan_amnt, xlab = "Grade",ylab="Amount of money requested by the borrower"  )
interaction.plot( loan$grade ,status ,loan$int_rate, xlab = "Grade",ylab="The interest rate" )
interaction.plot( loan$purpose , status ,loan$annual_inc, xlab = "",ylab="",las=2)

# training/test datasets
library(caret)
loan$loan_status<-as.factor(loan$loan_status)
train <- createDataPartition(loan$loan_status, p = 0.8)[[1]] #1 for take integer from list  (caret) 
loan_test <- loan[-train,]
loan_train <- loan[train,]

dim(loan_train)
dim(loan_test)
table(loan_train$loan_status)
table(loan_test$loan_status)

 

library(knitr)
rmarkdown::render('LendingClub.Rmd')

