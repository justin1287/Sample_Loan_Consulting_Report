
## Read in data:
data<-read.csv('Client 11 Data.csv')

## Remove rows with NA interest rates:
data<-data[!is.na(data$int_rate),]

## Remove last 6 rows:
data<-data[,-c(24:29)]

## Remove earliest credit line:
data<-data[,-15]

## Changing states to region factor levels:
state.key<-as.data.frame(cbind(state.abb,state.region))
dc<-as.data.frame(cbind('DC',2))
colnames(dc)<-c('state.abb','state.region')
state.key<-rbind(state.key,dc)
state.key$state.region<-ifelse(state.key$state.region==1 ,'Northeast',state.key$state.region)
state.key$state.region<-ifelse(state.key$state.region==2 ,'South',state.key$state.region)
state.key$state.region<-ifelse(state.key$state.region==3 ,'North/Central',state.key$state.region)
state.key$state.region<-ifelse(state.key$state.region==4 ,'West',state.key$state.region)
state.key$state.region<-as.factor(state.key$state.region)

data$addr_state <- state.key$state.region[match(data$addr_state, state.key$state.abb)]
colnames(data)[12]<-'Region'
data$Region
## Change loan purpose to: credit card, debt consolidation, or other:
data$purpose<-ifelse(data$purpose!='credit_card'&data$purpose!='debt_consolidation','other',data$purpose)
data$purpose<-as.factor(data$purpose)

## Only two rows have home ownership values of 'other' or 'none', so remove them
data<-data[data$home_ownership!='OTHER'&data$home_ownership!='NONE',]

## Turn months since last delinquency into a factor:
data$mths_since_last_delinq<-ifelse(is.na(data$mths_since_last_delinq)==T,999,data$mths_since_last_delinq)
data$mths_since_last_delinq<-ifelse(data$mths_since_last_delinq<=12,'Recent',data$mths_since_last_delinq)
data$mths_since_last_delinq<-ifelse(data$mths_since_last_delinq>12&data$mths_since_last_delinq<=36,'Less Recent',data$mths_since_last_delinq)
data$mths_since_last_delinq<-ifelse(data$mths_since_last_delinq>36&data$mths_since_last_delinq<999,'Long Ago',data$mths_since_last_delinq)
data$mths_since_last_delinq<-ifelse(data$mths_since_last_delinq==999,'Never',data$mths_since_last_delinq)
data$mths_since_last_delinq<-as.factor(data$mths_since_last_delinq)

## Create a variable for length of payment:
data$payment.length<-data$total_pymnt/data$installment

## Create a variable for risk indicator based on proportion of paid to loan obligation:
data$payment.proportion<-data$total_rec_prncp/data$funded_amnt

## Create a plot of residuals to find risk variable using proportion:
library(ggplot2)
data.36m<-data[data$term==' 36 months',]
data.60m<-data[data$term==' 60 months',]
ggplot(data=data.36m, aes(x=payment.length, y=payment.proportion))+geom_point()+geom_smooth(method = lm)+labs(x='Current Number of Months Paid',y="Payment Proportion",title="Payment Proportion Over Time for 36 Month Loans")
ggplot(data=data.60m, aes(x=payment.length, y=payment.proportion))+geom_point()+geom_smooth(method=loess,span=0.5)+labs(x='Current Number of Months Paid',y="Payment Proportion",title="Payment Proportion Over Time for 60 Month Loans")

data.36m.pt1<-data.36m[data.36m$payment.length<27,]
data.36m.pt2<-data.36m[data.36m$payment.length>=27,]

ggplot(data=data.36m.pt1, aes(x=payment.length, y=payment.proportion))+geom_point()+geom_smooth(method = lm)+labs(x='Current Number of Months Paid',y="Payment Proportion",title="Refit Payment Proportion Over Time for 36 Month Loans That \nAre Less Than 27 Months Old")
#### So fit a linear regression on data<27 months for 36 month term based on subset of 36 month but for data>27 months fit linear regression
#### based on entire 36 month data
#### For term=60 use entire data set to get loess curve

lm.mod.36.1<-lm(data.36m.pt1$payment.proportion~data.36m.pt1$payment.length)
data.36m.pt1$risk.indicator<- lm.mod.36.1$residuals*100

lm.mod.36.2<-lm(data.36m$payment.proportion~data.36m$payment.length)
data.36m$risk.indicator<-ifelse(data.36m$payment.length >=27,lm.mod.36.2$residuals*100,NA)
data.36m<-data.36m[!is.na(data.36m$risk.indicator ),]

loess.mod.60<-loess(data.60m$payment.proportion~data.60m$payment.length)
data.60m$risk.indicator<- loess.mod.60$residuals*100

data2<-rbind(data.36m.pt1,data.36m,data.60m)

## Create a column for total loan value if it takes full term to pay off loan:
data2$term<-as.numeric(gsub('months','',gsub('\\s+','', data2$term)))
data2$total.loan.value<-data2$term*data2$installment
data2$term<-as.factor(data2$term)
data2$home_ownership<-as.factor(data2$home_ownership)

sub.data<-data2[data2$loan_status=='Charged Off'|data2$loan_status=="Default"|data2$loan_status=="Fully Paid",]

## Comparing the proportions of the subset to total dataset:
table(data2$purpose)/nrow(data2)
table(sub.data$purpose)/nrow(sub.data)

## Comparing interest rates among factors:
data2$subset.indicator<-as.factor(ifelse(data2$loan_status=='Charged Off'|data2$loan_status=="Default"|data2$loan_status=="Fully Paid",1,0))
ggplot(data=data2,aes(x=Region,y=int_rate,fill=subset.indicator))+geom_boxplot(notch=T)+labs(x='Region',y="Interest Rate",title="Region vs. Interest Rate")+scale_fill_discrete(name = "Dataset", labels = c("Full Dataset", "Subset"))
ggplot(data=data2,aes(x=purpose,y=int_rate,fill=subset.indicator))+geom_boxplot(notch=T)+labs(x='Loan Purpose',y='Interest Rate',title='Loan Purpose vs. Interest Rate')+scale_fill_discrete(name = "Dataset", labels = c("Full Dataset", "Subset"))

## Perform logistic regression on the subset data:
sub.data$paid.off<-ifelse(sub.data$loan_status=='Fully Paid',1,0)

smp_size <- floor(0.8 * nrow(sub.data))

## set the seed to make your partition reproducible
set.seed(22)
train_ind <- sample(seq_len(nrow(sub.data)), size = smp_size)
train <- sub.data[train_ind, ][,-10]
test <- sub.data[-train_ind, ][-10]

## Full logistic regression model:
log.mod1<-glm(paid.off~ funded_amnt+term+int_rate+installment+home_ownership+annual_inc+purpose+Region+dti+delinq_2yrs+mths_since_last_delinq+
                open_acc+revol_bal+total_acc+out_prncp+total_pymnt+total_rec_prncp+payment.length+risk.indicator ,data=train,family = binomial(link=logit))
summary(log.mod1)

probabilities <- log.mod1 %>% predict(test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1,0)
# Model accuracy
mean(predicted.classes==test$paid.off)

## Too many predictors in model I think - use lasso regression
## Lasso regression using test and train data:
x<-model.matrix(paid.off~.,train)[,-c(1:2,40)]
y<-train$paid.off

library(glmnet)
set.seed(22)
cv.lasso<-cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, cv.lasso$lambda.min)
coef(cv.lasso, cv.lasso$lambda.1se)

lasso.model<- glmnet(x, y, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)
# Make prediction on test data
x.test <- model.matrix(paid.off~.,test)[,-c(1:2,40)]
probabilities <- lasso.model %>% predict(newx = x.test,type='response')
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- test$paid.off
mean(predicted.classes == observed.classes)

## Creating confusion matrix:
library(caret)
lasso.confusion<- confusionMatrix(data=factor(predicted.classes),reference = factor(observed.classes))

### Apply lasso regression probabilities to full dataset:
paid.off<-ifelse(data2$loan_status=='Fully Paid',1,0)
data3<-cbind(data2,paid.off)
data3<-data3[,-c(10,27)]
full.test<-model.matrix(paid.off~.,data3)[,-c(1:2,40)]
prob1<-as.data.frame(lasso.model %>% predict(newx = full.test,type='response'))
colnames(prob1)<-'Repayment.Prob'

## Add repayment probability column to full dataset:
data2<-cbind(data2,prob1)
data2$Expected.Total.Return<-data2$total_pymnt+(data2$Repayment.Prob*(data2$total.loan.value-data2$total_pymnt))

## Checking rsq:
subset.test<-model.matrix(paid.off~.-loan_status-payment.proportion,sub.data)[,-c(1:2)]
prob2<-as.data.frame(lasso.model %>% predict(newx = subset.test,type='response'))
predicted.classes2 <- ifelse(prob2 > 0.5, 1, 0)

sst<-sum((sub.data$paid.off-mean(sub.data$paid.off))^2)
sse<-sum((predicted.classes2-sub.data$paid.off)^2)
1-sse/sst


