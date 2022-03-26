library(tree)
library(rpart)
library(rpart.plot)
library(caTools)

drugs = read.csv('/Users/kiroshenouda/Desktop/COMPSCI/R ML DATASETS/drug200.csv')
head(drugs)
str(drugs)
summary(drugs)

colSums(is.na(drugs))
sum(is.na(drugs))
drugs$Sex = factor(drugs$Sex)
drugs$BP = factor(drugs$BP)
drugs$Cholesterol = factor(drugs$Cholesterol)
drugs$Drug = factor(drugs$Drug)
str(drugs)

sample = sample.split(drugs$Drug, SplitRatio =  0.8)
train = subset(drugs, sample == TRUE)
test = subset(drugs, sample == FALSE)

model1 = tree(Drug ~ ., data = drugs, method = 'class')
plot(model1)
text(model1, pretty = T)
model1.preds = predict(model1, drugs, type = 'class')
confusionMatrix(model1.preds, drugs$Drug)
table(model1.preds, drugs$Drug)

model2 = tree(Drug ~., data = train, method = 'class')
plot(model2)
text(model2, pretty = T)
model2.preds = predict(model2, train[1:5], type = 'class')
confusionMatrix(model2.preds, train$Drug)
table(model2.preds, train$Drug)






