# Data Mining Function - No Feature Selection

# Input: Set of training and testing data. The response variable is binary.
# Output: AUC and other performance metrics

library(earth)
library(randomForest)
library(DMwR)
library(caret)
library(caretEnsemble)
library(pROC)
library(tictoc)
library(cutpointr)

tic() # Execution time start

# Read in training and testing data. This data has already been preprocessed.
train_pred = read.csv("mice_d1148_1164_Pre_Both.csv") # Independent Variables
test_pred = read.csv("mice_d1168_Pre_Both.csv") # Dependent Variable
train_class = read.csv("d1148_1164_class.csv") # Independent Variables
test_class = read.csv("d1168_class.csv") # Dependent Variable

# Merge the indepedent and dependent variables together into one dataframe
names(train_class) = "crse_grade_off_cat"
data_train = cbind(train_pred, crse_grade_off_cat = train_class)
names(test_class) = "crse_grade_off_cat"
data_test = cbind(test_pred, crse_grade_off_cat = test_class)

set.seed(2018)
data_train$crse_grade_off_cat <- factor(data_train$crse_grade_off_cat)
# Apply SMOTE
data_train_smote<- SMOTE(crse_grade_off_cat ~ ., data = data_train, perc.over = 500,perc.under=100)

# Use SMOTE data as training corpus
data_all = rbind(data_train_smote, data_test)
data_all_dum = as.data.frame(model.matrix(crse_grade_off_cat ~ ., data = data_all))
crse_grade_off_ccat = data_all[,"crse_grade_off_cat"]
data_all_dum_ = cbind(data_all_dum, crse_grade_off_ccat)
levels(data_all_dum_$crse_grade_off_ccat) = c("F", "P")
EC_data_train = data_all_dum_[1:dim(data_train_smote)[1], ][,-1]
EC_data_test = data_all_dum_[(dim(data_train_smote)[1]+1):dim(data_all_dum_)[1], ][,-1] 

set.seed(949)

# Use caret to train the model using cross-validation and generate predictions on the testing corpus

ctrl = trainControl(method = "repeatedcv", repeats  = 1, classProbs = T, savePredictions = T, summaryFunction = twoClassSummary)

mymethods = c("glmnet") # Can be changed to any machine learning algorithm in caret
print ("Running methods")
out = caretList(crse_grade_off_ccat~., data = EC_data_train, methodList = mymethods, trControl = ctrl, tuneLength = 6)

print ("Creating prediction accuracy for training and testing data")
model_preds_train = lapply(out, predict, newdata = EC_data_train[, 1:(dim(EC_data_train)[2] - 1)], type = "prob")
model_preds_train = lapply(model_preds_train, function(x)x[,"F"])
model_preds_train = as.data.frame(model_preds_train)[, -4]

#calculate accuracy, sensitivity, specificity, recall, precision for training data. Adapted from Yaqi Xue

getacc = function(x, y){
  sum(x == y) / length(x);
}

get_sen = function(x, y){ #x is predictect, y is true label
  sum(x == "F" & y == "F")/sum(y == "F")
}
get_spe = function(x, y){
  sum(x == "P" & y == "P")/sum(y == "P")
}
get_pre = function(x, y){
  sum(x == "F" & y == "F")/sum(x == "F")
}

# Predict on testing data
model_preds_tst = lapply(out, predict, newdata = EC_data_test[, 1:(dim(EC_data_test)[2] - 1)], type = "prob")
model_preds_tst = lapply(model_preds_tst, function(x)x[,"F"])
model_preds_tst = as.data.frame(model_preds_tst)[,-4]
auc_test = caTools::colAUC(model_preds_tst, EC_data_test$crse_grade_off_ccat == "F", plotROC = T) # Print AUC
model_preds_lab_test = ifelse(model_preds_tst > 1, "F", "P")

# Use Youden's Index to select a cutpoint on the ROC Curve for confusion matrix performance metrics
cp <- cutpointr(model_preds_tst,EC_data_test$crse_grade_off_ccat, 
                method = maximize_metric, metric = youden,pos_class = "P")
summary(cp)
bal_acc <- (cp$sensitivity+cp$specificity)/2
bal_acc

ci(EC_data_test$crse_grade_off_ccat,model_preds_tst) # 95% confidence interval on AUC

print('H')
toc() # Execution time end