# Data pipeline incorporating the relief attribute evaluation feature seletion technique during
# data pre-processing. log_2(m) variables are chosen via Borda's method to be included in the final
# prediction model.

# These predictions were made using at week 9 of the course. The training corpus was the fall 2014,
# spring 2015, fall 2015, and spring 2016. The testing corpus was fall 2016 data.

Sys.setenv(JAVA_HOME='')
library(earth)
library(randomForest)
library(DMwR)
library(caret)
library(caretEnsemble)
library(pROC)
library(tictoc)
library(rJava)
library(XLConnectJars)
library(XLConnect)
library(openxlsx)

i = 1

tic()

# Names of files to store Borda tabulations
of="Week9_Both_RAE_Significant_GLMNET.xlsx"
of2="Week9_Both_RAE_Significant_GLMNET_Score.xlsx"

# Read in training and testing data
train_pred = read.csv("mice_d1148_1164_Week9_Both.csv")
test_pred = read.csv("mice_d1168_Week9_Both.csv")
train_class = read.csv("d1148_1164_class.csv")
test_class = read.csv("d1168_class.csv")

# Combine independent and dependent variable files
names(train_class) = "crse_grade_off_cat"
data_train = cbind(train_pred, crse_grade_off_cat = train_class)
names(test_class) = "crse_grade_off_cat"
data_test = cbind(test_pred, crse_grade_off_cat = test_class)

# Apply SMOTE
set.seed(2018)
data_train$crse_grade_off_cat <- factor(data_train$crse_grade_off_cat)
data_train_smote<- SMOTE(crse_grade_off_cat ~ ., data = data_train, perc.over = 500,perc.under=100)

data_all = rbind(data_train_smote, data_test)
data_all_dum = as.data.frame(model.matrix(crse_grade_off_cat ~ ., data = data_all))
crse_grade_off_ccat = data_all[,"crse_grade_off_cat"]
data_all_dum_ = cbind(data_all_dum, crse_grade_off_ccat)
levels(data_all_dum_$crse_grade_off_ccat) = c("F", "P")
EC_data_train = data_all_dum_[1:dim(data_train_smote)[1], ][,-1] #unbalanced class
EC_data_test = data_all_dum_[(dim(data_train_smote)[1]+1):dim(data_all_dum_)[1], ][,-1] #testing data should not be changed

# Cross-Validation

yourData <- EC_data_train[sample(nrow(EC_data_train)),]

# Create 10 equally size folds
folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)
listofdfs_model <- list()
listofdfs_train <- list() #Create a list in which you intend to save your df's.
listofdfs_validate <- list()

# Partition folds to obtain train and validation set to identify the 10 most significant predictors
# and their corresponding AUCs

for (j in 1:10) {
  testIndexes <- which(folds==j,arr.ind=TRUE)
  validationData <- yourData[testIndexes, ]
  trainData <- yourData[-testIndexes, ]
  listofdfs_train[[j]] <- trainData
  listofdfs_validate[[j]] <- validationData
}

# Apply the feature selection technique to each fold during ten-fold cross-validation

library("corrplot")
listofdfs_significant <- list() # Create a list in which you intend to save your dataframes
listofdfs_roc <- list()
listofdfs_model_glm <- list()

# Rename columns of newly created dataframes
borda_rank <- data.frame(colnames(EC_data_train)[1:length(EC_data_train)-1])
colnames(borda_rank) <- "predictor"
rownames(borda_rank) <- borda_rank$predictor

ranking <- data.frame(borda_rank$predictor)
rownames(ranking) <- borda_rank$predictor
ranking <- ranking[-c(1)]

# For each fold
for (k in 1:10){
  
  weights <- relief(crse_grade_off_ccat~.,listofdfs_train[[k]])
  print(k)
  
  # End Algorithm
  
  imp <- data.frame(overall = weights$attr_importance,
                    names   = rownames(weights))
  imp <- imp[order(abs(imp$overall),decreasing = T),]
  
  imp_all <- imp[1:nrow(imp),]
  imp_6 <- imp[1:6,]
  rownames(imp_6) <- imp_6$names
  
  a <- listofdfs_train[[k]][c(rownames(imp_6),"crse_grade_off_ccat")]
  
  listofdfs_significant[[k]] <- imp_all
  
}

# Read to excel file
sname_2<-paste("Worksheet",i,sep="")
ifelse(i==1,app<-"FALSE",app<-"TRUE")
write.xlsx(cbind(listofdfs_significant[[1]],listofdfs_significant[[2]],
                 listofdfs_significant[[3]],listofdfs_significant[[4]],
                 listofdfs_significant[[5]],listofdfs_significant[[6]],
                 listofdfs_significant[[7]],listofdfs_significant[[8]],
                 listofdfs_significant[[9]],listofdfs_significant[[10]]),
           of2, sheetName=sname_2,row.names = TRUE,append=as.logical(app))


sig_pred_glm <- cbind(as.character(listofdfs_significant[[1]]$names),
                      as.character(listofdfs_significant[[2]]$names),
                      as.character(listofdfs_significant[[3]]$names),
                      as.character(listofdfs_significant[[4]]$names),
                      as.character(listofdfs_significant[[5]]$names),
                      as.character(listofdfs_significant[[6]]$names),
                      as.character(listofdfs_significant[[7]]$names),
                      as.character(listofdfs_significant[[8]]$names),
                      as.character(listofdfs_significant[[9]]$names),
                      as.character(listofdfs_significant[[10]]$names))
library(dplyr)
library(openxlsx)
library(xlsx)

sig_pred <- as.data.frame(sig_pred_glm,stringsAsFactors=FALSE)


# For each ranking <= 7, assign the Borda count to be 7. This is because we are 
# only interested in the top six predictors identified by the algorithm

fold_1 <- data.frame(sig_pred[,1],1:nrow(sig_pred))
fold_1[,2][7:nrow(fold_1)] <- 7
rownames(fold_1) <- fold_1$sig_pred...1.
fold_1 <- fold_1[-c(1)]
colnames(fold_1) <- "Fold1"

fold_2 <- data.frame(sig_pred[,2],1:nrow(sig_pred))
fold_2[,2][7:nrow(fold_2)] <- 7
rownames(fold_2) <- fold_2$sig_pred...2.
fold_2 <- fold_2[-c(1)]
colnames(fold_2) <- "Fold2"

fold_3 <- data.frame(sig_pred[,3],1:nrow(sig_pred))
fold_3[,2][7:nrow(fold_3)] <- 7
rownames(fold_3) <- fold_3$sig_pred...3.
fold_3 <- fold_3[-c(1)]
colnames(fold_3) <- "Fold3"

fold_4 <- data.frame(sig_pred[,4],1:nrow(sig_pred))
fold_4[,2][7:nrow(fold_4)] <- 7
rownames(fold_4) <- fold_4$sig_pred...4.
fold_4 <- fold_4[-c(1)]
colnames(fold_4) <- "Fold4"

fold_5 <- data.frame(sig_pred[,5],1:nrow(sig_pred))
fold_5[,2][7:nrow(fold_5)] <- 7
rownames(fold_5) <- fold_5$sig_pred...5.
fold_5 <- fold_5[-c(1)]
colnames(fold_5) <- "Fold5"

fold_6 <- data.frame(sig_pred[,6],1:nrow(sig_pred))
fold_6[,2][7:nrow(fold_6)] <- 7
rownames(fold_6) <- fold_6$sig_pred...6.
fold_6 <- fold_6[-c(1)]
colnames(fold_6) <- "Fold6"

fold_7 <- data.frame(sig_pred[,7],1:nrow(sig_pred))
fold_7[,2][7:nrow(fold_7)] <- 7
rownames(fold_7) <- fold_7$sig_pred...7.
fold_7 <- fold_7[-c(1)]
colnames(fold_7) <- "Fold7"

fold_8 <- data.frame(sig_pred[,8],1:nrow(sig_pred))
fold_8[,2][7:nrow(fold_8)] <- 7
rownames(fold_8) <- fold_8$sig_pred...8.
fold_8 <- fold_8[-c(1)]
colnames(fold_8) <- "Fold8"

fold_9 <- data.frame(sig_pred[,9],1:nrow(sig_pred))
fold_9[,2][7:nrow(fold_9)] <- 7
rownames(fold_9) <- fold_9$sig_pred...9.
fold_9 <- fold_9[-c(1)]
colnames(fold_9) <- "Fold9"

fold_10 <- data.frame(sig_pred[,10],1:nrow(sig_pred))
fold_10[,2][7:nrow(fold_10)] <- 7
rownames(fold_10) <- fold_10$sig_pred...10.
fold_10 <- fold_10[-c(1)]
colnames(fold_10) <- "Fold10"

# Merge the columns together
folds_merged_1_2 <- merge(fold_1,fold_2,by="row.names",all.x=TRUE)
folds_merged_1_2 <- folds_merged_1_2[order(folds_merged_1_2$Row.names),]

folds_merged_3_4 <- merge(fold_3,fold_4,by="row.names",all.x=TRUE)
folds_merged_3_4 <- folds_merged_3_4[order(folds_merged_3_4$Row.names),]

folds_merged_5_6 <- merge(fold_5,fold_6,by="row.names",all.x=TRUE)
folds_merged_5_6 <- folds_merged_5_6[order(folds_merged_5_6$Row.names),]

folds_merged_7_8 <- merge(fold_7,fold_8,by="row.names",all.x=TRUE)
folds_merged_7_8 <- folds_merged_7_8[order(folds_merged_7_8$Row.names),]

folds_merged_9_10 <- merge(fold_9,fold_10,by="row.names",all.x=TRUE)
folds_merged_9_10 <- folds_merged_9_10[order(folds_merged_9_10$Row.names),]

# Compute the Borda ranking
fold_ranking <- cbind(folds_merged_1_2,folds_merged_3_4$Fold3,folds_merged_3_4$Fold4,
                      folds_merged_5_6$Fold5,folds_merged_5_6$Fold6,
                      folds_merged_7_8$Fold7,folds_merged_7_8$Fold8,
                      folds_merged_9_10$Fold9,folds_merged_9_10$Fold10)
rownames(fold_ranking) <- fold_ranking$Row.names
fold_ranking <- fold_ranking[-c(1)]
fold_ranking$summation <- rowSums(fold_ranking)

sname<-paste("Worksheet",i,sep="")
ifelse(i==1,app<-"FALSE",app<-"TRUE")
write.xlsx(fold_ranking, of, sheetName=sname,row.names = TRUE,append=as.logical(app))

# Keep top six predictors across all folds for the prediction
fold_ranking_final <- fold_ranking[-c(1:10)]
fold_ranking_final$predictor <- rownames(fold_ranking_final)
fold_ranking_final <- fold_ranking_final[order(fold_ranking_final$summation),]
top_ranking_scheme_mod <- fold_ranking_final[1:6,]
final_model <- as.data.frame(cbind(EC_data_train[top_ranking_scheme_mod$predictor][1:6],EC_data_train$crse_grade_off_ccat))

set.seed(949)

# Use caret to train the model (on the entire training corpus) and generate predictions on the testing data
ctrl = trainControl(method = "repeatedcv", repeats  = 1, classProbs = T, savePredictions = T, summaryFunction = twoClassSummary)

mymethods = c("glmnet") # Can be changed to any data mining method
print ("Running methods")
out = caretList(`EC_data_train$crse_grade_off_ccat`~., data = final_model, methodList = mymethods, trControl = ctrl, tuneLength = 6)

print ("Creating prediction accuracy for training and testing data")
model_preds_train = lapply(out, predict, newdata = final_model[, 1:(dim(final_model)[2] - 1)], type = "prob")
model_preds_train = lapply(model_preds_train, function(x)x[,"F"])
model_preds_train = as.data.frame(model_preds_train)[, -4]

# Apply model to testing data
model_preds_tst = lapply(out, predict, newdata = EC_data_test[, 1:(dim(EC_data_test)[2] - 1)], type = "prob")
model_preds_tst = lapply(model_preds_tst, function(x)x[,"F"])
model_preds_tst = as.data.frame(model_preds_tst)[,-4]
auc_test = caTools::colAUC(model_preds_tst, EC_data_test$crse_grade_off_ccat == "F", plotROC = T)

# Use Youden's index to select a cutpoint from the ROC curve and obtain other performance metrics
library(cutpointr)
cp <- cutpointr(model_preds_tst,EC_data_test$crse_grade_off_ccat, 
                method = maximize_metric, metric = youden,pos_class = "P")

# Store and compute a set of evaluation metrics
accu <- cp$acc
sensit <- cp$sensitivity
specif <- cp$specificity
bal_acc <- (cp$sensitivity+cp$specificity)/2
conf_ind <- ci(EC_data_test$crse_grade_off_ccat,model_preds_tst)
cut_point <- cp$optimal_cutpoint
youden_j <- cp$youden

model_preds_lab_test = ifelse(model_preds_tst > cp$optimal_cutpoint, "F", "P")

get_pre = function(x, y){ # Precision - adapted from Yaqi Xue
  sum(x == "P" & y == "P")/sum(x == "P")
}

precision <- get_pre(model_preds_lab_test,EC_data_test$crse_grade_off_ccat)

f1 <- (2*precision*sensit)/(precision+sensit)


get_tp = function(x,y){
  sum(x == "P" & y == "P")
}

tp <- get_tp(model_preds_lab_test,EC_data_test$crse_grade_off_ccat)


get_tn = function(x,y){
  sum(x == "F" & y == "F")
}

tn <- get_tn(model_preds_lab_test,EC_data_test$crse_grade_off_ccat)


get_fn = function(x,y){
  sum(x == "F" & y == "P")
}

fn <- get_fn(model_preds_lab_test,EC_data_test$crse_grade_off_ccat)

get_fp = function(x,y){
  sum(x == "P" & y == "F")
}

fp <- get_fp(model_preds_lab_test,EC_data_test$crse_grade_off_ccat) 
  
print('H')
toc()

# Write results to csv files
write.csv(auc_test,'AUC_GLMNET_Week9_Both_RAE.csv')
write.csv(accu,'ACCURACY_GLMNET_Week9_Both_RAE.csv')
write.csv(sensit,'SENSIT_GLMNET_Week9_Both_RAE.csv')
write.csv(specif,'SPECIF_GLMNET_Week9_Both_RAE.csv')
write.csv(bal_acc,'BALACC_GLMNET_Week9_Both_RAE.csv')
write.csv(cut_point,'CP_GLMNET_Week9_Both_RAE.csv')
write.csv(youden_j,'YOUDEN_GLMNET_Week9_Both_RAE.csv')
write.csv(f1,'F1_GLMNET_Week9_Both_RAE.csv')
write.csv(tp,'TP_GLMNET_Week9_Both_RAE.csv')
write.csv(tn,'TN_GLMNET_Week9_Both_RAE.csv')
write.csv(fn,'FN_GLMNET_Week9_Both_RAE.csv')
write.csv(fp,'FP_GLMNET_Week9_Both_RAE.csv')