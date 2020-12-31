# Biology-Course-Data-Pipeline

Data pipeline for predicting student performance in the collegiate biology classroom.

The pipeline encompasses the following steps (note that steps 1-5 have already been performed in the read in data files). 

1- University-specific data is combined with course-specific data

2- The amalgamated corpus is divided into training and testing sets

3- Categorical predictors are converted into indicator variables

4- Missing data are imputed using the predictive mean matching regression technique in R’s MICE (Multivariate Imputation via Chained Equations) package. This is a common imputation library used in education research. 

5- Each independent variable is transformed to a z-score, so all predictors follow the same distribution with mean zero and variance one

6- SMOTE (Synthetic Minority Oversampling Technique), an oversampling technique, is applied to balance the number of passing and failing students in the training corpus by generating synthetic data records of students who failed the course.

7- The model is trained using ten-fold cross-validation and applied to the testing data corpus (Kohavi 1995). When the four pre-processing filter feature selection techniques are applied, the independent variables are ranked across all ten folds using Borda’s method. We apply the cutoff by Khoshgoftaar et al. (2007, 2010) to select the top 6 relevant independent predictors for the final model.

8- The model’s performance on the testing set is evaluated using the AUC metric for five DMMs (logistic regression, elastic net regresison, random forest, extreme gradient boosting, and neural network) and four pre-processing filter feature selection techniques (correlation attribute evaluation CAE; Fisher's scoring algorithm FSA, information gain attribute evaluation IG, and relief attribute evaluation RAE). In addition to obtaining point estimates for the AUC when pre-processing feature selection techniques are applied and when they are omitted from the data pipeline.
