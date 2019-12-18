#-------------------------------------------------------------------------------
# Roadmap Positive Health
# vulnerable citizens
# script 3/4
# Train model
# author : Mark Henry Gremmen, in cooperation with Gemma Smulders (GGD HvB), Ester de Jonge (GGD ZHZ)
# DataScienceHub @ JADS, GGD Hart voor Brabant, GGD Zuid-Holland Zuid
# lud 2019-12-18
#-------------------------------------------------------------------------------

#clear environment
rm(list=ls())

#clear console
cat("\014")  

#packages
packages <- c("tools","here","tidyverse","caTools", "psych", "RColorBrewer", "xgboost",
              "ggplot2","ggfortify","tidyr", "nnet", "caret","randomForest", "gbm", "car",
              "stringr")
#install packages which are not available
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE)
#review
#sessionInfo()


#-------------------------------------------------------------------------------
# Global settings (change if/where needed)

#root location of this procedure (working directory)
root <- getwd()
root

#options
set.seed(123)  # for reproducibility
options(digits=3)

#set graphs location 
plots.loc <- paste0(root,'/PLOTS/')

#set data location 
data.loc <- paste0(root,'/DATA/')

#source file name (csv-format)
#make sure the filename corresponds with the paramaters used in procedure '01 DIMENSION'
source.file <- paste0(data.loc,'Xfinal-zorgwekkend_df25-k7-p35.csv')

SOURCE <- read.csv(source.file, T, ",")

str(SOURCE)

#set cluster membership of non-vulnerbale observations to zero 
SOURCE$cl_pam[is.na(SOURCE$cl_pam)] <- 0

head(SOURCE)

#original variables (not recoded, different likert scales) from the GGD Gezondheidsmonitor incl cluster membership cl_pam. 
#cluster membership must reside at the last position if the list 
#cols <- c("GGADS201","KLGGB201","MMIKB201","CALGA260","CALGA261","LGBPS205","GGEEB201","GGEEB203",
#          "GGEEB204","GGEEB207","GGEEB208","GGRLB201","GGRLB202","GGRLB204","GGRLB206","GGADB201",
#          "GGADB202","GGADB204","GGADB207","GGADB210","cl_pam") 

cols <- c("GGEES203","GGRLS202","GGADS201","KLGGA207", # <- outcome level
          "MMIKB201","CALGA260","CALGA261","LGBPS209","AGGWS205","GGEEB201","GGEEB203","GGEEB204","GGEEB207",
          "GGEEB208","GGRLB201","GGRLB202","GGRLB204","GGRLB206","GGADB201","GGADB202","GGADB204",
          "GGADB207","GGADB210","GGEEB210","GGEEB211", "MCMZOS304","cl_pam") 

SOURCE_SUBSET <- subset(SOURCE_ENRICHED, select = cols)

SOURCE_SUBSET <- subset(SOURCE, select = cols)
dim(SOURCE_SUBSET)

#number of features
df_tr <- ncol(SOURCE_SUBSET)
df_tr

#use only complete records (with cl_pam membership) for modelling
data <- na.omit(SOURCE_SUBSET)

# spliting the data into train and test
#SplitRatio : 60%:40% 
inTrain= sample.split(data,SplitRatio = 0.5)

#subsetting into train data
train =subset(data,inTrain==TRUE)

#subsetting into test data
test =subset(data,inTrain==FALSE)

#latent dimensions from PCA are used to train a classification model (to reduce noise)

# important scale data TRUE 
pca = prcomp(train[,-df_tr], scale=TRUE, center=TRUE)

#biplot
biplot(pca, cex=.7)
	
#screeplot	
screeplot(pca, type="line", main="Scree plot")
abline(h=1)

# checking attributes available PCA
attributes(pca)
pca$center

# analysing the PCA model
print(pca)
summary(pca)

#autoplot(prcomp(train), loadings= T,
#         loadings.colour = "blue", loadings.label = T, loadings.label.size=3,
#         frame= T)

# predicting training data with pca
pca.train <- predict(pca, train)
pca.train <- as.data.frame(pca.train)

# adding our target variable which was not included during the PCA training
# NSP is cluster membership
pca.train <- cbind(pca.train, NSP= train[,"cl_pam"])

#table(observed=, predicted=)

# predicting test data with pca
pca.test <- predict(pca, test)
pca.test <- as.data.frame(pca.test)

pca.test <- cbind(pca.test, NSP= test[,"cl_pam"])

#model with multinomial logistic regression

# changing NSP variable into factor variable for our model
pca.train$NSP <- as.factor(pca.train$NSP)
pca.test$NSP <- as.factor(pca.test$NSP)

pca.train$NSP <- relevel(pca.train$NSP, ref = "1")

# selecting first 10 components, together they are responsible for more than 85% of the variability of the data
# the number of dimensions effects the accuracy rate of the model.
# more dimensions is not always better
# the PCA-method and the selective approach in the number of dimensions is a way to get lose noise

#multinomial logistic regression
model_mnlr <- multinom(NSP ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                  data = pca.train)


#randomForest
model_rf <- randomForest(NSP ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                       data = pca.train)

#write prediction models
#multinomial logistic regression
saveRDS(model_mnlr , "model_mnlr_ses.RDS")
#randomForest (prefered for SES project)
saveRDS(model_rf , "model_rf_ses.RDS")

#variable importance plot RandonmForest for predictors
varImpPlot(model_rf, n.var = 10, cex=.6)

# prediction with the multinomial logistic regression
#train dataset
#pred.train <- predict(model_mnlr, newdata = pca.train)
#confusionMatrix(pred.train, pca.train$NSP)

#test dataset
pred.test.mnlr <- predict(model_mnlr, newdata = pca.test)
confusionMatrix(pred.test.mnlr, pca.test$NSP)

# prediction with the randomForest 
#pred.trainR <- predict(model_rf, newdata = pca.train)
#confusionMatrix(pred.trainR, pca.train$NSP)

pred.test.rf <- predict(model_rf, newdata = pca.test)
confusionMatrix(pred.test.rf, pca.test$NSP)

#accuracy rate in the confusionmatrices indicate that randomForest is 
#the best prediction model
