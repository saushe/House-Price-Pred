library(RED)
library(caret)
library(gam)
library(glmnet)

`test.(1)` <- read.csv("C:/Users/Ayasha/Downloads/test1.csv")
test_df<- `test.(1)` 
rm(`test.(1)` )

`train.(1)` <- read.csv("C:/Users/Ayasha/Downloads/train (1).csv", stringsAsFactors=FALSE)
hd<- `train.(1)`
rm(`train.(1)`)

##################### Train and Test Missing Value Treatment
# train ROW treatment 
Row_missing<- row_missing_count(hd)
# 7 rows have more than 15% missing values
# Dedlete those rows

hd$row_missing<- apply(hd, 1, function(x){length(which(is.na(x)==T))})
hd1<- hd[-which(hd$row_missing>10), ]
hd1<- select(hd1, - row_missing)

# Merge test ad train and perform missing treatment together
test_df$SalePrice<- 1
test_df$type<- "test"
hd1$type<- "train"

Overall_data<- rbind(hd1,test_df)
# There are certain rows which have been wrongly classified numeric. Change class of those rows
Overall_data$MSSubClass<- as.character(Overall_data$MSSubClass)
Overall_data$OverallQual<- as.character(Overall_data$OverallQual)
Overall_data$OverallCond<- as.character(Overall_data$OverallCond)
Overall_data$YearBuilt<- 2016 - Overall_data$YearBuilt
Overall_data$YearRemodAdd<- 2016 - Overall_data$YearRemodAdd
Overall_data$GarageYrBlt<- 2016 - Overall_data$GarageYrBlt
Overall_data$YrSold<- 2016 - Overall_data$YrSold

# Misisng Value Summary
missing<- col_missing_count(Overall_data)

# Five variables have more than 40% missing values. I will drop them

# Lets look at the univariate to decide how we will treat missng values
univchar<-univariate_charvar(Overall_data)
univnum<- univariate_numvar(Overall_data)

################## misisng value for numeric

# for yr. garage built
length(which(Overall_data$GarageYrBlt==Overall_data$YearBuilt)) # most of the values re common so I will replace the missing with year built
Overall_data$GarageYrBlt<- ifelse(is.na(Overall_data$GarageYrBlt)==T,hd1$YearBuilt,hd1$GarageYrBlt)

Overall_data<- mv_treatment_numvar(Overall_data, col.del_cutoff = 0.4, treatment_type = mean)

################# missing values for char vars
# Univariate summary
univchar<-univariate_charvar(Overall_data)
univnum<- univariate_numvar(Overall_data)
univchar$`34_GarageType` # replace missing with unknown
univchar$`36_GarageQual` # replace missing with mode 
univchar$`35_GarageFinish` # replace missing with uknown

univchar$`24_BsmtExposure`# replace with mode
univchar$`22_BsmtQual` # replace with unknown

# missing vaue treatment for categorical
Overall_data<- mv_treatment_charvar(Overall_data, default = F, col.del_cutoff = 0.4,
                           char_var_list1 = c("GarageType", "GarageCond", "GarageFinish", "BsmtQual","MasVnrType", "BsmtFinType1", "BsmtFinType2"), 
                           char_var_list2 = c("GarageQual", "BsmtQual", "BsmtExposure", "Electrical" ))
missing_count<- col_missing_count(Overall_data)

Overall_data<- mv_treatment_charvar(Overall_data, default = F, char_var_list1 = c("BsmtCond" ))

missing_count<- col_missing_count(Overall_data)

Overall_data<- mv_treatment_charvar(Overall_data, default = F, char_var_list2 = c("MSZoning","Utilities", "Functional", "Exterior1st", "Exterior2nd", "KitchenQual", "SaleType" ))
missing_count<- col_missing_count(Overall_data)

####################Variable Transformations#############

# Treatment type
#Cutoff_0.05 <- SaleCondition, RoofMatl,RoofStyle, BldgType, HouseStyle, Condition2,Condition1,Neighborhood,LotConfig,LandSlope, Functional, Heating, Foundation, MasVnrType, Exterior2nd, Exterior1st
#Cutoff_0.06 <- PavedDrive, GarageType,LotShap
#Cutoff 0.02 <-  SaleType
#Cutoff 0.03 <- BsmtFinType2

Overall_data<- replace_charvars(Overall_data, c("SaleCondition","HeatingQC", "RoofMatl","RoofStyle", "BldgType"))

Overall_data<- replace_charvars(Overall_data, c("HouseStyle", "Condition2","Condition1"))

Overall_data<- replace_charvars(Overall_data, c("LotConfig" ))

Overall_data<- replace_charvars(Overall_data, c("Heating", "Foundation", "MasVnrType", "Exterior2nd", "Exterior1st"))

Overall_data<- replace_charvars(Overall_data, c("LandSlope","Functional", "MSZoning"))

Overall_data<- replace_charvars(Overall_data, c( "GarageType","LotShape"), cutoff = 0.06)
Overall_data<- replace_charvars(Overall_data, c("Neighborhood"), 0.01)
Overall_data<- replace_charvars(Overall_data, c("SaleType"), 0.02)
Overall_data<- replace_charvars(Overall_data,c("BsmtFinType2", "BsmtCond", "BsmtQual"), 0.03 )

Overall_data$GarageCond<- ifelse(Overall_data$GarageCond =="Ex", "Gd", 
                        ifelse(Overall_data$GarageCond== "Unknown", "Po", Overall_data$GarageCond))

Overall_data$GarageQual<- ifelse(Overall_data$GarageQual == "Ex", "Gd",Overall_data$GarageQual )

Overall_data$ExterCond<- ifelse(Overall_data$ExterCond == "Ex", "Gd", ifelse(Overall_data$ExterCond == "Po", "Fa",Overall_data$ExterCond ))
Overall_data$OverallQual<- ifelse(Overall_data$OverallQual == "2", "4", ifelse(Overall_data$OverallQual == "3", "4", ifelse(Overall_data$OverallQual == "10", "9",Overall_data$OverallQual)))

Overall_data<- replace_charvars(Overall_data,c("ExterQual"), option = "rep_max", 0.02)


################### More variable transformation on numeric ars ##################

Overall_data$LotFrontage[which(Overall_data$LotFrontage>160)]<- mean(Overall_data$LotFrontage)
Overall_data$LotArea[which(Overall_data$LotArea>40000)]<- mean(Overall_data$LotArea)
# Remove BsmtFinSF2
Overall_data<- select(Overall_data, -(BsmtFinSF2))
Overall_data<- select(Overall_data, -(LowQualFinSF))
# Chnage BsmnFUll Bath, BSmntHalfBath, Ful Bath, Half Bath as categorical
Overall_data$BsmtFullBath[which(Overall_data$BsmtFullBath==3)]<- 2
Overall_data$BsmtFullBath<- as.character(Overall_data$BsmtFullBath)

Overall_data$BsmtHalfBath[which(Overall_data$BsmtHalfBath==2)]<- 1
Overall_data$BsmtHalfBath<- as.character(Overall_data$BsmtHalfBath)

Overall_data$BedroomAbvGr[which(Overall_data$BedroomAbvGr==8)]<- 6
Overall_data$KitchenAbvGr[which(Overall_data$KitchenAbvGr==0)]<- 1
Overall_data$KitchenAbvGr[which(Overall_data$KitchenAbvGr==3)]<- 2

Overall_data$GarageYrBlt[which(Overall_data$GarageYrBlt<1905)]<- 1910
Overall_data$GarageArea[which(Overall_data$GarageArea>1200)]<- mean(Overall_data$GarageArea)

Overall_data$YrSold<- as.character(Overall_data$YrSold)

################ Remove some the variables with zero variations

Overall_data2<- select(Overall_data, - c( Electrical, Utilities, Street, GarageCond,PoolArea,MiscVal))

# Final Missing check
missing_count<- col_missing_count(Overall_data2)

#################### Separate test and trian
train1<- Overall_data2[which(Overall_data2$type=="train"),]
train1<- select(train1, -c(type))
test1<- Overall_data2[which(Overall_data2$type=="test"),]
test1<- select(test1, -c(type, SalePrice))
test1$BsmtFullBath[which(test1$BsmtFullBath== "0.43162245952463")]<- "2"
test1$OverallQual[which(test1$OverallQual== "1")]<- "4"
test1$BsmtHalfBath[which(test1$BsmtHalfBath== "0.0616603513606614")]<- "1"
test1$OverallCond[which(test1$OverallCond== "1")]<- "2"
test1$MSSubClass[ which(test1$MSSubClass=="150")]<- "20"

train1<- train1[-c(1171,519,1286),]

write.csv(train1, "train1.csv")
write.csv(test1, "test1.csv")


################################# Running models ##################################

#======================= Run LM =======================

RSS<- (NA)
for ( i in 1:10)
{
  x<- createDataPartition(train1$SalePrice, p= .90, list = F)
  x<- as.vector(x)
  train<- train1[x,]
  test<- train1[-x,]
  fit<- lm(SalePrice~., data = train)
  
  pred<- predict(fit, test)
  test$Predicted<- pred
  RSS[i]<- sqrt(sum((log(test$SalePrice) - log(test$Predicted))^2) / nrow(test))
}
# Mean RSS = .141
# Min RSS = .128


# Run Splines
#===================================== GAM Spline
RSS<- (NA)
fitgam<- list()
for ( i in 1:10)
{
  x<- createDataPartition(train1$SalePrice, p= .95, list = F)
  x<- as.vector(x)
  train<- train1[x,]
  test<- train1[-x,]
  fit<- gam(SalePrice~ +MSSubClass	+ns(LotArea, df = 5)	+LandSlope	+BldgType	+ns(YearBuilt, df = 5)	+Exterior1st	+ExterQual	+BsmtCond	+BsmtFinType2	+HeatingQC	+ns(GrLivArea, df = 5)	+ns(HalfBath, df = 5)	+ns(TotRmsAbvGrd, df = 5)	+GarageQual	+ns(EnclosedPorch, df = 5)	+YrSold	+LotConfig	+Condition2	+MSZoning	+LotShape	+Neighborhood	+HouseStyle	+ns(YearRemodAdd, df = 5)	+Exterior2nd	+ExterCond	+BsmtExposure	+ns(BsmtUnfSF, df = 5)	+CentralAir	+BsmtFullBath	+ns(BedroomAbvGr, df = 5)	+Functional	+GarageFinish	+PavedDrive	+ns(X3SsnPorch, df = 5)	+SaleType	+ns(LotFrontage, df = 5)	+LandContour	+Condition1	+OverallQual	+RoofStyle	+MasVnrType	+Foundation	+BsmtFinType1	+ns(TotalBsmtSF, df = 5)	+ns(X1stFlrSF, df = 5)	+BsmtHalfBath	+ns(KitchenAbvGr, df = 5)	+ns(Fireplaces, df = 5)	+ns(GarageCars, df = 5)	+ns(WoodDeckSF, df = 5)	+ns(ScreenPorch, df = 5)	+SaleCondition	+OverallCond	+RoofMatl	+ns(MasVnrArea, df = 5)	+BsmtQual	+ns(BsmtFinSF1, df = 5)	+Heating	+ns(X2ndFlrSF, df = 5)	+ns(FullBath, df = 5)	+KitchenQual	+GarageType	+ns(GarageArea, df = 5)	+ns(OpenPorchSF, df = 5)	+ns(MoSold, df = 5)
            , data = train)
  # assign(paste0("fit", i), fit)
  fitgam[[i]] = fit
  pred<- predict(fit, test)
  test$Predicted<- pred
  RSS[i]<- sqrt(sum((log(test$SalePrice) - log(test$Predicted))^2) / nrow(test))
}
mean(RSS)
min(RSS)

# Mean RSS = 0.1179052
# Min RSS = 0.09554867
BestGam = fitgam[[3]]

#========================================= Local Regression
for ( i in 1:10)
{
  x<- createDataPartition(train1$SalePrice, p= .90, list = F)
  x<- as.vector(x)
  train<- train1[x,]
  test<- train1[-x,]
  gam_model2<- gam(SalePrice~ +lo(LotFrontage,	LotArea,	YearBuilt,	YearRemodAdd,	MasVnrArea,	BsmtFinSF1,	BsmtUnfSF,	TotalBsmtSF,	X1stFlrSF,	GrLivArea,
                                  GarageArea,MoSold,span = 0.8)     	+MSZoning	+LotShape	+LandContour	+LotConfig	+LandSlope	+Neighborhood	+Condition1	+Condition2	+BldgType	+HouseStyle	+OverallQual	+OverallCond	+RoofStyle	+RoofMatl	+Exterior1st	+Exterior2nd	+MasVnrType	+ExterQual	+ExterCond	+Foundation	+BsmtQual	+BsmtCond	+BsmtExposure	+BsmtFinType1	+BsmtFinType2	+Heating	+HeatingQC	+CentralAir	+BsmtFullBath	+BsmtHalfBath	+KitchenQual	+Functional	+GarageType	+GarageFinish	+GarageQual	+PavedDrive	+YrSold	+SaleType	+SaleCondition
                   , data = train)
  
  pred<- predict(fit, test)
  test$Predicted<- pred
  RSS[i]<- sqrt(sum((log(test$SalePrice) - log(test$Predicted))^2) / nrow(test))
}

# Trying out weighted least squares
# 10 fold cross validation
RSS<- (NA)
for ( i in 1:20)
{
  x<- createDataPartition(train1$SalePrice, p= .95, list = F)
  x<- as.vector(x)
  train<- train1[x,]
  test<- train1[-x,]
  fit<- lm(SalePrice~. ,data = train)
  resids<- residuals(fit)
  fit2<- lm(SalePrice~. , weights = 1/abs(resids),data = train)
  pred<- predict(fit2, test)
  test$Predicted<- pred
  RSS[i]<- sqrt(sum((log(test$SalePrice) - log(test$Predicted))^2) / nrow(test))
}
min(RSS)
#[1] 0.09981109
mean(RSS)
#[1] 0.128748

####### Final Models

fit_gam1<- gam(SalePrice~ +MSSubClass	+bs(LotArea, df = 5)	+LandSlope	+BldgType	+ns(YearBuilt, df = 5)	+Exterior1st	+ExterQual	+BsmtCond	+BsmtFinType2	+HeatingQC	+GrLivArea	+HalfBath	+TotRmsAbvGrd	+GarageQual	+EnclosedPorch	+YrSold	+LotConfig	+Condition2	+MSZoning	+LotShape	+Neighborhood	+HouseStyle	+ns(YearRemodAdd, df = 5)	+Exterior2nd	+ExterCond	+BsmtExposure	+s(BsmtUnfSF,df=5)	+CentralAir	+BsmtFullBath	+BedroomAbvGr	+Functional	+GarageFinish	+PavedDrive	+X3SsnPorch	+SaleType	+ns(LotFrontage,knots = c(39,58,100))	+LandContour	+Condition1	+OverallQual	+RoofStyle	+MasVnrType	+Foundation	+BsmtFinType1	+TotalBsmtSF	+X1stFlrSF	+BsmtHalfBath	+KitchenAbvGr	+Fireplaces	+GarageCars	+s(WoodDeckSF, df = 5)	+ScreenPorch	+SaleCondition	+OverallCond	+RoofMatl	+MasVnrArea	+BsmtQual	+ns(BsmtFinSF1, df=5)	+Heating	+bs(X2ndFlrSF,knots = c(10))	+FullBath	+KitchenQual	+GarageType	+ns(GarageArea, knots = c(50))	+s(OpenPorchSF, df = 5)	+MoSold, data = train1)

# Wiyhout MBSubClass and modefied LotArea
fit_gam2<- gam(SalePrice~ +MSSubClass	+ns(LotArea, df = 5)	+LandSlope	+BldgType	+ns(YearBuilt, df = 5)	+Exterior1st	+ExterQual	+BsmtCond	+BsmtFinType2	+HeatingQC	+ns(GrLivArea, df = 5)	+ns(HalfBath, df = 5)	+ns(TotRmsAbvGrd, df = 5)	+GarageQual	+ns(EnclosedPorch, df = 5)	+YrSold	+LotConfig	+Condition2	+MSZoning	+LotShape	+Neighborhood	+HouseStyle	+ns(YearRemodAdd, df = 5)	+Exterior2nd	+ExterCond	+BsmtExposure	+ns(BsmtUnfSF, df = 5)	+CentralAir	+BsmtFullBath	+ns(BedroomAbvGr, df = 5)	+Functional	+GarageFinish	+PavedDrive	+ns(X3SsnPorch, df = 5)	+SaleType	+ns(LotFrontage, df = 5)	+LandContour	+Condition1	+OverallQual	+RoofStyle	+MasVnrType	+Foundation	+BsmtFinType1	+ns(TotalBsmtSF, df = 5)	+ns(X1stFlrSF, df = 5)	+BsmtHalfBath	+ns(KitchenAbvGr, df = 5)	+ns(Fireplaces, df = 5)	+ns(GarageCars, df = 5)	+ns(WoodDeckSF, df = 5)	+ns(ScreenPorch, df = 5)	+SaleCondition	+OverallCond	+RoofMatl	+ns(MasVnrArea, df = 5)	+BsmtQual	+ns(BsmtFinSF1, df = 5)	+Heating	+ns(X2ndFlrSF, df = 5)	+ns(FullBath, df = 5)	+KitchenQual	+GarageType	+ns(GarageArea, df = 5)	+ns(OpenPorchSF, df = 5)	+ns(MoSold, df = 5)
, data = train1)



##### Fit WLS
test_fit<- lm(SalePrice~. - MSSubClass,data = train1)
resids<- residuals(test_fit)
test_fitWLS<- lm(SalePrice~. , weights = 1/abs(resids),data = train1)

##### Try Bagged GAM Splines
BGAMS1<- function(df, traindf,Iterations, SampleSize)
{
bagged<- data.frame(dummy_col = c(1:nrow(traindf)))

for ( i in 1:Iterations)
{
  x<- createDataPartition(df$SalePrice, p= SampleSize, list = F)
  x<- as.vector(x)
  train<- df[x,]
  test<- df[-x,]p
  fit<- gam(SalePrice~ +MSSubClass	+ns(LotArea, df = 5)	+LandSlope	+BldgType	+ns(YearBuilt, df = 5)	+Exterior1st	+ExterQual	+BsmtCond	+BsmtFinType2	+HeatingQC	+ns(GrLivArea, df = 5)	+ns(HalfBath, df = 5)	+ns(TotRmsAbvGrd, df = 5)	+GarageQual	+ns(EnclosedPorch, df = 5)	+YrSold	+LotConfig	+Condition2	+MSZoning	+LotShape	+Neighborhood	+HouseStyle	+ns(YearRemodAdd, df = 5)	+Exterior2nd	+ExterCond	+BsmtExposure	+ns(BsmtUnfSF, df = 5)	+CentralAir	+BsmtFullBath	+ns(BedroomAbvGr, df = 5)	+Functional	+GarageFinish	+PavedDrive	+ns(X3SsnPorch, df = 5)	+SaleType	+ns(LotFrontage, df = 5)	+LandContour	+Condition1	+OverallQual	+RoofStyle	+MasVnrType	+Foundation	+BsmtFinType1	+ns(TotalBsmtSF, df = 5)	+ns(X1stFlrSF, df = 5)	+BsmtHalfBath	+ns(KitchenAbvGr, df = 5)	+ns(Fireplaces, df = 5)	+ns(GarageCars, df = 5)	+ns(WoodDeckSF, df = 5)	+ns(ScreenPorch, df = 5)	+SaleCondition	+OverallCond	+RoofMatl	+ns(MasVnrArea, df = 5)	+BsmtQual	+ns(BsmtFinSF1, df = 5)	+Heating	+ns(X2ndFlrSF, df = 5)	+ns(FullBath, df = 5)	+KitchenQual	+GarageType	+ns(GarageArea, df = 5)	+ns(OpenPorchSF, df = 5)	+ns(MoSold, df = 5)
            , data = df)

  # OOBpredict(fit, traindf)
  pred<- predict(fit, traindf)
  bagged<- cbind(bagged, pred)
}
return(bagged)
}


######Trying Lasso

x <- model.matrix(SalePrice~., train1[,-1])[,-1]
y <- train1$SalePrice

train = sample (1: nrow(x), nrow(x)/1.1)
test = (1:nrow(x))[-train]
y.test =y[ test]

set.seed (1)
cv.out =cv.glmnet (x, y, alpha =1)
plot(cv.out)
bestlam= cv.out$lambda.min

grid = 10^ seq (10,-2, length =100)
lasso.mod = glmnet (x, y, alpha =1, lambda =grid)
plot(lasso.mod)
lasso.pred = predict (lasso.mod ,s=bestlam , newx=x[test ,])
sqrt(mean(( lasso.pred - y.test)^2))
sqrt(sum(log(lasso.pred) - log(y.test))^2 / length(y.test))

lasso.pred0 = predict(lasso.mod, s = 0, newx = x[test,], exact = T)
sqrt(mean(( lasso.pred0 - y.test)^2))
sqrt(sum(log(lasso.pred0) - log(y.test))^2 / length(y.test))

# MY CV function for lasso

x <- model.matrix(SalePrice~., train1[,-1])[,-1]
y <- train1$SalePrice

RSS_best<-(NA)
RSS0<- (NA)
lam<- (NA)
LassoModel<- list()
for ( i in 1:20)
{
  train = sample (1: nrow(x), nrow(x)/1.1)
  test = (1:nrow(x))[-train]
  y.test =y[ test]
  
  cv.out =cv.glmnet (x[train,], y[train], alpha =1)
  bestlam= cv.out$lambda.min
  lam[i]<-bestlam
  lasso.mod = glmnet (x[train,], y[train], alpha =1, lambda =grid)
  LassoModel[[i]]<- lasso.mod
  
  lasso.pred_best = predict (lasso.mod ,s=bestlam , newx=x[test ,])
  lasso.pred_0 = predict (lasso.mod ,s=0 , newx=x[test ,],exact = T)

  RSS_best[i]<- sqrt(sum((log(lasso.pred_best) - log(y.test))^2) / length(y.test))
  RSS0[i]<- sqrt(sum((log(lasso.pred_0) - log(y.test))^2) / length(y.test))
  
}
# Best Lasso
min(RSS_best)
mean(RSS_best)

Alam = lam[which(RSS_best== min(RSS_best))]
Bmodel = LassoModel[[which(RSS_best== min(RSS_best))]]

Best_lam_lasso = Alam
BestLasso<- Bmodel

#min(RSS_best) = 0.1055898
#mean(RSS_best) =  0.1254462


## Try Ridge
x <- model.matrix(SalePrice~., train1[,-1])[,-1]
y <- train1$SalePrice

RSS_best<-(NA)
RSS0<- (NA)
lam<- (NA)
RidgeModels<- list()
for ( i in 1:30)
{
  train = sample (1: nrow(x), nrow(x)/1.1)
  test = (1:nrow(x))[-train]
  y.test =y[ test]
  
  cv.out =cv.glmnet (x[train,], y[train], alpha =0)
  bestlam= cv.out$lambda.min
  lam[i]<-bestlam
  
  lasso.mod = glmnet (x[train,], y[train], alpha =0, lambda =grid)
  RidgeModels[[i]]<- lasso.mod
  lasso.pred_best = predict (lasso.mod ,s=bestlam , newx=x[test ,])
  lasso.pred_0 = predict (lasso.mod ,s=0 , newx=x[test ,],exact = T)
  
  RSS_best[i]<- sqrt(sum((log(lasso.pred_best) - log(y.test))^2) / length(y.test))
  RSS0[i]<- sqrt(sum((log(lasso.pred_0) - log(y.test))^2) / length(y.test))
  
}
# Best lambda = 6172.072
min(RSS_best)
mean(RSS_best)

Alam = lam[which(RSS_best== min(RSS_best))]
Bmodel = RidgeModels[[which(RSS_best== min(RSS_best))]]

Best_lam_ridge = Alam
BestRidge<- Bmodel

# Try transformation to  improve results of Ridge and lasso

numericPred<- which(summarise_all(train1,class)=="numeric")
numericPred<- train1[,numericPred]

Output<- data.frame(dummy_var = c(1:27), deg1 = NA, deg2 = NA, deg3 = NA, degSqrt = NA, logval = NA)
Output$Var<- colnames(numericPred)[-28]

dep_var<- train1$SalePrice
for (p in 1:28)
{
  indepvar = numericPred[p]
  for (i in 1:4)
  {
    if (i < 4)
    {
    indepvar1 = indepvar^i
    fit<- lm(dep_var~indepvar1)
    }
    if( i == 4)
    {
      indepvar1 = (indepvar)^1/2
      fit<- lm(dep_var~indepvar1)
    }

    Output[p,i+1]<- mean(residuals(fit)^2)
  }
}

write.csv(Output, "BestDegree.csv") 

cube<- function(x){x = x^3
return(x)}
square<- function(x){x = x^2
return(x)}

test2<- test1
test2$SalePrice = NA
new_data<- rbind(train1, test2)
new_data<- mutate_at(new_data, vars(TotalBsmtSF,X1stFlrSF, 
                                 FullBath, GarageCars, GarageArea, ScreenPorch, MoSold),square)

new_data<- mutate_at(new_data, vars(BsmtFinSF1,BsmtUnfSF,X2ndFlrSF,GarageYrBlt),cube)


train2<- new_data[which(is.na(new_data$SalePrice)==F),]
test2<- new_data[which(is.na(new_data$SalePrice)==T),]

x <- model.matrix(SalePrice~., train2[,-1])[,-1]
y <- train2$SalePrice


#lasso
RSS_best<-(NA)
RSS0<- (NA)
lam<- (NA)
LassoModel<- list()
for ( i in 1:20)
{
  train = sample (1: nrow(x), nrow(x)/1.1)
  test = (1:nrow(x))[-train]
  y.test =y[ test]
  cv.out =cv.glmnet (x[train,], y[train], alpha =1)
  bestlam= cv.out$lambda.min
  lam[i]<-bestlam
  lasso.mod = glmnet (x[train,], y[train], alpha =1, lambda =grid)
  LassoModel[[i]]<- lasso.mod
  lasso.pred_best = predict (lasso.mod ,s=bestlam , newx=x[test ,])
  lasso.pred_0 = predict (lasso.mod ,s=0 , newx=x[test ,],exact = T)
  RSS_best[i]<- sqrt(sum((log(lasso.pred_best) - log(y.test))^2) / length(y.test))
  RSS0[i]<- sqrt(sum((log(lasso.pred_0) - log(y.test))^2) / length(y.test))
}
# Best Lasso
min(RSS_best)
mean(RSS_best)
Alam = lam[which(RSS_best== min(RSS_best))]
Bmodel = LassoModel[[which(RSS_best== min(RSS_best))]]

Best_lam_lasso2 = Alam
BestLasso2<- Bmodel


# Ridge Transformed
x <- model.matrix(SalePrice~., train2[,-1])[,-1]
y <- train2$SalePrice
RSS_best<-(NA)
RSS0<- (NA)
lam<- (NA)
RidgeModels<- list()
for ( i in 1:20)
{
  train = sample (1: nrow(x), nrow(x)/1.1)
  test = (1:nrow(x))[-train]
  y.test =y[ test]
  cv.out =cv.glmnet (x[train,], y[train], alpha =0)
  bestlam= cv.out$lambda.min
  lam[i]<-bestlam
  lasso.mod = glmnet (x[train,], y[train], alpha =0, lambda =grid)
  RidgeModels[[i]]<- lasso.mod
  lasso.pred_best = predict (lasso.mod ,s=bestlam , newx=x[test ,])
  lasso.pred_0 = predict (lasso.mod ,s=0 , newx=x[test ,],exact = T)
  RSS_best[i]<- sqrt(sum((log(lasso.pred_best) - log(y.test))^2) / length(y.test))
  RSS0[i]<- sqrt(sum((log(lasso.pred_0) - log(y.test))^2) / length(y.test))
}
# Best lambda = 6172.072
min(RSS_best)
mean(RSS_best)
Alam = lam[which(RSS_best== min(RSS_best))]
Bmodel = RidgeModels[[which(RSS_best== min(RSS_best))]]

Best_lam_ridge2 = Alam
BestRidge2<- Bmodel



##### TRY ENSEMBLE MODELS ###########

GAM_Pred<- predict(BestGam,train1)

x <- model.matrix(SalePrice~., train2[,-1])[,-1]
y <- train2$SalePrice
Ridge2Pred<- predict (BestRidge2 ,s=Best_lam_ridge2 , newx=x)
Lasso2Pred<- predict (BestLasso2 ,s=Best_lam_ridge2 , newx=x)


x <- model.matrix(SalePrice~., train1[,-1])[,-1]
y <- train1$SalePrice
Ridge_Pred<- predict (BestRidge ,s=Best_lam_ridge2 , newx=x)
Lasso_Pred<- predict (BestLasso ,s=Best_lam_lasso2 , newx=x)
t2 <- read.csv("E:/SS BAck Up/Downloads/College/In USA/1. Fall Semester/R Self/t2.csv")

Ensemble <- data.frame(Id = train1$Id,Gam = GAM_Pred, Ridge2 = Ridge2Pred, Ridge = Ridge_Pred, Lasso2 = Lasso2Pred, Lasso = Lasso_Pred, SalePrice = train1$SalePrice)
colnames(Ensemble)<- c("Id", "Gam","Ridge2", "Ridge", "Lasso2", "Lasso", "SalePrice" )

# LM


RSS<- (NA)
EnsembleFit<- list(NA)
for ( i in 1:50)
{
  x<- createDataPartition(Ensemble$SalePrice, p= .98, list = F)
  x<- as.vector(x)
  train<- Ensemble[x,]
  test<- Ensemble[-x,]
  fit<- lm(SalePrice~.-Id, data = train)
  EnsembleFit[[i]]<- fit
  pred<- predict(fit, test)
  test$Predicted<- pred
  RSS[i]<- sqrt(sum((log(test$SalePrice) - log(test$Predicted))^2) / nrow(test))
}
min(RSS)

BestEnsemble = EnsembleFit[[23]] #.087
BestEnsemble2 = EnsembleFit[[16]] #.074
BestEnsemble3 = EnsembleFit[[48]] #.064

# Run WLS
RSS<- (NA)
for ( i in 1:20)
{
  x<- createDataPartition(Ensemble$SalePrice, p= .90, list = F)
  x<- as.vector(x)
  train<- Ensemble[x,]
  test<- Ensemble[-x,]
  fit<- lm(SalePrice~.-Id ,data = train)
  resids<- residuals(fit)
  fit2<- lm(SalePrice~. , weights = 1/abs(resids),data = train)
  pred<- predict(fit2, test)
  test$Predicted<- pred
  RSS[i]<- sqrt(sum((log(test$SalePrice) - log(test$Predicted))^2) / nrow(test))
}
min(RSS)

# Ridge and Lasso Ensemble
# Lasso
x <- model.matrix(SalePrice~., Ensemble[,-1])[,-1]
y <- Ensemble$SalePrice
RSS_best<-(NA)
RSS0<- (NA)
lam<- (NA)
RidgeModels<- list()
for ( i in 1:20)
{
  train = sample (1: nrow(x), nrow(x)/1.1)
  test = (1:nrow(x))[-train]
  y.test =y[ test]
  cv.out =cv.glmnet (x[train,], y[train], alpha =1)
  bestlam= cv.out$lambda.min
  lam[i]<-bestlam
  lasso.mod = glmnet (x[train,], y[train], alpha =1, lambda =grid)
  RidgeModels[[i]]<- lasso.mod
  lasso.pred_best = predict (lasso.mod ,s=bestlam , newx=x[test ,])
  lasso.pred_0 = predict (lasso.mod ,s=0 , newx=x[test ,],exact = T)
  RSS_best[i]<- sqrt(sum((log(lasso.pred_best) - log(y.test))^2) / length(y.test))
  RSS0[i]<- sqrt(sum((log(lasso.pred_0) - log(y.test))^2) / length(y.test))
}

min(RSS_best)
mean(RSS_best)

lamk<- lam[1]
modelk<- RidgeModels[[1]]


################################ Final Model Prediction on TESTEnSEMBLE data

GAM_Pred_test<- predict(BestGam,test1)

test1$SalePrice<- seq(1:nrow(test1))
test2$SalePrice<- seq(1:nrow(test2))

x_test2 <- model.matrix(SalePrice~., test2[,-1])[,-1]
Ridge2Pred_t<- predict (BestRidge2 ,s=Best_lam_ridge2 , newx=x_test2)
Lasso2Pred_t<- predict (BestLasso2 ,s=Best_lam_ridge2 , newx=x_test2)


x_test1 <- model.matrix(SalePrice~., test1[,-1])[,-1]
Ridge_Pred_t<- predict (BestRidge ,s=Best_lam_ridge2 , newx=x_test1)
Lasso_Pred_t<- predict (BestLasso ,s=Best_lam_lasso2 , newx=x_test1)

Ensemble_test <- data.frame(Id = test1$Id,Gam = GAM_Pred_test, Ridge2 = Ridge2Pred_t, Ridge = Ridge_Pred_t, Lasso2 = Lasso2Pred_t, Lasso = Lasso_Pred_t)
colnames(Ensemble_test)<- c("Id", "Gam","Ridge2", "Ridge", "Lasso2", "Lasso" )
Ensemble_test$SalePrice<- seq(1:nrow(Ensemble_test))

Final_pred1<- predict(BestEnsemble,Ensemble_test)
Final_pred2<- predict(BestEnsemble2,Ensemble_test)
Final_pred3<- predict(BestEnsemble3,Ensemble_test)
x_ENS2 <- model.matrix(SalePrice~., Ensemble_test[,-1])[,-1]
Final_pred4<- predict (modelk ,s=lamk , newx=x_ENS2)

submisson3<- data.frame(Id = test1$Id, SalePrice = Final_pred3)
submisson4<- data.frame(Id = test1$Id, SalePrice = Final_pred4)
write.csv(submisson3,"Submisson_FP3.csv")
write.csv(submisson4,"Submisson_FP4.1.csv")
write.csv(Ensemble, "Ensemble_train.csv")
write.csv(Ensemble_test, "Ensemble_test.csv")


