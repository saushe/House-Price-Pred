#library(devtools)
#install_github("saushe/RED")
library(RED)

# House price prediction

`train.(1)` <- read.csv("C:/Users/Ayasha/Downloads/train (1).csv", stringsAsFactors=FALSE)
 hd<- `train.(1)`
 rm(`train.(1)`)
 
# EDA and misisng value treatment

 # Misisng Value Summary
missing_count<- col_missing_count(hd)
# Five variables have more than 40% missing values. I will drop them

Row_missing<- row_missing_count(hd)
# 7 rows have more than 15% missing values
# Dedlete those rows

hd$row_missing<- apply(hd, 1, function(x){length(which(is.na(x)==T))})
hd1<- hd[-which(hd$row_missing>10), ]

# Univariate summary
univchar<-univariate_charvar(hd)
univnum<- univariate_numvar(hd)
univchar$`34_GarageType` # replace missing with unknown
univchar$`36_GarageQual` # replace missing with mode 
univchar$`35_GarageFinish` # replace missing with uknown
# for yr. garage built
length(which(hd1$GarageYrBlt==hd1$YearBuilt)) # most of the values re common so I will replace the missing with year built
hd1$GarageYrBlt<- ifelse(is.na(hd1$GarageYrBlt)==T,hd1$YearBuilt,hd1$GarageYrBlt)

univchar$`24_BsmtExposure`# replace with mode
univchar$`22_BsmtQual` # replace with unknown

# missing vaue treatment for categorical
hd2<- mv_treatment_charvar(hd1, default = F, col.del_cutoff = 0.4,
                     char_var_list1 = c("GarageType", "GarageCond", "GarageFinish", "BsmtQual","MasVnrType", "BsmtFinType1", "BsmtFinType2"), 
                     char_var_list2 = c("GarageQual", "BsmtQual", "BsmtExposure", "Electrical" ))
missing_count<- col_missing_count(hd2)

hd2<- mv_treatment_charvar(hd2, default = F, char_var_list1 = c("MasVnrType","BsmtCond" ))

missing_count<- col_missing_count(hd2)

# misisng value for numeric
hd2<- mv_treatment_numvar(hd2, col.del_cutoff = 0.5, treatment_type = mean)

# Variable transformation - Group Categorical variables
num_univ<- univariate_numvar(hd2, 3)
char_univ<- univariate_charvar(hd2)

hd2$MSSubClass<- as.character(hd2$MSSubClass)
hd2$OverallQual<- as.character(hd2$OverallQual)
hd2$OverallCond<- as.character(hd2$OverallCond)
hd2$YearBuilt<- 2016 - hd2$YearBuilt
hd2$YearRemodAdd<- 2016 - hd2$YearRemodAdd

hd2<- select(hd2, - row_missing)

char_univ<- univariate_charvar(hd2)

# Treatment type
#Cutoff_0.05 <- SaleCondition, RoofMatl,RoofStyle, BldgType, HouseStyle, Condition2,Condition1,Neighborhood,LotConfig,LandSlope, Functional, Heating, Foundation, MasVnrType, Exterior2nd, Exterior1st
#Cutoff_0.06 <- PavedDrive, GarageType,LotShap
#Cutoff 0.02 <-  SaleType
#Cutoff 0.03 <- BsmtFinType2

hd2<- replace_charvars(hd2, c("SaleCondition","HeatingQC", "RoofMatl","RoofStyle", "BldgType"))

hd2<- replace_charvars(hd2, c("HouseStyle", "Condition2","Condition1"))

hd2<- replace_charvars(hd2, c("LotConfig" ))

hd2<- replace_charvars(hd2, c("Heating", "Foundation", "MasVnrType", "Exterior2nd", "Exterior1st"))

hd2<- replace_charvars(hd2, c("LandSlope","Functional", "MSZoning"))

hd2<- replace_charvars(hd2, c( "GarageType","LotShape"), cutoff = 0.06)
hd2<- replace_charvars(hd2, c("Neighborhood"), 0.01)
hd2<- replace_charvars(hd2, c("SaleType"), 0.02)
hd2<- replace_charvars(hd2,c("BsmtFinType2", "BsmtCond", "BsmtQual"), 0.03 )

hd2$GarageCond<- ifelse(hd2$GarageCond =="Ex", "Gd", 
                        ifelse(hd2$GarageCond== "Unknown", "Po", hd2$GarageCond))

hd2$GarageQual<- ifelse(hd2$GarageQual == "Ex", "Gd",hd2$GarageQual )

hd2$ExterCond<- ifelse(hd2$ExterCond == "Ex", "Gd", ifelse(hd2$ExterCond == "Po", "Fa",hd2$ExterCond ))
hd2$OverallQual<- ifelse(hd2$OverallQual == "2", "4", ifelse(hd2$OverallQual == "3", "4", ifelse(hd2$OverallQual == "10", "9",hd2$OverallQual)))

hd2<- replace_charvars(hd2,c("ExterQual"), option = "rep_max", 0.02)
# Other treatment - KitchenQual, ExterCond, ExterQual, OverallQual
#Drop - Electrical,Utilities,Street, Condition2
hd3<- select(hd2, - c(Id, Electrical, Utilities, Street, GarageCond))
# Drop some wors which look like outliers
hd4<- hd3[-c(1171,519,1286), ]

###########################################################################


# Check BsmtFinType1 - bivar, BsmtQual
model1<- lm(SalePrice~.-Id ,data = hd3)
summary(model1)
# The r-SQ looks OK. Now lets have a look at the diagnostics

#Check for colinearity
library("car")
vif(model1)
# it throws an error saying that 'there are aliased coefficients in the model'
# this means that there is perfect multicolinearity between some of the variables
# Use the function "alias" to catch such variables
LC_Var<- alias(model1)
# Object called "complete" in the alias output gives you all the list of variables causing perfect multicolinearity
LC_Var<- data.frame(LC_Var$Complete)
LC_Var<- data.frame(t(LC_Var))
LC_Var$var<- row.names(LC_Var)
LC_Var$tot<- apply(LC_Var[1:2], 1, sum)
LC_Var<- filter(LC_Var, tot>0)
# Following variables are causing problems, removing them
# Model 2 - after removing variables causing perfect collinearity
model2<- lm(SalePrice~.-Id -TotalBsmtSF -GrLivArea ,data = hd3)
summary(model2)
# good news  the R-Sq did not change


summary(model2)$sigma
plot(model1)
# Looking at the diacgnostics plot, it seems that there is not much heteroscadicity
# There is no autocorrelations
# Errors are normally distributed
plot(model1, which = 5)
# There seems to be certain influential oobservations

hd2$CooksD<- cooks.distance(model1)
hd2$HATV<- hatvalues(model1)
DfFits<- dffits(model1)
which(hd2$DfFits>2)

### test test
hd4<- hd3[-c(1171,519,1286), ]
# Model 3 - After removing influential observations and perfectly collinear vars
model3<- lm(SalePrice~.-Id -TotalBsmtSF -GrLivArea ,data = hd4)
vif(model3)

# Cross Varidation
# Leave one out
library("boot")
glm.fit<- glm(SalePrice~.-Id -TotalBsmtSF -GrLivArea ,data = hd4)
cv.err =cv.glm (hd4 ,glm.fit )

# 10 fold cross validation
RSS<- (NA)
for ( i in 1:50)
{
  x<- createDataPartition(hd4$SalePrice, p= .98, list = F)
  x<- as.vector(x)
  train<- hd4[x,]
  test<- hd4[-x,]
  fit<- lm(SalePrice~.-TotalBsmtSF -GrLivArea ,data = train)
  resids<- abs(residuals(fit))
  #auxreg<- lm(resids~train$SalePrice)
  dep<- train$SalePrice
  auxreg<- gam(resids~ns(dep, df  =20))
  resid_pred<- predict(auxreg)
  fit2<- lm(SalePrice~. , weights = 1/resid_pred, data = train)
  pred<- predict(fit2, test)
  test$Predicted<- pred
  RSS[i]<- sqrt(sum((log(test$SalePrice) - log(test$Predicted))^2) / nrow(test))
}

# Trying out splines and GAMs



data2<- hd3[-c(185, 246 ,250 ,264, 324, 344 ,395, 511 ,938,1171,519,1286),]
model2<- lm(SalePrice~.-Id ,data = data2)
# R - Sq improved significatly
summary(model3)$sigma
plot(model3)

# Neither hatvalues nor dffits work. There errosr message is "prediction from a rank-deficient fit may be misleading"
# May be due to collinearity





# OR the linearly dependent variables
# LC_Var2 <- attributes(alias(model1)$Complete)$dimnames[[1]]

# It can be seen that the multicolinearity is due to dummy variables
# Correcting this trap
# 1. It looks like that the OverallCond and OverallQuality have same distribuion. So I will have to use only one of them. When I build the model only using them, R-sq or OverallQual is .68 while for OverallCond is .12. So keeping only OverallQual 
# Other option could have been to categorize OverallCond into high medium and low
model1<- lm(SalePrice~.  ,data = hd2)


#########################################

model1<- lm(SalePrice~.
            -Id  -OverallCond - Street - Utilities - Condition2 - RoofMatl - Heating - GarageQual - X1stFlrSF - X2ndFlrSF - LowQualFinSF - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF
            ,data = ModeifiedDF)

Model_Data<- select(ModeifiedDF, -c( Id ,  OverallCond,   Street,   Utilities ,  Condition2,   RoofMatl ,  Heating,   GarageQual ,  X1stFlrSF ,  X2ndFlrSF ,  LowQualFinSF ,  BsmtFinSF1 ,  BsmtFinSF2 ,  BsmtUnfSF))

#summary(model1)

#Check for colinearity
library("car")
vif(model1)
# it throws an error saying that 'there are aliased coefficients in the model'
# this means that there is perfect multicolinearity between some of the variables
# Use the function "alias" to catch such variables

LC_Var<- alias(model1)

# Object called "complete" in the alias output gives you all the list of variables causing perfect multicolinearity
LC_Var<- data.frame(LC_Var$Complete)


model2<- lm(SalePrice~.
            -Id  -OverallCond - Street - Utilities - Condition2 - RoofMatl - Heating - GarageQual - X1stFlrSF - X2ndFlrSF - LowQualFinSF - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF
            ,weights = 1/abs(residuals),data = ModeifiedDF)

r1<- residuals(model1)

auxreg<- lm(log(residuals(model1)^2) ~ log(income), data = ccard0)


plot(model1)

#########################
# Bivariate for independent variables
df<- hd2
var_type<- data.frame(summarise_all(df,class))
int_var<- colnames(df[which(var_type == "integer" | var_type == "numeric") ])
col_index<- which(colnames(df)%in%int_var)
i = 1
for (i in 1:length(int_var))
{
  x<- col_index[i]
  p<- qplot(df[x],SalePrice, data = df, main = paste0(int_var[i]))
  dev.new()
  print(p)
}

##############################
# Loop for GAM

var_type<- data.frame(summarise_all(hd3,class))
int_var<- colnames(hd3[which(var_type == "integer" | var_type == "numeric") ])
int_var<- int_var[-c(1,15,16,17,18,19,20,22,24,31,34)]
output<- data.frame(var = NA, run1 = NA, run2 = NA, run3 = NA, run4 = NA, run5 = NA, run6 = NA, run7 = NA, run8 = NA, run9= NA, run10 = NA, runln = NA)

i= 1
for (i in 1:length(int_var))
{
new_df<- hd3[, c(int_var[i], "SalePrice")]
colnames(new_df)<- c("a", "SalePrice")
output[i,1] = int_var[i]
  for (j in 1:10)
  {
    for(k in 1:5)
    {
      x<- createDataPartition(new_df$SalePrice, p= .9, list = F)
      x<- as.vector(x)
      train<- new_df[x,]
      test<- new_df[-x,]
      gam.model<- gam(SalePrice~s(a,j), data = train)
      pred<- predict(gam.model, test)
      test$Predicted<- pred
      RSS[k]<- sqrt(sum((test$SalePrice - test$Predicted)^2) / nrow(test))
    }
    output[i,j+1]<- mean(RSS)
  }
 
}

i= 1
for (i in 1:length(int_var))
{
  new_df<- hd3[, c(int_var[i], "SalePrice")]
  colnames(new_df)<- c("a", "SalePrice")
for ( o in 1:5)
{
  x<- createDataPartition(new_df$SalePrice, p= .9, list = F)
  x<- as.vector(x)
  train<- new_df[x,]
  test<- new_df[-x,]
  fit<- lm(SalePrice~.,data = train)
  pred<- predict(fit, test)
  test$Predicted<- pred
  RSS[o]<- sqrt(sum((test$SalePrice - test$Predicted)^2) / nrow(test))
}
  output$runlm[i] = mean(RSS)
}



################
# Find out number of knots suitable for each variable

gam.test<- gam(SalePrice~ns(YearBuilt, 5)+s(LotFrontage, 4), data = hd3)
plot(gam.test, se = T)
