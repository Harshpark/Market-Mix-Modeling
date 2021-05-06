##############################################################################################################
############################## MMM - Revenue Model , Author - Harshal H. Parkhe ##############################
##############################################################################################################

rm(list = ls())
library(readr)
library(ggplot2)
library(car)
library(quantreg)
library(SparseM)
library(PerformanceAnalytics)
library(Hmisc)
library(ClustOfVar)
library(gam)
library(mgcv)
library(PCAmixdata)
library(gtools)
library(minpack.lm)
library(data.table)

#Load the data

Raw_Modeling_data <- read_csv("D:\\MarketMix\\input\\Modeling_data.csv", 
                              col_types = cols(Week_end = col_date(format = "%d-%m-%Y")))


#names(Raw_Modeling_data)
## remove redundant variables from spends and impressions, if we have both spend and
## impressions, retain impressions

#IM_CatCircs is same as SP_Catalog, hence keeping only IM_CatCircs

## IM_FSICircs, SP_FSI, SP_FSI_M are same, hence keeping only IM_FSICircs

## IM_DMCircs.adstock, SP_DM.adstock are same, hence keeping only IM_DMCircs.adstock

Modeling_data = subset(Raw_Modeling_data, select = -c(SP_Catalog,SP_FSI, SP_FSI_M, SP_DM))

## add calculated field for revenue, if revenue already present in the data, no need to calculate

Modeling_data$Revenue = Modeling_data$Online_Units*Modeling_data$PR_Online_PRU

attach(Modeling_data)

# Function for correlation matrix
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


#####################################################################
## Adstock rate optimization and adstock variable calculation
#####################################################################

# Define adstock function
#rm(adstock)
adstock <- function(x, rate=0){
  return(as.numeric(filter(x=x, filter=rate, method="recursive")))
}

#Read spend and impression variables, preferred to start names with either SP(Spend) or IM(Impression) in your data
Spend_Impr_Variables1 = names(Modeling_data[ , grepl( "SP|IM" , names(Modeling_data))])

#length(Spend_Impr_Variables1)

b = paste0("b",seq(1:length(Spend_Impr_Variables1)))
b1<-mixedsort(union(b,c("b0")))#adding b0
r = paste0("r",seq(1:length(Spend_Impr_Variables1)))
# r1<-mixedsort(union(r,c("r0")))
adstock_factors = paste0(b,"*adstock(",Spend_Impr_Variables1,",",r,")")

adstock_formula = as.formula(paste("Revenue~b0+", paste(adstock_factors, collapse="+")))
#typeof(adstock_formula)
#set start values

#For Start Argument
param<-rep(1,(length(Spend_Impr_Variables1)+1))
param1<-rep(0,length(Spend_Impr_Variables1))
param<-c(param,param1)
names(param)<-c(b1,r)#creating named vector for argument start

#For Lower Argument
param2<-rep(-Inf,(length(Spend_Impr_Variables1)+1))
param3<-rep(0,length(Spend_Impr_Variables1))
param2<-c(param2,param3)
names(param2)<-c(b1,r)#creating named vector for argument lower

#For upper Argument
param4<-rep(Inf,(length(Spend_Impr_Variables1)+1))
param5<-rep(1,length(Spend_Impr_Variables1))
param4<-c(param4,param5)
names(param4)<-c(b1,r)#creating named vector for argument upper

#Functions for returning the arguments
strt<-function(x){
  return(param)
}
lwr<-function(){
  return(param2)
}
uppr<-function(){
  return(param4)
}
#Function for adstock formula
adstock_formula1<-function(){
  adstock_formula1 = as.formula(paste("Revenue~b0+", paste(adstock_factors, collapse="+")))
  return(adstock_formula1)
}

# Optimize adstock, Solve with nlsLM
#This will throw an error related to data filter if dplyr is loaded already, internally nlslm runs base package stats -- stats::filter -- is being over-ridden by dplyr::filter
#detach and remove dplyr before running nlslm or load dplyr later where required
opt_adstock_rates1 = nlsLM(formula=adstock_formula1(),
                           data = Modeling_data,
                           algorithm = "LM",
                           start = strt(),
                           lower = lwr(),
                           upper = uppr())

# get coefficients and adstock rates
adstockrates = as.data.frame(round(coef(opt_adstock_rates1),digits = 2))

#convert rownames into 1st column
adstockrates = setDT(adstockrates, keep.rownames = TRUE)[]

colnames(adstockrates)[1] <- "opt_parameters"

colnames(adstockrates)[2] <- "opt_adstockrate"

DT = as.data.table(adstockrates)
#replacing adstock rates >0.5 to 0.4 as per business intuition
DT[opt_adstockrate>0.5, opt_adstockrate := 0.3]

#retain only adstock rate rows
DT <- DT[-grep("b", DT$opt_parameters), ]

#  Use grepl to get spend and impression variables, make sure spend variables start with "SP
# impression variables start with "IM"
#Spend_Impr_Variables = as.data.frame(names(Modeling_data[ , grepl( "SP|IM" , names(Modeling_data))]))

DT = cbind(DT,Spend_Impr_Variables1)

colnames(DT)[3]<- "Vehicle"

#Loop to create adstock variables and append to modeling data, vehicle spends with 0 adstock rate 
#will have adstock variables with same values as spend variables
for (i in 1:nrow(DT))
{
  Vehicle1 = as.character(DT[i,]$Vehicle)
  Modeling_data$x<-assign(paste(DT[i,]$Vehicle,".adstock",sep = ""), as.numeric(filter(x=Modeling_data[,Vehicle1[1]], filter=DT[i,]$opt_adstockrate, method="recursive")))
  names(Modeling_data)[names(Modeling_data) == 'x'] <- paste(Vehicle1,".adstock",sep = "")
}


##########################################################################
########################### GAM Transformations ##########################
##########################################################################

#############
# GAM model #
#############

gam1<-gam(Revenue~s(SP_Affiliates.adstock,bs = 'ps',sp = 0.5)+
                  s(SP_Search_L.adstock,bs = 'ps',sp = 0.5)+
            s(SP_BrandTV.adstock,bs = 'ps',sp = 0.5),
          data = Modeling_data[1:175,])


# Output model results and store intercept for plotting later on
summary_model      <- summary(gam1)
model_coefficients <- summary_model$p.table
model_intercept    <- model_coefficients["(Intercept)", 1]


# Plot the smooth predictor function to obtain the radio response curve
p    <- predict(gam1, type="lpmatrix")
beta <- coef(gam1)[grepl("SP_Affiliates.adstock", names(coef(gam1)))]
s    <- p[,grepl("SP_Affiliates.adstock", colnames(p))] %*% beta + model_intercept
print(s)

beta1 <- coef(gam1)[grepl("SP_Search_L.adstock", names(coef(gam1)))]
s1    <- p[,grepl("SP_Search_L.adstock", colnames(p))] %*% beta + model_intercept
print(s1)

beta2 <- coef(gam1)[grepl("SP_BrandTV.adstock", names(coef(gam1)))]
s2    <- p[,grepl("SP_BrandTV.adstock", colnames(p))] %*% beta + model_intercept
print(s2)

# affiliates_gam_data = cbind(actual = Modeling_data[1:175,]$SP_Affiliates.adstock,gam_transformed = s)
# 
# searchL_gam_data = cbind(actual = Modeling_data[1:175,]$SP_Search_L.adstock,gam_transformed = s1)
# 
# brandTV_gam_data = cbind(actual = Modeling_data[1:175,]$SP_BrandTV.adstock,gam_transformed = s2)
# 
# write.csv(affiliates_gam_data,"affiliates_gam_data.csv",row.names = FALSE)
# 
# write.csv(searchL_gam_data,"searchL_gam_data.csv",row.names = FALSE)
# 
# write.csv(brandTV_gam_data,"brandTV_gam_data.csv",row.names = FALSE)

plot(gam1)

plot(Modeling_data[1:175,]$SP_Search_L.adstock,s1)

smoothingSpline = smooth.spline(Modeling_data[1:175,]$SP_Search_L.adstock, s1, spar=0.5)
plot(Modeling_data[1:175,]$SP_Search_L.adstock,s1)
lines(smoothingSpline)

qplot(Modeling_data[1:175,]$SP_Search_L.adstock,s1, geom='smooth', span =0.5)

smoothingSpline = smooth.spline(Modeling_data[1:175,]$SP_BrandTV.adstock, s2, spar=0.5)
plot(Modeling_data[1:175,]$SP_BrandTV.adstock,s2)
lines(smoothingSpline)

qplot(Modeling_data[1:175,]$SP_BrandTV.adstock,s2, geom='smooth', span =0.5)


##########################################################################
########################## ADBUDG Transformations ########################
##########################################################################

#  Plot data  #
#channelName = as.character(myData$Channel[1])
maxX = 1.05*max(Modeling_data[1:175,]$SP_BrandTV.adstock)
maxY = 1.05*max(s2)

myPlotDataDF = data.frame(Return = s2, Spend = Modeling_data[1:175,]$SP_BrandTV.adstock)

simpleScatterPlot <- ggplot(myPlotDataDF, aes(x = Spend, y = Return)) +
  geom_point(color="black") +
  theme(panel.background = element_rect(fill = 'grey85'),
        panel.grid.major = element_line(colour = "white")) +
  coord_cartesian(ylim = c(0,maxY), xlim = c(0,maxX)) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = comma)  
#ggtitle(paste(channelName))

simpleScatterPlot

# We need a model that exhibits diminishing marginal returns.  
# Diminishing marginal return means that the rate of return will decrease the more you spend. 
# The ADBUDG model is a very flexible model that incorporates diminishing marginal returns.  
# The ADBUDG model is defined as follows:
# B + (A - B)*((Spend^C)/(D + (Spend^C)))
#   
#   
# Just as a linear model is governed by parameters (slope and intercept), 
# the ADBUDG model is also governed by parameters.  There are four ADBUDG model parameters:
#   
# A - The maximum amount of return possible for a campaign given a long term investment.
# B - The minimum amount of return possible for a campaign given a long term investment
# C - Controls the shape of the curve.
# D - Represents initial market share or market saturation effects.
# These four parameters actually give the shape and behavior of the model a lot of flexibility.  


# create a custom function that returns the total observed error based on the ADBUDG model

Ufun<-function(x, Spend, Return) {
  predictedReturn = x[2] + (x[1] - x[2])*((Spend^x[3])/(x[4] + (Spend^x[3])))
  errorSq = (predictedReturn - Return)^2
  sumSqError = sum(errorSq)
  return(sumSqError)
}

min(Modeling_data[1:175,]$SP_BrandTV.adstock)

max(s2)
min(s2)

#we can create some start values, as well as upper and lower bounds
startValVec = c(20000000,12000000,1.2,2500000)
minValVec = c(9000000,2000000,0.5,1000000)
maxValVec = c(28185739, 14489639,2, 10890890)

# The nlminb() function works by minimizing the return value from some function. 
optim.parms<-nlminb(objective=Ufun,start=startValVec,
                    lower=minValVec,
                    upper=maxValVec,
                    control=list(iter.max=100000,eval.max=2000),
                    Spend = Modeling_data[1:175,]$SP_BrandTV.adstock,
                    Return = s2)

optim.parms

#A convergence code of 0 indicates successful convergence

a = optim.parms$par[1]
b = optim.parms$par[2]
c = optim.parms$par[3]
d = optim.parms$par[4]

curveDFx = seq(from=0, to=max(Modeling_data[1:175,]$SP_BrandTV.adstock)*2, length.out=10000)
curveDFy = b+(a-b)*((curveDFx^c)/(d+(curveDFx^c)))
curveDF = data.frame(Spend = curveDFx, Return = curveDFy)

maxX = 1.05*max(curveDFx, max(Modeling_data[1:175,]$SP_BrandTV.adstock))
maxY = 1.05*max(curveDFy, max(s2))

myPlotDataDF = data.frame(Return = s2, Spend = Modeling_data[1:175,]$SP_BrandTV.adstock)
optimLineDF = data.frame(Spend = curveDFx, Return = curveDFy)

write.csv(optimLineDF,"adbudg_Search_U.csv",row.names = FALSE)

scatterPlotPlusFit <- ggplot(myPlotDataDF, aes(x = Spend, y = Return)) +
  geom_point(color="black", shape = 16) +
  theme(panel.background = element_rect(fill = 'grey85'),
        panel.grid.major = element_line(colour = "white")) +
  geom_line(data = optimLineDF, aes(x = Spend, y = Return, color = "blue"))  +
  scale_color_manual(labels = "Optimized ADBUDG Fit",values=c('blue')) +
  theme(legend.title=element_blank(), legend.position = "bottom") +
  coord_cartesian(ylim = c(0,maxY), xlim = c(0,maxX))
  # scale_x_continuous(labels = dollar) +
  # scale_y_continuous(labels = comma) 
# + 
#   ggtitle(paste(channelName, "Data & Model Fit", sep = " "))

scatterPlotPlusFit

##########################################################################
###################### Variable Transformation Module ####################
##########################################################################

#Create the dataframe of adstock variables and pricing variables for transformation test

Transformation_Variables = Modeling_data[,grep(".adstock|PR_Online_PMU|PR_Online_PRU", colnames(Modeling_data))]
Transformation_Variables[is.na(Transformation_Variables)] <- 0

#names(Transformation_Variables[1])

#summary(lm(Revenue~PR_Online_PMU))

#create a placeholder model summary dataframe
model_summary <- data.frame(model_name=character(), 
                 accuracy = double()) 

for(i in 1:length(Transformation_Variables)){
  
  model_Variables<-cbind(Revenue = Modeling_data$Revenue,Transformation_Variables[i])
  model_Variables[is.na(model_Variables)] <- 0
  
  #without transformation
  model_output_1 = data.frame(model_name = paste("linear_model_", names(Transformation_Variables[i]), sep = ""),
                              accuracy = round((summary(lm(Revenue ~ model_Variables[,2],data = model_Variables))$r.squared),digits = 2))
  
  # #log transformation
  model_output_2 = data.frame(model_name = paste("log_model_", names(Transformation_Variables[i]), sep = ""),
                              accuracy = round((summary(lm(Revenue ~ log(model_Variables[,2]+1),data = model_Variables))$r.squared),digits = 2))

  # #sqrt transformation
  model_output_3 = data.frame(model_name = paste("sqrt_model_", names(Transformation_Variables[i]), sep = ""),
                              accuracy = round((summary(lm(Revenue ~ sqrt(model_Variables[,2]),data = model_Variables))$r.squared),digits = 2))
  
  # #1/x transformation  
  model_output_4 = data.frame(model_name = paste("fractional_model_", names(Transformation_Variables[i]), sep = ""),
                              accuracy = round((summary(lm(Revenue ~ 1/(model_Variables[,2]+1),data = model_Variables))$r.squared),digits = 2))
  
  # #polynomial transformation
  #make sure no missing values before poly transform
  model_output_5 = data.frame(model_name = paste("poly_modelorder2_", names(Transformation_Variables[i]), sep = ""),
                              accuracy = round((summary(lm(Revenue ~ poly(model_Variables[,2],2),data = model_Variables))$r.squared),digits = 2))
  
  #Combine all model outputs
  model_output_6 = rbind(model_output_1,model_output_2,model_output_3,model_output_4,model_output_5)
  
  if (!exists("model_output_7")) {
    model_output_7 <- model_output_6
  } else if (exists("model_output_7")){
    model_output_7<-rbind(model_output_7, model_output_6)
  } 
}

#Extract string before first "_"
model_output_7$transformation = sub("\\_.*", "", model_output_7$model_name)

#Extract string after 2nd "_"
model_output_7$variable = gsub('^(?:[^_]*_){2}','',model_output_7$model_name)


# detach("package:dplyr", unload=TRUE)
# remove.packages("dplyr")
library(dplyr)
#For each variable, get the transform with highest accuracy
mo9 = model_output_7 %>%
  group_by(variable) %>%
  top_n(n = 1, wt = accuracy)

#retain variable and transformation
variable_transformations<-mo9[,c("variable","transformation")]

#variable_transformations$variable[1]

#Loop to lookup for the right transfomation and create transformed variables
for(i in 1:nrow(variable_transformations)){
  variable_t = as.character(variable_transformations[i,]$variable)
  if(variable_transformations[i,]$transformation =='log'){
    test1<-as.numeric(unlist(Transformation_Variables[,variable_t]))
    Transformation_Variables$x <-log(test1+1)
  }
  if(variable_transformations[i,]$transformation =='poly'){
    #unlist variable to accomodate polynomial function transform unique length constraint
    test1<-as.numeric(unlist(Transformation_Variables[,variable_t]))
    test2<-lm(Revenue~poly(test1,2))
    test3<-as.data.frame(test2$coefficients)
    order1<-test3[2,]
    order2<-test3[3,]
    Transformation_Variables$x <-order1*test1+order2*test1*test1
  }
  if(variable_transformations[i,]$transformation =='sqrt'){
    test1<-as.numeric(unlist(Transformation_Variables[,variable_t]))
    Transformation_Variables$x <-sqrt(test1)
  }
  if(variable_transformations[i,]$transformation =='fractional'){
    test1<-as.numeric(unlist(Transformation_Variables[,variable_t]))
    Transformation_Variables$x <-1/(test1+1)
  }
  if(variable_transformations[i,]$transformation =='linear'){
    test1<-as.numeric(unlist(Transformation_Variables[,variable_t]))
    Transformation_Variables$x <-test1
  }
  names(Transformation_Variables)[names(Transformation_Variables) == 'x'] <- paste(variable_transformations[i,]$variable,"_",variable_transformations[i,]$transformation,sep = "")
}

##########################################################################
############################## Clustering ################################
##########################################################################

Clustering_Variables = Transformation_Variables[,grep(".adstock_poly|.adstock_log|.adstock_sqrt|.adstock_linear|.adstock_fractional", colnames(Transformation_Variables))]

###########################################################################
## varclus - create clusters of similar variables
###########################################################################

#sapply(Clustering_Variables, mode)

clust_Var = data.matrix(Clustering_Variables)
#create cluster
tree <- hclustvar(clust_Var)
max_clusters<-ncol(clust_Var)#max number of clusters, same as number of spend/impression variables

for(opt_cluster_size in max_clusters:1){
  cluster_group <- cutreevar(tree, opt_cluster_size) 
  res3<-rcorr(as.matrix(cluster_group$scores))
  corr_mat_cluster<-flattenCorrMatrix(res3$r, res3$P) #check correlation among clusters
  
  if(max(corr_mat_cluster$cor)<0.3)#0.35 is correlation threshold, can choose as per requirement
    {
      Data_varclus <- as.data.frame(cluster_group$scores)#create dataframe for cluster variables
      break()#stop running the loop as soon as if condition is satisfied
    }
}

#remove effect of intercept from cluster 1, automate this process
Data_varclus$cluster1<-Data_varclus$cluster1+1.531837e+00

#check clusters and its components
cluster_group$coef

#create a dataframe for each cluster, this will be used later in the modeling phase
cluster_list <- list()
for(i in 1:length(cluster_group$coef)){
  cluster_list[[i]] <-setDT(as.data.frame(cluster_group$coef[i]), keep.rownames = TRUE)[]
#assign(paste("cluster",i,"_df",sep = ""), setDT(as.data.frame(cluster_group$coef[i]), keep.rownames = TRUE)[])
}
cluster_list

# Add 
# 1. revenue variable to the clustered dataset, this will be needed when we run models
# 2. individual transformed adstock variables, we will need to push them in the model in case cluster doesn't come significant
# 3. transformed margin or pricing variable, this must be included in the model

Data_varclus = cbind(Data_varclus,Revenue=Modeling_data$Revenue,Clustering_Variables,Margin=Transformation_Variables$PR_Online_PMU_poly)
attach(Data_varclus)
############################################################################
############################ Forecasting Models ############################
############################################################################



############################################################################
################################# 1. UCM ###################################
############################################################################

library(rucm)

# detach("package:rucm", unload=TRUE)
# 
# remove.packages("rucm")
# 
# devtools::install_github("kaushikrch/rucm")

#range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#scaling function - scale the data first as UCM models do not accept variables with very high variance
range02 <- 
  function(x){
    if(max(x)<=0){#handle division by 0 error
      x/((max(x))+1)
    }
    else{
      x/(max(x))
    }
    }

#Train & Test

#157 weeks of training data
Data_varclus_train<-Data_varclus[1:157,]
#23 weeks of test data
Data_varclus_test<-Data_varclus[158:175,]

#scale all columns in train and test data
Data_varclus_train_scaled = Data_varclus_train

for(i in names(Data_varclus_train))
{
  Data_varclus_train_scaled[[i]]= range02(Data_varclus_train[[i]])
}


Data_varclus_test_scaled = Data_varclus_test

for(i in names(Data_varclus_test))
{
  Data_varclus_test_scaled[[i]]= range02(Data_varclus_test[[i]])
}

#make sure Revenue >0, otherwise we will face error in mape calculation
Data_varclus_train_scaled$Revenue = Data_varclus_train_scaled$Revenue+0.001
Data_varclus_test_scaled$Revenue = Data_varclus_test_scaled$Revenue+0.001

#Push only clustered variables +Margin in the model
ucm_fit = ucm(formula = Revenue ~
                cluster1+cluster2+
                cluster3+cluster4+
                Margin
              ,data = Data_varclus_train_scaled,slope=TRUE,season = TRUE,season.length = 52
)

#If some clusters do not come out significant, push vehicles from those clusters independently
#in the model to check if any vehicle is significant

ucm_fit = ucm(formula = Revenue ~
                cluster1+
                #cluster2+
                SP_Email.adstock_log+
                SP_Search_U.adstock_poly+
                SP_Others.adstock_log+
                IM_CatCircs.adstock_log+
                #cluster3
                SP_Pl.adstock_poly+
                IM_DMCircs.adstock_log+
                IM_FSICircs.adstock_sqrt+
                #cluster4
                SP_OA_U.adstock_poly+
                SP_SM_UL.adstock_poly+
                SP_BrandTV.adstock_log+
                SP_mobiMe_marketing.adstock_log+
                #Pricing
                Margin
              ,data = Data_varclus_train_scaled,slope=TRUE,season = TRUE,season.length = 52
)

#model with only significant variables

ucm_fit = ucm(formula = Revenue ~
                cluster1+
                IM_DMCircs.adstock_log+
                Margin
              ,data = Data_varclus_train_scaled,slope=TRUE,season = TRUE,season.length = 52
)

ucm_fit$est

plot(ucm_fit$s.level)

plot(ucm_fit$s.slope)

#slope+level gives trend

trend = ucm_fit$s.level+ucm_fit$s.slope

plot(trend)

plot(ucm_fit$s.season)

#predict on train data
pd  = predict(ucm_fit)

#mape on train data
mean(abs((Data_varclus_train_scaled$Revenue-pd)/Data_varclus_train_scaled$Revenue))

#create state space model for test data and test ucm model on the test data
Newdata <- SSModel(rep(NA,nrow(Data_varclus_test_scaled)) ~ cluster1+
                     IM_DMCircs.adstock_log+
                     Margin+
                     SSMtrend(2, Q = list(ucm_fit$est.var.level, 
                                          ucm_fit$est.var.slope)) 
                   + SSMseasonal(52, Q = ucm_fit$est.var.season), 
                   H = ucm_fit$irr.var, data=Data_varclus_test_scaled)

#future forecast
pd_test = predict(ucm_fit$model,newdata = Newdata)

#mape on train data
mean(abs((Data_varclus_test_scaled$Revenue-pd_test)/Data_varclus_test_scaled$Revenue))

plot(Data_varclus_train_scaled$Revenue, ylab = "Sales",type = "l")
lines(pd, col = "blue")
legend("topright", legend = c("Observed sales","Predicted"), col = c("black","blue"), lty = 1)

plot(Data_varclus_test_scaled$Revenue, ylab = "Sales",type = "l")
lines(pd_test, col = "blue")
legend("topright", legend = c("Observed sales","Predicted"), col = c("black","blue"), lty = 1)

# #formula for revenue using the final model
# as.formula(
#   paste0("Revenue ~ ", round(ucm_fit$est[1],2), "*",names(ucm_fit$est[1]) ,
#          paste(sprintf(" %+.2f*%s ", 
#                        ucm_fit$est[-1],  
#                        names(ucm_fit$est[-1])), 
#                collapse="")
#   )
# )

#Combine train & test scaled & unscaled values df for further contribution and ROI calculations

Data_unscaled<-rbind(Data_varclus_train,Data_varclus_test)
Data_scaled<-rbind(Data_varclus_train_scaled,Data_varclus_test_scaled)

#Get the model results
ucm_results_df<-as.data.frame(ucm_fit$est)#model results in a dataframe

ucm_results_df<-setDT(ucm_results_df, keep.rownames = TRUE)[]

names(ucm_results_df)[2]<-"coefficient"
#vars=1

############################################################################
######## Media Vehicles & Pricing - Revenue Contribution ###########
############################################################################

#ROI and Revenue Contribution for Media Vehicles and Pricing component thrown out by the model
#make sure cluster_list is populated correctly before running this loop
for(vars in 1:nrow(ucm_results_df)){
  column_name<-ucm_results_df[vars,1]$rn
  if(column_name %like% "cluster"){#if cluster variable
    #ucm_results_df[vars,2]$coefficient*Data_scaled[column_name]*max(Data_unscaled$Revenue)
    #cluster1_revenue = as.data.frame(ucm_results_df[vars,2]$coefficient*Data_scaled[column_name]*max(Data_unscaled$Revenue))
    #rm(cluster1_contribution)
    #assign(paste(column_name,"_contribution",sep = ""),as.data.frame(ucm_results_df[vars,2]$coefficient*Data_scaled[column_name]*max(Data_unscaled$Revenue)))
    
    for(cl in 1:length(cluster_list)){#parse through cluster list
      if(column_name==names(cluster_list[[cl]])[2]){#check which cluster from the cluster list is significant
        names(cluster_list[[cl]])[2]<-"cluster"#change the name, this will ensure consistency in cluster contribution calculation formula below
        # create empty list  
        #rm(my_list)
        my_list <- list()
        #cl=1
        #vars1=2
        for(vars1 in 2:nrow(cluster_list[[cl]])){#parse through elements in the cluster
          column_name1<-cluster_list[[cl]][vars1,1]$rn #cluster cl from cluster list, get all column names
          my_list[[vars1]] <- as.data.frame(cluster_list[[cl]][vars1,2]$cluster*Data_unscaled[column_name1])
          #assign(paste(column_name,"_contribution",sep = ""),as.data.frame(cluster1_df[vars1,2]$cluster1*Data_unscaled[column_name]))
        }
        my_list<-plyr::compact(my_list)#remove null elements of list
        #assign(paste(column_name,"_contribution_sum",sep = ""),Reduce('+',my_list))#sum vectors in a list, to get sum of all elements of cluster
        #rm(cluster1_contribution)
        #assign(paste(column_name,"_contri",sep = ""), cbind(revenue = cluster1_contribution,revenue_sum=cluster1_contribution_sum))
        # calculate the multiplier/scaling factor for cluster components
        #cluster1_contri$ratio = cluster1_contri[,1]/cluster1_contri[,2]
        # rm(my_list_contribution)
        # rm(cluster1_contribution)
        my_list_contribution<-lapply(my_list, "*", (ucm_results_df[vars,2]$coefficient*Data_scaled[column_name]*max(Data_unscaled$Revenue))/(Reduce('+',my_list))) #multiply each list element by the scaling factor for each cluster
        assign(paste(column_name,"_contribution",sep = ""),as.data.frame(my_list_contribution))#get revenue contributions of each element of cluster
        #(ucm_results_df[vars,2]$coefficient*Data_scaled[column_name]*max(Data_unscaled$Revenue))/(Reduce('+',my_list))
      }
    }
  }
  else{
    assign(paste(column_name,"_contribution",sep = ""),as.data.frame(ucm_results_df[vars,2]$coefficient*Data_scaled[column_name]*max(Data_unscaled$Revenue)))
  }
}
rm(my_list_contribution)

###########################################################################
############## Baseline - Trend and seasonality contribution ##############
###########################################################################

#model will give level/slope/seasonality for only train datapoints, 
#to get level/slope/seasonality for train+test, either run the model on entire train+test, 
#or do reverse engineering based on test predictions(total revenue for test datapoints) to 
#figure out revenue contribution of trend & seasonality

## trend contribution

#slope+level gives trend

trend = ucm_fit$s.level+ucm_fit$s.slope

trend_revenue = as.data.frame(trend*max(Data_unscaled$Revenue))

names(trend_revenue)[1]<-"trend_revenue"

##seasonality contribution

seasonality_revenue = as.data.frame(ucm_fit$s.season*max(Data_unscaled$Revenue))

names(seasonality_revenue)[1]<-"seasonality_revenue"

###Baseline = Trend + Seasonality#####

baseline_revenue<-as.data.frame(trend_revenue + seasonality_revenue)
names(baseline_revenue)[1]<-"baseline_revenue"

#combine vehicle & margin/pricing contributions
rm(vehicle_contribution)
vehicle_contribution<-do.call(cbind, mget(ls(pattern = "_contribution")))

names(vehicle_contribution) = gsub(pattern = ".adstock.*", replacement = "", x = names(vehicle_contribution))

names(vehicle_contribution) = gsub(pattern = "*cluster._contribution.", replacement = "", x = names(vehicle_contribution))

#remove margin from vehicle contribution dataframe, use the name you have chosen, it could be pricing in your case, keeping vehicles separate will aid in ROI calculation later
vehicle_contribution<-vehicle_contribution[ , -which(names(vehicle_contribution) %in% c("Margin"))]

######################################################################
############ Media Vehicles ROI Calculation = Revenue/Spend ##########
######################################################################

my_list_ROI <- list()#placeholder for ROI of media vehicles
for(i in 1:ncol(vehicle_contribution)){
  if(names(vehicle_contribution[i]) %in% colnames(Raw_Modeling_data)){
    colname_revenue<-names(vehicle_contribution[i])
    colname_spend<-names(vehicle_contribution[i])#spend variable for impressions would be different and hence a separate object for spend column name
    #Map each impression to its corresponding spend for ROI calculation
    if(colname_spend %like% "IM_DM"){
      colname_spend<-"SP_DM"
    }
    if(colname_spend %like% "IM_Cat"){
      colname_spend<-"SP_Catalog"
    }
    if(colname_spend %like% "IM_FSI"){
      colname_spend<-"SP_FSI"
    }
    my_list_ROI[[i]] <- as.data.frame(vehicle_contribution[colname_revenue]/as.double(Raw_Modeling_data[1:175,][[colname_spend]]))#converting to double as IM variables have character datatype, adding 0.1 to avoid division by zero issues
  }
}

vehicle_ROI<-as.data.frame(my_list_ROI)