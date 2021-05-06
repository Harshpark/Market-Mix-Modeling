#############################################################################################################
############################## MMM - Revenue Model , Author - Harshal H. Parkhe##############################
#############################################################################################################

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

#Load the data

Raw_Modeling_data <- read_csv("D:\\MarketMix\\input\\Modeling_data.csv", 
                              col_types = cols(Week_end = col_date(format = "%d-%m-%Y")))

sapply(Raw_Modeling_data, function(x) sum(is.na(x)))

names(Raw_Modeling_data)
## remove redundant variables from spends and impressions, where-ever we have both spend and
## impressions, retain impressions

#IM_CatCircs is same as SP_Catalog, hence keeping only IM_CatCircs

## IM_FSICircs, SP_FSI, SP_FSI_M are same, hence keeping only IM_FSICircs

## IM_DMCircs.adstock, SP_DM.adstock are same, hence keeping only IM_DMCircs.adstock

Modeling_data = subset(Raw_Modeling_data, select = -c(SP_Catalog,SP_FSI, SP_FSI_M, SP_DM))

## add calculated field for revenue

Modeling_data$Revenue = Modeling_data$Online_Units*Modeling_data$PR_Online_PRU

attach(Modeling_data)

## Split into train and test, we have data for top 180 rows, using 130 rows to train the model
## and 50 rows will be used to validate

#Modeling_data = Modeling_data[1:130]

#Trend Chart

ggplot(Modeling_data, aes(Week_end, Online_Units)) + geom_line() + xlab("") + ylab("Online Units")

scatterplot(Online_Units ~ SP_Affiliates, data= Modeling_data, 
            xlab="SP_Affiliates ", ylab="Online_Units", 
            main="Enhanced Scatter Plot", labels=row.names(Modeling_data))


# scatter plot matrix/ Bivariate charts
scatterplotMatrix(~Online_Units+Visits+Offline_units+Calls, data=Modeling_data)

my_data <- Modeling_data[, c("Online_Units","Visits","Offline_units","Calls")]

#correlation chart 
chart.Correlation(my_data, histogram=TRUE, pch=19)

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

res2<-rcorr(as.matrix(my_data))
flattenCorrMatrix(res2$r, res2$P) 

#####################################################################
## Adstock rate optimization and adstock variable calculation
#####################################################################

# Define adstock function
adstock <- function(x, rate=0){
  return(as.numeric(filter(x=x, filter=rate, method="recursive")))
}

#Read spend and impression variables
Spend_Impr_Variables1 = names(Modeling_data[ , grepl( "SP|IM" , names(Modeling_data))])

length(Spend_Impr_Variables1)

b = paste0("b",seq(1:length(Spend_Impr_Variables1)))
b1<-mixedsort(union(b,c("b0")))#adding b0
r = paste0("r",seq(1:length(Spend_Impr_Variables1)))
# r1<-mixedsort(union(r,c("r0")))
adstock_factors = paste0(b,"*adstock(",Spend_Impr_Variables1,",",r,")")

adstock_formula = as.formula(paste("Revenue~b0+", paste(adstock_factors, collapse="+")))
#typeof(adstock_formula)
#set start values

#For Start Arguement
param<-rep(1,(length(Spend_Impr_Variables1)+1))
param1<-rep(0,length(Spend_Impr_Variables1))
param<-c(param,param1)
names(param)<-c(b1,r)#creating named vector for arguement start

#For Lower Arguement
param2<-rep(-Inf,(length(Spend_Impr_Variables1)+1))
param3<-rep(0,length(Spend_Impr_Variables1))
param2<-c(param2,param3)
names(param2)<-c(b1,r)#creating named vector for arguement lower

#For upper Arguemnet
param4<-rep(Inf,(length(Spend_Impr_Variables1)+1))
param5<-rep(1,length(Spend_Impr_Variables1))
param4<-c(param4,param5)
names(param4)<-c(b1,r)#creating named vector for argument upper

#Functions for returning the arguements
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
library(minpack.lm)
opt_adstock_rates1 = nlsLM(formula=adstock_formula1(),
                           data = Modeling_data,
                           algorithm = "LM",
                           start = strt(),
                           lower = lwr(),
                           upper = uppr())


# opt_adstock_rates = nlsLM(Revenue~b0 +
#                             b1*adstock(SP_Affiliates,r1)+
#                             b2*adstock(SP_Email,r2)+
#                             b3*adstock(SP_Pl,r3)+
#                             b4*adstock(SP_OA_L,r4)+
#                             b5*adstock(SP_Search_L,r5)+
#                             b6*adstock(SP_OA_U,r6)+
#                             b7*adstock(SP_Search_U,r7)+
#                             b8*adstock(SP_SM_UL,r8)+
#                             # b9*adstock(SP_Catalog,r9)+
#                             # b10*adstock(SP_DM,r10)+
#                             # b11*adstock(SP_FSI,r11)+
#                             # b12*adstock(SP_FSI_M,r12)+
#                             b13*adstock(SP_ActionTV,r13)+
#                             b14*adstock(SP_BrandTV,r14)+
#                             b15*adstock(SP_mobiMe_marketing,r15)+
#                             b16*adstock(SP_Others,r16)+
#                             b17*adstock(IM_CatCircs,r17)+
#                             b18*adstock(IM_DMCircs,r18)+
#                             b19*adstock(IM_FSICircs,r19)
#                           , data = Modeling_data,
#                           algorithm = "LM",
#                           start     = c(b0=1,
#                                         b1=1,
#                                         b2=1,
#                                         b3=1,
#                                         b4=1,
#                                         b5=1,
#                                         b6=1,
#                                         b7=1,
#                                         b8=1,
#                                         # b9=1,
#                                         # b10=1,
#                                         # b11=1,
#                                         # b12=1,
#                                         b13=1,
#                                         b14=1,
#                                         b15=1,
#                                         b16=1,
#                                         b17=1,
#                                         b18=1,
#                                         b19=1,
#                                         r1=0,
#                                         r2=0,
#                                         r3=0,
#                                         r4=0,
#                                         r5=0,
#                                         r6=0,
#                                         r7=0,
#                                         r8=0,
#                                         # r9=0,
#                                         # r10=0,
#                                         # r11=0,
#                                         # r12=0,
#                                         r13=0,
#                                         r14=0,
#                                         r15=0,
#                                         r16=0,
#                                         r17=0,
#                                         r18=0,
#                                         r19=0),
#                           lower     = c(b0=-Inf,
#                                         b1=-Inf,
#                                         b2=-Inf,
#                                         b3=-Inf,
#                                         b4=-Inf,
#                                         b5=-Inf,
#                                         b6=-Inf,
#                                         b7=-Inf,
#                                         b8=-Inf,
#                                         # b9=-Inf,
#                                         # b10=-Inf,
#                                         # b11=-Inf,
#                                         # b12=-Inf,
#                                         b13=-Inf,
#                                         b14=-Inf,
#                                         b15=-Inf,
#                                         b16=-Inf,
#                                         b17=-Inf,
#                                         b18=-Inf,
#                                         b19=-Inf,
#                                         r1=0,
#                                         r2=0,
#                                         r3=0,
#                                         r4=0,
#                                         r5=0,
#                                         r6=0,
#                                         r7=0,
#                                         r8=0,
#                                         # r9=0,
#                                         # r10=0,
#                                         # r11=0,
#                                         # r12=0,
#                                         r13=0,
#                                         r14=0,
#                                         r15=0,
#                                         r16=0,
#                                         r17=0,
#                                         r18=0,
#                                         r19=0),
#                           upper     = c(b0=Inf,
#                                         b1=Inf,
#                                         b2=Inf,
#                                         b3=Inf,
#                                         b4=Inf,
#                                         b5=Inf,
#                                         b6=Inf,
#                                         b7=Inf,
#                                         b8=Inf,
#                                         # b9=Inf,
#                                         # b10=Inf,
#                                         # b11=Inf,
#                                         # b12=Inf,
#                                         b13=Inf,
#                                         b14=Inf,
#                                         b15=Inf,
#                                         b16=Inf,
#                                         b17=Inf,
#                                         b18=Inf,
#                                         b19=Inf,
#                                         r1=1,
#                                         r2=1,
#                                         r3=1,
#                                         r4=1,
#                                         r5=1,
#                                         r6=1,
#                                         r7=1,
#                                         r8=1,
#                                         # r9=1,
#                                         # r10=1,
#                                         # r11=1,
#                                         # r12=1,
#                                         r13=1,
#                                         r14=1,
#                                         r15=1,
#                                         r16=1,
#                                         r17=1,
#                                         r18=1,
#                                         r19=1))

# get coefficients and adstock rates
adstockrates = as.data.frame(round(coef(opt_adstock_rates1),digits = 2))

#adstockrates_revenue = as.data.frame(round(coef(opt_adstock_rates_Revenue),digits = 2))

#convert rownames into 1st column
library(data.table)
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

## create adstock variables where adstock rate >0 using optimized rates, 
## needs to be automated

#Vehicle = DT[3,]$Vehicle[[1]]

# DT[3,]$Vehicle
# 
# Modeling_data[,c(DT[2,]$Vehicle)]
# 
# Vehicle1 = as.character(DT[3,]$Vehicle)
# 
# Modeling_data[,Vehicle1[1]]

#subset(table1, gene_ID %in% accessions40$V1)

#Loop to create adstock variables and append to modeling data
for (i in 1:nrow(DT))
{
  Vehicle1 = as.character(DT[i,]$Vehicle)
  Modeling_data$x<-assign(paste(DT[i,]$Vehicle,".adstock",sep = ""), as.numeric(filter(x=Modeling_data[,Vehicle1[1]], filter=DT[i,]$opt_adstockrate, method="recursive")))
  names(Modeling_data)[names(Modeling_data) == 'x'] <- paste(Vehicle1,".adstock",sep = "")
}

### No need to run adstock variable creation separately now

# SP_Email.adstock <- as.numeric(filter(x=SP_Email, filter=0.3, method="recursive"))
# #SP_Search_L.adstock <- as.numeric(filter(x=SP_Search_L, filter=0.06, method="recursive"))
# #SP_OA_U.adstock <- as.numeric(filter(x=SP_OA_U, filter=0.3, method="recursive"))
# SP_SM_UL.adstock <- as.numeric(filter(x=SP_SM_UL, filter=0.35, method="recursive"))
# SP_mobiMe_marketing.adstock <- as.numeric(filter(x=SP_mobiMe_marketing, filter=0.15, method="recursive"))
# SP_Others.adstock <- as.numeric(filter(x=SP_Others, filter=0.3, method="recursive"))
# IM_FSICircs.adstock <- as.numeric(filter(x=IM_FSICircs, filter=0.3, method="recursive"))
# IM_DMCircs.adstock <- as.numeric(filter(x=IM_DMCircs, filter=0.3, method="recursive"))
# #SP_ActionTV.adstock <- as.numeric(filter(x=SP_ActionTV, filter=0.3, method="recursive"))
# #SP_OA_L.adstock <- as.numeric(filter(x=SP_OA_L, filter=0.42, method="recursive"))
# SP_BrandTV.adstock <- as.numeric(filter(x=SP_BrandTV, filter=0.3, method="recursive"))
# SP_Affiliates.adstock <- as.numeric(filter(x=SP_Affiliates, filter=0.3, method="recursive"))
# SP_Pl.adstock <- as.numeric(filter(x=SP_Pl, filter=0.3, method="recursive"))
# SP_Search_U.adstock <- as.numeric(filter(x=SP_Search_U, filter=0.3, method="recursive"))
# IM_CatCircs.adstock <- as.numeric(filter(x=IM_CatCircs, filter=0.15, method="recursive"))

###########################################################################
## varclus - create clusters of similar variables
###########################################################################

#club adstock and non adstock variables

#club adstock and non adstock variables
#rm(Modeling_data_withadstock)
# Modeling_data_withadstock = cbind(Modeling_data,SP_Email.adstock,
#                                   SP_SM_UL.adstock,
#                                   SP_mobiMe_marketing.adstock,
#                                   SP_Others.adstock,
#                                   IM_FSICircs.adstock,
#                                   IM_DMCircs.adstock,
#                                   SP_BrandTV.adstock,
#                                   SP_Affiliates.adstock,
#                                   SP_Pl.adstock,
#                                   SP_Search_U.adstock,
#                                   IM_CatCircs.adstock)


Modeling_data_withadstock<-Modeling_data

typeof(Modeling_data_withadstock)

#consider spend, impressions, adstock and pricing variable
#Indep_Var <- Modeling_data_withadstock[1:175,c("SP_Email.adstock","SP_Search_L","SP_OA_U","SP_SM_UL.adstock","SP_mobiMe_marketing.adstock","SP_Others.adstock","IM_FSICircs.adstock","IM_DMCircs.adstock","SP_ActionTV","SP_OA_L","SP_BrandTV.adstock","SP_Affiliates.adstock","SP_Pl.adstock","SP_Search_U.adstock","IM_CatCircs.adstock")]

Indep_Var <- Modeling_data_withadstock[1:175,c("SP_Email.adstock","SP_Search_L","SP_OA_U","SP_SM_UL.adstock","SP_mobiMe_marketing.adstock","SP_Others.adstock","IM_FSICircs.adstock","IM_DMCircs.adstock","SP_ActionTV","SP_OA_L","SP_BrandTV.adstock","SP_Affiliates.adstock","SP_Pl.adstock","SP_Search_U.adstock","IM_CatCircs.adstock")]
typeof(Indep_Var)
Indep_Var = data.matrix(Indep_Var)
typeof(Indep_Var)
#create cluster
tree <- hclustvar(Indep_Var)

plot(tree)
part2 <- cutreevar(tree, 3) 
#optimize on number of clusters,start with number of variables and optimize based on correlation<=0.4
head(part2$scores)
part2$coef$cluster2
#predict(part2, X.quanti = X1[test, ], X.quali = X2[test, ])

##cluster 1 has negtaive co-efficient for SP_Email.adstock, consider cluster 1 variables
##separately in the model

res3<-rcorr(as.matrix(part2$scores))
flattenCorrMatrix(res3$r, res3$P) #check correlation among clusters
rm(Data_varclus)
Data_varclus <- as.data.frame(cbind(Modeling_data_withadstock[1:175,]$Online_Units ,Modeling_data_withadstock[1:175,]$Revenue,Modeling_data_withadstock[1:175,]$PR_Online_PRU,Modeling_data_withadstock[1:175,]$PR_Online_PMU, part2$scores))
res4<-rcorr(as.matrix(Data_varclus))
flattenCorrMatrix(res4$r, res4$P) 

names(Data_varclus)[1]<-"Online_Units"
names(Data_varclus)[2]<-"Revenue"
names(Data_varclus)[3]<-"PR_Online_PRU"
names(Data_varclus)[4]<-"PR_Online_PMU"

## Adding another 20 rows to data_varclus as this would be needed for future forecast later

forecastdata = Modeling_data[176:195,]

#Data_varclus1 = rbind(Data_varclus,)

typeof(Data_varclus)

###################################################################
## Run GAM models on clustered data
###################################################################

#use mgcv package for gam models

## include margin variable as well

gam1<-gam(Revenue~s(cluster1,k=3) + s(cluster2, k=3) + s(cluster3, k=3)
          + s(PR_Online_PMU,k=3),
          data = Data_varclus[1:175,])

#gam1<-gam(Online_Units~s(SP_Affiliates) + s(SP_OA_L), data = Modeling_data[1:175,])
summary(gam1)

# gam.check runs a simple simulation based check on the basis dimensions, which can help 
# to flag up terms for which k is too low
gam.check(gam1)

# Low p-value (k-index<1) may
# indicate that k is too low, especially if edf is close to k'.

#Plotting the Model
#to partition the Plotting Window

#se stands for standard error Bands
plot(gam1,se=TRUE)

gam1$coefficients

#running insignificant clusters without smoothing to check if they come out significant
gam2<-gam(Revenue~ s(cluster1,k=3) + s(cluster2,k=3) + cluster3
          +s(PR_Online_PMU,k=3),
          data = Data_varclus[1:175,])

summary(gam2)

gam2$coefficients

## clusters 3 is not significant.
# variables from clusters 3 to be used separately in the modeling phase

# Running gam by considering variables from clusters 3 separately

# also consider cluster 1 variables separately as SP_Email.adstock has a negative coefficient
# while others in cluster 1 are positive coefficients


# Add required variables to the data

Data_varclus = cbind(Data_varclus,SP_OA_U=Modeling_data_withadstock[1:175,]$SP_OA_U,
                     SP_SM_UL.adstock=Modeling_data_withadstock[1:175,]$SP_SM_UL.adstock,
                     SP_mobiMe_marketing.adstock=Modeling_data_withadstock[1:175,]$SP_mobiMe_marketing.adstock,
                     SP_Email.adstock=Modeling_data_withadstock[1:175,]$SP_Email.adstock,
                     SP_Others.adstock=Modeling_data_withadstock[1:175,]$SP_Others.adstock,
                     SP_BrandTV.adstock=Modeling_data_withadstock[1:175,]$SP_BrandTV.adstock,
                     SP_Search_L=Modeling_data_withadstock[1:175,]$SP_Search_L,
                     IM_FSICircs.adstock=Modeling_data_withadstock[1:175,]$IM_FSICircs.adstock,
                     IM_DMCircs.adstock=Modeling_data_withadstock[1:175,]$IM_DMCircs.adstock,
                     SP_ActionTV=Modeling_data_withadstock[1:175,]$SP_ActionTV,
                     SP_OA_L=Modeling_data_withadstock[1:175,]$SP_OA_L,
                     SP_Affiliates.adstock=Modeling_data_withadstock[1:175,]$SP_Affiliates.adstock,
                     SP_Pl.adstock=Modeling_data_withadstock[1:175,]$SP_Pl.adstock,
                     SP_Search_U.adstock=Modeling_data_withadstock[1:175,]$SP_Search_U.adstock,
                     IM_CatCircs.adstock=Modeling_data_withadstock[1:175,]$IM_CatCircs.adstock)

gam3<-gam(Revenue~#cluster1
            SP_Email.adstock+SP_Others.adstock+SP_BrandTV.adstock
          +s(cluster2,k=3)
          #cluster 3
          +SP_OA_U+SP_SM_UL.adstock+SP_mobiMe_marketing.adstock
          +s(PR_Online_PMU,k=3),
          data = Data_varclus[1:175,])

summary(gam3) # cluster 2 ,margin, SP_Email.adstock and SP_OA_U variables come out significant

plot(gam3, se=TRUE)

str(plotgam3)


gam3$smooth



#########################################################################################
## try log, 1/x, sqrt and polynomial transforms on cluster variables and check which one
## gives best fit
#########################################################################################

#### cluster 1

#without transformation
model_cluster1 <- lm(Online_Units ~ cluster1,data = Data_varclus)

#log transformation
log_model_cluster1 <- lm(Online_Units ~ log(Data_varclus$cluster1+3),data = Data_varclus)

#sqrt transformation
sqrt_model_cluster1 <- lm(Online_Units ~ sqrt(Data_varclus$cluster1+3),data = Data_varclus)

#1/x transformation
fractional_model_cluster1 <- lm(Online_Units ~ 1/Data_varclus$cluster1,data = Data_varclus)

#polynomial transformation
poly_model_cluster1 <- lm(Online_Units ~ poly(Data_varclus$cluster1,2),data = Data_varclus)

summary(poly_model_cluster1)

##no transform needed


#### cluster 2

#without transformation
model_cluster2 <- lm(Revenue ~ cluster2,data = Data_varclus)

#log transformation
log_model_cluster2 <- lm(Revenue ~ log(Data_varclus$cluster2+4),data = Data_varclus)

#sqrt transformation
sqrt_model_cluster2 <- lm(Revenue ~ sqrt(Data_varclus$cluster2+4),data = Data_varclus)

#1/x transformation
fractional_model_cluster2 <- lm(Revenue ~ 1/Data_varclus$cluster2,data = Data_varclus)

#polynomial transformation
poly_model_cluster2 <- lm(Revenue ~ poly(Data_varclus$cluster2,2),data = Data_varclus)

summary(poly_model_cluster2)

poly_model_cluster2

## polynomial transform

#### cluster 3

#without transformation
model_cluster3 <- lm(Online_Units ~ cluster3,data = Data_varclus)

#log transformation
log_model_cluster3 <- lm(Online_Units ~ log(Data_varclus$cluster3+3),data = Data_varclus)

#sqrt transformation
sqrt_model_cluster3 <- lm(Online_Units ~ sqrt(Data_varclus$cluster3+3),data = Data_varclus)

#1/x transformation
fractional_model_cluster3 <- lm(Online_Units ~ 1/Data_varclus$cluster3,data = Data_varclus)

#polynomial transformation
poly_model_cluster3 <- lm(Online_Units ~ poly(Data_varclus$cluster3,2),data = Data_varclus)

summary(log_model_cluster3)


#### cluster 4

#without transformation
model_cluster4 <- lm(Online_Units ~ cluster4,data = Data_varclus)

#log transformation
log_model_cluster4 <- lm(Online_Units ~ log(Data_varclus$cluster4+3),data = Data_varclus)

#sqrt transformation
sqrt_model_cluster4 <- lm(Online_Units ~ sqrt(Data_varclus$cluster4+3),data = Data_varclus)

#1/x transformation
fractional_model_cluster4 <- lm(Online_Units ~ 1/Data_varclus$cluster4,data = Data_varclus)

#polynomial transformation
poly_model_cluster4 <- lm(Online_Units ~ poly(Data_varclus$cluster4,2),data = Data_varclus)

summary(log_model_cluster4)

## log or sqrt transform

### Pricing variable 

#without transformation
model_pricing <- lm(Online_Units ~ PR_Online_PRU,data = Data_varclus)

#log transformation
log_model_pricing <- lm(Online_Units ~ log(PR_Online_PRU),data = Data_varclus)

#sqrt transformation
sqrt_model_pricing <- lm(Online_Units ~ sqrt(PR_Online_PRU),data = Data_varclus)

#1/x transformation
fractional_model_pricing <- lm(Online_Units ~ 1/PR_Online_PRU,data = Data_varclus)

#polynomial transformation
poly_model_pricing <- lm(Online_Units ~ poly(PR_Online_PRU,2),data = Data_varclus)

summary(log_model_pricing)

## 2nd order polynomial or log

### Margin variable 

#without transformation
model_margin <- lm(Revenue ~ PR_Online_PMU,data = Data_varclus)

#log transformation
log_model_margin <- lm(Revenue ~ log(PR_Online_PMU),data = Data_varclus)

#sqrt transformation
sqrt_model_margin <- lm(Online_Units ~ sqrt(PR_Online_PMU),data = Data_varclus)

#1/x transformation
fractional_model_margin <- lm(Online_Units ~ 1/PR_Online_PRU,data = Data_varclus)

#polynomial transformation
poly_model_margin <- lm(Revenue ~ poly(PR_Online_PMU,2),data = Data_varclus)

summary(poly_model_margin)

## log for margin

### SP_Email.adstock

#without transformation
model_SP_Email.adstock <- lm(Revenue ~ SP_Email.adstock,data = Data_varclus)

#log transformation
log_model_SP_Email.adstock <- lm(Revenue ~ log(SP_Email.adstock),data = Data_varclus)

#sqrt transformation
sqrt_model_SP_Email.adstock <- lm(Revenue ~ sqrt(SP_Email.adstock),data = Data_varclus)

#1/x transformation
fractional_model_SP_Email.adstock <- lm(Online_Units ~ 1/SP_Email.adstock,data = Data_varclus)

#polynomial transformation
poly_model_SP_Email.adstock <- lm(Revenue ~ poly(SP_Email.adstock,2),data = Data_varclus)

summary(fractional_model_SP_Email.adstock)

# log

### SP_Email.adstock

#without transformation
model_SP_OA_U <- lm(Revenue ~ SP_OA_U,data = Data_varclus)

#log transformation
log_model_SP_OA_U <- lm(Revenue ~ log(SP_OA_U),data = Data_varclus)

#sqrt transformation
sqrt_model_SP_Email.adstock <- lm(Revenue ~ sqrt(SP_Email.adstock),data = Data_varclus)

#1/x transformation
fractional_model_SP_Email.adstock <- lm(Online_Units ~ 1/SP_Email.adstock,data = Data_varclus)

#polynomial transformation
poly_model_SP_OA_U <- lm(Revenue ~ poly(SP_OA_U,2),data = Data_varclus)

summary(poly_model_SP_OA_U)


### SP_Search_L - linear

#without transformation
model_SP_Search_L <- lm(Revenue ~ SP_Search_L,data = Data_varclus)

#log transformation
log_model_SP_Search_L <- lm(Revenue ~ log(SP_Search_L),data = Data_varclus)

#sqrt transformation
sqrt_model_SP_Email.adstock <- lm(Revenue ~ sqrt(SP_Email.adstock),data = Data_varclus)

#1/x transformation
fractional_model_SP_Email.adstock <- lm(Online_Units ~ 1/SP_Email.adstock,data = Data_varclus)

#polynomial transformation
poly_model_SP_Search_L <- lm(Revenue ~ poly(SP_Search_L,2),data = Data_varclus)

summary(log_model_SP_Search_L)

### IM_FSICircs.adstock - poly

#without transformation
model_IM_FSICircs.adstock <- lm(Revenue ~ IM_FSICircs.adstock,data = Data_varclus)

#log transformation
log_model_IM_FSICircs.adstock <- lm(Revenue ~ log(IM_FSICircs.adstock+2),data = Data_varclus)

#sqrt transformation
sqrt_model_SP_Email.adstock <- lm(Revenue ~ sqrt(SP_Email.adstock),data = Data_varclus)

#1/x transformation
fractional_model_SP_Email.adstock <- lm(Online_Units ~ 1/SP_Email.adstock,data = Data_varclus)

#polynomial transformation
poly_model_IM_FSICircs.adstock <- lm(Revenue ~ poly(IM_FSICircs.adstock,2),data = Data_varclus)

summary(poly_model_IM_FSICircs.adstock)


### IM_DMCircs.adstock - linear

#without transformation
model_IM_DMCircs.adstock <- lm(Revenue ~ IM_DMCircs.adstock,data = Data_varclus)

#log transformation
log_model_IM_DMCircs.adstock <- lm(Revenue ~ log(IM_DMCircs.adstock+2),data = Data_varclus)

#sqrt transformation
sqrt_model_SP_Email.adstock <- lm(Revenue ~ sqrt(SP_Email.adstock),data = Data_varclus)

#1/x transformation
fractional_model_SP_Email.adstock <- lm(Online_Units ~ 1/SP_Email.adstock,data = Data_varclus)

#polynomial transformation
poly_model_IM_DMCircs.adstock <- lm(Revenue ~ poly(IM_DMCircs.adstock,2),data = Data_varclus)

summary(poly_model_IM_DMCircs.adstock)

### SP_ActionTV - log+2

#without transformation
model_SP_ActionTV <- lm(Revenue ~ SP_ActionTV,data = Data_varclus)

#log transformation
log_model_SP_ActionTV <- lm(Revenue ~ log(SP_ActionTV+2),data = Data_varclus)

#sqrt transformation
sqrt_model_SP_Email.adstock <- lm(Revenue ~ sqrt(SP_Email.adstock),data = Data_varclus)

#1/x transformation
fractional_model_SP_Email.adstock <- lm(Online_Units ~ 1/SP_Email.adstock,data = Data_varclus)

#polynomial transformation
poly_model_SP_ActionTV <- lm(Revenue ~ poly(SP_ActionTV,2),data = Data_varclus)

summary(poly_model_SP_ActionTV)

### SP_OA_L - poly

#without transformation
model_SP_OA_L <- lm(Revenue ~ SP_OA_L,data = Data_varclus)

#log transformation
log_model_SP_OA_L <- lm(Revenue ~ log(SP_OA_L+2),data = Data_varclus)

#sqrt transformation
sqrt_model_SP_Email.adstock <- lm(Revenue ~ sqrt(SP_Email.adstock),data = Data_varclus)

#1/x transformation
fractional_model_SP_Email.adstock <- lm(Online_Units ~ 1/SP_Email.adstock,data = Data_varclus)

#polynomial transformation
poly_model_SP_OA_L <- lm(Revenue ~ poly(SP_OA_L,2),data = Data_varclus)

summary(poly_model_SP_OA_L)

### SP_Affiliates.adstock - poly

#without transformation
model_SP_Affiliates.adstock <- lm(Revenue ~ SP_Affiliates.adstock,data = Data_varclus)

#log transformation
log_model_SP_Affiliates.adstock <- lm(Revenue ~ log(SP_Affiliates.adstock+2),data = Data_varclus)

#sqrt transformation
sqrt_model_SP_Email.adstock <- lm(Revenue ~ sqrt(SP_Email.adstock),data = Data_varclus)

#1/x transformation
fractional_model_SP_Email.adstock <- lm(Online_Units ~ 1/SP_Email.adstock,data = Data_varclus)

#polynomial transformation
poly_model_SP_Affiliates.adstock <- lm(Revenue ~ poly(SP_Affiliates.adstock,2),data = Data_varclus)

summary(poly_model_SP_Affiliates.adstock)


### SP_Pl.adstock - poly

#without transformation
model_SP_Pl.adstock <- lm(Revenue ~ SP_Pl.adstock,data = Data_varclus)

#log transformation
log_model_SP_Pl.adstock <- lm(Revenue ~ log(SP_Pl.adstock+2),data = Data_varclus)

#sqrt transformation
sqrt_model_SP_Email.adstock <- lm(Revenue ~ sqrt(SP_Email.adstock),data = Data_varclus)

#1/x transformation
fractional_model_SP_Email.adstock <- lm(Online_Units ~ 1/SP_Email.adstock,data = Data_varclus)

#polynomial transformation
poly_model_SP_Pl.adstock <- lm(Revenue ~ poly(SP_Pl.adstock,2),data = Data_varclus)

summary(poly_model_SP_Pl.adstock)


### SP_Search_U.adstock - poly

#without transformation
model_SP_Search_U.adstock <- lm(Revenue ~ SP_Search_U.adstock,data = Data_varclus)

#log transformation
log_model_SP_Search_U.adstock <- lm(Revenue ~ log(SP_Search_U.adstock+2),data = Data_varclus)

#sqrt transformation
sqrt_model_SP_Email.adstock <- lm(Revenue ~ sqrt(SP_Email.adstock),data = Data_varclus)

#1/x transformation
fractional_model_SP_Email.adstock <- lm(Online_Units ~ 1/SP_Email.adstock,data = Data_varclus)

#polynomial transformation
poly_model_SP_Search_U.adstock <- lm(Revenue ~ poly(SP_Search_U.adstock,2),data = Data_varclus)

summary(poly_model_SP_Search_U.adstock)

### IM_CatCircs.adstock - log+2

#without transformation
model_IM_CatCircs.adstock <- lm(Revenue ~ IM_CatCircs.adstock,data = Data_varclus)

#log transformation
log_model_IM_CatCircs.adstock <- lm(Revenue ~ log(IM_CatCircs.adstock+2),data = Data_varclus)

#sqrt transformation
sqrt_model_SP_Email.adstock <- lm(Revenue ~ sqrt(SP_Email.adstock),data = Data_varclus)

#1/x transformation
fractional_model_SP_Email.adstock <- lm(Online_Units ~ 1/SP_Email.adstock,data = Data_varclus)

#polynomial transformation
poly_model_IM_CatCircs.adstock <- lm(Revenue ~ poly(IM_CatCircs.adstock,2),data = Data_varclus)

summary(poly_model_IM_CatCircs.adstock)

#Individual transforms for cluster 2 components
# SP_Search_L - linear
# IM_FSICircs.adstock - poly
# IM_DMCircs.adstock - linear
# SP_ActionTV - log+2
# SP_OA_L - poly
# SP_Affiliates.adstock - poly
# SP_Pl.adstock - poly
# SP_Search_U.adstock - poly
# IM_CatCircs.adstock - log+2

# Do required transforms on data, log transform on margin and polynomial on cluster 2, try log
# for SP_Email.adstock and SP_OA_U

#remove effect of intercept from cluster2
Data_varclus$cluster2 = Data_varclus$cluster2+5.386334e+00

Data_varclus$logMargin = log(Data_varclus$PR_Online_PMU)
Data_varclus$log_SP_Email.adstock = log(Data_varclus$SP_Email.adstock)
Data_varclus$log_SP_OA_U = log(Data_varclus$SP_OA_U)
Data_varclus$poly_cluster2 = 67238823*Data_varclus$cluster2+23873947*Data_varclus$cluster2*Data_varclus$cluster2

Data_varclus$poly_IM_FSICircs.adstock = 46326665*Data_varclus$IM_FSICircs.adstock+14497880*Data_varclus$IM_FSICircs.adstock*Data_varclus$IM_FSICircs.adstock
Data_varclus$log_SP_ActionTV = log(Data_varclus$SP_ActionTV+2)
Data_varclus$poly_SP_OA_L = 59823128*Data_varclus$SP_OA_L+27221359*Data_varclus$SP_OA_L*Data_varclus$SP_OA_L
Data_varclus$poly_SP_Affiliates.adstock = 58599269*Data_varclus$SP_Affiliates.adstock+17407127*Data_varclus$SP_Affiliates.adstock*Data_varclus$SP_Affiliates.adstock
Data_varclus$poly_SP_Pl.adstock = 43862840*Data_varclus$SP_Pl.adstock+27455511*Data_varclus$SP_Pl.adstock*Data_varclus$SP_Pl.adstock
Data_varclus$poly_SP_Search_U.adstock = 23332707*Data_varclus$SP_Search_U.adstock+4361288*Data_varclus$SP_Search_U.adstock*Data_varclus$SP_Search_U.adstock
Data_varclus$log_IM_CatCircs.adstock = log(Data_varclus$IM_CatCircs.adstock+2)




#plot(Data_varclus$logMargin,Data_varclus$Revenue)

# #cluster1
# SP_Email.adstock+SP_Others.adstock+IM_DMCircs.adstock+SP_BrandTV.adstock+IM_CatCircs
# #cluster 2
# +SP_OA_U.adstock+SP_SM_UL.adstock+SP_mobiMe_marketing
# +s(cluster3,k=3)
# +s(PR_Online_PRU,k=3),
############################################################################
#################################### UCM ####################################
############################################################################

library(rucm)

#detach("package:rucm", unload=TRUE)

#remove.packages("rucm")

#devtools::install_github("kaushikrch/rucm")

#range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#scaling function
range02 <- function(x){x/(max(x))}

#scale all columns
# for(i in names(Data_varclus_scaled1))
# {
#   Data_varclus_scaled1[[i]]= range02(Data_varclus[[i]])
# }

Data_varclus_scaled2 = Data_varclus

for(i in names(Data_varclus))
{
  Data_varclus_scaled2[[i]]= range02(Data_varclus[[i]])
}

# Data_varclus$cluster2/max(Data_varclus$cluster2)#= scaled value
# 
# Data_varclus_scaled1$cluster2*max(Data_varclus$cluster2)

#make sure online units >0, otherwise we will face error in mape calculation
Data_varclus_scaled2$Revenue = Data_varclus_scaled2$Revenue+0.001


# using transforms based on best identified in step above

Data_varclus_scaled2 = cbind(Data_varclus_scaled2,Flag_BTS=                    Modeling_data_withadstock[1:175,]$Flag_BTS,
                             Flag_PPD=                   Modeling_data_withadstock[1:175,]$Flag_PPD,
                             Flag_PD=                   Modeling_data_withadstock[1:175,]$Flag_PD,
                             Flag_Consumer_Units_PBF=     Modeling_data_withadstock[1:175,]$Flag_Consumer_Units_PBF,
                             Flag_H1=                     Modeling_data_withadstock[1:175,]$Flag_H1,
                             Flag_XP_UP=                  Modeling_data_withadstock[1:175,]$Flag_XP_UP,
                             Flag_H2=                Modeling_data_withadstock[1:175,]$Flag_H2,
                             Flag_CM=                    Modeling_data_withadstock[1:175,]$Flag_CM,
                             Flag_Consumer_Units_BF=      Modeling_data_withadstock[1:175,]$Flag_Consumer_Units_BF,
                             Flag_Hours_Cut=             Modeling_data_withadstock[1:175,]$Flag_Hours_Cut,
                             Flag_Sunday_Cut=            Modeling_data_withadstock[1:175,]$Flag_Sunday_Cut,
                             Flag_xpup=                   Modeling_data_withadstock[1:175,]$Flag_xpup,
                             Flag_Pre.President.Day.flag= Modeling_data_withadstock[1:175,]$Flag_Pre.President.Day.flag,
                             Flag_Presidents.day.flag=   Modeling_data_withadstock[1:175,]$Flag_Presidents.day.flag,
                             Flag_BTS.Flag=              Modeling_data_withadstock[1:175,]$Flag_BTS.Flag,
                             Flag_Pre.BF.Flag=            Modeling_data_withadstock[1:175,]$Flag_Pre.BF.Flag,
                             Flag_Black.Friday.flag=    Modeling_data_withadstock[1:175,]$Flag_Black.Friday.flag,
                             Flag_CM=                     Modeling_data_withadstock[1:175,]$Flag_CM,
                             Flag_Green_Monday=          Modeling_data_withadstock[1:175,]$Flag_Green_Monday,
                             Flag_MLK_Day=              Modeling_data_withadstock[1:175,]$Flag_MLK_Day,
                             Flag_GoodFriday =             Modeling_data_withadstock[1:175,]$Flag_GoodFriday
)

Data_varclus_scaled2$Flag_BTS  =as.factor(Data_varclus_scaled2$Flag_BTS)
Data_varclus_scaled2$Flag_PPD=as.factor(Data_varclus_scaled2$Flag_PPD)
Data_varclus_scaled2$Flag_PD=as.factor(Data_varclus_scaled2$Flag_PD)
Data_varclus_scaled2$Flag_Consumer_Units_PBF=as.factor(Data_varclus_scaled2$Flag_Consumer_Units_PBF)
Data_varclus_scaled2$Flag_H1=as.factor(Data_varclus_scaled2$Flag_H1)
Data_varclus_scaled2$Flag_XP_UP=as.factor(Data_varclus_scaled2$Flag_XP_UP)
Data_varclus_scaled2$Flag_H2=as.factor(Data_varclus_scaled2$Flag_H2)
Data_varclus_scaled2$Flag_CM=as.factor(Data_varclus_scaled2$Flag_CM)
Data_varclus_scaled2$Flag_Consumer_Units_BF=as.factor(Data_varclus_scaled2$Flag_Consumer_Units_BF)
Data_varclus_scaled2$Flag_Hours_Cut=as.factor(Data_varclus_scaled2$Flag_Hours_Cut)
Data_varclus_scaled2$Flag_Sunday_Cut=as.factor(Data_varclus_scaled2$Flag_Sunday_Cut)
Data_varclus_scaled2$Flag_xpup=as.factor(Data_varclus_scaled2$Flag_xpup)
Data_varclus_scaled2$Flag_Pre.President.Day.flag=as.factor(Data_varclus_scaled2$Flag_Pre.President.Day.flag)
Data_varclus_scaled2$Flag_Presidents.day.flag=as.factor(Data_varclus_scaled2$Flag_Presidents.day.flag)
Data_varclus_scaled2$Flag_BTS.Flag=as.factor(Data_varclus_scaled2$Flag_BTS.Flag)
Data_varclus_scaled2$Flag_Pre.BF.Flag=as.factor(Data_varclus_scaled2$Flag_Pre.BF.Flag)
Data_varclus_scaled2$Flag_Black.Friday.flag=as.factor(Data_varclus_scaled2$Flag_Black.Friday.flag)
Data_varclus_scaled2$Flag_CM=as.factor(Data_varclus_scaled2$Flag_CM)
Data_varclus_scaled2$Flag_Green_Monday=as.factor(Data_varclus_scaled2$Flag_Green_Monday)
Data_varclus_scaled2$Flag_MLK_Day=as.factor(Data_varclus_scaled2$Flag_MLK_Day)
Data_varclus_scaled2$Flag_GoodFriday=as.factor(Data_varclus_scaled2$Flag_GoodFriday)

summary(Data_varclus_scaled2)

ucm_fit = ucm(formula = Revenue ~# #cluster1
                logMargin+log_SP_Email.adstock+log_SP_OA_U+poly_cluster2
              ,data = Data_varclus_scaled2[1:157,],slope=TRUE,season = TRUE,season.length = 52
)

# ucm_fit = ucm(formula = Revenue ~# #cluster1
#                 logMargin+log_SP_Email.adstock+log_SP_OA_U+cluster2
#               ,data = Data_varclus_scaled2[1:157,],slope=TRUE,season = TRUE,season.length = 52
# )
# 
# ucm_fit = ucm(formula = Revenue ~# #cluster1
#                 logMargin+cluster2
#               ,data = Data_varclus_scaled2[1:157,],slope=TRUE,season = TRUE,season.length = 52
# )


ucm_fit = ucm(formula = Revenue ~ poly_cluster2+logMargin,
              data = Data_varclus_scaled2[1:157,],slope=TRUE,season = TRUE,season.length = 52
)

#consider cluster 2 components separately
ucm_fit = ucm(formula = Revenue ~ SP_Search_L+
                IM_DMCircs.adstock+
                poly_IM_FSICircs.adstock+
                log_SP_ActionTV+
                poly_SP_OA_L+
                poly_SP_Affiliates.adstock+
                poly_SP_Pl.adstock+
                poly_SP_Search_U.adstock+
                log_IM_CatCircs.adstock+
                logMargin,
              data = Data_varclus_scaled2[1:157,],slope=TRUE,season = TRUE,season.length = 52
)

#only significant variables
ucm_fit = ucm(formula = Revenue ~ IM_DMCircs.adstock+
                poly_SP_OA_L+
                poly_SP_Affiliates.adstock+
                logMargin,
              data = Data_varclus_scaled2[1:157,],slope=TRUE,season = TRUE,season.length = 52
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
mean(abs((Data_varclus_scaled2[1:157,]$Revenue-pd)/Data_varclus_scaled2[1:157,]$Revenue))


names(Data_varclus_scaled2)
# SP_Others.adstock+IM_DMCircs.adstock+SP_mobiMe_marketing+
#   cluster3+logPricing

Independents = Data_varclus_scaled2[,c(2,6,14,15,16,17)]

names(Independents_forecast)
Independents_forecast = Data_varclus_scaled2[158:175,]

Independents_forecast = Independents_forecast[,c(23,24)]

#read future scaled values
fv = read_csv("D:/Dropbox (eClerx Services Ltd.)/My Documents/abhi/COE/MMX tool/futureforecastscaledvalues.csv")

names(fv)

fv = fv[,c(3,13)]

Independents_forecast = rbind(Independents_forecast,fv)

Newdata <- SSModel(rep(NA,nrow(Independents_forecast)) ~ poly_cluster2+logMargin+ 
                     SSMtrend(2, Q = list(ucm_fit$est.var.level, 
                                          ucm_fit$est.var.slope)) 
                   + SSMseasonal(52, Q = ucm_fit$est.var.season), 
                   H = ucm_fit$irr.var, data=Independents_forecast)

Newdata <- SSModel(rep(NA,nrow(Independents_forecast)) ~ SP_Search_L+
                     IM_DMCircs.adstock+
                     poly_IM_FSICircs.adstock+
                     log_SP_ActionTV+
                     poly_SP_OA_L+
                     poly_SP_Affiliates.adstock+
                     poly_SP_Pl.adstock+
                     poly_SP_Search_U.adstock+
                     log_IM_CatCircs.adstock+
                     logMargin+ 
                     SSMtrend(2, Q = list(ucm_fit$est.var.level, 
                                          ucm_fit$est.var.slope)) 
                   + SSMseasonal(52, Q = ucm_fit$est.var.season), 
                   H = ucm_fit$irr.var, data=Independents_forecast)

#future forecast
pd_test = predict(ucm_fit$model,newdata = Newdata)

pd_test*49228620

#mape on train data
mean(abs((Data_varclus_scaled2[158:175,]$Revenue-pd_test)/Data_varclus_scaled2[158:175,]$Revenue))

plot(Data_varclus_scaled2[1:157,]$Revenue, ylab = "Sales",type = "l")
lines(pd, col = "blue")
legend("topright", legend = c("Observed sales","Predicted"), col = c("black","blue"), lty = 1)

##unscale the data to get actual contributions

## SP_Others.adstock contribution and ROI

#SP_Others.adstock_contribution = as.data.frame(0.05848974*Data_varclus_scaled2$SP_Others.adstock*max(Data_varclus$SP_Others.adstock)*max(Data_varclus$Online_Units))

# SP_Others.adstock_contribution = as.data.frame(0.05848974*Data_varclus_scaled2$SP_Others.adstock*max(Data_varclus$Online_Units))
# 
# names(SP_Others.adstock_contribution)[1]<- "SP_Others.adstock_contribution"
# 
# SP_Others.adstock_contribution$SP_Others.adstock_ROI = (SP_Others.adstock_contribution$SP_Others.adstock_contribution/Modeling_data_withadstock[1:175,]$SP_Others.adstock)*Data_varclus$PR_Online_PRU
# 
# SP_Others.adstock_contribution$SP_Others.adstock_Revenue = (SP_Others.adstock_contribution$SP_Others.adstock_contribution)*Data_varclus$PR_Online_PRU


## SP_mobiMe_marketing contribution and ROI
# rm(SP_mobiMe_marketing_contribution)
# SP_mobiMe_marketing_contribution = as.data.frame(0.02834696*Data_varclus_scaled2$SP_mobiMe_marketing*max(Data_varclus$Online_Units))
# names(SP_mobiMe_marketing_contribution)[1]<- "SP_mobiMe_marketing_contribution"
# 
# SP_mobiMe_marketing_contribution$SP_mobiMe_marketing_ROI = (SP_mobiMe_marketing_contribution$SP_mobiMe_marketing_contribution/Modeling_data_withadstock[1:175,]$SP_mobiMe_marketing)*Data_varclus$PR_Online_PRU
# 
# SP_mobiMe_marketing_contribution$SP_mobiMe_marketing_Revenue = (SP_mobiMe_marketing_contribution$SP_mobiMe_marketing_contribution)*Data_varclus$PR_Online_PRU

## IM_DMCircs.adstock contribution and ROI
# rm(IM_DMCircs.adstock_contribution)
# IM_DMCircs.adstock_contribution = as.data.frame( 0.01994304*Data_varclus_scaled2$IM_DMCircs.adstock*max(Data_varclus$Online_Units))
# names(IM_DMCircs.adstock_contribution)[1]<- "IM_DMCircs.adstock_contribution"
# 
# IM_DMCircs.adstock_contribution$IM_DMCircs.adstock_ROI = (IM_DMCircs.adstock_contribution$IM_DMCircs.adstock_contribution/Raw_Modeling_data[1:175,]$SP_DM)*Data_varclus$PR_Online_PRU
# 
# IM_DMCircs.adstock_contribution$IM_DMCircs.adstock_Revenue = (IM_DMCircs.adstock_contribution$IM_DMCircs.adstock_contribution)*Data_varclus$PR_Online_PRU

write.csv(Modeling_data_withadstock,"Modeling_data_withadstock.csv",row.names = FALSE)
## calculate contribution of cluster 2
rm(cluster2_revenue)
cluster2_revenue = as.data.frame(0.3219157*Data_varclus_scaled2$poly_cluster2*max(Data_varclus$Revenue))

names(cluster2_revenue)[1]<- "cluster2_revenue"
rm(cluster2_revenue_sum)
#do not consider intercept while summing
cluster2_revenue_sum = as.data.frame(1.986889e-06*Modeling_data_withadstock[1:175,]$SP_Search_L+1.953291e-02*Modeling_data_withadstock[1:175,]$IM_FSICircs.adstock+8.087802e-08*Modeling_data_withadstock[1:175,]$IM_DMCircs.adstock+3.796504e-07*Modeling_data_withadstock[1:175,]$SP_ActionTV+4.951850e-06*Modeling_data_withadstock[1:175,]$SP_OA_L+2.117773e-06*Modeling_data_withadstock[1:175,]$SP_Affiliates.adstock+ 1.006293e-05*Modeling_data_withadstock[1:175,]$SP_Pl.adstock+ 6.419491e-06*Modeling_data_withadstock[1:175,]$SP_Search_U.adstock+ 2.575625e-08*Modeling_data_withadstock[1:175,]$IM_CatCircs.adstock)

names(cluster2_revenue_sum)[1]<- "sum_cluster2_revenue"
rm(cluster2_rev)
cluster2_rev = cbind(cluster2_revenue,cluster2_revenue_sum)

# calculate the multiplier
cluster2_rev$ratio = cluster2_rev$cluster2_revenue/cluster2_rev$sum_cluster2_revenue

write.csv(cluster2_rev$ratio,"cluster2_rev_ratio.csv",row.names = FALSE)

## calculate contribution for cluster 2 vehicles

#cluster2_rev$cluster2_baseline_revenue = -5.386334e+00*cluster2_rev$ratio

cluster2_rev$SP_Search_L_revenue = cluster2_rev$ratio*1.986889e-06*Modeling_data_withadstock[1:175,]$SP_Search_L

cluster2_rev$IM_FSICircs.adstock_revenue = cluster2_rev$ratio*1.953291e-02*Modeling_data_withadstock[1:175,]$IM_FSICircs.adstock

cluster2_rev$IM_DMCircs.adstock_revenue = cluster2_rev$ratio*8.087802e-08*Modeling_data_withadstock[1:175,]$IM_DMCircs.adstock

cluster2_rev$SP_ActionTV_revenue = cluster2_rev$ratio*3.796504e-07*Modeling_data_withadstock[1:175,]$SP_ActionTV

cluster2_rev$SP_OA_L_revenue = cluster2_rev$ratio*4.951850e-06*Modeling_data_withadstock[1:175,]$SP_OA_L

cluster2_rev$SP_Affiliates.adstock_revenue = cluster2_rev$ratio*2.117773e-06*Modeling_data_withadstock[1:175,]$SP_Affiliates.adstock

cluster2_rev$SP_Pl.adstock_revenue = cluster2_rev$ratio*1.006293e-05*Modeling_data_withadstock[1:175,]$SP_Pl.adstock

cluster2_rev$SP_Search_U.adstock_revenue = cluster2_rev$ratio*6.419491e-06*Modeling_data_withadstock[1:175,]$SP_Search_U.adstock

cluster2_rev$IM_CatCircs.adstock_revenue = cluster2_rev$ratio*2.575625e-08*Modeling_data_withadstock[1:175,]$IM_CatCircs.adstock

cluster2_rev$sum_revenue = cluster2_rev$SP_Search_L_revenue+cluster2_rev$IM_FSICircs.adstock_revenue+cluster2_rev$IM_DMCircs.adstock_revenue+cluster2_rev$SP_ActionTV_revenue+cluster2_rev$SP_OA_L_revenue+cluster2_rev$SP_Affiliates.adstock_revenue+cluster2_rev$SP_Pl.adstock_revenue+cluster2_rev$SP_Search_U.adstock_revenue+cluster2_rev$IM_CatCircs.adstock_revenue


# ROI for cluster 2 vehicles, (contribution/spend)*price per unit OR Revenue/spend

cluster2_rev$SP_Search_L_ROI = cluster2_rev$SP_Search_L_revenue/Raw_Modeling_data[1:175,]$SP_Search_L

cluster2_rev$IM_FSICircs.adstock_ROI = cluster2_rev$IM_FSICircs.adstock_revenue/Raw_Modeling_data[1:175,]$SP_FSI

cluster2_rev$IM_DMCircs.adstock_ROI = cluster2_rev$IM_DMCircs.adstock_revenue/Raw_Modeling_data[1:175,]$SP_DM

cluster2_rev$SP_ActionTV_ROI = cluster2_rev$SP_ActionTV_revenue/Raw_Modeling_data[1:175,]$SP_ActionTV

cluster2_rev$SP_OA_L_ROI = cluster2_rev$SP_OA_L_revenue/Raw_Modeling_data[1:175,]$SP_OA_L

cluster2_rev$SP_Affiliates.adstock_ROI = cluster2_rev$SP_Affiliates.adstock_revenue/Modeling_data_withadstock[1:175,]$SP_Affiliates.adstock

cluster2_rev$SP_Pl.adstock_ROI = cluster2_rev$SP_Pl.adstock_revenue/Modeling_data_withadstock[1:175,]$SP_Pl.adstock

cluster2_rev$SP_Search_U.adstock_ROI = cluster2_rev$SP_Search_U.adstock_revenue/Modeling_data_withadstock[1:175,]$SP_Search_U.adstock

cluster2_rev$IM_CatCircs.adstock_ROI = cluster2_rev$IM_CatCircs.adstock_revenue/Raw_Modeling_data[1:175,]$SP_Catalog


## Pricing contribution
# rm(pricing_contribution)
# pricing_contribution = as.data.frame(-2.22668195*Data_varclus_scaled2$logPricing*max(Data_varclus$Online_Units))
# 
# names(pricing_contribution)[1]<-"pricing_contribution"
# 
# #to make sure, we see +ve pricing contribution, subtract each value from highest negative value
# 
# min_pricing_contri = min(pricing_contribution$pricing_contribution)
# 
# pricing_contribution$pricing_contribution_rescaled = pricing_contribution$pricing_contribution-min_pricing_contri

## Margin contribution
rm(margin_contribution)
margin_contribution = as.data.frame(-0.146304923*Data_varclus_scaled2$logMargin*max(Data_varclus$Revenue))

names(margin_contribution)[1]<-"margin_contribution"

#to make sure, we see +ve pricing contribution, subtract each value from highest negative value

min_margin_contri = min(margin_contribution$margin_contribution)

margin_contribution$margin_contribution_rescaled = margin_contribution$margin_contribution-min_margin_contri


## SP_Search_L contribution
rm(SP_Search_L_contribution)
SP_Search_L_contribution = as.data.frame(0.023255157*Data_varclus_scaled2$SP_Search_L*max(Data_varclus$Revenue))

names(SP_Search_L_contribution)[1]<-"SP_Search_L_contribution"

## IM_DMCircs.adstock contribution
rm(IM_DMCircs.adstock_contribution)
IM_DMCircs.adstock_contribution = as.data.frame(0.018216504*Data_varclus_scaled2$IM_DMCircs.adstock*max(Data_varclus$Revenue))

names(IM_DMCircs.adstock_contribution)[1]<-"IM_DMCircs.adstock_contribution"


## trend contribution

plot(ucm_fit$s.level)

plot(ucm_fit$s.slope)

#slope+level gives trend

trend = ucm_fit$s.level+ucm_fit$s.slope

plot(trend)

trend_component = as.data.frame(trend)

names(trend_component)[1]<-"trend_component"

write.csv(trend_component,"trend_component.csv",row.names = FALSE)

trend_contribution = as.data.frame(trend*max(Data_varclus$Revenue))

names(trend_contribution)[1]<-"trend_contribution"

##seasonality contribution

plot(ucm_fit$s.season)

seasonality_component = as.data.frame(ucm_fit$s.season)

names(seasonality_component)[1] <- "seasonality_component"

write.csv(seasonality_component,"seasonality_component.csv",row.names = FALSE)

seasonality_contribution = as.data.frame(ucm_fit$s.season*max(Data_varclus$Revenue))

names(seasonality_contribution)[1]<-"seasonality_contribution"

#rescaling to see +ve seasonality contributions

min_seasonality_contri = min(seasonality_contribution$seasonality_contribution)

seasonality_contribution$rescaled_seasonality_contri = seasonality_contribution$seasonality_contribution-min_seasonality_contri

##############
rm(MMX_Results_HLData)
MMX_Results_HLData = cbind(cluster2_rev,margin_contribution,
                           #pricing_contribution,
                           #seasonality_contribution,trend_contribution,
                           #spends
                           #spend_SP_Others.adstock=Modeling_data_withadstock[1:175,]$SP_Others.adstock,
                           #spend_SP_mobiMe_marketing=Modeling_data_withadstock[1:175,]$SP_mobiMe_marketing,
                           spend_SP_DM=Raw_Modeling_data[1:175,]$SP_DM,
                           spend_SP_FSI=Raw_Modeling_data[1:175,]$SP_FSI,
                           spend_SP_ActionTV=Modeling_data_withadstock[1:175,]$SP_ActionTV,
                           spend_SP_OA_L=Modeling_data_withadstock[1:175,]$SP_OA_L,
                           spend_SP_Affiliates.adstock=Modeling_data_withadstock[1:175,]$SP_Affiliates.adstock,
                           spend_SP_Search_U.adstock=Modeling_data_withadstock[1:175,]$SP_Search_U.adstock,
                           spend_SP_Pl.adstock=Modeling_data_withadstock[1:175,]$SP_Pl.adstock,
                           spend_SP_Search_L=Modeling_data_withadstock[1:175,]$SP_Search_L,
                           spend_SP_Catalog=Raw_Modeling_data[1:175,]$SP_Catalog)

MMX_Results_HLData = MMX_Results_HLData[1:157,]

MMX_Results_HLData = cbind(MMX_Results_HLData,seasonality_contribution,trend_contribution)

write.csv(MMX_Results_HLData,"MMX_Results_HLData_RevModelv3withmargin.csv",row.names = FALSE)
#############

plot(x = Raw_Modeling_data[1:175,]$SP_Search_U,
     y = cluster3_contri$SP_Search_U_Revenue,type = "p")



ggplot(MMX_Results_HLData,aes(x = spend_SP_Search_L,y = SP_Search_L_Revenue )) + geom_line() + xlab("") + ylab("Online Units")
# 
scatterplot(SP_Search_L_Revenue ~ spend_SP_Search_L, data= MMX_Results_HLData, 
            xlab="Spend_Search_L ", ylab="Revenue_Search_L", 
            main="Enhanced Scatter Plot", labels=row.names(MMX_Results_HLData))

names(MMX_Results_HLData)

# library(forecast)
# arima_fit <- auto.arima(Data_varclus$Online_Units, xreg = Data_varclus[,c(2:4,6,10)])
# 
# mean(abs(arima_fit$residuals/arima_fit$x))
#                         
# arima_fit$coef
# 
# lm_model = lm(Online_Units~cluster1+poly(cluster2,2)+log(cluster4+3)
#    + poly(PR_Online_PRU,2) + poly(SP_OA_U.adstock,2),data = Data_varclus)
# 
# summary(lm_model)

# lm_model = lm(Online_Units~cluster1+cluster2+cluster4
#                   + PR_Online_PRU + SP_OA_U.adstock,data = Data_varclus)

circs_allocaton <- function (allocation,data) {  #Functiaon added by Arabinda
  
  splitx = strsplit(allocation, "__")   #Get details for alocation
  circ <- splitx[[1]][1]
  a <- as.numeric(splitx[[1]][2])
  b <- as.numeric(splitx[[1]][3])
  c <- as.numeric(splitx[[1]][4])
  
  x <- 0
  #Alocate 
  for(i in 1:nrow(data))
  {
    if(i == 1)
    { x[i] <- data[i,circ] *(a/10)  }
    
    else if(i == 2)
    { x[i] <- (data[(i-1),circ]*(b/10)) + 
      (data[i,circ]*(a/10))     }
    else
    { x[i] <- (data[(i-2),circ]*(c/10)) + 
      (data[(i-1),circ]*(b/10)) + 
      (data[i,circ]*(a/10))     }
    
    i = i+1
  }
  return (x)    #Return alocated list
}

BrandTV_ADstock <- function (Adstock,data)  {           #Functiaon added by Arabinda
  
  splitx = strsplit(Adstock, "__")   #Get details for alocation
  circ <- splitx[[1]][1]
  a <- as.numeric(splitx[[1]][2])
  
  options(scipen = 999)
  
  x <- 0
  #Alocate 
  for(i in 1:nrow(data))
  {
    if(i == 1)
    { x[i] <- data[i,circ]  }
    
    else
    { 
      x[i] <- data[(i),circ]
      x[i] <- (x[(i-1)]*(a)) + x[i] 
      x[i] <- round(as.numeric(x[i]), 0) 
    }
    
    i = i+1
  }
  return (x)    #Return alocated list
}

IM_FSICircs_9_1 <-  circs_allocaton(allocation = "IM_FSICircs__9__1__0",data = Modeling_data)

TV_Adstock_.1 <-  BrandTV_ADstock(Adstock = "IM_FSICircs__.8", data = as.data.frame(Modeling_data))
