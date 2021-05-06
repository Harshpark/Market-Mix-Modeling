##################################################################################################################
############################## MMM - Units/Sales Model , Author - Harshal H. Parkhe ##############################
##################################################################################################################

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

#Load the data

Raw_Modeling_data <- read_csv("D:\\Dropbox (eClerx Services Ltd.)\\Harshal Parkhe\\Documents\\Hilton\\Abhishek_Final\\MarketMix\\input\\Modeling_data.csv", 
                              col_types = cols(Week_end = col_date(format = "%d-%m-%Y")))


names(Raw_Modeling_data)
## remove redundant variables from spends and impressions, where-ever we have both spend and
## impressions, retain impressions

#IM_CatCircs is same as SP_Catalog, hence keeping only IM_CatCircs

## IM_FSICircs, SP_FSI, SP_FSI_M are same, hence keeping only IM_FSICircs

## IM_DMCircs.adstock, SP_DM.adstock are same, hence keeping only IM_DMCircs.adstock

Modeling_data = subset(Raw_Modeling_data, select = -c(SP_Catalog,SP_FSI, SP_FSI_M, SP_DM))

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

# Optimize adstock, Solve with nlsLM
library(minpack.lm)
opt_adstock_rates = nlsLM(Online_Units~b0 + 
                            b1*adstock(SP_Affiliates,r1)+
                            b2*adstock(SP_Email,r2)+
                            b3*adstock(SP_Pl,r3)+
                            b4*adstock(SP_OA_L,r4)+
                            b5*adstock(SP_Search_L,r5)+
                            b6*adstock(SP_OA_U,r6)+
                            b7*adstock(SP_Search_U,r7)+
                            b8*adstock(SP_SM_UL,r8)+
                            #b9*adstock(SP_Catalog,r9)+
                            #b10*adstock(SP_DM,r10)+
                            #b11*adstock(SP_FSI,r11)+
                            #b12*adstock(SP_FSI_M,r12)+
                            b13*adstock(SP_ActionTV,r13)+
                            b14*adstock(SP_BrandTV,r14)+
                            b15*adstock(SP_mobiMe_marketing,r15)+
                            b16*adstock(SP_Others,r16)+
                            b17*adstock(IM_CatCircs,r17)+
                            b18*adstock(IM_DMCircs,r18)+
                            b19*adstock(IM_FSICircs,r19)
                          , data = Modeling_data,
                          algorithm = "LM",
                          start     = c(b0=1,
                                        b1=1,
                                        b2=1,
                                        b3=1,
                                        b4=1,
                                        b5=1,
                                        b6=1,
                                        b7=1,
                                        b8=1,
                                        #b9=1,
                                        #b10=1,
                                        # b11=1,
                                        # b12=1,
                                        b13=1,
                                        b14=1,
                                        b15=1,
                                        b16=1,
                                        b17=1,
                                        b18=1,
                                        b19=1,
                                        r1=0,
                                        r2=0,
                                        r3=0,
                                        r4=0,
                                        r5=0,
                                        r6=0,
                                        r7=0,
                                        r8=0,
                                        #r9=0,
                                        #r10=0,
                                        # r11=0,
                                        # r12=0,
                                        r13=0,
                                        r14=0,
                                        r15=0,
                                        r16=0,
                                        r17=0,
                                        r18=0,
                                        r19=0),
                          lower     = c(b0=-Inf,
                                        b1=-Inf,
                                        b2=-Inf,
                                        b3=-Inf,
                                        b4=-Inf,
                                        b5=-Inf,
                                        b6=-Inf,
                                        b7=-Inf,
                                        b8=-Inf,
                                        #b9=-Inf,
                                        #b10=-Inf,
                                        # b11=-Inf,
                                        # b12=-Inf,
                                        b13=-Inf,
                                        b14=-Inf,
                                        b15=-Inf,
                                        b16=-Inf,
                                        b17=-Inf,
                                        b18=-Inf,
                                        b19=-Inf,
                                        r1=0,
                                        r2=0,
                                        r3=0,
                                        r4=0,
                                        r5=0,
                                        r6=0,
                                        r7=0,
                                        r8=0,
                                        #r9=0,
                                        #r10=0,
                                        # r11=0,
                                        # r12=0,
                                        r13=0,
                                        r14=0,
                                        r15=0,
                                        r16=0,
                                        r17=0,
                                        r18=0,
                                        r19=0),
                          upper     = c(b0=Inf,
                                        b1=Inf,
                                        b2=Inf,
                                        b3=Inf,
                                        b4=Inf,
                                        b5=Inf,
                                        b6=Inf,
                                        b7=Inf,
                                        b8=Inf,
                                        #b9=Inf,
                                        #b10=Inf,
                                        # b11=Inf,
                                        # b12=Inf,
                                        b13=Inf,
                                        b14=Inf,
                                        b15=Inf,
                                        b16=Inf,
                                        b17=Inf,
                                        b18=Inf,
                                        b19=Inf,
                                        r1=1,
                                        r2=1,
                                        r3=1,
                                        r4=1,
                                        r5=1,
                                        r6=1,
                                        r7=1,
                                        r8=1,
                                        #r9=1,
                                        #r10=1,
                                        # r11=1,
                                        # r12=1,
                                        r13=1,
                                        r14=1,
                                        r15=1,
                                        r16=1,
                                        r17=1,
                                        r18=1,
                                        r19=1))

# get coefficients and adstock rates
adstockrates = as.data.frame(round(coef(opt_adstock_rates),digits = 2))

#convert rownames into 1st column
library(data.table)
adstockrates = setDT(adstockrates, keep.rownames = TRUE)[]

colnames(adstockrates)[1] <- "opt_parameters"

colnames(adstockrates)[2] <- "opt_adstockrate"

DT = as.data.table(adstockrates)
#replacing adstock rates >0.8 to 0.4 as per business intuition
DT[opt_adstockrate>0.5, opt_adstockrate := 0.3]

#retain only adstock rate rows
DT <- DT[-grep("b", DT$opt_parameters), ]

#  Use grepl to get spend and impression variables, make sure spend variables start with "SP
# impression variables start with "IM"
Spend_Impr_Variables = as.data.frame(names(Modeling_data[ , grepl( "SP|IM" , names(Modeling_data))]))

DT = cbind(DT,Spend_Impr_Variables)

colnames(DT)[3]<- "Vehicle"

## create adstock variables where adstock rate >0 using optimized rates, 
## needs to be automated

SP_Email.adstock <- as.numeric(filter(x=SP_Email, filter=0.3, method="recursive"))
#SP_Search_L.adstock <- as.numeric(filter(x=SP_Search_L, filter=0.06, method="recursive"))
SP_OA_U.adstock <- as.numeric(filter(x=SP_OA_U, filter=0.3, method="recursive"))
SP_SM_UL.adstock <- as.numeric(filter(x=SP_SM_UL, filter=0.03, method="recursive"))
SP_DM.adstock <- as.numeric(filter(x=SP_DM, filter=0.3, method="recursive"))
SP_FSI_M.adstock <- as.numeric(filter(x=SP_FSI_M, filter=0.3, method="recursive"))
#SP_mobiMe_marketing.adstock <- as.numeric(filter(x=SP_mobiMe_marketing, filter=0.08, method="recursive"))
SP_Others.adstock <- as.numeric(filter(x=SP_Others, filter=0.3, method="recursive"))
IM_FSICircs.adstock <- as.numeric(filter(x=IM_FSICircs, filter=0.3, method="recursive"))
IM_DMCircs.adstock <- as.numeric(filter(x=IM_DMCircs, filter=0.3, method="recursive"))
SP_ActionTV.adstock <- as.numeric(filter(x=SP_ActionTV, filter=0.3, method="recursive"))
SP_OA_L.adstock <- as.numeric(filter(x=SP_OA_L, filter=0.42, method="recursive"))
SP_BrandTV.adstock <- as.numeric(filter(x=SP_BrandTV, filter=0.3, method="recursive"))
SP_Affiliates.adstock <- as.numeric(filter(x=SP_Affiliates, filter=0.25, method="recursive"))
#SP_Pl.adstock <- as.numeric(filter(x=SP_Pl, filter=0.3, method="recursive"))



###########################################################################
## varclus - create clusters of similar variables
###########################################################################

#club adstock and non adstock variables

#club adstock and non adstock variables
#rm(Modeling_data_withadstock)
Modeling_data_withadstock = cbind(Modeling_data,SP_Email.adstock,
                                  SP_OA_U.adstock,
                                  SP_SM_UL.adstock,
                                  SP_Others.adstock,
                                  IM_FSICircs.adstock,
                                  IM_DMCircs.adstock,
                                  SP_ActionTV.adstock,
                                  SP_OA_L.adstock,
                                  SP_BrandTV.adstock,
                                  SP_Affiliates.adstock)

#consider spend, impressions, adstock and pricing variable
Indep_Var <- Modeling_data_withadstock[1:175,c("SP_Email.adstock","SP_OA_U.adstock","SP_SM_UL.adstock","SP_Others.adstock","IM_FSICircs.adstock","IM_DMCircs.adstock","SP_ActionTV.adstock","SP_OA_L.adstock","SP_BrandTV.adstock","SP_Affiliates.adstock","SP_Search_U","SP_Pl","SP_mobiMe_marketing","IM_CatCircs","SP_Search_L")]


Indep_Var = data.matrix(Indep_Var)

tree <- hclustvar(Indep_Var)

plot(tree)
part2 <- cutreevar(tree, 4) 
#optimize on number of clusters,start with number of variables and optimize based on correlation<=0.4
head(part2$scores)
part2$coef$cluster3
#predict(part2, X.quanti = X1[test, ], X.quali = X2[test, ])

res3<-rcorr(as.matrix(part2$scores))
flattenCorrMatrix(res3$r, res3$P) #check correlation among clusters
Data_varclus <- as.data.frame(cbind(Modeling_data_withadstock[1:175,]$Online_Units ,Modeling_data_withadstock[1:175,]$PR_Online_PRU,Modeling_data_withadstock[1:175,]$PR_Online_PMU, part2$scores))
res4<-rcorr(as.matrix(Data_varclus))
flattenCorrMatrix(res4$r, res4$P) 

names(Data_varclus)[1]<-"Online_Units"
names(Data_varclus)[2]<-"PR_Online_PRU"
names(Data_varclus)[3]<-"PR_Online_PMU"
typeof(Data_varclus)

###################################################################
## Run GAM models on clustered data
###################################################################

#use mgcv package for gam models

## include pricing variable as well

gam1<-gam(Online_Units~s(cluster1,k=3) + s(cluster2, k=3) + s(cluster3, k=3)
          + s(PR_Online_PRU,k=3),
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
gam2<-gam(Online_Units~ cluster1 + cluster2 + s(cluster3, k=3)
          +s(PR_Online_PRU,k=3),
          data = Data_varclus[1:175,])

summary(gam2)

gam2$coefficients

## clusters 1 and 2 are not significant.
# variables from clusters 1 and 2 to be used separately in the modeling phase

# Running gam by considering variables from clusters 1 and 2 separately


# Add required variables to the data

Data_varclus = cbind(Data_varclus,SP_OA_U.adstock=Modeling_data_withadstock[1:175,]$SP_OA_U.adstock,
                     SP_SM_UL.adstock=Modeling_data_withadstock[1:175,]$SP_SM_UL.adstock,
                     SP_mobiMe_marketing=Modeling_data_withadstock[1:175,]$SP_mobiMe_marketing,
                     SP_Email.adstock=Modeling_data_withadstock[1:175,]$SP_Email.adstock,
                     SP_Others.adstock=Modeling_data_withadstock[1:175,]$SP_Others.adstock,
                     IM_DMCircs.adstock=Modeling_data_withadstock[1:175,]$IM_DMCircs.adstock,
                     SP_BrandTV.adstock=Modeling_data_withadstock[1:175,]$SP_BrandTV.adstock,
                     IM_CatCircs=Modeling_data_withadstock[1:175,]$IM_CatCircs)

gam3<-gam(Online_Units~#cluster1
            SP_Email.adstock+SP_Others.adstock+IM_DMCircs.adstock+SP_BrandTV.adstock+IM_CatCircs
          #cluster 2
          +SP_OA_U.adstock+SP_SM_UL.adstock+SP_mobiMe_marketing
          +s(cluster3,k=3)
          +s(PR_Online_PRU,k=3),
          data = Data_varclus[1:175,])

summary(gam3) # only cluster 3 and pricing variables come out significant

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
model_cluster2 <- lm(Online_Units ~ cluster2,data = Data_varclus)

#log transformation
log_model_cluster2 <- lm(Online_Units ~ log(Data_varclus$cluster2+3),data = Data_varclus)

#sqrt transformation
sqrt_model_cluster2 <- lm(Online_Units ~ sqrt(Data_varclus$cluster2+3),data = Data_varclus)

#1/x transformation
fractional_model_cluster2 <- lm(Online_Units ~ 1/Data_varclus$cluster2,data = Data_varclus)

#polynomial transformation
poly_model_cluster2 <- lm(Online_Units ~ poly(Data_varclus$cluster2,2),data = Data_varclus)

summary(model_cluster2)

poly_model_cluster2

## no transform needed

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
model_margin <- lm(Online_Units ~ PR_Online_PMU,data = Data_varclus)

#log transformation
log_model_margin <- lm(Online_Units ~ log(PR_Online_PMU),data = Data_varclus)

#sqrt transformation
sqrt_model_pricing <- lm(Online_Units ~ sqrt(PR_Online_PRU),data = Data_varclus)

#1/x transformation
fractional_model_pricing <- lm(Online_Units ~ 1/PR_Online_PRU,data = Data_varclus)

#polynomial transformation
poly_model_pricing <- lm(Online_Units ~ poly(PR_Online_PRU,2),data = Data_varclus)

summary(log_model_margin)

## 2nd order polynomial or log

### SP_OA_U.adstock

#without transformation
model_SP_OA_U.adstock <- lm(Online_Units ~ SP_OA_U.adstock,data = Data_varclus)

#log transformation
log_model_pricing <- lm(Online_Units ~ log(SP_OA_U.adstock),data = Data_varclus)

#sqrt transformation
sqrt_model_pricing <- lm(Online_Units ~ sqrt(SP_OA_U.adstock),data = Data_varclus)

#1/x transformation
fractional_model_pricing <- lm(Online_Units ~ 1/SP_OA_U.adstock,data = Data_varclus)

#polynomial transformation
poly_model_SP_OA_U.adstock <- lm(Online_Units ~ poly(SP_OA_U.adstock,2),data = Data_varclus)

summary(poly_model_SP_OA_U.adstock)


# polynomial of 2nd degree or log

# Do required transforms on data
Data_varclus = cbind(Data_varclus,PR_Online_PMU=Modeling_data_withadstock[1:175,]$PR_Online_PMU)
#Data_varclus$logcluster4 = log(Data_varclus$cluster4+3)
Data_varclus$logPricing = log(Data_varclus$PR_Online_PRU)
Data_varclus$logMargin = log(Data_varclus$PR_Online_PMU)
#Data_varclus$log_SP_OA_U.adstock = log(Data_varclus$SP_OA_U.adstock)


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
Data_varclus_scaled2$Online_Units = Data_varclus_scaled2$Online_Units+0.001


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

ucm_fit = ucm(formula = Online_Units ~# #cluster1
                SP_Email.adstock+SP_Others.adstock+IM_DMCircs.adstock+SP_BrandTV.adstock+IM_CatCircs
              #cluster 2
              +SP_OA_U.adstock+SP_SM_UL.adstock+SP_mobiMe_marketing
              +cluster3
              #+logPricing
              +logMargin
              #+Flag_BTS+
              # Flag_PPD+
              # Flag_PD+
              #   Flag_Consumer_Units_PBF+
              #   Flag_H1+
              #   Flag_XP_UP+
              #   Flag_H2+
              #   Flag_CM+
              #   Flag_Consumer_Units_BF+
              #   #Flag_Hours_Cut # has only one level, ,no variation
              #   Flag_Sunday_Cut+
              #   Flag_xpup+
              #   Flag_Pre.President.Day.flag+
              #   Flag_Presidents.day.flag+
              #   Flag_BTS.Flag+
              #   Flag_Pre.BF.Flag+
              #   Flag_Black.Friday.flag+
              #   Flag_Green_Monday+
              #   Flag_MLK_Day+
              #   Flag_GoodFriday
              ,data = Data_varclus_scaled2[1:157,],slope=TRUE,season = TRUE,season.length = 52
)

## flag variables do not have much variation in the data

# lm_model = lm(Online_Units~SP_Email.adstock+SP_Others.adstock+IM_DMCircs.adstock+SP_BrandTV.adstock+IM_CatCircs
#               #cluster 2
#               +SP_OA_U.adstock+SP_SM_UL.adstock+SP_mobiMe_marketing
#               +cluster3
#               +logPricing+Flag_BTS+
#                 Flag_PPD+
#                 Flag_PD+
#                 Flag_Consumer_Units_PBF+
#                 Flag_H1+
#                 Flag_XP_UP+
#                 Flag_H2+
#                 Flag_CM+
#                 Flag_Consumer_Units_BF+
#                 #Flag_Hours_Cut # has only one level, ,no variation
#                 Flag_Sunday_Cut+
#                 Flag_xpup+
#                 Flag_Pre.President.Day.flag+
#                 Flag_Presidents.day.flag+
#                 Flag_BTS.Flag+
#                 Flag_Pre.BF.Flag+
#                 Flag_Black.Friday.flag+
#                 Flag_Green_Monday+
#                 Flag_MLK_Day+
#                 Flag_GoodFriday,data = Data_varclus_scaled2)
# 
# summary(lm_model)
# 
# Data_varclus_scaled2$Flag_BTS_test = Data_varclus_scaled2$Flag_BTS

#consider only significant variables

ucm_fit = ucm(formula = Online_Units ~ SP_mobiMe_marketing+
                cluster3+logMargin,
              data = Data_varclus_scaled2[1:157,],slope=TRUE,season = TRUE,season.length = 52
)



# names(Data_varclus_scaled2)
# 11,8,5,14
## since cluster 1 coefficient from ucm model is +ve, and cluster 1 has SP_Email.adstock with 
## negative coefficient and SP_Others.adstock with positive coefficient
## consider cluster 1 vehicles separately in ucm model and check the results

# ucm_fit = ucm(formula = Online_Units ~ 
#                 #cluster1
#                 SP_Email.adstock+SP_Others.adstock
#               +cluster2
#               +logPricing,
#               data = Data_varclus_scaled2,slope=TRUE,season = TRUE,season.length = 52
# )
# 
# # SP_Email.adstock is not significant
# 
# ucm_fit = ucm(formula = Online_Units ~ 
#                 #cluster1
#                 #SP_Email.adstock+
#                 SP_Others.adstock
#               +cluster2
#               +logPricing,
#               data = Data_varclus_scaled2,slope=TRUE,season = TRUE,season.length = 52
# )

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
mean(abs((Data_varclus_scaled2[1:157,]$Online_Units-pd)/Data_varclus_scaled2[1:157,]$Online_Units))


names(Data_varclus_scaled2)
# SP_Others.adstock+IM_DMCircs.adstock+SP_mobiMe_marketing+
#   cluster3+logPricing

#Independents = Data_varclus_scaled2[,c(1,5,8,16)]
Independents = Data_varclus_scaled2[,c("Online_Units","cluster3","SP_mobiMe_marketing","logMargin")]

Independents_forecast = Independents[158:175,]

Newdata <- SSModel(rep(NA,nrow(Independents_forecast)) ~ SP_mobiMe_marketing
                   +cluster3+logMargin + 
                     SSMtrend(2, Q = list(ucm_fit$est.var.level, 
                                          ucm_fit$est.var.slope)) 
                   + SSMseasonal(52, Q = ucm_fit$est.var.season), 
                   H = ucm_fit$irr.var, data=Independents_forecast)

#future forecast
pd_test = predict(ucm_fit$model,newdata = Newdata)

#mape on train data
mean(abs((Data_varclus_scaled2[158:175,]$Online_Units-pd_test)/Data_varclus_scaled2[158:175,]$Online_Units))

plot(Data_varclus_scaled2$Online_Units, ylab = "Sales",type = "l")
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
rm(SP_mobiMe_marketing_contribution)
SP_mobiMe_marketing_contribution = as.data.frame(0.02834696*Data_varclus_scaled2$SP_mobiMe_marketing*max(Data_varclus$Online_Units))
names(SP_mobiMe_marketing_contribution)[1]<- "SP_mobiMe_marketing_contribution"

SP_mobiMe_marketing_contribution$SP_mobiMe_marketing_ROI = (SP_mobiMe_marketing_contribution$SP_mobiMe_marketing_contribution/Modeling_data_withadstock[1:175,]$SP_mobiMe_marketing)*Data_varclus$PR_Online_PRU

SP_mobiMe_marketing_contribution$SP_mobiMe_marketing_Revenue = (SP_mobiMe_marketing_contribution$SP_mobiMe_marketing_contribution)*Data_varclus$PR_Online_PRU

## IM_DMCircs.adstock contribution and ROI
# rm(IM_DMCircs.adstock_contribution)
# IM_DMCircs.adstock_contribution = as.data.frame( 0.01994304*Data_varclus_scaled2$IM_DMCircs.adstock*max(Data_varclus$Online_Units))
# names(IM_DMCircs.adstock_contribution)[1]<- "IM_DMCircs.adstock_contribution"
# 
# IM_DMCircs.adstock_contribution$IM_DMCircs.adstock_ROI = (IM_DMCircs.adstock_contribution$IM_DMCircs.adstock_contribution/Raw_Modeling_data[1:175,]$SP_DM)*Data_varclus$PR_Online_PRU
# 
# IM_DMCircs.adstock_contribution$IM_DMCircs.adstock_Revenue = (IM_DMCircs.adstock_contribution$IM_DMCircs.adstock_contribution)*Data_varclus$PR_Online_PRU


## calculate contribution of cluster 3
rm(cluster3_contribution)
cluster3_contribution = as.data.frame(0.12631737*Data_varclus_scaled2$cluster3*max(Data_varclus$Online_Units))

names(cluster3_contribution)[1]<- "cluster3_contrib"
rm(cluster3_contrib_sum)
cluster3_contrib_sum = as.data.frame(-5.436320e+00+2.004699e-02*Modeling_data_withadstock[1:175,]$IM_FSICircs.adstock+4.283946e-07*Modeling_data_withadstock[1:175,]$SP_ActionTV.adstock+3.263225e-06*Modeling_data_withadstock[1:175,]$SP_OA_L.adstock+2.295075e-06*Modeling_data_withadstock[1:175,]$SP_Affiliates.adstock+8.599869e-06*Modeling_data_withadstock[1:175,]$SP_Search_U+1.261864e-05*Modeling_data_withadstock[1:175,]$SP_Pl+ 2.021992e-06*Modeling_data_withadstock[1:175,]$SP_Search_L)

names(cluster3_contrib_sum)[1]<- "sum_cluster3_contrib"
rm(cluster3_contri)
cluster3_contri = cbind(cluster3_contribution,cluster3_contrib_sum)

# calculate the multiplier
cluster3_contri$ratio = cluster3_contri$cluster3_contrib/cluster3_contri$sum_cluster3_contrib

## calculate contribution for cluster 2 vehicles

cluster3_contri$cluster3_baseline_contribution = -5.436320e+00*cluster3_contri$ratio

cluster3_contri$IM_FSICircs.adstock_contribution = cluster3_contri$ratio*2.004699e-02*Modeling_data_withadstock[1:175,]$IM_FSICircs.adstock

cluster3_contri$SP_ActionTV.adstock_contribution = cluster3_contri$ratio*4.283946e-07*Modeling_data_withadstock[1:175,]$SP_ActionTV.adstock

cluster3_contri$SP_OA_L.adstock_contribution = cluster3_contri$ratio*3.263225e-06*Modeling_data_withadstock[1:175,]$SP_OA_L.adstock

cluster3_contri$SP_Affiliates.adstock_contribution = cluster3_contri$ratio*2.295075e-06*Modeling_data_withadstock[1:175,]$SP_Affiliates.adstock

cluster3_contri$SP_Search_U_contribution = cluster3_contri$ratio*8.599869e-06*Modeling_data_withadstock[1:175,]$SP_Search_U

cluster3_contri$SP_Pl_contribution = cluster3_contri$ratio*1.261864e-05*Modeling_data_withadstock[1:175,]$SP_Pl

cluster3_contri$SP_Search_L_contribution = cluster3_contri$ratio*2.021992e-06*Modeling_data_withadstock[1:175,]$SP_Search_L

cluster3_contri$sum_contribution = cluster3_contri$cluster3_baseline_contribution+cluster3_contri$IM_FSICircs.adstock_contribution+cluster3_contri$SP_ActionTV.adstock_contribution+ cluster3_contri$SP_OA_L.adstock_contribution +cluster3_contri$SP_Affiliates.adstock_contribution+cluster3_contri$SP_Search_U_contribution  +cluster3_contri$SP_Pl_contribution+cluster3_contri$SP_Search_L_contribution

# ROI for cluster 3 vehicles, (contribution/spend)*price per unit OR Revenue/spend

#get price per unit
cluster3_contri$Priceperunit = Modeling_data_withadstock[1:175,]$PR_Online_PRU

cluster3_contri$IM_FSICircs.adstock_ROI = (cluster3_contri$IM_FSICircs.adstock_contribution/Raw_Modeling_data[1:175,]$SP_FSI)*cluster3_contri$Priceperunit

cluster3_contri$SP_ActionTV.adstock_ROI = (cluster3_contri$SP_ActionTV.adstock_contribution/Modeling_data_withadstock[1:175,]$SP_ActionTV.adstock)*cluster3_contri$Priceperunit

cluster3_contri$SP_OA_L.adstock_ROI = (cluster3_contri$SP_OA_L.adstock_contribution/Modeling_data_withadstock[1:175,]$SP_OA_L.adstock)*cluster3_contri$Priceperunit

cluster3_contri$SP_Affiliates.adstock_ROI = (cluster3_contri$SP_Affiliates.adstock_contribution/Modeling_data_withadstock[1:175,]$SP_Affiliates.adstock)*cluster3_contri$Priceperunit

cluster3_contri$SP_Search_U_ROI = (cluster3_contri$SP_Search_U_contribution/Modeling_data_withadstock[1:175,]$SP_Search_U)*cluster3_contri$Priceperunit

cluster3_contri$SP_Pl_ROI = (cluster3_contri$SP_Pl_contribution/Modeling_data_withadstock[1:175,]$SP_Pl)*cluster3_contri$Priceperunit

cluster3_contri$SP_Search_L_ROI = (cluster3_contri$SP_Search_L_contribution/Modeling_data_withadstock[1:175,]$SP_Search_L)*cluster3_contri$Priceperunit



## Revenue for each vehicle

cluster3_contri$IM_FSICircs.adstock_Revenue = (cluster3_contri$IM_FSICircs.adstock_contribution)*cluster3_contri$Priceperunit

cluster3_contri$SP_ActionTV.adstock_Revenue = (cluster3_contri$SP_ActionTV.adstock_contribution)*cluster3_contri$Priceperunit

cluster3_contri$SP_OA_L.adstock_Revenue = (cluster3_contri$SP_OA_L.adstock_contribution)*cluster3_contri$Priceperunit

cluster3_contri$SP_Affiliates.adstock_Revenue = (cluster3_contri$SP_Affiliates.adstock_contribution)*cluster3_contri$Priceperunit

cluster3_contri$SP_Search_U_Revenue = (cluster3_contri$SP_Search_U_contribution)*cluster3_contri$Priceperunit

cluster3_contri$SP_Pl_Revenue = (cluster3_contri$SP_Pl_contribution)*cluster3_contri$Priceperunit

cluster3_contri$SP_Search_L_Revenue = (cluster3_contri$SP_Search_L_contribution)*cluster3_contri$Priceperunit

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
margin_contribution = as.data.frame(-0.43261891*Data_varclus_scaled2$logMargin*max(Data_varclus$Online_Units))

names(margin_contribution)[1]<-"margin_contribution"

#to make sure, we see +ve pricing contribution, subtract each value from highest negative value

min_margin_contri = min(margin_contribution$margin_contribution)

margin_contribution$margin_contribution_rescaled = margin_contribution$margin_contribution-min_margin_contri


## trend contribution

plot(ucm_fit$s.level)

plot(ucm_fit$s.slope)

#slope+level gives trend

trend = ucm_fit$s.level+ucm_fit$s.slope

plot(trend)

trend_contribution = as.data.frame(trend*max(Data_varclus$Online_Units))

names(trend_contribution)[1]<-"trend_contribution"

##seasonality contribution

plot(ucm_fit$s.season)

seasonality_contribution = as.data.frame(ucm_fit$s.season*max(Data_varclus$Online_Units))

names(seasonality_contribution)[1]<-"seasonality_contribution"

#rescaling to see +ve seasonality contributions

min_seasonality_contri = min(seasonality_contribution$seasonality_contribution)

seasonality_contribution$rescaled_seasonality_contri = seasonality_contribution$seasonality_contribution-min_seasonality_contri

##############
rm(MMX_Results_HLData)
MMX_Results_HLData = cbind(SP_mobiMe_marketing_contribution,
      #IM_DMCircs.adstock_contribution,
      cluster3_contri,margin_contribution,
      #pricing_contribution,
      #seasonality_contribution,trend_contribution,
      #spends
      #spend_SP_Others.adstock=Modeling_data_withadstock[1:175,]$SP_Others.adstock,
      spend_SP_mobiMe_marketing=Modeling_data_withadstock[1:175,]$SP_mobiMe_marketing,
      spend_SP_DM=Raw_Modeling_data[1:175,]$SP_DM,
      spend_SP_FSI=Raw_Modeling_data[1:175,]$SP_FSI,
      spend_SP_ActionTV.adstock=Modeling_data_withadstock[1:175,]$SP_ActionTV.adstock,
      spend_SP_OA_L.adstock=Modeling_data_withadstock[1:175,]$SP_OA_L.adstock,
      spend_SP_Affiliates.adstock=Modeling_data_withadstock[1:175,]$SP_Affiliates.adstock,
      spend_SP_Search_U=Modeling_data_withadstock[1:175,]$SP_Search_U,
      spend_SP_Pl=Modeling_data_withadstock[1:175,]$SP_Pl,
      spend_SP_Search_L=Modeling_data_withadstock[1:175,]$SP_Search_L)

MMX_Results_HLData = MMX_Results_HLData[1:157,]

MMX_Results_HLData = cbind(MMX_Results_HLData,seasonality_contribution,trend_contribution)

write.csv(MMX_Results_HLData,"MMX_Results_HLData_withmargin_22082019.csv",row.names = FALSE)
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
