library(devtools)
load_all("~/Desktop/OPTtesting")
library(OPTtesting)
library(readr)
all_funds <- read_csv("~/Desktop/OPTtesing_data/all_funds_10_21.csv")
fund_type <- read_csv("~/Desktop/OPTtesing_data/fund_type_10_21.csv")
variables_00_19 <- read_csv("~/Desktop/OPTtesing_data/variables_10_20.csv")

fund_type_E = fund_type[which(substr(fund_type$crsp_obj_cd, 1,1)  == "E"),]
fund_type_IMO = fund_type[which(substr(fund_type$crsp_obj_cd, 1,1)  %in% c("I","M","O")),]

library(plyr)
fund= all_funds
fund = na.omit(fund)
fund =  fund[- which(fund$mret == "R"),]
fund$mret = as.numeric(fund$mret)
fund = fund[- which(fund$mret == 0),]

variables = variables_00_19
year1 = 20110120
year2 = 20201235
years = variables$dateff[which(variables$dateff >= year1 & variables$dateff <= year2)]
time = length(years)

fund = fund[which(fund$caldt >= year1 & fund$caldt <= year2),]
S = fund$crsp_fundno
coun = count(S, vars = NULL, wt_var = NULL)
S =coun$x[which(coun$freq == time)] #funds survived during these 10 years 
s2 = unique(fund_type_IMO$crsp_fundno[which(fund_type_IMO$caldt>= year1 & fund_type_IMO$caldt <= year2)])
s3 = unique(fund_type_E$crsp_fundno[which(fund_type_E$caldt>= year1 & fund_type_E$caldt <= year2)])
s3 = setdiff(s3,s2)
S = intersect(S,s3)

lgth = length(S)
fund = fund[which(fund$crsp_fundno %in% S),]

variables = variables[which(variables$dateff >= year1 & variables$dateff <= year2),]
rf = as.numeric(variables$rf) 
mktrf = as.numeric(variables$mktrf)
smb = as.numeric(variables$smb)
hml = as.numeric(variables$hml)
umd =  as.numeric(variables$umd)

fund_ana = NULL
fund_ana2 = NULL
tret_set = NULL
names = NULL
R_set = NULL
for (i in S){
  names = c(names, paste("rm", i , sep = ""))
  
  mret = fund$mret[which(fund$crsp_fundno == i)]
  rm = mret - rf  #excess returns
  fund_ana = cbind(fund_ana, rm) 
  
  lmMod <- lm(rm ~ mktrf + smb + hml + umd) 
  R_set = c(R_set , summary(lmMod)$r.squared)  # R^2
  
  sharpe = mean(rm)/sd(mret) # sharpe ratio
  fund_ana2 = c(fund_ana2,sharpe)
  
  tret = prod(mret + 1) #asset value
  tret_set  = c(tret_set,tret)
  
}



lgth2 = length(S) 
covariance = cov(fund_ana)


R = as.matrix(cbind(mktrf,smb,hml,umd)) # four-factor model
allone = matrix(c(rep(1,time)),nrow = time)
library(MASS)
n = time
Ht =  ginv(n - t(allone)%*%R%*%ginv(t(R)%*%R)%*%t(R)%*%allone)%*%t(allone) - (1/n)*t(allone)%*%R %*%ginv(t(R)%*%R - (1/n)*t(R)%*%allone%*%t(allone)%*%R)%*%t(R)
covar = covariance* as.numeric(Ht%*%t(Ht))
alpha = NULL
Z = NULL
for (i in 1:lgth2){
  a = as.numeric(Ht%*%fund_ana[,i]) 
  alpha = c(alpha, a) # alpha
  Z = c(Z,a/sqrt(covar[i,i]))  # Z
}

cor = cov2cor(covar) # covariance matrix

fund_analysis = data.frame(crsp_fundno = S, alpha  = alpha, Z = Z,sharpe = fund_ana2, value = tret_set)

Z1 = fund_analysis$Z
indexx = which(Z1 > -5 & Z1 <5) 
Sigma = cor[indexx, indexx]
Z = Z1[indexx] # select Z between -5 and 5 to estimate parameters 
p = length(Z)
library(RSpectra)


##############################
#find the best set of three-parts parameters by total variation 
set.seed(20200803)
set_nu = - c(0:3)*0.1  
set1= c(0:40)*0.01 + 0.05

set2 = c(0:40)*0.01 + 0.05

setp = c(0:6)*0.1 + 0.1

vari_l = 1

eig = eigs_sym(Sigma,119, which = "LM")
#best_set = AEB(Z,Sigma,eig,1,set_nu,set1,set2,setp)
best_set =AEB(Z,Sigma,eig = eig,eig_tol = 1,set_nu = set_nu,set1= set1,set2 = set2,setp = setp)
print(c(best_set$tau_sqr_1,best_set$tau_sqr_2,best_set$pi_0, best_set$pi_1, best_set$pi_2,best_set$nu_0, best_set$mu_1, best_set$mu_2))


A <- density(Z, from = -5, to = 5, width = 1, kernel = "gaussian")
B <- density(best_set$Z_hat, from = -5, to = 5, width = 1, kernel = "gaussian")
vari = TVD(A$x,A$y,B$y)
vari 
library(ggplot2)

print(c(best_set$tau_sqr_1,best_set$tau_sqr_2,best_set$pi_0, best_set$pi_1, best_set$pi_2,best_set$nu_0, best_set$mu_1, best_set$mu_2))

table_plot = NULL

table_plot$value =c(Z,best_set$Z_hat)
table_plot$type = c(rep("Z", length(Z)),rep("AEB",length(best_set$Z_hat)))
table_plot$type = factor(table_plot$type,  levels = c("Z", "AEB"))
table_plot = data.frame(table_plot)

p1 <-ggplot(table_plot, aes(x=value, color=type)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.1, fill="white" , binwidth=0.18
                 , size = 0.5)+
  geom_density(alpha=0.1, bw=0.2 , aes(fill = type), size = 1.2)+   scale_color_manual(values = c("#377EB8","#E41A1C" )) +   scale_fill_manual(values = c("#377EB8","#E41A1C" ))+
  xlim(-4,4)  +
  theme(text = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20)) + theme(legend.position="right") +  
  theme(legend.key.size = unit(1, 'cm'), 
        legend.title = element_text(size=20), 
        legend.text = element_text(size=20))
p1





