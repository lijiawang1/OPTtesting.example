# all the data is in OPTtesting_data.zip
library(OPTtesting)
library(readr)
all_funds <- read_csv("~/Desktop/OPTtesting_data/all_funds_10_21.csv")
fund_analysis_2000_2009_E <- read_csv("~/Desktop/OPTtesting_data/outputs/fund_analysis_2000_2009_E.csv")
fund_analysis_2001_2010_E <- read_csv("~/Desktop/OPTtesting_data/outputs/fund_analysis_2001_2010_E.csv")
fund_analysis_2002_2011_E <- read_csv("~/Desktop/OPTtesting_data/outputs/fund_analysis_2002_2011_E.csv")
fund_analysis_2003_2012_E <- read_csv("~/Desktop/OPTtesting_data/outputs/fund_analysis_2003_2012_E.csv")
fund_analysis_2004_2013_E <- read_csv("~/Desktop/OPTtesting_data/outputs/fund_analysis_2004_2013_E.csv")
fund_analysis_2005_2014_E <- read_csv("~/Desktop/OPTtesting_data/outputs/fund_analysis_2005_2014_E.csv")
fund_analysis_2006_2015_E <- read_csv("~/Desktop/OPTtesting_data/outputs/fund_analysis_2006_2015_E.csv")
fund_analysis_2007_2016_E <- read_csv("~/Desktop/OPTtesting_data/outputs/fund_analysis_2007_2016_E.csv")
fund_analysis_2008_2017_E <- read_csv("~/Desktop/OPTtesting_data/outputs/fund_analysis_2008_2017_E.csv")
fund_analysis_2009_2018_E <- read_csv("~/Desktop/OPTtesting_data/outputs/fund_analysis_2009_2018_E.csv")
load("~/Desktop/OPTtesting_data/fund_analysis_2010_2019_E.csv")
fund_analysis_2010_2019_E = fund_analysis
load("~/Desktop/OPTtesting_data/fund_analysis_2011_2020_E.csv")
fund_analysis_2011_2020_E = fund_analysis

library(OPTtesting)
level = 0.15

dynamics <-function(level,col_i){
fund = fund_analysis_2001_2010_E

S3 = fund$crsp_fundno
Z = fund$Z

Optimal_decision_1 = Optimal_procedure_3(fund$prob_1,level)
select_fund_p = S3[which(Optimal_decision_1$ai == 1)]


length(select_fund_p)

Optimal_decision_2 = Optimal_procedure_3(fund$prob_2,level)
select_fund_n = S3[which(Optimal_decision_2$ai == 1)]

length(select_fund_n)

nonselect_fund = setdiff(S3,select_fund_n)
nonselect_fund  = setdiff(nonselect_fund,select_fund_p)

table_results = NULL

#################################################

fund_ana_table= fund_analysis_2001_2010_E
set_results = NULL
index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)

set_results = c(median(fund_ana_table$alpha[index_p])*12,
                median(fund_ana_table$alpha[index_n])*12,
                median(fund_ana_table$alpha[index_non])*12)

table_results = rbind(table_results, set_results)

#################################################

fund_ana_table= fund_analysis_2002_2011_E
set_results = NULL
index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)

set_results = c(median(fund_ana_table$alpha[index_p])*12,
                median(fund_ana_table$alpha[index_n])*12,
                median(fund_ana_table$alpha[index_non])*12)

table_results = rbind(table_results, set_results)


#################################################

fund_ana_table= fund_analysis_2003_2012_E
set_results = NULL
index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)

set_results = c(median(fund_ana_table$alpha[index_p])*12,
                median(fund_ana_table$alpha[index_n])*12,
                median(fund_ana_table$alpha[index_non])*12)

table_results = rbind(table_results, set_results)


#################################################

fund_ana_table= fund_analysis_2004_2013_E
set_results = NULL
index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)

set_results = c(median(fund_ana_table$alpha[index_p])*12,
                median(fund_ana_table$alpha[index_n])*12,
                median(fund_ana_table$alpha[index_non])*12)

table_results = rbind(table_results, set_results)

#################################################

fund_ana_table= fund_analysis_2005_2014_E
set_results = NULL
index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)

set_results = c(median(fund_ana_table$alpha[index_p])*12,
                median(fund_ana_table$alpha[index_n])*12,
                median(fund_ana_table$alpha[index_non])*12)

table_results = rbind(table_results, set_results)

#################################################

fund_ana_table= fund_analysis_2006_2015_E
set_results = NULL
index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)

set_results = c(median(fund_ana_table$alpha[index_p])*12,
                median(fund_ana_table$alpha[index_n])*12,
                median(fund_ana_table$alpha[index_non])*12)

table_results = rbind(table_results, set_results)

#################################################

fund_ana_table= fund_analysis_2007_2016_E
set_results = NULL
index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)

set_results = c(median(fund_ana_table$alpha[index_p])*12,
                median(fund_ana_table$alpha[index_n])*12,
                median(fund_ana_table$alpha[index_non])*12
)

table_results = rbind(table_results, set_results)

#################################################

fund_ana_table= fund_analysis_2008_2017_E
set_results = NULL
index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)


set_results = c(median(fund_ana_table$alpha[index_p])*12,
                median(fund_ana_table$alpha[index_n])*12,
                median(fund_ana_table$alpha[index_non])*12)

table_results = rbind(table_results, set_results)

#################################################

fund_ana_table= fund_analysis_2009_2018_E
set_results = NULL
index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)

set_results = c(median(fund_ana_table$alpha[index_p])*12,
                median(fund_ana_table$alpha[index_n])*12,
                median(fund_ana_table$alpha[index_non])*12)

table_results = rbind(table_results, set_results)


#################################################

fund_ana_table= fund_analysis_2010_2019_E
set_results = NULL
index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)

set_results = c(median(fund_ana_table$alpha[index_p])*12,
                median(fund_ana_table$alpha[index_n])*12,
                median(fund_ana_table$alpha[index_non])*12
)

table_results = rbind(table_results, set_results)

fund_ana_table= fund_analysis_2011_2020_E
set_results = NULL
index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)

set_results = c(median(fund_ana_table$alpha[index_p])*12,
                median(fund_ana_table$alpha[index_n])*12,
                median(fund_ana_table$alpha[index_non])*12
)

table_results = rbind(table_results, set_results)

################################################# Figure 7 fourth plot
year_set = c("01-10","02-11", "03-12", "04-13","05-14","06-15","07-16","08-17","09-18","10-19","11-20")
table_plot = NULL
table_plot$alpha = c(table_results[,col_i])
table_plot$Year = c(rep(year_set,1))
table_plot$type = c(rep(paste0("Skilled ",level),length(year_set)))
table_plot = data.frame(table_plot)
return(table_plot)}

col_i  = 1
table_plot = rbind (dynamics(0.3,col_i ),dynamics(0.25,col_i ),dynamics(0.2,col_i ),dynamics(0.15,col_i ),dynamics(0.1,col_i ))

table_plot = data.frame(table_plot )

table_plot #OPTtesting results in table 3


#########################

# Farmtest results in table 3

all_funds <- read_csv("~/Desktop/OPTtesing_data/OPTtesting_data/all_funds.csv")
fund_type <- read_csv("~/Desktop/OPTtesing_data/OPTtesting_data/fund_type.csv")
variables_00_19 <- read_csv("~/Desktop/OPTtesing_data/OPTtesting_data/variables_00_19.csv")

fund_type_E = fund_type[which(substr(fund_type$crsp_obj_cd, 1,1)  == "E"),]
fund_type_IMO = fund_type[which(substr(fund_type$crsp_obj_cd, 1,1)  %in% c("I","M","O")),]

library(plyr)
fund= all_funds
fund = na.omit(fund)
fund =  fund[- which(fund$mret == "R"),]
fund$mret = as.numeric(fund$mret)
fund = fund[- which(fund$mret == 0),]

variables = variables_00_19
year1 = 20010120
year2 = 20101235
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
for (i in S){
  names = c(names, paste("rm", i , sep = ""))
  
  mret = fund$mret[which(fund$crsp_fundno == i)]
  rm = mret - rf  #excess returns
  fund_ana = cbind(fund_ana, rm) 
  
  
}

R = cbind(mktrf,smb,hml,umd )



library(FarmTest)


level_set =  c(0.1, 0.15,0.2,0.25,0.3)
set.seed(2022)
for (level in level_set ){
  output = farm.test(fund_ana, fX = R, p.method ="normal")
  output = farm.test(fund_ana - R %*% t(output$loadings), KX = 2, alternative = "greater", alpha = level)
  select_p = output$reject
  select_fund_p =S[select_p ]
  
  
  
  
  
  output = farm.test(fund_ana, fX = R, p.method ="normal")
  output = farm.test(fund_ana - R %*% t(output$loadings), KX = 2,alternative = "less", alpha = level)
  select_n = output$reject
  select_fund_n =S[select_n]
  
  
  
  fund = fund_analysis_2001_2010_E
  
  S3 = fund$crsp_fundno
  
  
  
  
  nonselect_fund = setdiff(S3,select_fund_n)
  nonselect_fund  = setdiff(nonselect_fund,select_fund_p)
  
  table_results = NULL
  
  #################################################
  
  fund_ana_table= fund_analysis_2001_2010_E
  set_results = NULL
  index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
  index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
  index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)
  
  set_results = c(median(fund_ana_table$alpha[index_p])*12,
                  median(fund_ana_table$alpha[index_n])*12,
                  median(fund_ana_table$alpha[index_non])*12)
  
  table_results = rbind(table_results, set_results)
  
  #################################################
  
  fund_ana_table= fund_analysis_2002_2011_E
  set_results = NULL
  index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
  index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
  index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)
  
  set_results = c(median(fund_ana_table$alpha[index_p])*12,
                  median(fund_ana_table$alpha[index_n])*12,
                  median(fund_ana_table$alpha[index_non])*12)
  
  table_results = rbind(table_results, set_results)
  
  
  #################################################
  
  fund_ana_table= fund_analysis_2003_2012_E
  set_results = NULL
  index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
  index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
  index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)
  
  set_results = c(median(fund_ana_table$alpha[index_p])*12,
                  median(fund_ana_table$alpha[index_n])*12,
                  median(fund_ana_table$alpha[index_non])*12)
  
  table_results = rbind(table_results, set_results)
  
  
  #################################################
  
  fund_ana_table= fund_analysis_2004_2013_E
  set_results = NULL
  index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
  index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
  index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)
  
  set_results = c(median(fund_ana_table$alpha[index_p])*12,
                  median(fund_ana_table$alpha[index_n])*12,
                  median(fund_ana_table$alpha[index_non])*12)
  
  table_results = rbind(table_results, set_results)
  
  #################################################
  
  fund_ana_table= fund_analysis_2005_2014_E
  set_results = NULL
  index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
  index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
  index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)
  
  set_results = c(median(fund_ana_table$alpha[index_p])*12,
                  median(fund_ana_table$alpha[index_n])*12,
                  median(fund_ana_table$alpha[index_non])*12)
  
  table_results = rbind(table_results, set_results)
  
  #################################################
  
  fund_ana_table= fund_analysis_2006_2015_E
  set_results = NULL
  index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
  index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
  index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)
  
  set_results = c(median(fund_ana_table$alpha[index_p])*12,
                  median(fund_ana_table$alpha[index_n])*12,
                  median(fund_ana_table$alpha[index_non])*12)
  
  table_results = rbind(table_results, set_results)
  
  #################################################
  
  fund_ana_table= fund_analysis_2007_2016_E
  set_results = NULL
  index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
  index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
  index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)
  
  set_results = c(median(fund_ana_table$alpha[index_p])*12,
                  median(fund_ana_table$alpha[index_n])*12,
                  median(fund_ana_table$alpha[index_non])*12
  )
  
  table_results = rbind(table_results, set_results)
  
  #################################################
  
  fund_ana_table= fund_analysis_2008_2017_E
  set_results = NULL
  index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
  index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
  index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)
  
  
  set_results = c(median(fund_ana_table$alpha[index_p])*12,
                  median(fund_ana_table$alpha[index_n])*12,
                  median(fund_ana_table$alpha[index_non])*12)
  
  table_results = rbind(table_results, set_results)
  
  #################################################
  
  fund_ana_table= fund_analysis_2009_2018_E
  set_results = NULL
  index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
  index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
  index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)
  
  set_results = c(median(fund_ana_table$alpha[index_p])*12,
                  median(fund_ana_table$alpha[index_n])*12,
                  median(fund_ana_table$alpha[index_non])*12)
  
  table_results = rbind(table_results, set_results)
  
  
  #################################################
  
  fund_ana_table= fund_analysis_2010_2019_E
  set_results = NULL
  index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
  index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
  index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)
  
  set_results = c(median(fund_ana_table$alpha[index_p])*12,
                  median(fund_ana_table$alpha[index_n])*12,
                  median(fund_ana_table$alpha[index_non])*12
  )
  
  table_results = rbind(table_results, set_results)
  
  fund_ana_table= fund_analysis_2011_2020_E
  set_results = NULL
  index_p = which(fund_ana_table$crsp_fundno %in% select_fund_p)
  index_n = which(fund_ana_table$crsp_fundno %in% select_fund_n)
  index_non = which(fund_ana_table$crsp_fundno %in% nonselect_fund)
  
  set_results = c(median(fund_ana_table$alpha[index_p])*12,
                  median(fund_ana_table$alpha[index_n])*12,
                  median(fund_ana_table$alpha[index_non])*12
  )
  
  table_results = rbind(table_results, set_results)
  print(level)
  print(table_results[,1])}

################################################# 

level_set =  c(0.1, 0.15,0.2,0.25,0.3)
set.seed(2022)
for (level in level_set ){
  output = farm.test(fund_ana, fX = R, p.method ="normal")
  output = farm.test(fund_ana - R %*% t(output$loadings), KX = 2, alternative = "greater", alpha = level)
  select_p = output$reject
  select_fund_p =S[select_p ]
  
  
  
  
  
  output = farm.test(fund_ana, fX = R, p.method ="normal")
  output = farm.test(fund_ana - R %*% t(output$loadings), KX = 2,alternative = "less", alpha = level)
  select_n = output$reject
  select_fund_n =S[select_n]
  print(c(level,length(select_fund_p),length(select_fund_n)))
  
}






