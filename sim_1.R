
library(OPTtesting)
library(RSpectra)
set.seed(20213)
p = 1000
n_col = 10
pi_0 = 0.1
pi_1 = 0.7
pi_2 = 0.2
nu_0 = 0
nu_1 = -0.5
nu_2 = 1.2
tau_sqr_1 = 0.1
tau_sqr_2 = 0.1


A = matrix(rnorm(p*n_col,0,2), nrow = p, ncol = n_col, byrow = TRUE)
Sigma = A %*% t(A) + 5*diag(p) #covariance matrix
# s = svd(Sigma)
# num = 100 # dim(Sigma)
# D = diag(c(s$d[1:num],rep(0,p - num)))
# Sigma = s$u %*% D %*% t(s$v)
Sigma = cov2cor(Sigma)

eig = eigs_sym(Sigma, 999, which = "LM")

library(MASS)    
Bj_time = 100#number of Z
Z_set = NULL
ui_sets = NULL
for(i in 1:Bj_time){
  b = rmultinom(p, size = 1, prob = c(pi_0,pi_1,pi_2))
  ui = b[1,]*nu_0 + b[2,]*rnorm(p, mean = nu_1, sd = sqrt(tau_sqr_1)) + b[3,]*rnorm(p, mean = nu_2, sd = sqrt(tau_sqr_2)) # actual situation
  #ui = c(rep(nu_0,p*pi_0),rnorm(p*pi_1, mean = nu_1, sd = sqrt(tau_sqr_1)),rnorm(p*pi_2, mean = nu_2, sd = sqrt(tau_sqr_2)))
  Z = mvrnorm(n = 1,c(ui), Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE) 
  ui_sets = cbind(ui_sets,ui)
  Z_set = cbind(Z_set,Z)
}


# write.csv(ui_sets, "~/Desktop/simulation_fund/Z and ui/ui_set_4.csv")
# write.csv(Z_set, "~/Desktop/simulation_fund/Z and ui/Z_set_4.csv")

#####################################
#Parameter estimation

eig = eigs_sym(Sigma, 999, which = "LM")


eig_tol = 1
set_nu = 0
set1= c(0:40)*0.02 + 0.05
set2 = c(0:40)*0.02 + 0.05
setp = c(0.05,0.1,0.15)
eig = eigs_sym(Sigma, 999, which = "LM")
eig_value = 0.43 #0.12 #the smallest eigen values used in the calculation
sim_size = 3000#simulation size
level = 0.1 #significance level

set.seed(2022)
results = NULL
for (j in 1:Bj_time ){
  Z = Z_set[,j]
  p = length(Z)
  ui = ui_sets[,j]
best_set = AEB(Z,Sigma,eig = eig,eig_tol = eig_tol,set_nu = set_nu,set1= set1,set2 = set2,setp = setp)
A <- density(Z, from = -5, to = 5, width = 1, kernel = "gaussian")
B <- density(best_set$Z_hat, from = -5, to = 5, width = 1, kernel = "gaussian")
vari = TVD(A$x,A$y,B$y)

#####################################
#d-value calculation
#necessary part for calculate d-values

#d-value (p(ui <= 0 |Z))
prob_p = d_value(Z, Sigma, eig = eig, eig_value = eig_value, sim_size = sim_size, best_set = best_set)
decision_p = Optimal_procedure_3(prob_p,level)
FDP_records = FDP_compute(decision_p$ai,ui,TRUE)
results = rbind(results, c(decision_p$cj, FDP_records$FDP,FDP_records$FNP,vari))
print( c(decision_p$cj, FDP_records$FDP,FDP_records$FNP,vari))
}

results[is.na(results)] = 0
colMeans(results)

# > colMeans(results)
# [1] 156.03000000   0.09732953   0.12033919   0.03782892




library(qvalue)
library(sgof)


results_1 = NULL
for (i in 1:Bj_time ){
  
  Z = Z_set[,i]
  p_value = pnorm(Z, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
  
  rej = BH(p_value, alpha = 0.1)
  select_p = which(p_value <= p_value[order(p_value)[rej$Rejections]])
  ui = ui_sets[,i]
  
  true_p = which(ui > 0)
  
  FDR = length(setdiff(select_p,true_p))/max(length(select_p),1)

  
  FNR =  length(setdiff(true_p,select_p))/max((p - length(select_p)),1)
  


  results_1  = rbind(results_1,c(FDR,FNR,length(select_p)))
  print(c(FDR,FNR,length(select_p)))
  
}
colMeans(results_1)
# > colMeans(results_1)
# [1] 0.01646827 0.23665841 7.61000000

results_1 = NULL
for (i in 1:Bj_time ){
  
  Z = Z_set[,i]
  p_value = pnorm(Z, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
  
  
  select_p = which(qvalue(p_value )$qvalue <0.1)
  ui = ui_sets[,i]
  
  true_p = which(ui > 0)
  
  FDR = length(setdiff(select_p,true_p))/max(length(select_p),1)
  
  FNR =  length(setdiff(true_p,select_p))/max((p - length(select_p)),1)
  


  
  results_1  = rbind(results_1,c(FDR,FNR,length(select_p)))
  print(c(FDR,FNR,length(select_p)))
  
}
colMeans(results_1)
# > colMeans(results_1)
# [1] 0.01646827 0.23665841 7.61000000



set.seed(2021) 
level = 0.1
results_1 = NULL


for (i in 1:Bj_time ){
  
  
  Z = Z_set[,i]
  
  test_result = pfa.test(Z,Sigma=Sigma)
  
  
  index_test = max(which(test_result$FDP[,4] <= 0.1))
  num_rej = test_result$FDP[index_test ,2]
  
  index_1 = test_result$adjPvalue[1:142,2]
  index_2 = which(Z >0)
  select_p = intersect(index_1, index_2)
  ui = ui_sets[,i]
  
  true_p = which(ui > 0)
  
  FDR = length(setdiff(select_p,true_p))/max(length(select_p),1)
  
  FNR =  length(setdiff(true_p,select_p))/max((p - length(select_p)),1)
  
  results_1  = rbind(results_1,c(FDR,FNR,length(select_p)))
  print(c(FDR,FNR,length(select_p)))
}



colMeans(results_1)
# > colMeans(results_1)
# [1]  0.01388545  0.16790025 90.57000000










set.seed(2021) 
level = 0.1
results_1 = NULL


for (i in 1:Bj_time ){
  
  
  Z = Z_set[,i]
  
  test_result = pfa.test(Z,Sigma=Sigma,tval = exp(-c(1:80)*0.1))
  
  
  index_test = which(test_result$FDP[,4] <= 0.1)
  num_rej = max(test_result$FDP[index_test ,2])
  
  index_1 = test_result$adjPvalue[1:num_rej,2]
  index_2 = which(Z >0)
  select_p = intersect(index_1, index_2)
  ui = ui_sets[,i]
  
  true_p = which(ui > 0)
  
  FDR = length(setdiff(select_p,true_p))/max(length(select_p),1)
  
  FNR =  length(setdiff(true_p,select_p))/max((p - length(select_p)),1)
  
  results_1  = rbind(results_1,c(FDR,FNR,length(select_p)))
  print(c(FDR,FNR,length(select_p)))
}



colMeans(results_1)
# > colMeans(results_1)
# [1]  0.003143842  0.191964508 61.640000000



