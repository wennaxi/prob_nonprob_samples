

rm(list = ls())



library(dplyr)
# library(mpr)
# library(betareg)
library(survey)

library(foreach)
library(doParallel)


library(ggplot2) #split dataset into equal sized subgroups (weight 4)








###generate data
set.seed(1)
N = 50000
nsim = 5000
ncore = 20

scenario = 2



####################1 Generate the Population#################### 
population = as.data.frame(matrix(NA, nrow = N, ncol = 11))
names(population) = c('ID', 'S1', 'S2', 'Y1', 'Y2', 'Y3', 'Y4', 'X1', 'X2', 'X3', 'X4')
head(population)
population$ID = rep(1:N)
population$S1 = c(rep(0, N / 2), rep(1, N / 2))
population$S2 = c(rep(1, 1000), rep(2, 1400), rep(3, 1800), rep(4, 2200), 
                  rep(5, 2600), rep(6, 3000), rep(7, 5000), rep(8, 8000),
                  rep(9, 10000), rep(10, 15000)) #2 urban counties
population$X1 = rnorm(n = N, mean = 20, sd = 3)
population$X2 = c(rbinom(n = N / 2, size = 1, p = .9), rbinom(n = N / 2, size = 1, p = .65))
population$X3 = c(t(rmultinom(n = N / 2, size = 1, prob = c(.45, .3, .25))) %*% rep(1:3),
                  t(rmultinom(n = N / 2, size = 1, prob = c(.25, .3, .45))) %*% rep(1:3))
population$X4 = t(rmultinom(n = N, size = 1, prob = c(.1, .3, .6))) %*% rep(1:3)

x3.2 = ifelse(population$X3 == 2, 1, 0)
x3.3 = ifelse(population$X3 == 3, 1, 0)
x4.2 = ifelse(population$X4 == 2, 1, 0)
x4.3 = ifelse(population$X4 == 3, 1, 0)

population$Y1 = rnorm(n = N, mean = 1 + 2 * population$S1 + 3 * population$X1 + 4 * population$X2 + 5 * x4.2 + 6 * x4.3, sd = 10)
population$Y2 = rnorm(n = N, mean = 1 + 2 * population$S1 + 3 * population$X1 + 4 * population$X2 + .5 * x4.2 + .6 * x4.3, sd = 10)

logit.prob = - 5 + .2 * population$S1 + .3 * population$X1 - .4 * population$X2 - .5 * x3.2 - .6 * x3.3 - 3 * x4.2 - 4 * x4.3
mean(logit.prob)
prob = exp(logit.prob) / (1 + exp(logit.prob))
mean(prob)
for (i in 1:N) {
  population$Y3[i] = rbinom(n = 1, size = 1, p = prob[i])
}

logit.prob = - 5 + .2 * population$S1 + .3 * population$X1 - .4 * population$X2 - .5 * x3.2 - .6 * x3.3 - .3 * x4.2 - .4 * x4.3
mean(logit.prob)
prob = exp(logit.prob) / (1 + exp(logit.prob))
mean(prob)
for (i in 1:N) {
  population$Y4[i] = rbinom(n = 1, size = 1, p = prob[i])
}


population$fpc1 = c(rep(25000, 25000), rep(25000, 25000))
population$fpc2 = c(rep(1000, 1000), rep(1400, 1400), rep(1800, 1800), rep(2200, 2200), 
                  rep(2600, 2600), rep(3000, 3000), rep(5000, 5000), rep(8000, 8000),
                  rep(10000, 10000), rep(15000, 15000)) #2 urban counties

table(population$S1)
table(population$S2)

length(population$X1)
mean(population$X1)
table(population$X2)
table(population$S1, population$X2)
table(population$X3)
table(population$S1, population$X3)
table(population$X4)

length(population$Y1)
mean(population$Y1)
sd(population$Y1)

length(population$Y2)
mean(population$Y2)
sd(population$Y2)

length(population$Y3)
mean(population$Y3)
sd(population$Y3)

length(population$Y4)
mean(population$Y4)
sd(population$Y4)

table(population$X4, population$S2)
####################1 Generate the Population#################### 











####################2 Generate Survey Samples#################### 
#####generate samples
# rural = c(1000, 1400, 1800, 2200, 2600, 3000, 5000, 8000)
# urban = c(10000, 15000)
p_sample = .1 #10% sampling rate
p_prop_prob = c(.1, .25, .5, .75, .9, 1) #% prob sample
p_prob = p_sample * p_prop_prob #10% sampling rate * % prob sample
p_nonprob = p_sample * (1 - p_prop_prob) #10% sampling rate * % non-prob sample
p_prob
p_nonprob
# n_prob_rural = sum(rural) * p_prob
# n_prob_rural
# n_prob_urban = sum(urban) * p_prob
# n_prob_urban 
# n_prob_rural == n_prob_urban
n_prob = p_prob * N
n_nonprob = p_nonprob * N
n_prob
n_nonprob
# ##prob sample sizes
# p_prob %*% t(rural)
# p_prob %*% t(urban)
# cbind(round(p_prob %*% t(rural)), round(p_prob %*% t(urban)))
# rowSums(cbind(round(p_prob %*% t(rural)), round(p_prob %*% t(urban))))
















# pprob = 1,2,3,4,5,6
# wt = 1,2,3,4
# wt_norm = "Elliott", "Regular"
# ps = "No", "External", "No External", "Separate" 
# Method "Separate" dropped on 7/18/2018

sim = function(pprob) {
#####Prob Sample
sample_prob = data.frame(population %>% 
                           group_by(S2) %>% 
                           sample_frac(p_prob[pprob]))
sample_prob$Z = 0
# dim(sample_prob)
# head(sample_prob)
# table(sample_prob$S2)
# sum(table(sample_prob$S2))
# n_prob



#####Non-prob Sample
population_nonprob = population[which(!(population$ID %in% sample_prob$ID)), ]
# dim(population_nonprob)
# head(population_nonprob)
# table(population_nonprob$S2)
# table(population_nonprob$X4)


# sum(sample_prob$ID %in% population_nonprob$ID > 0)
# p_nonprob = n_nonprob / dim(population_nonprob)[1]
# p_nonprob #sampling rate for non-prob sampling

##non-prob sample sizes
# round(p_nonprob %*% t(table(population_nonprob$S2))) #nonprob sample sizes by county
# rowSums(cbind(round(p_nonprob %*% t(table(population_nonprob$S2))))) #should match next row
# n_nonprob #should match previous row

nonprob.x3.2 = ifelse(population_nonprob$X3 == 2, 1, 0)
nonprob.x3.3 = ifelse(population_nonprob$X3 == 3, 1, 0)
nonprob.x4.2 = ifelse(population_nonprob$X4 == 2, 1, 0)
nonprob.x4.3 = ifelse(population_nonprob$X4 == 3, 1, 0)

p_nonprob = exp(- 10 + 1 * population_nonprob$X1 - 2 * population_nonprob$X2 + 3 * nonprob.x3.2 + 4 * nonprob.x3.3 - 5 * nonprob.x4.2 - 6 * nonprob.x4.3 + 7 * population_nonprob$Y4) / 
  (exp(- 10 + 1 * population_nonprob$X1 - 2 * population_nonprob$X2 + 3 * nonprob.x3.2 + 4 * nonprob.x3.3 - 5 * nonprob.x4.2 - 6 * nonprob.x4.3 + 7 * population_nonprob$Y4) + 1)

# head(p_nonprob)
# head(population_nonprob)
# exp(- 10 + 1 * 24.78584 - 2 * 1 + 3 * 1 - 4 * 2 + 5 * 1) / 
#   (exp(- 10 + 1 * 24.78584 - 2 * 1 + 3 * 1 - 4 * 2 + 5 * 1) + 1)
# exp(- 10 + 1 * 20.98852 - 2 * 1 + 3 * 3 - 4 * 3 + 5 * 1) / 
#   (exp(- 10 + 1 * 20.98852 - 2 * 1 + 3 * 3 - 4 * 3 + 5 * 1) + 1)

# summary(p_nonprob)
# plot(p_nonprob)



sample_nonprob = data.frame(population_nonprob %>%
                              sample_n(size = n_nonprob[pprob], weight = p_nonprob))


#dim(sample_nonprob)
sample_nonprob$Z = 1
# dim(sample_nonprob)
# head(sample_nonprob)
# table(sample_nonprob$S2)
# sum(table(sample_nonprob$S2))
# n_nonprob


return(sample_nonprob)
}



sample_nonprob = array(NA, dim = c(n_nonprob[scenario], 14, nsim))
#dim(sample_nonprob[, , 1])
for (i in 1:nsim) {
  sample_nonprob[, , i] = as.matrix(sim(pprob = scenario))
}

# as.vector(sample_nonprob[1:10,4,]) #Y1
# sample_nonprob[1:10,5,] #Y2
# sample_nonprob[1:10,6,] #Y3
# sample_nonprob[1:10,7,] #Y4
# 
# 
# sample_nonprob[1:10,8,] #X1
# sample_nonprob[1:10,9,] #X2
# sample_nonprob[1:10,10,] #X3
# sample_nonprob[1:10,11,] #X4

summary(as.vector(sample_nonprob[1:10,8,])) #X1
table(as.vector(sample_nonprob[1:10,9,])) #X2
table(as.vector(sample_nonprob[1:10,10,])) #X3
table(as.vector(sample_nonprob[1:10,11,])) #X4

pdf(paste("Scenario_", scenario, "_X.pdf"))
par(mfrow=c(2,2))
hist(as.vector(sample_nonprob[1:10,8,]), main = "Distribtuion of X1", xlab = "X1", ylab = "Count") #X1
barplot(table(as.vector(sample_nonprob[1:10,9,])), main = "Distribtuion of X2", xlab = "X2", ylab = "Count") #X2
barplot(table(as.vector(sample_nonprob[1:10,10,])), main = "Distribtuion of X3", xlab = "X3", ylab = "Count") #X3
barplot(table(as.vector(sample_nonprob[1:10,11,])), main = "Distribtuion of X4", xlab = "X4", ylab = "Count") #X4
dev.off()

summary(as.vector(sample_nonprob[1:10,4,])) #Y1
summary(as.vector(sample_nonprob[1:10,5,])) #Y2
table(as.vector(sample_nonprob[1:10,6,])) #Y3
table(as.vector(sample_nonprob[1:10,7,])) #Y4

pdf(paste("Scenario_", scenario, "_Y.pdf"))
par(mfrow=c(2,2))
hist(as.vector(sample_nonprob[1:10,4,]), main = "Distribtuion of Y1", xlab = "Y1", ylab = "Count") #Y1
hist(as.vector(sample_nonprob[1:10,5,]), main = "Distribtuion of Y2", xlab = "Y2", ylab = "Count") #Y2
barplot(table(as.vector(sample_nonprob[1:10,6,])), main = "Distribtuion of Y3", xlab = "Y3", ylab = "Count") #Y3
barplot(table(as.vector(sample_nonprob[1:10,7,])), main = "Distribtuion of Y4", xlab = "Y4", ylab = "Count") #Y4
dev.off()






#Truth in Population
summary(population$X1)
table(population$X2)
table(population$X3)
table(population$X4)


summary(population$Y1)
summary(population$Y2)
table(population$Y3)
table(population$Y4)

pdf(paste("Scenario_", scenario, "_X_population.pdf"))
par(mfrow=c(2,2))
hist(population$X1, main = "Distribtuion of X1", xlab = "X1", ylab = "Count") #X1
barplot(table(population$X2), main = "Distribtuion of X2", xlab = "X2", ylab = "Count") #X2
barplot(table(population$X3), main = "Distribtuion of X3", xlab = "X3", ylab = "Count") #X3
barplot(table(population$X4), main = "Distribtuion of X4", xlab = "X4", ylab = "Count") #X4
dev.off()

pdf(paste("Scenario_", scenario, "_Y_population.pdf"))
par(mfrow=c(2,2))
hist(population$Y1, main = "Distribtuion of Y1", xlab = "Y1", ylab = "Count") #Y1
hist(population$Y2, main = "Distribtuion of Y2", xlab = "Y2", ylab = "Count") #Y2
barplot(table(population$Y3), main = "Distribtuion of Y3", xlab = "Y3", ylab = "Count") #Y3
barplot(table(population$Y4), main = "Distribtuion of Y4", xlab = "Y4", ylab = "Count") #Y4
dev.off()


