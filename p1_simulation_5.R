

rm(list = ls())



library(dplyr)
library(survey)

library(foreach)
library(doParallel)


library(ggplot2) #split dataset into equal sized subgroups (weight 4)








###generate data
set.seed(1)
N = 50000
nsim = 5000
ncore = 20

scenario = 5



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

logit.prob = - 2 + .2 * population$S1 + .3 * population$X1 - .4 * population$X2 - .5 * x3.2 - .6 * x3.3 - 3 * x4.2 - 4 * x4.3
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

sim = function(pprob, wt, wt_norm, ps) {
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


dim(sample_nonprob)
sample_nonprob$Z = 1
# dim(sample_nonprob)
# head(sample_nonprob)
# table(sample_nonprob$S2)
# sum(table(sample_nonprob$S2))
# n_nonprob

#combined sample
sample_all = rbind(sample_prob, sample_nonprob)
# length(unique(sample_all$ID))
# table(sample_all$Z)
# dim(sample_all)
# head(sample_all)
####################2 Generate Survey Samples#################### 

####################3a pseudo weight#################### 
if (wt == 1) {
  ####Weight 1
  w1 = data.frame(sample_all %>%
                    count(S2) %>%
                    mutate(weight =  table(population$S2) / n))
  sample_all = merge(sample_all, w1, by = 'S2')[, -15]
  # dim(sample_all)
  # head(sample_all)
  
  sample_prob = sample_all[which(sample_all$Z == 0), ]
  sample_nonprob = sample_all[which(sample_all$Z == 1), ]
} else if (wt == 2) {
  ######weight2
  w2_prob = data.frame(sample_prob %>%
                         count(S2) %>%
                         mutate(weight =  table(population$S2) / n))
  sample_prob = merge(sample_prob, w2_prob, by = 'S2')[, -15]
  # dim(sample_prob)
  # head(sample_prob)
  
  sample_nonprob$weight = 1
  
  sample_all = rbind(sample_prob, sample_nonprob)
  # dim(sample_all)
  # head(sample_all)
  # tail(sample_all)
} else if (wt == 3) {
  ######weight3
  # if (sample_all$nonprob == 1 && sample_all$prob == 0) {
  #   sample_all$Z = 1} else if (sample_all$nonprob == 0 && sample_all$prob == 1) {
  #     sample_all$Z = 0
  #   }
  # table(sample_all$Z)
  # #sample_all[which(sample_all$nonprob == 1 && sample_all$prob == 0), ]
  # dim(sample_all)
  # head(sample_all)
  
  
  model = glm(Z ~ X1 + X2 + factor(X3) + factor(X4), family = binomial(link='logit'), 
              data = sample_all)
  # summary(model)
  
  
  sample_all$p.hat = predict(model, type="response") #P(Z_i = 1 | x_i = x_0)
  sample_nonprob = sample_all[which(sample_all$Z == 1), ]
  # dim(sample_nonprob)
  # head(sample_nonprob)
  temp = rep(NA, dim(sample_nonprob)[1])
  temp = (1 - sample_nonprob$p.hat) / sample_nonprob$p.hat #P(Z_i = 0 | x_i = x_0) / P(Z_i = 1 | x_i = x_0)
  sample_nonprob$weight = temp * (dim(population)[1] / sum(temp)) #normalize the weight to represent the population
  sample_nonprob = sample_nonprob[, -15]
  
  w3_prob = data.frame(sample_prob %>%
                         count(S2) %>%
                         mutate(weight =  table(population$S2) / n))
  sample_prob = merge(sample_prob, w3_prob, by = 'S2')[, -15]
  # dim(sample_prob)
  # head(sample_prob)
  
  
  sample_all = rbind(sample_prob, sample_nonprob)
  # dim(sample_all)
  # head(sample_all)
} else if (wt == 4) {
  ######weight4
  
  w4_prob = data.frame(sample_prob %>%
                         count(S2) %>%
                         mutate(weight =  table(population$S2) / n))
  sample_prob = merge(sample_prob, w4_prob, by = 'S2')[, -15]
  sample_nonprob$weight = 1 #everyone in the non-prob sample got weight = 1
  sample_all = rbind(sample_prob, sample_nonprob)
  # head(sample_prob)
  # head(sample_nonprob)
  # head(sample_all)
  # table(sample_all$weight)
  
  sample_all$P = 1 - sample_all$Z #P == 1 <=> Z == 0 <=> prob sample (random sample)
  model = glm(P ~ X1 + X2 + factor(X3) + factor(X4), family = binomial(link='logit'), 
              data = sample_all, weights = weight) #weighted logistic regression
  sample_all$e = predict(model, type="response")
  
  # sample_all %>% head
  
  sample_all$subgroups = as.numeric(cut_number(sample_all$e, 20)) #20 equal-sized subgroups
  # table(sample_all$subgroups)
  

  
  sample_all = data.frame(sample_all %>%
                            group_by(subgroups) %>%
                            mutate(ave_e = mean(e), 
                                   # ave_Z = mean(Z),
                                   ps_weight = ave_e / (1 - ave_e))) 
  # head(sample_all)
  # table(sample_all$ave_e)
  
  sample_all[which(sample_all$P == 1), ]$ps_weight = 1 #set the ps_weight for prob sample (random sample) to 1

  # table(sample_all$ps_weight)
  # table(sample_all$P == 1)


  
  # sample_all %>%
  #   group_by(subgroups) %>%
  #   summarise(sum(P), sum(ps_weight))

  sample_all = data.frame(sample_all %>% 
                            group_by(subgroups) %>%
                            mutate(sub_weight = sum(P * weight, na.rm = T),
                                   sum.ps_weight = sum(ps_weight),
                                   weight_new = sub_weight / sum.ps_weight * ps_weight))

  # head(sample_all)
  # sum(sample_all[which(sample_all$subgroups == 20), ]$ps_weight)
  # table(sample_all[which(sample_all$subgroups == 20), ]$ps_weight)
  # 
  # table(sample_all[which(sample_all$subgroups == 20), ]$weight_new)
  # sum(sample_all[which(sample_all$subgroups == 20), ]$weight_new)
  
  
  
  
  drop = c("weight", "P", "e", "subgroups", "ave_e", "ps_weight", "sub_weight", "sum.ps_weight")
  sample_all = sample_all[, !(names(sample_all) %in% drop)]
  sample_all = rename(sample_all, weight = weight_new)
  
  # # summary(sample_all$weight)
  # for (i in 1:dim(sample_all)[1]) {
  #   if (sample_all$weight[i] < 1) {
  #     sample_all$weight[i] = 1
  #   }
  # }

  
  sample_prob = sample_all[which(sample_all$Z == 0), ]
  sample_nonprob = sample_all[which(sample_all$Z == 1), ]
} else if (wt == 5) {
  ####Weight 5 -- Prob sample only
  w1 = data.frame(sample_prob %>%
                    count(S2) %>%
                    mutate(weight =  table(population$S2) / n))
  sample_all = merge(sample_prob, w1, by = 'S2')[, -15]
  sample_prob = sample_all
}
####################3a pseudo weight#################### 

####################3b weight normalization#################### 
if (wt_norm == "Elliott") {
  #####Elliott's method#####
  sum.num = sum((1 - sample_all$Z) * sample_all$weight)
  sum.den = sum(sample_all$Z * sample_all$weight)
  
  sample_all$weight_norm = ifelse(sample_all$Z == 1,
                                  (n_nonprob[pprob] / (n_prob[pprob] + n_nonprob[pprob])) * (sum.num / sum.den) * sample_all$weight,
                                  (n_prob[pprob] / (n_prob[pprob] + n_nonprob[pprob])) * sample_all$weight)
  
  sample_all$weight = sample_all$weight_norm
  sample_all = sample_all[, -16]
  #head(sample_all)
  # sum(sample_all$weight)
  # temp = data.frame(sample_all %>%
  #   group_by(S2) %>%
  #   summarise(Frequency = sum(weight)))
  # sum(temp$Frequency)
} else if (wt_norm == "Regular") {
  ####Regular method#####
  sum.weight = data.frame(sample_all %>%
                            group_by(S2) %>%
                            summarise(Frequency = sum(weight)))
  sample_all$weight_norm = (table(population$S2) / sum.weight[,2])[sample_all$S2] * sample_all$weight
  
  
  
  #sample_all$weight_norm = (N / sum(sample_all$weight)) * sample_all$weight
  sample_all$weight = sample_all$weight_norm
  sample_all = sample_all[, -16]
  #head(sample_all)
  # sum(sample_all$weight)
  # sample_all %>%
  #   group_by(S2) %>%
  #   summarise(Frequency = sum(weight))
}
####################3b weight normalization#################### 

####################3c post-stratification#################### 
#svysample = svydesign(id = ~1, strata = ~S1+S2, weights = ~weight, fpc= ~fpc1+fpc2, data = sample_all)
if (ps == "No") {
  #####3ciii no post-stratification
  dstrats = svydesign(id = ~1, strata = ~S2, weights = ~weight, fpc = ~fpc2, data = sample_all)
  #summary(dstrats)
} else if (ps == "External") {
  #####3ci1 joint post-stratification (external info)
  dstrat = svydesign(id = ~1, strata = ~S2, weights = ~weight, fpc = ~fpc2, data = sample_all)
  # pop.table = xtabs(~X2+X3+X4, population)
  # dstratps = postStratify(dstrat, ~X2+X3+X4, pop.table, partial = TRUE)
  dstrats = rake(dstrat, list(~X2, ~X3, ~X4), list(xtabs(~X2, population), xtabs(~X3, population), xtabs(~X4, population)))
  
  # sample_all$weight = weights(dstratps)
  # #####Weight Normalization#####
  # sample_all$weight_norm = (N / sum(sample_all$weight)) * sample_all$weight
  # sample_all$weight = sample_all$weight_norm
  # sample_all = sample_all[, -16]
  # 
  # dstrats = svydesign(id = ~1, strata = ~S2, weights = ~weight, fpc = ~fpc2, data = sample_all)
  # #svytable(~X2+X3+X4, dstrats, round=TRUE)
} else if (ps == "No External") {
  #####3ci1 joint post-stratification (no external info)
  dstrat = svydesign(id = ~1, strata = ~S2, weights = ~weight, fpc = ~fpc2, data = sample_all)
  # pop.table = xtabs(~X2+X3+X4, sample_prob)
  # dstratps = postStratify(dstrat, ~X2+X3+X4, pop.table, partial = TRUE)
  dstratps = rake(dstrat, list(~X2, ~X3, ~X4), list(xtabs(~X2, sample_prob), xtabs(~X3, sample_prob), xtabs(~X4, sample_prob)))
  
  sample_all$weight = weights(dstratps)
  #####Weight Normalization#####
  sample_all$weight_norm = (N / sum(sample_all$weight)) * sample_all$weight
  sample_all$weight = sample_all$weight_norm
  sample_all = sample_all[, -16]

  dstrats = svydesign(id = ~1, strata = ~S2, weights = ~weight, fpc = ~fpc2, data = sample_all)
  #svytable(~X2+X3+X4, dstrats, round=TRUE)
} #else if (ps == "Separate") {
#   #####3cii separate post-stratification
#   svysample_prob = svydesign(id = ~1, strata = ~S2, weights = ~weight, fpc = ~fpc2, data = sample_prob)
#   svysample_nonprob = svydesign(id = ~1, strata = ~S2, weights = ~weight, fpc = ~fpc2, data = sample_nonprob)
#   
#   pop.table = xtabs(~X2+X3+X4, population)
#   svysample_probps = postStratify(svysample_prob, ~X2+X3+X4, pop.table, partial = TRUE)
#   svysample_nonprobps = postStratify(svysample_nonprob, ~X2+X3+X4, pop.table, partial = TRUE)
#   # svytable(~X2+X3+X4, svysample_probps, round=TRUE)
#   # svytable(~X2+X3+X4, svysample_nonprobps, round=TRUE)
#   
#   
#   sample_prob$weight = weights(svysample_probps) / 2
#   sample_nonprob$weight = weights(svysample_nonprobps) / 2
#   
#   
#   # tail(sample_all)
#   # tail(sample_prob)
#   # tail(sample_nonprob)
#   # 
#   # table(sample_all$weight)
#   # table(sample_prob$weight)
#   # table(sample_nonprob$weight)
#   
#   sample_all = rbind(sample_prob, sample_nonprob)
# 
#   #####Weight Normalization#####
#   sample_all$weight_norm = (N / sum(sample_all$weight)) * sample_all$weight
#   sample_all$weight = sample_all$weight_norm
#   sample_all = sample_all[, -16]
#   
#   dstrats = svydesign(id = ~1, strata = ~S2, weights = ~weight, fpc = ~fpc2, data = sample_all)
# }
###################3c post-stratification#################### 

####################3d variance estimation#################### 
# svymean(~Y1+Y2, dstrats)
# 
# # weighted.mean(sample_all$Y1, sample_all$weight)
# # weighted.mean(sample_all$Y2, sample_all$weight)
# # 
# # sum(sample_all$Y1 * sample_all$weight) / sum(sample_all$weight)
# 
# 
# temp = as.data.frame(sample_all %>%
#   group_by(S2) %>%
#   summarise_at(vars(Y2), funs(var(.))))
# # c(1000, 1400, 1800, 2200, 2600, 3000, 5000, 8000, 10000, 15000) %*% temp$Y1 / 50000
# temp
# 
# 
# temp2 = as.data.frame(sample_all %>%
#   count(S2) )
# temp2
# 
# N = c(1000, 1400, 1800, 2200, 2600, 3000, 5000, 8000, 10000, 15000)
# sqrt(sum(temp$Y2 / temp2$n * (1 - temp2$n / N) *( N^2))/(50000^2))
# 
# 
# 
# sum(sample_all$weight)
# sum(sample_prob$weight)
# sum(sample_nonprob$weight)


#jkstrats = as.svrepdesign(dstrats, type="JKn")
#summary(jkstrats)
#list(svymean(~Y1+Y2+Y3+Y4, dstrats), svymean(~Y1+Y2+Y3+Y4, jkstrats))
#list(cbind(data.frame(svymean(~Y1+Y2+Y3+Y4, dstrats)), data.frame(svymean(~Y1+Y2+Y3+Y4, jkstrats))))


Y1.mean = data.frame(svymean(~Y1, dstrats, deff = TRUE))[, 1]
Y1.se = data.frame(svymean(~Y1, dstrats, deff = TRUE))[, 2]
Y1.deff = data.frame(svymean(~Y1, dstrats, deff = TRUE))[, 3]
Y1.95ci = ifelse(mean(population$Y1) >= Y1.mean - qnorm(.975) * Y1.se && 
                 mean(population$Y1) <= Y1.mean + qnorm(.975) * Y1.se, 
                 1, 0)

Y2.mean = data.frame(svymean(~Y2, dstrats, deff = TRUE))[, 1]
Y2.se = data.frame(svymean(~Y2, dstrats, deff = TRUE))[, 2]
Y2.deff = data.frame(svymean(~Y2, dstrats, deff = TRUE))[, 3]
Y2.95ci = ifelse(mean(population$Y2) >= Y2.mean - qnorm(.975) * Y2.se && 
                 mean(population$Y2) <= Y2.mean + qnorm(.975) * Y2.se, 
                 1, 0)

Y3.mean = data.frame(svymean(~Y3, dstrats, deff = TRUE))[, 1]
Y3.se = data.frame(svymean(~Y3, dstrats, deff = TRUE))[, 2]
Y3.deff = data.frame(svymean(~Y3, dstrats, deff = TRUE))[, 3]
Y3.95ci = ifelse(mean(population$Y3) >= Y3.mean - qnorm(.975) * Y3.se && 
                 mean(population$Y3) <= Y3.mean + qnorm(.975) * Y3.se, 
                 1, 0)

Y4.mean = data.frame(svymean(~Y4, dstrats, deff = TRUE))[, 1]
Y4.se = data.frame(svymean(~Y4, dstrats, deff = TRUE))[, 2]
Y4.deff = data.frame(svymean(~Y4, dstrats, deff = TRUE))[, 3]
Y4.95ci = ifelse(mean(population$Y4) >= Y4.mean - qnorm(.975) * Y4.se && 
                 mean(population$Y4) <= Y4.mean + qnorm(.975) * Y4.se, 
                 1, 0)

list(cbind(Y1.mean, Y1.se, Y1.95ci, Y1.deff, Y2.mean, Y2.se, Y2.95ci, Y2.deff,
           Y3.mean, Y3.se, Y3.95ci, Y3.deff, Y4.mean, Y4.se, Y4.95ci, Y4.deff
           ))

####################3d variance estimation#################### 
}




# pprob = 1,2,3,4,5,6
# wt = 1,2,3
# wt_norm = "No", "Elliott", "Regular"
# ps = "No", "External", "No External", "Separate"
# Method "Separate" dropped on 7/18/2018



run.sim = function(npprob) {
    temp1 = sim(pprob = npprob, wt = 1, wt_norm = "No", ps = "No")
    temp2 = sim(pprob = npprob, wt = 1, wt_norm = "No", ps = "External")
    temp3 = sim(pprob = npprob, wt = 1, wt_norm = "No", ps = "No External")

    temp4 = sim(pprob = npprob, wt = 2, wt_norm = "Elliott", ps = "No")
    temp5 = sim(pprob = npprob, wt = 2, wt_norm = "Elliott", ps = "External")
    temp6 = sim(pprob = npprob, wt = 2, wt_norm = "Elliott", ps = "No External")
    temp7 = sim(pprob = npprob, wt = 2, wt_norm = "Regular", ps = "No")
    temp8 = sim(pprob = npprob, wt = 2, wt_norm = "Regular", ps = "External")
    temp9 = sim(pprob = npprob, wt = 2, wt_norm = "Regular", ps = "No External")

    temp10 = sim(pprob = npprob, wt = 3, wt_norm = "Elliott", ps = "No")
    temp11 = sim(pprob = npprob, wt = 3, wt_norm = "Elliott", ps = "External")
    temp12 = sim(pprob = npprob, wt = 3, wt_norm = "Elliott", ps = "No External")
    temp13 = sim(pprob = npprob, wt = 3, wt_norm = "Regular", ps = "No")
    temp14 = sim(pprob = npprob, wt = 3, wt_norm = "Regular", ps = "External")
    temp15 = sim(pprob = npprob, wt = 3, wt_norm = "Regular", ps = "No External")
    
    temp16 = sim(pprob = npprob, wt = 4, wt_norm = "Elliott", ps = "No")
    temp17 = sim(pprob = npprob, wt = 4, wt_norm = "Elliott", ps = "External")
    temp18 = sim(pprob = npprob, wt = 4, wt_norm = "Elliott", ps = "No External")
    temp19 = sim(pprob = npprob, wt = 4, wt_norm = "Regular", ps = "No")
    temp20 = sim(pprob = npprob, wt = 4, wt_norm = "Regular", ps = "External")
    temp21 = sim(pprob = npprob, wt = 4, wt_norm = "Regular", ps = "No External")
    
    temp22 = sim(pprob = npprob, wt = 5, wt_norm = "No", ps = "No")
    temp23 = sim(pprob = npprob, wt = 5, wt_norm = "No", ps = "External")
    temp24 = sim(pprob = npprob, wt = 5, wt_norm = "No", ps = "No External")

    list(temp1, temp2, temp3, 
         temp4, temp5, temp6, temp7, temp8, temp9, 
         temp10, temp11, temp12, temp13, temp14, temp15, 
         temp16, temp17, temp18, temp19, temp20, temp21,
         temp22, temp23, temp24
         )
    #list(temp1, temp2) #test
}





set.seed(324)
###Parallel Computing###
cl<-makeCluster(ncore)
registerDoParallel(cl)

#set.seed(423424, kind = "L'Ecuyer-CMRG")
clusterSetRNGStream(cl, 32435)

starttime = Sys.time()
tick <- proc.time()
result <- foreach(nit=1:nsim,
                  .packages = c("dplyr", "survey", "ggplot2") 
) %dopar% {
  run.sim(npprob = scenario)
}
endtime = Sys.time()
tock <- proc.time()


#invisible(stopCluster(cl))
stopCluster(cl)








results = array(NA, dim = c(24, 16, nsim))

for (n in 1:nsim) {
  for (i in 1:24) {
    results[i, , n] = matrix(unlist(result[n][[1]][[i]]), nrow = 1, ncol = 16)
    
  }
}

# result[1][[1]][[1]]
# result[1][[1]][[2]]
# result[2][[1]][[1]]
# result[2][[1]][[2]]
# result[3][[1]][[1]]



table.main = apply(results, c(1,2), mean)
table.mcse = apply(results[, c(1, 5, 9, 13), ], c(1,2), sd)
table.sdse = apply(results[, c(2, 6, 10, 14), ], c(1,2), sd)
table.sddeff = apply(results[, c(4, 8, 12, 16), ], c(1,2), sd)
table.mse = cbind(rowMeans((mean(population$Y1) - results[,1,])^2),
                  rowMeans((mean(population$Y2) - results[,5,])^2),
                  rowMeans((mean(population$Y3) - results[,9,])^2),
                  rowMeans((mean(population$Y4) - results[,13,])^2))
table.bias = cbind((table.main[,1] - mean(population$Y1)) / mean(population$Y1) * 100,
                   (table.main[,5] - mean(population$Y2)) / mean(population$Y2) * 100,
                   (table.main[,9] - mean(population$Y3)) / mean(population$Y3) * 100,
                   (table.main[,13] - mean(population$Y4)) / mean(population$Y4) * 100)


cbind(table.main[,1], table.bias[,1], table.main[,2], table.main[,3] * 100, table.mcse[,1], table.sdse[,1], table.mse[,1], table.main[,4],
      table.main[,5], table.bias[,2], table.main[,6], table.main[,7] * 100, table.mcse[,2], table.sdse[,2], table.mse[,2], table.main[,8])

cbind(table.main[,9], table.bias[,3], table.main[,10], table.main[,11] * 100, table.mcse[,3], table.sdse[,3], table.mse[,3], table.main[,12],
      table.main[,13], table.bias[,4], table.main[,14], table.main[,15] * 100, table.mcse[,4], table.sdse[,4], table.mse[,4], table.main[,16])


#Truth in Population
mean(population$Y1)
mean(population$Y2)
mean(population$Y3)
mean(population$Y4)



tock - tick
endtime - starttime
starttime
endtime
