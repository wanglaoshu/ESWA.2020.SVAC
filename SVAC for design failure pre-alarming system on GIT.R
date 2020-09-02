#########################################################################################################################
### Project  : SVAC for design failure pre-alarming system 
### Script   : SVAC for design failure pre-alarming system on GIT.R
### Contents : A Score and Vote based Associative Classification
#########################################################################################################################

#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load Library
pkgs <- c("tidyr", "arules", "caret", "arulesCBA")
sapply(pkgs, require, character.only = T)

# Load data
load("experiment_result.RData")   
load("implementation_table.Rdata")
load('data.Rdata')
# Load functions
load("functions.Rdata")

#########################################################################################################################
### Analysis
#########################################################################################################################

# Table 4. Example of generated rules
table.4 <- rules.sub[order(rules.sub$confidence,
                           rules.sub$support,
                           -(rules.sub$LHS),
                           decreasing = T), -11]
table.4['Rank'] <- c(1:5)

print(table.4)


# Table 5. Example of ranked rules
table.5 <- rules.sub[order(rules.sub$LHS,
                           rules.sub$GM,
                           rules.sub$lift,
                           decreasing = T), -11]

table.5['Rank'] <- c(1:5)

print(table.5)

# Table 6. Test case
table.6 <- df.test[6556, 1:5]

print(table.6)

# Table 7. Example of applicable rules for the test case
table.7 <- rbind(df.rules.cand[46, c(1:8, 14)],
                 df.rules.cand[370, c(1:8, 14)],
                 df.rules.cand[23, c(1:8, 14)],
                 df.rules.cand[367, c(1:8, 14)],
                 df.rules.cand[387, c(1:8, 14)])

print(table.7)

# Table 8. Candidate revision codes for the test case
table.8 <- cbind(data.frame('Rank'  = c(1,2,3,4,5)),
                 'RSN_CD'           = df.rules.cand.tmp[order(df.rules.cand.tmp$scoreSum,
                                                              decreasing = T), ][c(1:4, 6), 6],
                 'Aggregated_Score' = round(df.rules.cand.tmp[order(df.rules.cand.tmp$scoreSum,
                                                                    decreasing = T), ][c(1:4, 6), 15], digits = 4))

print(table.8)

# Table 9. Average accuracy of each algorithm
algorithms <- c('WCBA','MMAC','CMAR','MCAR','CBA','CBA + multiple minimum support',
                'PCAR','MAC','SVAC - support and confidence','SVAC - rule voting',
                'SVAC - feature weight','SVAC')

accuracy <- c()

for(i in c(31,16,28,22,13,19,34,25,7,4,10,1)){
  accuracy <- c(accuracy, round(mean(df.result[, i]), 4))
}

sensitivity <- c()

for(i in c(41,21,37,29,17,25,45,33,9,5,13,1)){
  sensitivity <- c(sensitivity, round(mean(result.measures[, i]), 4))
}

specificity <- c()

for(i in c(42,22,38,30,18,26,46,34,10,6,14,2)){
  specificity <- c(specificity, round(mean(result.measures[, i]), 4))
}

ruleSize <- c()

for(i in c(32,17,29,23,14,20,35,26,8,5,11,2)){
  ruleSize <- c(ruleSize, round(mean(df.result[, i]), 0))
}

defaultClasses <- c()

for(i in c(33,18,30,24,15,21,36,27,9,6,12,3)){
  defaultClasses <- c(defaultClasses, round(mean(df.result[, i]), 0))
}

table.9 <- data.frame('Algorithms'  = algorithms,
                      'Accuracy'    = accuracy,
                      'Sensitivity' = sensitivity,
                      'Specificity' = specificity,
                      'Rule_size'   = ruleSize,
                      'Prediction_made_by_default_classes' = defaultClasses)

print(table.9)

#########################################################################################################################
### Appendix. Experiment
#########################################################################################################################

# Input
seed       <- 1
support    <- 0.001
confidence <- 0.01 
df.i       <- df.raw

# K-fold SVAC
kfold.svac(data = df.i, support = 0.001, confidence = 0.01, K = 5)

# K-fold SVAC without rule voting
kfold.svac_1(data = df.i, support = 0.001, confidence = 0.01, K = 5)

# K-fold SVAC without geoemetric mean of support and confidence
kfold.svac_2(data = df.i, support = 0.001, confidence = 0.01, K = 5)

# K-fold SVAC without feature weights
kfold.svac_3(data = df.i, support = 0.001, confidence = 0.01, K = 5)

# K-fold MMAC
kfold.mmac(data = df.i, support = 0.001, confidence = 0.01, K = 5)

# K-fold CBA
kfold.cba(data = df.i, support = 0.001, confidence = 0.01, K = 5)

# K-fold CBA with multiple minimum supports
kfold.cba(data = df.i, support = 0.001, confidence = 0.01, K = 5, balanceSupport = T)

# K-fold MCAR 
kfold.mcar(data = df.i, support = 0.001, confidence = 0.01, K = 5)

# K-fold MAC
kfold.mac(data = df.i, support = 0.001, confidence = 0.01, K = 5)

# K-fold CMAR
kfold.cmar(data = df.i, support = 0.001, confidence = 0.01, K = 5)

# K-fold WCBA
kfold.wcba(data = df.i, support = 0.001, confidence = 0.01, K = 5)

# K-fold PCAR
kfold.mac(data = df.i, support = 0.001, confidence = 0.01, K = 5)
