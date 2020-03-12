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

# Table 4. Test case
table.4 <- df.test[6556, 1:5]

print(table.4)

# Table 5. Example of applicable rules for the test case
table.5 <- rbind(df.rules.cand[2, c(1:8, 14)],
                 df.rules.cand[370, c(1:8, 14)],
                 df.rules.cand[7, c(1:8, 14)],
                 df.rules.cand[367, c(1:8, 14)],
                 df.rules.cand[387, c(1:8, 14)]
                 )

print(table.5)

# Table 6. Candidate revision codes for the test case
table.6 <- cbind(data.frame('Rank'  = c(1,2,3,4,5)),
                 'RSN_CD'           = df.rules.cand.tmp[order(df.rules.cand.tmp$scoreSum,
                                                              decreasing = T), ][c(1:4, 6), 6],
                 'Aggregated_Score' = round(df.rules.cand.tmp[order(df.rules.cand.tmp$scoreSum,
                                                                    decreasing = T), ][c(1:4, 6), 15], digits = 4)
                 )

print(table.6)

# Table 7. Average accuracy for eadch algorithm
table.7 <- data.frame('Algorithms' = c('MMAC', 
                                        'CBA', 
                                        'CBA + multiple minimum support',
                                        'SVAC - support and confidence',
                                        'SVAC - rule voting',
                                        'SVAC - feature weight',
                                        'SVAC'),
                      'Accuracy'   = c(percent(mean(df.result.comp[, 16])), 
                                       percent(mean(df.result.comp[, 13])),
                                       percent(mean(df.result.comp[, 19])),
                                       percent(mean(df.result.comp[, 7])),
                                       percent(mean(df.result.comp[, 4])),
                                       percent(mean(df.result.comp[, 10])),
                                       percent(mean(df.result.comp[, 1]))),
                      'Rule_size'  = c(round(mean(df.result.comp[, 17]), 0), 
                                       round(mean(df.result.comp[, 14]), 0),
                                       round(mean(df.result.comp[, 20]), 0),
                                       round(mean(df.result.comp[, 8]), 0),
                                       round(mean(df.result.comp[, 5]), 0),
                                       round(mean(df.result.comp[, 11]), 0),
                                       round(mean(df.result.comp[, 2]), 0)),
                      'Number_of_default_class' = c(round(mean(df.result.comp[, 18]), 0), 
                                                    round(mean(df.result.comp[, 15]), 0),
                                                    round(mean(df.result.comp[, 21]), 0),
                                                    round(mean(df.result.comp[, 9]), 0),
                                                    round(mean(df.result.comp[, 6]), 0),
                                                    round(mean(df.result.comp[, 12]), 0),
                                                    round(mean(df.result.comp[, 3]), 0))
                 )

print(table.7)

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
