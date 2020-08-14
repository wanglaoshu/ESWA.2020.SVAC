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

# Table 4. Generated rules



# Table 6. Test case
table.6 <- df.test[6556, 1:5]

print(table.6)

# Table 7. Example of applicable rules for the test case
table.7 <- rbind(df.rules.cand[2, c(1:8, 14)],
                 df.rules.cand[370, c(1:8, 14)],
                 df.rules.cand[7, c(1:8, 14)],
                 df.rules.cand[367, c(1:8, 14)],
                 df.rules.cand[387, c(1:8, 14)]
)

print(table.7)

# Table 8. Candidate revision codes for the test case
table.8 <- cbind(data.frame('Rank'  = c(1,2,3,4,5)),
                 'RSN_CD'           = df.rules.cand.tmp[order(df.rules.cand.tmp$scoreSum,
                                                              decreasing = T), ][c(1:4, 6), 6],
                 'Aggregated_Score' = round(df.rules.cand.tmp[order(df.rules.cand.tmp$scoreSum,
                                                                    decreasing = T), ][c(1:4, 6), 15], digits = 4)
)

print(table.8)

# Table 9. Average accuracy for eadch algorithm
table.9 <- data.frame('Algorithms' = c('WCBA',
                                       'MMAC',
                                       'CMAR',
                                       'MCAR',
                                       'CBA', 
                                       'CBA + multiple minimum support',
                                       'PCAR',
                                       'MAC',
                                       'SVAC - support and confidence',
                                       'SVAC - rule voting',
                                       'SVAC - feature weight',
                                       'SVAC'),
                      'Accuracy'   = c(round(mean(df.result.comp[, 31]), 4),
                                       round(mean(df.result.comp[, 16]), 4),
                                       round(mean(df.result.comp[, 28]), 4),
                                       round(mean(df.result.comp[, 22]), 4),
                                       round(mean(df.result.comp[, 13]), 4),
                                       round(mean(df.result.comp[, 19]), 4),
                                       round(mean(df.result.comp[, 34]), 4),
                                       round(mean(df.result.comp[, 25]), 4),
                                       round(mean(df.result.comp[, 7]), 4),
                                       round(mean(df.result.comp[, 4]), 4),
                                       round(mean(df.result.comp[, 10]), 4),
                                       round(mean(df.result.comp[, 1])), 4),
                      'Sensitivity' = c(round(mean(result.measures[, 52]), 4),
                                       round(mean(result.measures[, 27]), 4),
                                       round(mean(result.measures[, 47]), 4),
                                       round(mean(result.measures[, 27]), 4),
                                       round(mean(result.measures[, 22]), 4),
                                       round(mean(result.measures[, 32]), 4),
                                       round(mean(result.measures[, 57]), 4),
                                       round(mean(result.measures[, 42]), 4),
                                       round(mean(result.measures[, 12]), 4),
                                       round(mean(result.measures[, 7]), 4),
                                       round(mean(result.measures[, 17]), 4),
                                       round(mean(result.measures[, 2])), 4),
                      'Specificity' = c(round(mean(result.measures[, 53]), 4),
                                        round(mean(result.measures[, 28]), 4),
                                        round(mean(result.measures[, 48]), 4),
                                        round(mean(result.measures[, 28]), 4),
                                        round(mean(result.measures[, 23]), 4),
                                        round(mean(result.measures[, 33]), 4),
                                        round(mean(result.measures[, 58]), 4),
                                        round(mean(result.measures[, 43]), 4),
                                        round(mean(result.measures[, 13]), 4),
                                        round(mean(result.measures[, 8]), 4),
                                        round(mean(result.measures[, 18]), 4),
                                        round(mean(result.measures[, 3])), 4),
                      'Rule_size'  = c(round(mean(df.result.comp[, 29]), 0),
                                       round(mean(df.result.comp[, 17]), 0),
                                       round(mean(df.result.comp[, 23]), 0),
                                       round(mean(df.result.comp[, 14]), 0),
                                       round(mean(df.result.comp[, 20]), 0),
                                       round(mean(df.result.comp[, 26]), 0),
                                       round(mean(df.result.comp[, 8]), 0),
                                       round(mean(df.result.comp[, 5]), 0),
                                       round(mean(df.result.comp[, 11]), 0),
                                       round(mean(df.result.comp[, 2]), 0)),
                      'Number_of_default_class' = c(round(mean(df.result.comp[, 30]), 0),
                                                    round(mean(df.result.comp[, 18]), 0),
                                                    round(mean(df.result.comp[, 24]), 0),
                                                    round(mean(df.result.comp[, 15]), 0),
                                                    round(mean(df.result.comp[, 21]), 0),
                                                    round(mean(df.result.comp[, 27]), 0),
                                                    round(mean(df.result.comp[, 9]), 0),
                                                    round(mean(df.result.comp[, 6]), 0),
                                                    round(mean(df.result.comp[, 12]), 0),
                                                    round(mean(df.result.comp[, 3]), 0))
)

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


