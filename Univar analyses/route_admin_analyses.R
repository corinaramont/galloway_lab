# Question: are there differences between the routes of administrations of drugs 
# patient is on among the different outcome groups 
# (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both)?

# packages
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(writexl)
library(ggplot2)
library(tictoc)
library(ggpubr)
library(DescTools)
library(psych)

# loading function multiplesheets() from "read_multiple_sheets.R"
# allows us to differentiate and read multiple sheets in a single Excel file
source("Functions/read_multiple_sheets.R")

# loads in datasets of routes of admin of patients we have info on
path = "Datasets/K01_admin_routes_outcomes.xlsx"
data = (multiplesheets(path))$Data

###################### PLOTS ######################################
ggplot(data = data, aes(ARI, fill = OR)) + geom_bar() + ggtitle("ARI and OR")

ggplot(data = data, aes(ARI, fill = IV)) + geom_bar() + ggtitle("ARI and IV")

ggplot(data = data, aes(ARI, fill = IJ)) + geom_bar() + ggtitle("ARI and IJ")

ggplot(data = data, aes(ARC, fill = OR)) + geom_bar() + ggtitle("ARC and OR")

ggplot(data = data, aes(ARC, fill = IV)) + geom_bar() + ggtitle("ARC and IV")

ggplot(data = data, aes(ARC, fill = IJ)) + geom_bar() + ggtitle("ARC and IJ")

ggplot(data = data, aes(Both, fill = OR)) + geom_bar() + ggtitle("Both and OR")

ggplot(data = data, aes(Both, fill = IV)) + geom_bar() + ggtitle("Both and IV")

ggplot(data = data, aes(Both, fill = IJ)) + geom_bar() + ggtitle("Both and IJ")

###################### DIFFERENCES ################################
pvals = c()

# 1. ARI and OR
temp_table = table(data$OR, data$ARI)
analysis_temp = matrix(data = c(temp_table[[1]], temp_table[[2]], 
                                temp_table[[3]], temp_table[[4]]), nrow = 2)
analysis_temp = as.data.frame(analysis_temp)
rownames(analysis_temp) = c("not OR", "OR")
colnames(analysis_temp) = c("not ARI", "ARI")
analysis_temp
test = chisq.test(analysis_temp, correct = T)
pvals = c(pvals, test$p.value)

# 2. ARI and IV
temp_table2 = table(data$IV, data$ARI)
analysis_temp2 = matrix(data = c(temp_table2[[1]], temp_table2[[2]], 
                                 temp_table2[[3]], temp_table2[[4]]), nrow = 2)
analysis_temp2 = as.data.frame(analysis_temp2)
rownames(analysis_temp2) = c("not IV", "IV")
colnames(analysis_temp2) = c("not ARI", "ARI")
test = fisher.test(analysis_temp2)
pvals = c(pvals, test$p.value)

# 3. ARI and IJ
temp_table3 = table(data$IJ, data$ARI)
analysis_temp3 = matrix(data = c(temp_table3[[1]], temp_table3[[2]], 
                                 temp_table3[[3]], temp_table3[[4]]), nrow = 2)
analysis_temp3 = as.data.frame(analysis_temp3)
rownames(analysis_temp3) = c("not IJ", "IJ")
colnames(analysis_temp3) = c("not ARI", "ARI")
test = fisher.test(analysis_temp3)
pvals = c(pvals, test$p.value)

# 4. ARC and OR 
temp_table4 = table(data$OR, data$ARC)
analysis_temp4 = matrix(data = c(temp_table4[[1]], temp_table4[[2]], 
                                 temp_table4[[3]], temp_table4[[4]]), nrow = 2)
analysis_temp4 = as.data.frame(analysis_temp4)
rownames(analysis_temp4) = c("not OR", "OR")
colnames(analysis_temp4) = c("not ARC", "ARC")
test = chisq.test(analysis_temp4, correct = T)
pvals = c(pvals, test$p.value)

# 5. ARC and IV
temp_table5 = table(data$IV, data$ARC)
analysis_temp5 = matrix(data = c(temp_table5[[1]], temp_table5[[2]], 
                                 temp_table5[[3]], temp_table5[[4]]), nrow = 2)
analysis_temp5 = as.data.frame(analysis_temp5)
rownames(analysis_temp5) = c("not IV", "IV")
colnames(analysis_temp5) = c("not ARC", "ARC")
test = fisher.test(analysis_temp5)
pvals = c(pvals, test$p.value)

# 6. ARC and IJ
temp_table6 = table(data$IJ, data$ARC)
analysis_temp6 = matrix(data = c(temp_table6[[1]], temp_table6[[2]], 
                                 temp_table6[[3]], temp_table6[[4]]), nrow = 2)
analysis_temp6 = as.data.frame(analysis_temp6)
rownames(analysis_temp6) = c("not IJ", "IJ")
colnames(analysis_temp6) = c("not ARC", "ARC")
test = chisq.test(analysis_temp6, correct = T)
pvals = c(pvals, test$p.value)

# 7. Both and OR
temp_table7 = table(data$OR, data$Both)
analysis_temp7 = matrix(data = c(temp_table7[[1]], temp_table7[[2]], 
                                 temp_table7[[3]], temp_table7[[4]]), nrow = 2)
analysis_temp7 = as.data.frame(analysis_temp7)
rownames(analysis_temp7) = c("not OR", "OR")
colnames(analysis_temp7) = c("not Both", "Both")
test = chisq.test(analysis_temp7, correct = T)
pvals = c(pvals, test$p.value)

# 8. Both and IV
temp_table8 = table(data$IV, data$Both)
analysis_temp8 = matrix(data = c(temp_table8[[1]], temp_table8[[2]], 
                                 temp_table8[[3]], temp_table8[[4]]), nrow = 2)
analysis_temp8 = as.data.frame(analysis_temp8)
rownames(analysis_temp8) = c("not IV", "IV")
colnames(analysis_temp8) = c("not Both", "Both")
test = fisher.test(analysis_temp8)
pvals = c(pvals, test$p.value)

# 9. Both and IJ
temp_table9 = table(data$IJ, data$Both)
analysis_temp9 = matrix(data = c(temp_table9[[1]], temp_table9[[2]], 
                                 temp_table9[[3]], temp_table9[[4]]), nrow = 2)
analysis_temp9 = as.data.frame(analysis_temp9)
rownames(analysis_temp9) = c("not IJ", "IJ")
colnames(analysis_temp9) = c("not Both", "Both")
test = chisq.test(analysis_temp9, correct = T)
pvals = c(pvals, test$p.value)

adj = p.adjust(pvals, method = "fdr", n = length(pvals))

chi_fisher_results = data.frame(outcome = c(rep("ARI",3),rep("ARC",3),rep("Both",3)),
                      route = rep(c("OR", "IV", "IJ"),3), pvals = pvals,
                      pvals_adj = adj)
######################### ASSOCIATIONS ###############################

# phi coef similar to Pearson's (0 = no association, +1 = positive association, etc)
# contingency coef
cont_coef_est = c()

# 1.
phi(analysis_temp, digits=3)

cont_coef_test = Assocs(temp_table)
cont_coef_est = c(cont_coef_est, cont_coef_test[1])

# 2. 
phi(analysis_temp2, digits = 3)

cont_coef_test = Assocs(temp_table2)
cont_coef_est = c(cont_coef_est, cont_coef_test[1])


# 3. 
phi(analysis_temp3, digits = 3)

cont_coef_test = Assocs(temp_table3)
cont_coef_est = c(cont_coef_est, cont_coef_test[1])

# 4. 
phi(analysis_temp4, digits = 3)

cont_coef_test = Assocs(temp_table4)
cont_coef_est = c(cont_coef_est, cont_coef_test[1])

# 5. 
phi(analysis_temp5, digits = 3)

cont_coef_test = Assocs(temp_table5)
cont_coef_est = c(cont_coef_est, cont_coef_test[1])

# 6.
phi(analysis_temp6, digits = 3)

cont_coef_test = Assocs(temp_table6)
cont_coef_est = c(cont_coef_est, cont_coef_test[1])

# 7.
phi(analysis_temp7, digits = 3)

cont_coef_test = Assocs(temp_table7)
cont_coef_est = c(cont_coef_est, cont_coef_test[1])

# 8.
phi(analysis_temp8, digits = 3)

cont_coef_test = Assocs(temp_table8)
cont_coef_est = c(cont_coef_est, cont_coef_test[1])

# 9. 
phi(analysis_temp9, digits = 3)

cont_coef_test = Assocs(temp_table9)
cont_coef_est = c(cont_coef_est, cont_coef_test[1])

# phi coefficient results compiled
admin = rep(c("OR", "IV", "IJ"), 3)
outcome = c(rep("ARI", 3), rep("ARC", 3), rep("Both",3))
phi_values = c(phi(analysis_temp, digits = 3), phi(analysis_temp2, digits = 3),
               phi(analysis_temp3, digits = 3), phi(analysis_temp4, digits = 3),
               phi(analysis_temp5, digits = 3), phi(analysis_temp6, digits = 3),
               phi(analysis_temp7, digits = 3), phi(analysis_temp8, digits = 3),
               phi(analysis_temp9, digits = 3))
planB_corr_results = data.frame(route_admin = admin, outcome = outcome, 
                                phi = phi_values)

# contingency coefficient results compiled
planB_cont_coef_results = data.frame(route_admin = admin, outcome = outcome,
                                     coef_est = cont_coef_est)

################################################################################
# Question 1b: are there differences between the routes of administrations of drugs 
# patient is on among the different outcome groups 
# (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both) as well as gain/loss of ARGs?

path2 = "Datasets/bl_eos_arg_counts.xlsx"
ARG_count = (multiplesheets(path2))$collection_info

path3 = "Datasets/K01_AntibioticData_for_ARG.xlsx"
new_admin_data = (multiplesheets(path3))$Filtered

data2 = ARG_count[,1:3]

IV = rep(0, nrow(data2))
IJ = rep(0, nrow(data2))
OR = rep(0, nrow(data2))

for(i in 1:nrow(data2)){
  temp = new_admin_data %>% filter(mrn == data2$mrn[i])
  if(nrow(temp) > 0){
    temp_route = unique(temp$Route)
    if("IV" %in% temp_route == T){
      IV[i] = 1
    }
    if("IJ" %in% temp_route == T){
      IJ[i] = 1
    }
    if("OR" %in% temp_route == T){
      OR[i] = 1
    }
  }else{
    IV[i] = NA
    IJ[i] = NA
    OR[i] = NA
  }
}
data2 = cbind(data2, IV, IJ, OR, ARG_count[,7:8])
new_data = na.omit(data2)

new_data$delta = new_data$EOS - new_data$BL
new_data$gain = rep(0, nrow(new_data))
new_data$gain2 = rep("gain", nrow(new_data))

# combining the ARG BL and EOS counts from ARG_count dataframe and the data
for(i in 1:nrow(new_data)){
  if(new_data$delta[i] > 0){
    new_data$gain[i] = 1
    new_data$gain2[i] = "gain"
  }else if(new_data$delta[i] < 0){
    new_data$gain[i] = 0
    new_data$gain2[i] = "loss"
  }else{
    new_data$gain[i] = NA
    new_data$gain2[i] = "neither"
  }
}
new_data$gain = as.factor(new_data$gain)
new_data_copy = na.omit(new_data)
###################### DIFFERENCES ################################
# 1. OR
temp_table1 = table(new_data_copy$OR, new_data_copy$gain)
analysis_temp1 = matrix(data = c(temp_table1[[1]], temp_table1[[2]], 
                                temp_table1[[3]], temp_table1[[4]]), nrow = 2)
analysis_temp1 = as.data.frame(analysis_temp1)
rownames(analysis_temp1) = c("not OR", "OR")
colnames(analysis_temp1) = c("gain = 0", "gain = 1")
chisq.test(analysis_temp1, correct = T)

# 2. IV
temp_table2 = table(new_data_copy$IV, new_data_copy$gain)
analysis_temp2 = matrix(data = c(temp_table2[[1]], temp_table2[[2]], 
                                temp_table2[[3]], temp_table2[[4]]), nrow = 2)
analysis_temp2 = as.data.frame(analysis_temp2)
rownames(analysis_temp2) = c("not IV", "IV")
colnames(analysis_temp2) = c("gain = 0", "gain = 1")
chisq.test(analysis_temp2, correct = T)

# 3. IJ
temp_table3 = table(new_data_copy$IJ, new_data_copy$gain)
analysis_temp3 = matrix(data = c(temp_table3[[1]], temp_table3[[2]], 
                                temp_table3[[3]], temp_table3[[4]]), nrow = 2)
analysis_temp3 = as.data.frame(analysis_temp3)
rownames(analysis_temp3) = c("not IJ", "IJ")
colnames(analysis_temp3) = c("gain = 0", "gain = 1")
chisq.test(analysis_temp3, correct = T)

######################### ASSOCIATIONS ###############################

# phi coef similar to Pearson's (0 = no association, +1 = positive association, etc)
# contingency coef

# 1.
phi(analysis_temp1, digits=3)
Assocs(temp_table1)

# 2.
phi(analysis_temp2, digits=3)
Assocs(temp_table2)

# 3.
phi(analysis_temp3, digits=3)
Assocs(temp_table3)

################################################################################
# Question 1c: are there differences between the routes of administrations of drugs 
# patient is on among the different outcome groups 
# (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both) as well as deltas?
wilcox.test(new_data$delta ~ new_data$OR)
wilcox.test(new_data$delta ~ new_data$IV)
wilcox.test(new_data$delta ~ new_data$IJ)


