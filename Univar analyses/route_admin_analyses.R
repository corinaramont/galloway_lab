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

# 1. ARI and OR
temp_table = table(data$OR, data$ARI)
analysis_temp = matrix(data = c(temp_table[[1]], temp_table[[2]], 
                                temp_table[[3]], temp_table[[4]]), nrow = 2)
analysis_temp = as.data.frame(analysis_temp)
rownames(analysis_temp) = c("not OR", "OR")
colnames(analysis_temp) = c("not ARI", "ARI")
analysis_temp
chisq.test(analysis_temp, correct = T)

# 2. ARI and IV
temp_table2 = table(data$IV, data$ARI)
analysis_temp2 = matrix(data = c(temp_table2[[1]], temp_table2[[2]], 
                                 temp_table2[[3]], temp_table2[[4]]), nrow = 2)
analysis_temp2 = as.data.frame(analysis_temp2)
rownames(analysis_temp2) = c("not IV", "IV")
colnames(analysis_temp2) = c("not ARI", "ARI")
fisher.test(analysis_temp2)

# 3. ARI and IJ
temp_table3 = table(data$IJ, data$ARI)
analysis_temp3 = matrix(data = c(temp_table3[[1]], temp_table3[[2]], 
                                 temp_table3[[3]], temp_table3[[4]]), nrow = 2)
analysis_temp3 = as.data.frame(analysis_temp3)
rownames(analysis_temp3) = c("not IJ", "IJ")
colnames(analysis_temp3) = c("not ARI", "ARI")
fisher.test(analysis_temp3)

# 4. ARC and OR 
temp_table4 = table(data$OR, data$ARC)
analysis_temp4 = matrix(data = c(temp_table4[[1]], temp_table4[[2]], 
                                 temp_table4[[3]], temp_table4[[4]]), nrow = 2)
analysis_temp4 = as.data.frame(analysis_temp4)
rownames(analysis_temp4) = c("not OR", "OR")
colnames(analysis_temp4) = c("not ARC", "ARC")
chisq.test(analysis_temp4, correct = T)

# 5. ARC and IV
temp_table5 = table(data$IV, data$ARC)
analysis_temp5 = matrix(data = c(temp_table5[[1]], temp_table5[[2]], 
                                 temp_table5[[3]], temp_table5[[4]]), nrow = 2)
analysis_temp5 = as.data.frame(analysis_temp5)
rownames(analysis_temp5) = c("not IV", "IV")
colnames(analysis_temp5) = c("not ARC", "ARC")
fisher.test(analysis_temp5)

# 6. ARC and IJ
temp_table6 = table(data$IJ, data$ARC)
analysis_temp6 = matrix(data = c(temp_table6[[1]], temp_table6[[2]], 
                                 temp_table6[[3]], temp_table6[[4]]), nrow = 2)
analysis_temp6 = as.data.frame(analysis_temp6)
rownames(analysis_temp6) = c("not IJ", "IJ")
colnames(analysis_temp6) = c("not ARC", "ARC")
chisq.test(analysis_temp6, correct = T)

# 7. Both and OR
temp_table7 = table(data$OR, data$Both)
analysis_temp7 = matrix(data = c(temp_table7[[1]], temp_table7[[2]], 
                                 temp_table7[[3]], temp_table7[[4]]), nrow = 2)
analysis_temp7 = as.data.frame(analysis_temp7)
rownames(analysis_temp7) = c("not OR", "OR")
colnames(analysis_temp7) = c("not Both", "Both")
chisq.test(analysis_temp7, correct = T)

# 8. Both and IV
temp_table8 = table(data$IV, data$Both)
analysis_temp8 = matrix(data = c(temp_table8[[1]], temp_table8[[2]], 
                                 temp_table8[[3]], temp_table8[[4]]), nrow = 2)
analysis_temp8 = as.data.frame(analysis_temp8)
rownames(analysis_temp8) = c("not IV", "IV")
colnames(analysis_temp8) = c("not Both", "Both")
fisher.test(analysis_temp8)

# 9. Both and IJ
temp_table9 = table(data$IJ, data$Both)
analysis_temp9 = matrix(data = c(temp_table9[[1]], temp_table9[[2]], 
                                 temp_table9[[3]], temp_table9[[4]]), nrow = 2)
analysis_temp9 = as.data.frame(analysis_temp9)
rownames(analysis_temp9) = c("not IJ", "IJ")
colnames(analysis_temp9) = c("not Both", "Both")
chisq.test(analysis_temp9, correct = T)


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

path2 = "Datasets/amr_analysis_pt1_counts.xlsx"
ARG_count = (multiplesheets(path2))$Combined

ARG_BL = c()
ARG_EOS = c()

# combining the ARG BL and EOS counts from ARG_count dataframe and the data
for(i in 1:nrow(data)){
  cohort = data$Cohort[i]
  pt_id = data$pt_id[i]
  temp = ARG_count %>% filter(cohort == cohort, pt == pt_id)
  if(nrow(temp) > 0){
    ARG_BL[i] = temp$BL
    ARG_EOS[i] = temp$EOS
  }else{
    ARG_BL[i] = NA
    ARG_EOS[i] = NA
  }
}

# create new columns in data to hold ARG counts
data$ARG_BL = ARG_BL
data$ARG_EOS = ARG_EOS

# take only the patients that have both BL and EOS ARG counts for our next analyses
new_data = na.omit(data)
gain = c()

for(i in 1:nrow(new_data)){
  if(new_data$ARG_EOS[i] - new_data$ARG_BL[i] > 0){
    gain[i] = 1
  }else{
    gain[i] = 0
  }
}

# new_data has added binary variable called gain (0 = loss/neither, 1 = gain)
new_data$gain = as.factor(gain)

###################### DIFFERENCES ################################
# 1. OR
temp_table1 = table(new_data$OR, new_data$gain)
analysis_temp1 = matrix(data = c(temp_table1[[1]], temp_table1[[2]], 
                                temp_table1[[3]], temp_table1[[4]]), nrow = 2)
analysis_temp1 = as.data.frame(analysis_temp1)
rownames(analysis_temp1) = c("not OR", "OR")
colnames(analysis_temp1) = c("gain = 0", "gain = 1")
chisq.test(analysis_temp1, correct = T)

# 2. IV
temp_table2 = table(new_data$IV, new_data$gain)
analysis_temp2 = matrix(data = c(temp_table2[[1]], temp_table2[[2]], 
                                temp_table2[[3]], temp_table2[[4]]), nrow = 2)
analysis_temp2 = as.data.frame(analysis_temp2)
rownames(analysis_temp2) = c("not IV", "IV")
colnames(analysis_temp2) = c("gain = 0", "gain = 1")
chisq.test(analysis_temp2, correct = T)

# 3. IJ
temp_table3 = table(new_data$IJ, new_data$gain)
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
