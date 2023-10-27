# Question 1a: are there differences between the number of unique drugs a 
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
library(FSA)

# loading function multiplesheets() from "read_multiple_sheets.R"
# allows us to differentiate and read multiple sheets in a single Excel file
source("Functions/read_multiple_sheets.R")

# loads in datasets of # of unique drugs administered of patients we have info on
path = "Datasets/K01_uniq_drug_outcomes.xlsx"
data = (multiplesheets(path))$Data

# checking normality of counts
ggdensity(data$num_of_uniq_drugs, 
          main = "Density plot of counts of # of unique drugs", 
          xlab = "# of unique drugs")
ggqqplot(data$num_of_uniq_drugs, main = "QQ Plot for # of unique drugs counts")
shapiro.test(data$num_of_uniq_drugs)

###################### DIFFERENCES ################################

# running mann whitney u aka wilcoxon rank sum tests to see if the 2 groups come 
# from the same distribution or not
test1 = wilcox.test(num_of_uniq_drugs ~ ARI, data = data)
test2 = wilcox.test(num_of_uniq_drugs ~ ARC, data = data)
test3 = wilcox.test(num_of_uniq_drugs ~ Both, data = data)
p = c(test1$p.value, test2$p.value, test3$p.value)
uniq_drug_wilcox = data.frame(outcome = c("non-ARI vs. ARI", "non-ARC vs. ARC",
                                          "non-Both vs. Both"), 
                              pvals = p)
adj = p.adjust(p, method = "fdr", n = length(p))
uniq_drug_wilcox$pvals_adj = adj
###################### ASSOCIATIONS ###############################

# kruskal-wallis actually done right
num_AR_outcome = rep(0, nrow(data))
for(i in 1:nrow(data)){
  if((data$ARI[i] == 0 & data$ARC[i] == 1) | (data$ARI[i] == 1 & data$ARC[i] == 0)){
    num_AR_outcome[i] = 1
  }
  if(data$ARI[i] == 1 & data$ARC[i] == 1){
    num_AR_outcome[i] = 2
  }
}
num_AR_outcome = as.factor(num_AR_outcome)
new_data = cbind(data[,1:4], num_AR_outcome)
kw_test = kruskal.test(num_of_uniq_drugs ~ num_AR_outcome, data = new_data)
#dunnTest(num_of_uniq_drugs ~ num_AR_outcome, data = new_data)


################################################################################

# Question 1b: are there differences between the number of unique drugs a 
# patient is on among the different outcome groups 
# (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both) as well as gain/loss of ARGs?

path2 = "Datasets/bl_eos_arg_counts.xlsx"
ARG_count = (multiplesheets(path2))$collection_info

path3 = "Datasets/K01_AntibioticData_for_ARG.xlsx"
new_admin_data = (multiplesheets(path3))$Filtered

data2 = ARG_count[,1:3]

num_uniq_drug = c()

for(i in 1:nrow(data2)){
  temp = new_admin_data %>% filter(mrn == data2$mrn[i])
  if(nrow(temp) > 0){
    temp_drugs = unique(temp$Genericname)
    num_uniq_drug = c(num_uniq_drug, length(temp_drugs))
  }else{
    num_uniq_drug = c(num_uniq_drug, NA)
  }
}

data2 = cbind(data2, num_uniq_drug, ARG_count[,7:8])
new_data2 = na.omit(data2)

new_data2$delta = new_data2$EOS - new_data2$BL
new_data2$gain = rep(0, nrow(new_data2))
new_data2$gain2 = rep("gain", nrow(new_data2))

# combining the ARG BL and EOS counts from ARG_count dataframe and the data
for(i in 1:nrow(new_data2)){
  if(new_data2$delta[i] > 0){
    new_data2$gain[i] = 1
    new_data2$gain2[i] = "gain"
  }else if(new_data2$delta[i] < 0){
    new_data2$gain[i] = 0
    new_data2$gain2[i] = "loss"
  }else{
    new_data2$gain[i] = NA
    new_data2$gain2[i] = "neither"
  }
}
new_data2$gain = as.factor(new_data2$gain)
new_data2_copy = na.omit(new_data2)


###################### DIFFERENCES ################################
wilcox.test(num_uniq_drug ~ gain, data = new_data2_copy)

###################### ASSOCIATIONS ################################
kruskal.test(num_uniq_drug ~ gain2, data = new_data2)

#########################################################################
# Question 1c: are there differences between the number of unique drugs a 
# patient is on among the different outcome groups 
# (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both) as well as delta ARG counts?
cor.test(new_data2$num_uniq_drug, new_data2$delta, method = "spearman")
