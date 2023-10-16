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
wilcox.test(num_of_uniq_drugs ~ ARI, data = data)
wilcox.test(num_of_uniq_drugs ~ ARC, data = data)
wilcox.test(num_of_uniq_drugs ~ Both, data = data)

###################### ASSOCIATIONS ###############################

# kruskal-wallis
kruskal_ARI = kruskal.test(num_of_uniq_drugs ~ ARI, data = data)
kruskal_ARC = kruskal.test(num_of_uniq_drugs ~ ARC, data = data)
kruskal_Both = kruskal.test(num_of_uniq_drugs ~ Both, data = data)

# store p-values into data frame kruskal_results
kruskal_results = data.frame(outcome = c("ARI", "ARC", "Both"), 
                             pvals = c(kruskal_ARI$p.value, kruskal_ARC$p.value,
                                       kruskal_Both$p.value))
################################################################################

# Question 1b: are there differences between the number of unique drugs a 
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
wilcox.test(num_of_uniq_drugs ~ gain, data = new_data)

#########################################################################
# Question 1b: are there differences between the number of unique drugs a 
# patient is on among the different outcome groups 
# (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both) as well as delta ARG counts?
new_data$delta = new_data$ARG_EOS - new_data$ARG_BL
cor.test(new_data$num_of_uniq_drugs, new_data$delta, method = "spearman")
