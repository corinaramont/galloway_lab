# Question: are there differences between the total number of admins a patient receives on a particular drug 
# among the different outcome groups (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both)?

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
library(FSA)

# loading function multiplesheets() from "read_multiple_sheets.R"
# allows us to differentiate and read multiple sheets in a single Excel file
source("Functions/read_multiple_sheets.R")

# loads in datasets of total admins of particular drugs administered to patients we have info on
path = "Datasets/antibiotic_admins_pulsed.xlsx"
data = (multiplesheets(path))$Pulsed

# collect all unique patient mrn's into vector
# initalize new vector total_admins to store results from upcoming for loop
all_ids = unique(data$mrn)
total_admins = c()

for(i in 1:length(all_ids)){
  # filter data based on desired mrn
  temp = data %>% filter(mrn == all_ids[i])
  # collect all the unique drugs associated with that mrn
  temp_drug = unique(temp$Genericname)
  for(j in 1:length(temp_drug)){
    # go through and collect information
    temp2 = temp %>% filter(Genericname == temp_drug[j])
    temp_fill = c(temp2$Cohort[1], temp2$`Patient ID`[1], all_ids[i], temp_drug[j], nrow(temp2))
    total_admins = rbind(total_admins, temp_fill)
  }
}

# convert the results/info into a dataframe data2
data2 = as.data.frame(total_admins)
rownames(data2) = c()
colnames(data2) = c("cohort", "pt_id", "mrn", "drug", "total_admins")

# loads in datasets of route admins for drugs to patients we have info on
path2 = "Datasets/K01_admin_routes_outcomes.xlsx"
route_data = (multiplesheets(path2))$Data

# initialize vector data3 to store following for loop results
data3 = c()
for(i in 1:length(all_ids)){
  # filter by specific mrn and then finding the match in the route_data
  temp = data2 %>% filter(mrn == all_ids[i])
  temp2 = route_data %>% filter(mrn == all_ids[i])
  for(j in 1:nrow(temp)){
    # extract the ARI,ARC,Both values from route_data and combine with data2 stuff
    # insert into data3
    temp_fill = c(temp$cohort[j], temp$pt_id[j], all_ids[i], 
                  temp$drug[j], temp$total_admins[j], temp2$ARI, temp2$ARC, temp2$Both)
    data3 = rbind(data3, temp_fill)
  }
}

# convert vector data3 into dataframe
data3 = as.data.frame(data3)
rownames(data3) = c()
colnames(data3) = c("cohort", "pt_id", "mrn", "drug", "total_admins", "ARI", "ARC", "Both")
data3$total_admins = as.integer(data3$total_admins)
data3$ARI = as.factor(data3$ARI)
data3$ARC = as.factor(data3$ARC)
data3$Both = as.factor(data3$Both)

################## DIFFERENCES #################################

# run wilcox tests

#1. ARI
all_drug = unique(data3$drug)
ARI_wilcox_pvals = c()
for(i in 1:length(all_drug)){
  temp = data3 %>% filter(drug == all_drug[i])
  if(nrow(temp) > 1){
    if((0 %in% temp$ARI == T) & (1 %in% temp$ARI == T)){
      test = wilcox.test(total_admins ~ ARI, data = temp)
      ARI_wilcox_pvals = c(ARI_wilcox_pvals, test$p.value)
    }else{
      ARI_wilcox_pvals = c(ARI_wilcox_pvals, NA)
    }
  }else{
    ARI_wilcox_pvals = c(ARI_wilcox_pvals, NA)
  }
}

#2. ARC
ARC_wilcox_pvals = c()
for(i in 1:length(all_drug)){
  temp = data3 %>% filter(drug == all_drug[i])
  if(nrow(temp) > 1){
    if((0 %in% temp$ARC == T) & (1 %in% temp$ARC == T)){
      test = wilcox.test(total_admins ~ ARC, data = temp)
      ARC_wilcox_pvals = c(ARC_wilcox_pvals, test$p.value)
    }else{
      ARC_wilcox_pvals = c(ARC_wilcox_pvals, NA)
    }
  }else{
    ARC_wilcox_pvals = c(ARC_wilcox_pvals, NA)
  }
}

#3. Both
Both_wilcox_pvals = c()
for(i in 1:length(all_drug)){
  temp = data3 %>% filter(drug == all_drug[i])
  if(nrow(temp) > 1){
    if((0 %in% temp$Both == T) & (1 %in% temp$Both == T)){
      test = wilcox.test(total_admins ~ Both, data = temp)
      Both_wilcox_pvals = c(Both_wilcox_pvals, test$p.value)
    }else{
      Both_wilcox_pvals = c(Both_wilcox_pvals, NA)
    }
  }else{
    Both_wilcox_pvals = c(Both_wilcox_pvals, NA)
  }
}

total_num_admins_wilcox = data.frame(drug = all_drug, ARI = ARI_wilcox_pvals, 
                                     ARC = ARC_wilcox_pvals, Both = Both_wilcox_pvals)

################## ASSOCIATIONS/CORRELATIONS #################################

#1. ARI
all_drug = unique(data3$drug)
ARI_kw_pvals = c()
for(i in 1:length(all_drug)){
  temp = data3 %>% filter(drug == all_drug[i])
  if(nrow(temp) > 1){
    if((0 %in% temp$ARI == T) & (1 %in% temp$ARI == T)){
      test = kruskal.test(total_admins ~ ARI, data = temp)
      ARI_kw_pvals = c(ARI_kw_pvals, test$p.value)
    }else{
      ARI_kw_pvals = c(ARI_kw_pvals, NA)
    }
  }else{
    ARI_kw_pvals = c(ARI_kw_pvals, NA)
  }
}

#2. ARC
ARC_kw_pvals = c()
for(i in 1:length(all_drug)){
  temp = data3 %>% filter(drug == all_drug[i])
  if(nrow(temp) > 1){
    if((0 %in% temp$ARC == T) & (1 %in% temp$ARC == T)){
      test = kruskal.test(total_admins ~ ARC, data = temp)
      ARC_kw_pvals = c(ARC_kw_pvals, test$p.value)
    }else{
      ARC_kw_pvals = c(ARC_kw_pvals, NA)
    }
  }else{
    ARC_kw_pvals = c(ARC_kw_pvals, NA)
  }
}

#3. Both
Both_kw_pvals = c()
for(i in 1:length(all_drug)){
  temp = data3 %>% filter(drug == all_drug[i])
  if(nrow(temp) > 1){
    if((0 %in% temp$Both == T) & (1 %in% temp$Both == T)){
      test = kruskal.test(total_admins ~ Both, data = temp)
      Both_kw_pvals = c(Both_kw_pvals, test$p.value)
    }else{
      Both_kw_pvals = c(Both_kw_pvals, NA)
    }
  }else{
    Both_kw_pvals = c(Both_kw_pvals, NA)
  }
}
total_num_admins_kw = data.frame(drugs = all_drug, ARI = ARI_kw_pvals, 
                                 ARC = ARC_kw_pvals, Both = Both_kw_pvals)


# kruskal wallis done right
num_AR_outcome = rep(0, nrow(data3))
for(i in 1:nrow(data3)){
  if((data3$ARI[i] == 0 & data3$ARC[i] == 1) | (data3$ARI[i] == 1 & data3$ARC[i] == 0)){
    num_AR_outcome[i] = 1
  }
  if(data3$ARI[i] == 1 & data3$ARC[i] == 1){
    num_AR_outcome[i] = 2
  }
}
num_AR_outcome = as.factor(num_AR_outcome)
new_data = cbind(data3[,1:5], num_AR_outcome)
kw_test = kruskal.test(total_admins ~ num_AR_outcome, data = new_data)
kw_test

###############################################################################

# Question: are there differences between the total number of admins a patient receives on a particular drug 
# among the different outcome groups (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both)
# as well as gain/loss ARG counts?

path3 = "Datasets/amr_analysis_pt1_counts.xlsx"
ARG_count = (multiplesheets(path3))$Combined
arg_wilcox_pvalues = c()
arg_kw_pvalues = c()
complete_data_total_admin = c()

for(i in 1:length(all_drug)){
  tempdata = data3 %>% filter(drug == all_drug[i])
  ARG_BL = c()
  ARG_EOS = c()
  for(j in 1:nrow(tempdata)){
    cohort = tempdata$cohort[j]
    pt_id = tempdata$pt_id[j]
    temp = ARG_count %>% filter(cohort == cohort, pt == pt_id)
    if(nrow(temp) > 0){
      ARG_BL[j] = temp$BL
      ARG_EOS[j] = temp$EOS
    }else{
      ARG_BL[j] = NA
      ARG_EOS[j] = NA
    }
  }
  # create new columns in data to hold ARG counts
  tempdata$ARG_BL = ARG_BL
  tempdata$ARG_EOS = ARG_EOS
  
  # take only the patients that have both BL and EOS ARG counts for our next analyses
  new_data = na.omit(tempdata)
  gain = c()
  complete_data_total_admin = rbind(complete_data_total_admin, new_data)
  
  if(nrow(new_data) > 1){
    for(k in 1:nrow(new_data)){
      sum = new_data$ARG_EOS[k] - new_data$ARG_BL[k]
      if(sum > 0){
        gain[k] = 1
      }else{
        gain[k] = 0
      }
    }
    if((0 %in% gain == T) & (1 %in% gain == T)){
      test = wilcox.test(total_admins ~ gain, data = new_data)
      test2 = kruskal.test(total_admins ~ gain, data = new_data)
      arg_wilcox_pvalues = c(arg_wilcox_pvalues, test$p.value)
      arg_kw_pvalues = c(arg_kw_pvalues, test2$p.value)
    }else{
      arg_wilcox_pvalues = c(arg_wilcox_pvalues, NA)
      arg_kw_pvalues = c(arg_kw_pvalues, NA)
    }
  }else{
    arg_wilcox_pvalues = c(arg_wilcox_pvalues, NA)
    arg_kw_pvalues = c(arg_kw_pvalues, NA)
  }
}
arg_drug_results = data.frame(drug = all_drug, pval = arg_wilcox_pvalues)
arg_kw_results = data.frame(drug = all_drug, pval = arg_kw_pvalues)

rownames(complete_data_total_admin) = c()
#write_xlsx(complete_data_total_admin, 
#           path = "~/Documents/research for dr.g-p/my created datasets/complete_data_total_admin.xlsx")

###############################################################################

# Question: are there differences between the total number of admins a patient receives on a particular drug 
# among the different outcome groups (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both)
# as well as delta ARG counts?

delta_pvals = c()

for(i in 1:length(all_drug)){
  tempdata = data3 %>% filter(drug == all_drug[i])
  ARG_BL = c()
  ARG_EOS = c()
  for(j in 1:nrow(tempdata)){
    cohort = tempdata$cohort[j]
    pt_id = tempdata$pt_id[j]
    temp = ARG_count %>% filter(cohort == cohort, pt == pt_id)
    if(nrow(temp) > 0){
      ARG_BL[j] = temp$BL
      ARG_EOS[j] = temp$EOS
    }else{
      ARG_BL[j] = NA
      ARG_EOS[j] = NA
    }
  }
  # create new columns in data to hold ARG counts
  tempdata$ARG_BL = ARG_BL
  tempdata$ARG_EOS = ARG_EOS
  
  # take only the patients that have both BL and EOS ARG counts for our next analyses
  new_data = na.omit(tempdata)
  delta = c()
  
  if(nrow(new_data) > 1){
    for(k in 1:nrow(new_data)){
      delta[k] = new_data$ARG_EOS[k] - new_data$ARG_BL[k]
    }
    
    test = cor.test(new_data$total_admins, delta, method = "spearman")
    delta_pvals = c(delta_pvals, test$p.value)
  }else{
    delta_pvals = c(delta_pvals, test$p.value)
  }
}

delta_spear_results = data.frame(drug = all_drug, pval = delta_pvals)
