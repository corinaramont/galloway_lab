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
p = c(ARI_wilcox_pvals, ARC_wilcox_pvals, Both_wilcox_pvals)
adj = p.adjust(p, method = "fdr", n = length(p))
total_num_admins_wilcox$ARI_adj = adj[1:21]
total_num_admins_wilcox$ARC_adj = adj[22:42]
total_num_admins_wilcox$Both_adj = adj[43:63]

################## ASSOCIATIONS/CORRELATIONS #################################

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

kw_pvals = c()
for(i in 1:length(all_drug)){
  temp = new_data %>% filter(drug == all_drug[i])
  if(nrow(temp) > 1){
    if((0 %in% temp$num_AR_outcome == T) & (1 %in% temp$num_AR_outcome == T) & (2 %in% temp$num_AR_outcome == T)){
      test = kruskal.test(total_admins ~ num_AR_outcome, data = temp)
      kw_pvals = c(kw_pvals, test$p.value)
    }else{
      kw_pvals = c(kw_pvals, NA)
    }
  }else{
    kw_pvals = c(kw_pvals, NA)
  }
}
total_admin_kw_results = data.frame(drug = all_drug, pvals = kw_pvals)

###############################################################################

# Question: are there differences between the total number of admins a patient receives on a particular drug 
# among the different outcome groups (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both)
# as well as gain/loss ARG counts?

path2 = "Datasets/bl_eos_arg_counts.xlsx"
ARG_count = (multiplesheets(path2))$collection_info

path3 = "Datasets/K01_AntibioticData_for_ARG.xlsx"
new_admin_data = (multiplesheets(path3))$Filtered

new = c()

for(i in 1:nrow(ARG_count)){
  temp = new_admin_data %>% filter(mrn == ARG_count$mrn[i])
  if(nrow(temp) > 0){
    temp_drug = unique(temp$Genericname)
    for(j in 1:length(temp_drug)){
      temp2 = temp %>% filter(Genericname == temp_drug[j])
      fill = c(ARG_count[i,1:3], temp_drug[j], nrow(temp2), ARG_count[i,7:8])
      new = rbind(new, fill)
    }
  }
}
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
