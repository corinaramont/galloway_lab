# Question: are there differences between the number of days a patient is on a particular drug 
# on among the different outcome groups (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both)?

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

# loads in datasets of # of days drugs administered of patients we have info on
path = "Datasets/antibiotic_admins_pulsed.xlsx"
num_days = (multiplesheets(path))$reduced

all_mrn = unique(num_days$mrn)
data = c()
mrn_fill_count = 0

error_fill = c()

for(i in 1:length(all_mrn)){
  temp = num_days %>% filter(mrn == all_mrn[i])
  temp_drug = unique(temp$Genericname)
  # error checking
  fill2 = c(mrn[i], length(temp_drug))
  error_fill = rbind(error_fill,fill2)
}
error_fill = as.data.frame(error_fill)

for(i in 1:length(all_mrn)){
  temp = num_days %>% filter(mrn == all_mrn[i])
  temp_drug = unique(temp$Genericname)
  
  fill = data.frame(cohort = rep(temp$Cohort[1],length(temp_drug)),
                    pt_id = rep(temp$`Patient ID`[1], length(temp_drug)),
                    mrn = rep(all_mrn[i],length(temp_drug)),
                    Genericname = temp_drug)
  data = rbind(data, fill)
}

data$num_days = rep(0,nrow(data))

for(i in 1:nrow(data)){
  temp = num_days %>% filter(mrn == data$mrn[i],Genericname == data$Genericname[i])
  data$num_days[i] = nrow(temp)
}
#write_xlsx(data, path = "Datasets/K01_num_days_correct.xlsx")

################### save for later #########################

# create new variable called num_AR_outcome for KW testing purposes
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
data = cbind(data, num_AR_outcome)

############################################################

# collect all the unique drug names
drugs = unique(data$Genericname)

# let's go look at some pretty plots and ofc also do some fun analyses

# note that wilcoxon test aka mann whitney tests if the 2 populations are identical or not

# checking normality of counts
ggdensity(data$Days_on_drug, 
          main = "Density plot of # of days on drugs", 
          xlab = "# of days on drugs")
ggqqplot(data$Days_on_drug, main = "QQ Plot for days on drugs counts")
shapiro.test(data$Days_on_drug)

######################### 1. ceFEPime HCL ###########################
kruskal_pvals = c()
wilcox_pvals = c()

# create a new dataframe called data1
# filtered to keep only drug name ceFEPime HCL for analysis
data1 = (data %>% filter(Genericname == drugs[1]))

# ARI
ggplot(data1, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[1])
test = wilcox.test(Days_on_drug ~ ARI, data = data1)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data1, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[1])
test = wilcox.test(Days_on_drug ~ ARC, data = data1)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data1, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[1])
test = wilcox.test(Days_on_drug ~ Both, data = data1)
wilcox_pvals = c(wilcox_pvals, test$p.value)


# associations
kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data1)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

#################### 2. cefPODoxime Proxetil ############################

# create a new dataframe called data2
# filtered to keep only drug name cefPODoxime Proxetil for analysis
data2 = (data %>% filter(Genericname == drugs[2]))

# ARI
ggplot(data2, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[2])
test = wilcox.test(Days_on_drug ~ ARI, data = data2)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data2, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[2])
test = wilcox.test(Days_on_drug ~ ARC, data = data2)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data2, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "purple", size = 0.4, alpha = 0.9) + ggtitle(drugs[2])
test = wilcox.test(Days_on_drug ~ Both, data = data2)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data2)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

#dunnTest(Days_on_drug ~ num_AR_outcome, data = data2)

##################### 3. CIPROfloxacin ##################################

data3 = (data %>% filter(Genericname == drugs[3]))

# ARI
ggplot(data3, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[3])
test = wilcox.test(Days_on_drug ~ ARI, data = data3)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data3, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[3])
test = wilcox.test(Days_on_drug ~ ARC, data = data3)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data3, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[3])
test = wilcox.test(Days_on_drug ~ Both, data = data3)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data3)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

######################### 4. Levofloxacin ################################

data4 = (data %>% filter(Genericname == drugs[4]))

# ARI
ggplot(data4, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[4])
test = wilcox.test(Days_on_drug ~ ARI, data = data4)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data4, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[4])
test = wilcox.test(Days_on_drug ~ ARC, data = data4)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data4, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[4])
test = wilcox.test(Days_on_drug ~ Both, data = data4)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data4)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

######################### 5. Linezolid ###################################

data5 = (data %>% filter(Genericname == drugs[5]))

# ARI
ggplot(data5, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[5])
test = wilcox.test(Days_on_drug ~ ARI, data = data5)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data5, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[5])
test = wilcox.test(Days_on_drug ~ ARC, data = data5)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data5, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[5])
test = wilcox.test(Days_on_drug ~ Both, data = data5)
wilcox_pvals = c(wilcox_pvals, test$p.value)


kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data5)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

########################### 6. Amikacin ###################################

data6 = (data %>% filter(Genericname == drugs[6]))

# ARI
ggplot(data6, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[6])
test = wilcox.test(Days_on_drug ~ ARI, data = data6)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data6, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[6])
test = wilcox.test(Days_on_drug ~ ARC, data = data6)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data6, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[6])
test = wilcox.test(Days_on_drug ~ Both, data = data6)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data6)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)
############################ 7. MeroPENEM #################################

data7 = (data %>% filter(Genericname == drugs[7]))

# ARI
ggplot(data7, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[7])
test = wilcox.test(Days_on_drug ~ ARI, data = data7)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data7, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[7])
test = wilcox.test(Days_on_drug ~ ARC, data = data7)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data7, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[7])
test = wilcox.test(Days_on_drug ~ Both, data = data7)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data7)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

########################## 8. Tigecycline #################################

data8 = (data %>% filter(Genericname == drugs[8]))

# ARI
ggplot(data8, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[8])
test = wilcox.test(Days_on_drug ~ ARI, data = data8)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data8, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[8])
test = wilcox.test(Days_on_drug ~ ARC, data = data8)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data8, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[8])
test = wilcox.test(Days_on_drug ~ Both, data = data8)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data8)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

######################### 9. Piperacillin/Tazobactam #######################

data9 = (data %>% filter(Genericname == drugs[9]))

# ARI
ggplot(data9, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[9])
test = wilcox.test(Days_on_drug ~ ARI, data = data9)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data9, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[9])
test = wilcox.test(Days_on_drug ~ ARC, data = data9)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data9, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[9])
test = wilcox.test(Days_on_drug ~ Both, data = data9)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data9)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

######################## 10. AZITHromycin ##################################

data10 = (data %>% filter(Genericname == drugs[10]))

# ARI
ggplot(data10, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[10])
#wilcox.test(Days_on_drug ~ ARI, data = data10)
wilcox_pvals = c(wilcox_pvals, NA)

# ARC
ggplot(data10, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[10])
test = wilcox.test(Days_on_drug ~ ARC, data = data10)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data10, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[10])
test = wilcox.test(Days_on_drug ~ Both, data = data10)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data10)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

######################## 11. Ertapenem Sodium ############################

data11 = (data %>% filter(Genericname == drugs[11]))

# ARI
ggplot(data11, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[11])
test = wilcox.test(Days_on_drug ~ ARI, data = data11)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data11, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[11])
test = wilcox.test(Days_on_drug ~ ARC, data = data11)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data11, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[11])
test = wilcox.test(Days_on_drug ~ Both, data = data11)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data11)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

###################### 12. Metronidazole #############################

data12 = (data %>% filter(Genericname == drugs[12]))

# ARI
ggplot(data12, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[12])
test = wilcox.test(Days_on_drug ~ ARI, data = data12)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data12, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[12])
test = wilcox.test(Days_on_drug ~ ARC, data = data12)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data12, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[12])
test = wilcox.test(Days_on_drug ~ Both, data = data12)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data12)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)
################## 13. Trimethoprim/Sulfamethoxazole ####################

data13 = (data %>% filter(Genericname == drugs[13]))

# ARI
ggplot(data13, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[13])
#wilcox.test(Days_on_drug ~ ARI, data = data13)
wilcox_pvals = c(wilcox_pvals, NA)

# ARC
ggplot(data13, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[13])
test = wilcox.test(Days_on_drug ~ ARC, data = data13)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data13, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[13])
test = wilcox.test(Days_on_drug ~ Both, data = data13)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data13)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)


######################### 14. DAPTOmycin #################################

data14 = (data %>% filter(Genericname == drugs[14]))

# ARI
ggplot(data14, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[14])
test = wilcox.test(Days_on_drug ~ ARI, data = data14)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data14, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[14])
test = wilcox.test(Days_on_drug ~ ARC, data = data14)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data14, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[14])
test = wilcox.test(Days_on_drug ~ Both, data = data14)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data14)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

######################### 15. cefTAZidime ###############################

data15 = (data %>% filter(Genericname == drugs[15]))

# ARI
ggplot(data15, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[15])
#wilcox.test(Days_on_drug ~ ARI, data = data15)
wilcox_pvals = c(wilcox_pvals, NA)

# ARC
ggplot(data15, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[15])
test = wilcox.test(Days_on_drug ~ ARC, data = data15)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data15, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[15])
test = wilcox.test(Days_on_drug ~ Both, data = data15)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data15)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

####################### 16. Minocycline ##################################

data16 = (data %>% filter(Genericname == drugs[16]))

# ARI
ggplot(data16, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[16])
test = wilcox.test(Days_on_drug ~ ARI, data = data16)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# ARC
ggplot(data16, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[16])
test = wilcox.test(Days_on_drug ~ ARC, data = data16)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data16, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[16])
test = wilcox.test(Days_on_drug ~ Both, data = data16)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data16)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

####################### 17. cefTRIAXone Sodium #########################

data17 = (data %>% filter(Genericname == drugs[17]))

# ARI
ggplot(data17, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[17])
#wilcox.test(Days_on_drug ~ ARI, data = data17)
wilcox_pvals = c(wilcox_pvals, NA)

# ARC
ggplot(data17, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[17])
test = wilcox.test(Days_on_drug ~ ARC, data = data17)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data17, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[17])
test = wilcox.test(Days_on_drug ~ Both, data = data17)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data17)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)
################## 18. Amoxicillin & Pot Clavulanate #####################

data18 = (data %>% filter(Genericname == drugs[18]))

# ARI
ggplot(data18, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[18])
#wilcox.test(Days_on_drug ~ ARI, data = data18)
wilcox_pvals = c(wilcox_pvals, NA)

# ARC
ggplot(data18, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[18])
test = wilcox.test(Days_on_drug ~ ARC, data = data18)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data18, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[18])
test = wilcox.test(Days_on_drug ~ Both, data = data18)
wilcox_pvals = c(wilcox_pvals, test$p.value)


kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data18)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

######################## 19. Aztreonam #####################################

data19 = (data %>% filter(Genericname == drugs[19]))

# ARI
ggplot(data19, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[19])
#wilcox.test(Days_on_drug ~ ARI, data = data19)
wilcox_pvals = c(wilcox_pvals, NA)

# ARC
ggplot(data19, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[19])
test = wilcox.test(Days_on_drug ~ ARC, data = data19)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data19, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[19])
test = wilcox.test(Days_on_drug ~ Both, data = data19)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data19)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

######################## 20. Vancomycin ##############################

data20 = (data %>% filter(Genericname == drugs[20]))

# ARI
ggplot(data20, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[20])
#wilcox.test(Days_on_drug ~ ARI, data = data20)
wilcox_pvals = c(wilcox_pvals, NA)

# ARC
ggplot(data20, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[20])
test = wilcox.test(Days_on_drug ~ ARC, data = data20)
wilcox_pvals = c(wilcox_pvals, test$p.value)

# Both
ggplot(data20, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[20])
test = wilcox.test(Days_on_drug ~ Both, data = data20)
wilcox_pvals = c(wilcox_pvals, test$p.value)

kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data20)
kruskal_pvals = c(kruskal_pvals, kw_test$p.value)

###################### 21. Tobramycin Sulfate #############################

data21 = (data %>% filter(Genericname == drugs[21]))

# ARI
ggplot(data21, aes(ARI, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "red", size = 0.4, alpha = 0.9) + ggtitle(drugs[21])
#wilcox.test(Days_on_drug ~ ARI, data = data21)
wilcox_pvals = c(wilcox_pvals, NA)

# ARC
ggplot(data21, aes(ARC, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "blue", size = 0.4, alpha = 0.9) + ggtitle(drugs[21])
#wilcox.test(Days_on_drug ~ ARC, data = data21)
wilcox_pvals = c(wilcox_pvals, NA)

# Both
ggplot(data21, aes(Both, Days_on_drug)) + geom_boxplot() + 
  geom_jitter(color = "green", size = 0.4, alpha = 0.9) + ggtitle(drugs[21])
#wilcox.test(Days_on_drug ~ Both, data = data21)
wilcox_pvals = c(wilcox_pvals, NA)

#kw_test = kruskal.test(Days_on_drug ~ num_AR_outcome, data = data21)
kruskal_pvals = c(kruskal_pvals, NA)

################### WILCOX results compiled ############################
all_drug = c()
for(i in 1:length(drugs)){
  all_drug = c(all_drug, rep(drugs[i], 3))
}
outcome = rep(c("ARI", "ARC", "Both"), 3)

planC_wilcox = data.frame(drug = all_drug, outcome = outcome, pval = wilcox_pvals)
planC_wilcox = planC_wilcox %>% spread(outcome, pval)
#write_xlsx(planC_wilcox, path = "~/Documents/research for dr.g-p/my created datasets/univar_planC_wilcox_results.xlsx")

################### corr analysis results compiled ############################

# kruskal wallis results compiled
planC_kruskal_results = data.frame(drug = drugs, pvals = kruskal_pvals)

#write_xlsx(planC_kruskal_results, path = "Datasets/planC_kruskal_wallis_results.xlsx")

################################################################################

# Question: are there differences between the number of days a patient is on a particular drug 
# on among the different outcome groups (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both) 
# as well as gain/loss of ARGs?

path2 = "Datasets/bl_eos_arg_counts.xlsx"
ARG_count = (multiplesheets(path2))$collection_info

path3 = "Datasets/K01_AntibioticData_for_ARG.xlsx"
new_admin_data = (multiplesheets(path3))$Uniq_days

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
new = as.data.frame(new)
colnames(new)[4:5] = c("Genericname","Days_on_drug")
rownames(new) = c()
new$delta = as.numeric(new$EOS) - as.numeric(new$BL)
new$gain = rep(0, nrow(new))
new$gain2 = rep("gain", nrow(new))

# combining the ARG BL and EOS counts from ARG_count dataframe and the data
for(i in 1:nrow(new)){
  if(new$delta[i] > 0){
    new$gain[i] = 1
    new$gain2[i] = "gain"
  }else if(new$delta[i] < 0){
    new$gain[i] = 0
    new$gain2[i] = "loss"
  }else{
    new$gain[i] = NA
    new$gain2[i] = "neither"
  }
}
new$gain = as.factor(new$gain)

for(i in 1:ncol(new)){
  new[,i] = unlist(new[,i])
}
new1 = na.omit(new)

arg_wilcox_pvalues = c()
arg_kw_pvalues = c()

for(i in 1:length(drugs)){
  temp = new1 %>% filter(Genericname == drugs[i])
  temp2 = new %>% filter(Genericname == drugs[i])
  if(nrow(temp) > 1){
    if((0 %in% temp$gain == T) & (1 %in% temp$gain == T)){
      test = wilcox.test(Days_on_drug ~ gain, data = temp)
      test2 = kruskal.test(Days_on_drug ~ gain2, data = temp2)
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

arg_wilcox_results = data.frame(drug = drugs, pval = arg_wilcox_pvalues)
arg_kw_results = data.frame(drug = drugs, pval = arg_kw_pvalues)

# dunn's correction for linezolid
dunnTest(Days_on_drug ~ gain2, data = new %>% filter(Genericname == drugs[5]))

################################################################################

# Question: are there differences between the number of days a patient is on a particular drug 
# on among the different outcome groups (non-ARI vs. ARI, non-ARC vs. ARC, non-Both vs. Both) 
# as well as delta ARG counts?

delta_pvals = c()



