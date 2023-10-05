# Question: are there differences between the number of unique drugs a 
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

###################### DIFFERENCES ###############################

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