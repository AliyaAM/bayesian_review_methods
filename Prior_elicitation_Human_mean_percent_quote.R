#install.packages("RColorBrewer")

library(tidyverse)
library(dplyr)
library(assertthat)
library(ggplot2)
library(filenamer)
library(reshape2)  
library(tibble)
library(compute.es)
library(metafor)
#library(bayesplot)
library(ggplot2)
library(ggridges)
#library(rstan) 
library(coda)
library(bayestestR)
library(HDInterval)
library(assertthat)
library(RColorBrewer)
library(meta)

## Set the root directory to look for source code.
#SOURCE_ROOT = "/Users/aliyaamirova/proj/bayesian_meta_analysis/"


print("IMPORTANT NEXT STEP: IMPORTANT NEXT STEP IMPORTANT NEXT STEP IMPORTANT NEXT STEP IMPORTANT NEXT STEP change the code below, so we can go over all constructs or atleast those that were measured in the likelihood (detailed in the comments below) if we change BaCapenblr_selfEfficacy to generic construct or index it somehow then we can run a function that goes through all constructs") 



print("check carefully for quotes on how much physical activty each participant did")
print("we should reduce to those constructs that are measured using perceived latent variables (self-efficacy, positive attitude, negative attitude, perceived social support (CHECK THIS ONE), symptom distress")
print("consider how to deal with constructs with more than one belief statements (aggregate/average...?)")
print("decide on the variance for the prior")
print("systematically include all constructs from quant and from qual")
print("plot prior from Human and Human prior next to each other")
print("compare Human and Human prior using prior–data conflict determination using data agreement criterion")
print("plot violin distributions for belief quotes etc...")
print("check overleaf for to do list")


########### DIRECTORY

#directory = "/Users/aliyaamirova/"
#directory = "/Users/aliya/my_docs/"
directory = "/Users/k2147340/OneDrive - King's College London/Documents/"


###########  source root 
SOURCE_ROOT = paste(directory, "proj/bayesian_review_methods/", sep = "")

###########  data root
DATA_ROOT = paste(directory, "proj/bayesian_review_methods/DATA/", sep = "")

########### Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/", sep = "")


############ DATA

Human_perBS_perBot = read.csv(paste(DATA_ROOT, "Human_perBS_per_participant_FINAL.csv", sep = "")) 


#############

#fraction of quotes concerning 'construct' when 'participant' was active 
#fraction of quotes concerning 'construct' when 'participant' was sedentary

#1 - fraction of quotes concerning 'construct' when 'participant' was active  
#1 - fraction of quotes concerning 'construct' when 'participant' was sedentary

#OR = from the contingency table of the above 

#############

#Human self-efficacy: #BaCapenblr_selfEfficacy


sedentary = subset(Human_perBS_perBot, Human_perBS_perBot$PA_status == 0)
nrow(sedentary)
  
unique(Human_perBS_perBot$Cases)
Human_perBS_perBot[rowSums(is.na(Human_perBS_perBot)) > 0,]
ls(Human_perBS_perBot)
ncol(Human_perBS_perBot)

Human_perBS_perBot = tibble(Human_perBS_perBot) %>% 
  replace(is.na(.), 0) %>%
  mutate(sum_quotes_per_participant = rowSums(across(where(is.numeric)))) %>%
  mutate(name = Cases)


matrix_empty = matrix(nrow = 1, ncol = 0)

OR_df = data.frame(matrix_empty) 
SMD_df_merged_BS = data.frame(matrix_empty) 
SE_df_merged_BS = data.frame(matrix_empty) 

for (i in colnames(Human_perBS_perBot[,c(-1, -127, -128)])){
  

  print(i)
  
  x = unlist(as.vector(Human_perBS_perBot[,i]))
  
  
  
  Human_perBS_perBot$sum_quotes_per_participant
  
  Human_perBS_perBot = Human_perBS_perBot  %>% 
    mutate(x_fraction = x/sum_quotes_per_participant + 0.001)
  
  print('got here')
  
  
  long_data = Human_perBS_perBot %>%
    select(name, PA_status, x_fraction) %>%
    gather("key", 'x_fraction', -name, -PA_status) %>%
    mutate(fraction_not_present = 1 - x_fraction + 0.001) %>%
    arrange(PA_status) %>%
    group_by(PA_status) %>%
    group_split(PA_status)
  
  mean_fraction_sedentary = mean(long_data[[1]]$x_fraction) 
  mean_fraction_not_present_sedentary = mean(long_data[[1]]$fraction_not_present) 
  
  mean_fraction_active = mean(long_data[[2]]$x_fraction) 
  mean_fraction_not_present_active = mean(long_data[[2]]$fraction_not_present) 
  
  OR_temp = (mean_fraction_active*mean_fraction_not_present_sedentary)/(mean_fraction_not_present_active*mean_fraction_sedentary)
  #OR_temp = (active_present*sedentary_not_present)/(active_not_present*sedentary_present)
  

  OR_df = cbind(OR_df, OR_temp)
  print("decide on how to obtain variance for ORs for each construct in the human prior because it is only a single number")
  
  both_means = c(mean_fraction_active, mean_fraction_sedentary)
  
  SMD_temp = (mean_fraction_active - mean_fraction_sedentary)/(sd(both_means)+0.001)
  SE_temp = (sd(both_means) + 0.001 * sqrt(2))
  
  SMD_df_merged_BS = cbind(SMD_df_merged_BS, SMD_temp)
  SE_df_merged_BS = cbind(SE_df_merged_BS, SE_temp)

  #variance = mean_fraction_sedentary-mean_fraction_active

}

names_columns_df = colnames(Human_perBS_perBot[,c(-1, -127, -128, -129)])
class(names_columns_df)

colnames(SMD_df_merged_BS) = names_columns_df
colnames(SE_df_merged_BS) = names_columns_df
colnames(OR_df) = names_columns_df


write.table(SMD_df_merged_BS, file = paste(OUTPUT_ROOT, "SMD2OR_df_merged_BS_Human_prior.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



OR_df_from_SMD = data.frame(matrix_empty) 

for (col in colnames(SMD_df_merged_BS)){
  
  print(col)
  OR_vector = c()
  
  for (row in 1:length(SMD_df_merged_BS[,col])){
    
    print(row)
    
    value_temp = SMD_df_merged_BS[row, col]
    
    OR = meta::smd2or(smd = value_temp, se.smd = 0.001, backtransf = FALSE)
    
    OR_vector = c(OR_vector, OR$data$lnOR)
    
  }
  
  OR_df_from_SMD = cbind(OR_df_from_SMD, OR_vector)
}

names_columns_OR_df = colnames(Human_perBS_perBot[,c(-1, -127, -128, -129)])
class(names_columns_OR_df)
colnames(OR_df_from_SMD) = names_columns_OR_df



write.table(OR_df_from_SMD, file = paste(OUTPUT_ROOT, "OR_df_merged_BS_ChatGPT_prior_fromSMD.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )
