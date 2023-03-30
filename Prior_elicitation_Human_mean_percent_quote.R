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

Human_perBS_per_participant = read.csv(paste(DATA_ROOT, "Human_perBS_per_participant_FINAL.csv", sep = "")) 


#############

#fraction of quotes concerning 'construct' when 'participant' was active 
#fraction of quotes concerning 'construct' when 'participant' was sedentary

#1 - fraction of quotes concerning 'construct' when 'participant' was active  
#1 - fraction of quotes concerning 'construct' when 'participant' was sedentary

#OR = from the contingency table of the above 

#############

matrix_new_data = matrix(nrow = 16, ncol = 0)
new_data_merged_BS_human = data.frame(matrix_new_data)

ls(Human_perBS_per_participant)

new_data_merged_BS_human$Cases = Human_perBS_per_participant$Cases
new_data_merged_BS_human$PA_status = Human_perBS_per_participant$PA_status
new_data_merged_BS_human$SocialSupport = Human_perBS_per_participant$SIenblr_social_support_emotional + Human_perBS_per_participant$SIenblr_social_support_practical_plan
new_data_merged_BS_human$SocialSupport1 = Human_perBS_per_participant$SIenblr_social_support_emotional
new_data_merged_BS_human$SocialSupport2 = Human_perBS_per_participant$SIenblr_social_support_practical_plan


new_data_merged_BS_human$NegativeAttitude = Human_perBS_per_participant$BaConbrrier_lack_pos_pos_expctncy + Human_perBS_per_participant$BaConbrrier_neg_expctncy_symptoms_HF_fatigue + Human_perBS_per_participant$BaConbrrier_neg_expctncy_symptoms_HF_tightchest + Human_perBS_per_participant$BaConbrrier_neg_expctncy_symptoms_HF_dyspnoea + Human_perBS_per_participant$BaConbrrier_neg_expctncy_symptoms_HF_oedema + Human_perBS_per_participant$BaConbrrier_risk_heart
new_data_merged_BS_human$NegativeAttitude1 = Human_perBS_per_participant$BaConbrrier_lack_pos_pos_expctncy
new_data_merged_BS_human$NegativeAttitude2 = Human_perBS_per_participant$BaConbrrier_neg_expctncy_symptoms_HF_fatigue + Human_perBS_per_participant$BaConbrrier_neg_expctncy_symptoms_HF_tightchest + Human_perBS_per_participant$BaConbrrier_neg_expctncy_symptoms_HF_dyspnoea + Human_perBS_per_participant$BaConbrrier_neg_expctncy_symptoms_HF_oedema
new_data_merged_BS_human$NegAt_BaConbrrier = Human_perBS_per_participant$BaConbrrier

new_data_merged_BS_human$PositiveAttitude = Human_perBS_per_participant$BaConenblr_pos_expctncy_health + Human_perBS_per_participant$BaConenblr_pos_expctncy_cardio_health
new_data_merged_BS_human$PositiveAttitude1 = Human_perBS_per_participant$BaConenblr_pos_expctncy_health 
new_data_merged_BS_human$PositiveAttitude2 = Human_perBS_per_participant$BaConenblr_pos_expctncy_cardio_health
new_data_merged_BS_human$PosAt_BaConenblr = Human_perBS_per_participant$BaConenblr 

new_data_merged_BS_human$SymptomsDistress = Human_perBS_per_participant$Emotionbrrier_fear
new_data_merged_BS_human$SymptomsDistress1 = Human_perBS_per_participant$MADPbrrier_symptom_hypervigilance
new_data_merged_BS_human$Symp_Dis_Emotionbrrier = Human_perBS_per_participant$Emotionbrrier

new_data_merged_BS_human$SelfEfficacy = Human_perBS_per_participant$BaCapenblr_selfEfficacy
new_data_merged_BS_human$SE_BaCapbrrier = Human_perBS_per_participant$BaCapbrrier

new_data_merged_BS_human$SelfEfficacy1 = Human_perBS_per_participant$BaCapenblr_selfEfficacy - Human_perBS_per_participant$BaCapbrrier

new_data_merged_BS_human$PerceivedSymptoms = Human_perBS_per_participant$BaCapbrrier_selfEfficacy_prcvd_smptmsHF 

new_data_merged_BS_human$Dysphoria =  Human_perBS_per_participant$Emotionbrrier_mood
new_data_merged_BS_human$Emotionbrrier = Human_perBS_per_participant$Emotionbrrier
new_data_merged_BS_human$Dysphoria1 = Human_perBS_per_participant$Emotionbrrier_mood + Human_perBS_per_participant$Emotionbrrier

#Human self-efficacy: #BaCapenblr_selfEfficacy


sedentary = subset(new_data_merged_BS_human, new_data_merged_BS_human$PA_status == 0)
nrow(sedentary)
  
unique(new_data_merged_BS_human$Cases)
new_data_merged_BS_human[rowSums(is.na(new_data_merged_BS_human)) > 0,]
ls(new_data_merged_BS_human)
ncol(new_data_merged_BS_human)

new_data_merged_BS_human = tibble(new_data_merged_BS_human) %>% 
  replace(is.na(.), 0) %>%
  mutate(sum_quotes_per_participant = rowSums(across(where(is.numeric)))) %>%
  mutate(name = Cases)


matrix_empty = matrix(nrow = 1, ncol = 0)

OR_df = data.frame(matrix_empty) 
SMD_df_merged_BS = data.frame(matrix_empty) 
SE_df_merged_BS = data.frame(matrix_empty) 

for (i in colnames(new_data_merged_BS_human[,c(-1, -25)])){
  

  print(i)
  
  x = unlist(as.vector(new_data_merged_BS_human[,i]))
  
  
  
  new_data_merged_BS_human$sum_quotes_per_participant
  
  new_data_merged_BS_human = new_data_merged_BS_human  %>% 
    mutate(x_fraction = x/sum_quotes_per_participant + 0.001)
  
  print('got here')
  
  
  long_data = new_data_merged_BS_human %>%
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
  
  OR_temp = log((mean_fraction_active*mean_fraction_not_present_sedentary)/(mean_fraction_not_present_active*mean_fraction_sedentary))
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

names_columns_df = colnames(new_data_merged_BS_human[,c(-1, -25, -26)])
class(names_columns_df)

colnames(SMD_df_merged_BS) = names_columns_df
colnames(SE_df_merged_BS) = names_columns_df
colnames(OR_df) = names_columns_df


write.table(SMD_df_merged_BS, file = paste(OUTPUT_ROOT, "SMD2OR_df_merged_BS_Human_prior.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )

write.table(OR_df, file = paste(OUTPUT_ROOT, "OR_df_merged_BS_Human_prior_from_meanquote.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
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

names_columns_OR_df = colnames(new_data_merged_BS_human[,c(-1, -25, -26)])
class(names_columns_OR_df)
colnames(OR_df_from_SMD) = names_columns_OR_df



write.table(OR_df_from_SMD, file = paste(OUTPUT_ROOT, "OR_df_merged_BS_Human_prior_fromSMD.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )
