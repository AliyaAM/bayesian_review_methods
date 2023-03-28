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

## Set the root directory to look for source code.
#SOURCE_ROOT = "/Users/aliyaamirova/proj/bayesian_meta_analysis/"


print("IMPORTANT NEXT STEP: IMPORTANT NEXT STEP IMPORTANT NEXT STEP IMPORTANT NEXT STEP IMPORTANT NEXT STEP change the code below, so we can go over all constructs or atleast those that were measured in the likelihood (detailed in the comments below) if we change BaCapenblr_selfEfficacy to generic construct or index it somehow then we can run a function that goes through all constructs") 



print("check carefully for quotes on how much physical activty each participant did")
print("we should reduce to those constructs that are measured using perceived latent variables (self-efficacy, positive attitude, negative attitude, perceived social support (CHECK THIS ONE), symptom distress")
print("consider how to deal with constructs with more than one belief statements (aggregate/average...?)")
print("decide on the variance for the prior")
print("systematically include all constructs from quant and from qual")
print("plot prior from chatGPT and ChatGPT prior next to each other")
print("compare chatGPT and ChatGPT prior using prior–data conflict determination using data agreement criterion")
print("plot violin distributions for belief quotes etc...")
print("check overleaf for to do list")


########### DIRECTORY

#directory = "/Users/aliyaamirova/"
#directory = "/Users/aliya/my_docs/"
directory = "/Users/k2147340/OneDrive - King's College London/Documents/"


###########  source root 
#SOURCE_ROOT = paste(directory, "proj/bayesian_review_methods/", sep = "")

###########  data root
DATA_ROOT = paste(directory, "proj/bayesian_review_methods/DATA/", sep = "")

########### Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/", sep = "")


############ DATA

#Active_inactive_ChatGPT = read.csv(paste(DATA_ROOT, "Active_inactive_ChatGPT_March06_FINAL.csv", sep =""))
#Active_inactive_ChatGPT = read.csv(paste(DATA_ROOT, "Active_inactive_ChatGPT_March06_FINAL.csv", sep =""))

ChatGPT_perBS_perBot = read.csv(paste(DATA_ROOT, "ChatGPT_perBS_perBot_FINAL.csv", sep = "")) 

#############


#fraction of quotes concerning 'construct' when 'participant' was active 
#fraction of quotes concerning 'construct' when 'participant' was sedentary

#1 - fraction of quotes concerning 'construct' when 'participant' was active  
#1 - fraction of quotes concerning 'construct' when 'participant' was sedentary

#OR = from the contingency table of the above 


#############
############# concatenate into the same vector these below before calculating fractions/contingencies: 

#ChatGPT social support:  this is based on the examination of the scale used in the likelihood (Galagher et al., 2011)
# SI+_social_support_practical_compan
# SI+_social_support_emotional

ChatGPT_perBS_perBot$SIenblr_social_support_practical_compan
ChatGPT_perBS_perBot$SIenblr_social_support_emotional

  
#ChatGPT negative attitude: this is based on the examination of the scale used in the likelihood (Pozehl et al., 2018)
# BaCon-_neg_expctncy_sympcomrbd
###### this two below alone: 
ChatGPT_perBS_perBot$BaConbrrier_neg_expctncy_sympcomrbd
ChatGPT_perBS_perBot$BaConbrrier_neg_expctncy_symptoms_HF
###### OR:
ChatGPT_perBS_perBot$BaConbrrier


#ChatGPT Positive attitude: this is based on the examination of the scale used in the likelihood (Pozehl et al., 2018)
#ChatGPT: BaCon+_pos_expctncy_health, 
###### this two below only: 
ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_health
ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_cardio_health
###### OR: 
ChatGPT_perBS_perBot$BaConenblr
###### which also includes: 
###### ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_health
###### ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_cardio_health
###### ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_HFmanage
###### ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_RAmanage
###### ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_mobility
###### ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_mood




#ChatGPT symptom distress
#ChatGPT: Emotion-_fear
ChatGPT_perBS_perBot$Emotionbrrier_fear
###### OR 
ChatGPT_perBS_perBot$Emotionbrrier

##### 


#BaCap+_selfEfficacy, 
ChatGPT_perBS_perBot$BaCapenblr_selfEfficacy
###### OR 
ChatGPT_perBS_perBot$BaCapbrrier # remember to flip this one 
###### which also includes: 
# 1/BaCap-_selfEfficacy_prcvd_smptmsHF, 
# 1/BaCap-_selfEfficacy_HF, 
# 1/BaCap-_selfEfficacy_heart, 
# 1/BaCap-_selfEfficacy_prcvd_smptms, 
# 1/BaCap-_perceived_exertion, 
# 1/BaCap-_selfEfficacy_hlth_cndtns_typePA, 
# 1/BaCap-_selfEfficacy_ hlth_cndtns, 
# 1/BaCap-_selfEfficacy_older_age


#ChatGPT perceived symptoms
ChatGPT_perBS_perBot$BaCapbrrier_selfEfficacy_prcvd_smptmsHF
ChatGPT_perBS_perBot$BaCapbrrier_selfEfficacy_prcvd_smptms
#ChatGPT: BaCap-_selfEfficacy_prcvd_smptmsHF, BaCap-_selfEfficacy_prcvd_smptms

#ChatGPT Symptom dysphoria 
#ChatGPT: Emotion-_negative_emotions, Emotion-_mood
ChatGPT_perBS_perBot$Emotionbrrier_negative_emotions
ChatGPT_perBS_perBot$Emotionbrrier_mood


matrix_new_data = matrix(nrow = 32, ncol = 0)
new_data_merged_BS = data.frame(matrix_new_data)

new_data_merged_BS$Cases = ChatGPT_perBS_perBot$Cases
new_data_merged_BS$PA_status = ChatGPT_perBS_perBot$PA_status
new_data_merged_BS$SocialSupport = ChatGPT_perBS_perBot$SIenblr_social_support_practical_compan + ChatGPT_perBS_perBot$SIenblr_social_support_emotional
new_data_merged_BS$SocialSupport1 = ChatGPT_perBS_perBot$SIenblr_social_support_emotional
new_data_merged_BS$SocialSupport2 = ChatGPT_perBS_perBot$SIenblr_social_support_practical_compan
new_data_merged_BS$NegativeAttitude = ChatGPT_perBS_perBot$BaConbrrier_neg_expctncy_sympcomrbd + ChatGPT_perBS_perBot$BaConbrrier_neg_expctncy_symptoms_HF
new_data_merged_BS$NegativeAttitude1 = ChatGPT_perBS_perBot$BaConbrrier_neg_expctncy_sympcomrbd
new_data_merged_BS$NegativeAttitude2 = ChatGPT_perBS_perBot$BaConbrrier_neg_expctncy_symptoms_HF
new_data_merged_BS$NegAt_BaConbrrier = ChatGPT_perBS_perBot$BaConbrrier
new_data_merged_BS$PositiveAttitude = ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_health + ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_cardio_health
new_data_merged_BS$PositiveAttitude1 = ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_health 
new_data_merged_BS$PositiveAttitude2 = ChatGPT_perBS_perBot$BaConenblr_pos_expctncy_cardio_health
new_data_merged_BS$PosAt_BaConenblr = ChatGPT_perBS_perBot$BaConenblr 
new_data_merged_BS$SymptomsDistress = ChatGPT_perBS_perBot$Emotionbrrier_fear
new_data_merged_BS$Symp_Dis_Emotionbrrier = ChatGPT_perBS_perBot$Emotionbrrier
new_data_merged_BS$SelfEfficacy = ChatGPT_perBS_perBot$BaCapenblr_selfEfficacy
new_data_merged_BS$SE_BaCapbrrier = ChatGPT_perBS_perBot$BaCapbrrier
new_data_merged_BS$PerceivedSymptoms = ChatGPT_perBS_perBot$BaCapbrrier_selfEfficacy_prcvd_smptmsHF + ChatGPT_perBS_perBot$BaCapbrrier_selfEfficacy_prcvd_smptms 
new_data_merged_BS$PerceivedSymptoms1 = ChatGPT_perBS_perBot$BaCapbrrier_selfEfficacy_prcvd_smptmsHF
new_data_merged_BS$PerceivedSymptoms2 = ChatGPT_perBS_perBot$BaCapbrrier_selfEfficacy_prcvd_smptms
new_data_merged_BS$Dysphoria = ChatGPT_perBS_perBot$Emotionbrrier_negative_emotions + ChatGPT_perBS_perBot$Emotionbrrier_mood
new_data_merged_BS$Dysphoria1 = ChatGPT_perBS_perBot$Emotionbrrier_negative_emotions
new_data_merged_BS$Dysphoria2 = ChatGPT_perBS_perBot$Emotionbrrier_mood



unique(new_data_merged_BS$Cases)
new_data_merged_BS[rowSums(is.na(new_data_merged_BS)) > 0,]
ls(new_data_merged_BS)
ncol(new_data_merged_BS)

new_data_merged_BS = tibble(new_data_merged_BS) %>% 
  replace(is.na(.), 0) %>%
  mutate(sum_quotes_per_participant = rowSums(across(where(is.numeric)))) %>%
  mutate(name = case_when( str_detect(Cases, 'William') ~ 'William',
                           str_detect(Cases, 'Patricia') ~ 'Patricia', 
                           str_detect(Cases, 'Mark') ~ 'Mark',
                           str_detect(Cases, 'mark') ~ 'Mark',
                           str_detect(Cases, 'David') ~ 'David', 
                           str_detect(Cases, 'Linda') ~ 'Linda',
                           str_detect(Cases, 'Muhammad') ~ 'Muhammad', 
                           str_detect(Cases, 'Mary') ~ 'Mary',
                           str_detect(Cases, 'Thomas') ~ 'Thomas', 
                           str_detect(Cases, 'Robert') ~ 'Robert',
                           str_detect(Cases, 'Nancy') ~ 'Nancy', 
                           str_detect(Cases, 'John') ~ 'John',
                           str_detect(Cases, 'john') ~ 'John',
                           str_detect(Cases, 'Fares') ~ 'Fares', 
                           str_detect(Cases, 'James') ~ 'James',
                           str_detect(Cases, 'james') ~ 'James',
                           str_detect(Cases, 'Michael') ~ 'Michael', 
                           str_detect(Cases, 'Johan') ~ 'Johan',
                           str_detect(Cases, 'Richard') ~ 'Richard'))


matrix_empty = matrix(nrow = 16, ncol = 0)

OR_df_merged_BS = data.frame(matrix_empty) 


for (i in colnames(new_data_merged_BS[,c(-1, -25)])){
  
  OR_vector = c()
  print(i)
  
  x = unlist(as.vector(new_data_merged_BS[,i]))
  class(x)
  
  
  new_data_merged_BS$sum_quotes_per_participant
  
  new_data_merged_BS = new_data_merged_BS  %>% 
    mutate(x_fraction = x/sum_quotes_per_participant + 0.001)
  
  
  long_data = new_data_merged_BS %>%
    select(name, PA_status, x_fraction) %>%
    gather("key", 'x_fraction', -name, -PA_status) %>%
    mutate(fraction_not_present = 1 - x_fraction + 0.001) %>%
    arrange(name, PA_status) %>%
    group_by(name) %>%
    group_split(name)
  
  
  for (j in 1:length(long_data)){
    
    print(j)
    
    print(long_data[[j]][2,4])
    print(long_data[[j]][1,5])
    print(long_data[[j]][2,5])
    print(long_data[[j]][1,4])
    OR_temp = (long_data[[j]][2,4]*long_data[[j]][1,5])/(long_data[[j]][2,5]*long_data[[j]][1,4])
    OR_vector = c(OR_vector, OR_temp$x_fraction[1])
    
  }
  
  OR_df_merged_BS = cbind(OR_df_merged_BS, OR_vector)
  
}
names_columns_OR_df = colnames(new_data_merged_BS[,c(-1, -25, -26)])
class(names_columns_OR_df)
colnames(OR_df_merged_BS) = names_columns_OR_df


write.table(OR_df_merged_BS, file = paste(OUTPUT_ROOT, "OR_df_merged_BS_ChatGPT_prior.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
          eol = "\r", na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"),
          fileEncoding = "" )


####################################
####################################
####################################
####################################

# 
# unique(ChatGPT_perBS_perBot$Cases)
# ChatGPT_perBS_perBot[rowSums(is.na(ChatGPT_perBS_perBot)) > 0,]
# ls(ChatGPT_perBS_perBot)
# ncol(ChatGPT_perBS_perBot)
# 
# ChatGPT_perBS_perBot = tibble(ChatGPT_perBS_perBot) %>% 
#   replace(is.na(.), 0) %>%
#   mutate(sum_quotes_per_participant = rowSums(across(where(is.numeric)))) %>%
#   mutate(name = case_when( str_detect(Cases, 'William') ~ 'William',
#                          str_detect(Cases, 'Patricia') ~ 'Patricia', 
#                          str_detect(Cases, 'Mark') ~ 'Mark',
#                          str_detect(Cases, 'mark') ~ 'Mark',
#                          str_detect(Cases, 'David') ~ 'David', 
#                          str_detect(Cases, 'Linda') ~ 'Linda',
#                          str_detect(Cases, 'Muhammad') ~ 'Muhammad', 
#                          str_detect(Cases, 'Mary') ~ 'Mary',
#                          str_detect(Cases, 'Thomas') ~ 'Thomas', 
#                          str_detect(Cases, 'Robert') ~ 'Robert',
#                          str_detect(Cases, 'Nancy') ~ 'Nancy', 
#                          str_detect(Cases, 'John') ~ 'John',
#                          str_detect(Cases, 'john') ~ 'John',
#                          str_detect(Cases, 'Fares') ~ 'Fares', 
#                          str_detect(Cases, 'James') ~ 'James',
#                          str_detect(Cases, 'james') ~ 'James',
#                          str_detect(Cases, 'Michael') ~ 'Michael', 
#                          str_detect(Cases, 'Johan') ~ 'Johan',
#                          str_detect(Cases, 'Richard') ~ 'Richard'))
# 
# 
# matrix_empty = matrix(nrow = 16, ncol = 0)
# OR_df = data.frame(matrix_empty) 
# 
# for (i in colnames(ChatGPT_perBS_perBot[,c(-1, -157, -158)])){
# 
#     OR_vector = c()
#   print(i)
#   
#   x = unlist(as.vector(ChatGPT_perBS_perBot[,i]))
# 
#   
#   
#   ChatGPT_perBS_perBot$sum_quotes_per_participant
# 
#   ChatGPT_perBS_perBot = ChatGPT_perBS_perBot  %>% 
#     mutate(x_fraction = x/sum_quotes_per_participant + 0.001)
# 
# 
#   long_data = ChatGPT_perBS_perBot %>%
#     select(name, PA_status, x_fraction) %>%
#     gather("key", 'x_fraction', -name, -PA_status) %>%
#     mutate(fraction_not_present = 1 - x_fraction + 0.001) %>%
#     arrange(name, PA_status) %>%
#     group_by(name) %>%
#     group_split(name)
# 
# 
#   for (j in 1:length(long_data)){
# 
#     print(j)
# 
#     print(long_data[[j]][2,4])
#     print(long_data[[j]][1,5])
#     print(long_data[[j]][2,5])
#     print(long_data[[j]][1,4])
#     OR_temp = (long_data[[j]][2,4]*long_data[[j]][1,5])/(long_data[[j]][2,5]*long_data[[j]][1,4])
#     OR_vector = c(OR_vector, OR_temp$x_fraction[1])
#     
#   }
# 
#   OR_df = cbind(OR_df, OR_vector)
#   
# }
# 
# colnames(OR_df) = names_columns_OR_df
# names_columns_OR_df = colnames(ChatGPT_perBS_perBot[,c(-1, -157, -158, -159)])
# class(names_columns_OR_df)

 # write.table(OR_df, file = paste(OUTPUT_ROOT, "OR_df_ChatGPT_prior.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
 #           eol = "\r", na = "NA", dec = ".", row.names = FALSE,
 #           col.names = TRUE, qmethod = c("escape", "double"),
 #           fileEncoding = "" )


# ChatGPT_perBS_perBot$Cases

#we could produce the OR or each participant, or group by PA_status and produce OR all active vs all sedentary (average over)


# 1/BaCap-_selfEfficacy_prcvd_smptmsHF, 
# 1/BaCap-_selfEfficacy_HF, 
# 1/BaCap-_selfEfficacy_heart, 
# 1/BaCap-_selfEfficacy_prcvd_smptms, 
# 1/BaCap-_perceived_exertion, 
# 1/BaCap-_selfEfficacy_hlth_cndtns_typePA, 
# 1/BaCap-_selfEfficacy_ hlth_cndtns, 
# 1/BaCap-_selfEfficacy_older_age


#ChatGPT social support:  this is based on the examination of the scale used in the likelihood (Galagher et al., 2011)

# SI+_social_support_practical_compan
# SI+_social_support_emotional

#ChatGPT negative attitude: this is based on the examination of the scale used in the likelihood (Pozehl et al., 2018)
# BaCon-_neg_expctncy_sympcomrbd

#ChatGPT Positive attitude: this is based on the examination of the scale used in the likelihood (Pozehl et al., 2018)
#ChatGPT: BaCon+_pos_expctncy_health, 

#ChatGPT symptom distress
#ChatGPT: Emotion-_fear

#ChatGPT perceived symptoms
#ChatGPT: BaCap-_selfEfficacy_prcvd_smptmsHF, BaCap-_selfEfficacy_prcvd_smptms


#ChatGPT Symptom dysphoria 
#ChatGPT: Emotion-_negative_emotions, Emotion-_mood
