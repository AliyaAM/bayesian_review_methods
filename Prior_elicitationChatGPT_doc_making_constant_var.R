

#library(tidyverse)
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

print("decide on how to obtain variance for ORs for each construct in the human prior because it is only a single number")

print("include mean quote as the statitic for the prior contingency table")
print("check carefully for quotes on how much physical activty each participant did")

print("we should reduce to those constructs that are measured using perceived latent variables (self-efficacy, positive attitude, negative attitude, perceived social support (CHECK THIS ONE), symptom distress")
print("consider how to deal with constructs with more than one belief statements (aggregate/average...?)")
print("systematically include all constructs from quant and from qual")
print("plot prior from chatGPT and human prior next to each other")
print("compare chatGPT and human prior using prior–data conflict determination using data agreement criterion")
print("plot violin distributions for belief quotes etc...")
print("check overleaf for to do list")


########### DIRECTORY

#directory = "/Users/aliyaamirova/"
directory = "/Users/aliya/my_docs/"
#directory = "/Users/k2147340/OneDrive - King's College London/Documents/"


###########  source root 
SOURCE_ROOT = paste(directory, "proj/bayesian_review_methods/", sep = "")

###########  data root
DATA_ROOT = paste(directory, "proj/bayesian_review_methods/DATA/", sep = "")

########### Set the root location on the user's local machine to save output files.
#OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/Human/", sep = "")
OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/ChatGPT/results_matched_characters/", sep = "")

###### old prior (using mta-analysis to pool varius belief statements underlying the construct): 
#x = read.csv(paste(DATA_ROOT, "input_Human_prior.csv", sep="")) # new qualitative data
#x = read.csv(paste(DATA_ROOT, "input_ChatGPT_prior.csv", sep="")) # new qualitative data

#x: names for the variables that are used throughout the rest of the code: Construct, logOR_prior_elicitation, variance_prior_elicitation, participant_source.
#the cosntruct names that are used throughout the rest of the code: 




print("rerun below if data for the CHatGPT prior changes (ie., belief statements and counts")

#source(paste(SOURCE_ROOT, "Prior_elicitation_ChatGPT_mean_percent_quote.R", sep = ""))

########## DATA FOR THE PRIOR: OR FOR BEING PRESENT IN CHARACTERS THAT ARE ACTIVE: 

#OR_df_merged_BS = read.csv(paste(directory, "proj/bayesian_review_methods/RESULTS/OR_df_merged_BS_ChatGPT_prior.csv", sep="")) 
OR_df_merged_BS = read.csv(paste(directory, "proj/bayesian_review_methods/RESULTS/OR_df_merged_BS_ChatGPT_prior_fromSMD.csv", sep="")) 





#############
#ChatGPT social support:  this is based on the examination of the scale used in the likelihood (Galagher et al., 2011)
# SI+_social_support_practical_compan
# SI+_social_support_emotional

print("try with different constructs, if mulptiple BS/cosntruscts are present for the same construct in likelihood")

var = 0.1

ls(OR_df_merged_BS)
SocialSupport_mean = mean(OR_df_merged_BS$SocialSupport) 
SocialSupport_var = var

SocialSupport1_mean = mean(OR_df_merged_BS$SocialSupport1) 
SocialSupport1_var = var

SocialSupport2_mean = mean(OR_df_merged_BS$SocialSupport2) 
SocialSupport2_var = var


#ChatGPT negative attitude: this is based on the examination of the scale used in the likelihood (Pozehl et al., 2018)
# BaCon-_neg_expctncy_sympcomrbd

NegativeAttitude_mean = mean(OR_df_merged_BS$NegativeAttitude)
NegativeAttitude_var = var


NegativeAttitude1_mean = mean(OR_df_merged_BS$NegativeAttitude1)
NegativeAttitude1_var = var

NegativeAttitude2_mean = mean(OR_df_merged_BS$NegativeAttitude2)
NegativeAttitude2_var = var


#ChatGPT Positive attitude: this is based on the examination of the scale used in the likelihood (Pozehl et al., 2018)
#ChatGPT: BaCon+_pos_expctncy_health, 

PositiveAttitude_mean = mean(OR_df_merged_BS$PositiveAttitude)
PositiveAttitude_var = var

PositiveAttitude1_mean = mean(OR_df_merged_BS$PositiveAttitude1)
PositiveAttitude1_var = var

PositiveAttitude2_mean = mean(OR_df_merged_BS$PositiveAttitude2)
PositiveAttitude2_var = var

#ChatGPT symptom distress
#ChatGPT: Emotion-_fear

Symptoms_distress_mean = mean(OR_df_merged_BS$SymptomsDistress)
Symptoms_distress_var = var


Symp_Dis_Emotionbrrier_mean = mean(OR_df_merged_BS$Symp_Dis_Emotionbrrier)
Symp_Dis_Emotionbrrier_var = var

#ChatGPT self-efficacy: 
#BaCap+_selfEfficacy, 
# 1/BaCap-_selfEfficacy_prcvd_smptmsHF, 
# 1/BaCap-_selfEfficacy_HF, 
# 1/BaCap-_selfEfficacy_heart, 
# 1/BaCap-_selfEfficacy_prcvd_smptms, 
# 1/BaCap-_perceived_exertion, 
# 1/BaCap-_selfEfficacy_hlth_cndtns_typePA, 
# 1/BaCap-_selfEfficacy_ hlth_cndtns, 
# 1/BaCap-_selfEfficacy_older_age
# OR_df_merged_BS$BaCapenblr_selfEfficacy
# OR_df_merged_BS$BaCapbrrier_selfEfficacy_.hlth_cndtns
# OR_df_merged_BS$BaCapbrrier_selfEfficacy_heart
# OR_df_merged_BS$BaCapbrrier_selfEfficacy_HF
# OR_df_merged_BS$BaCapbrrier_selfEfficacy_hlth_cndtns_typePA
# OR_df_merged_BS$BaCapbrrier_selfEfficacy_prcvd_smptms
# OR_df_merged_BS$BaCapbrrier_selfEfficacy_prcvd_smptmsHF
# OR_df_merged_BS$BaCapbrrier_selfEfficacy_older_age
# OR_df_merged_BS$BaCapbrrier_perceived_exertion


SelfEfficacy_mean = mean(OR_df_merged_BS$SelfEfficacy)
SelfEfficacy_var = var

SE_BaCapbrrier_mean = mean(OR_df_merged_BS$SE_BaCapbrrier)
SE_BaCapbrrier_var = var

#ChatGPT perceived symptoms
#ChatGPT: BaCap-_selfEfficacy_prcvd_smptmsHF, BaCap-_selfEfficacy_prcvd_smptms

fewerPerceivedSymptoms_mean = 1/mean(OR_df_merged_BS$PerceivedSymptoms) 
fewerPerceivedSymptoms_var = var

fewerPerceivedSymptoms1_mean = 1/mean(OR_df_merged_BS$PerceivedSymptoms1)
fewerPerceivedSymptoms1_var = var

fewerPerceivedSymptoms2_mean = 1/mean(OR_df_merged_BS$PerceivedSymptoms2)
fewerPerceivedSymptoms2_var = var

#ChatGPT Symptom dysphoria 
#ChatGPT: Emotion-_negative_emotions, Emotion-_mood



Dysphoria_mean = mean(OR_df_merged_BS$Dysphoria)
Dysphoria_var = var

Dysphoria1_mean = mean(OR_df_merged_BS$Dysphoria1)
Dysphoria1_var = var

Dysphoria2_mean = mean(OR_df_merged_BS$Dysphoria2)
Dysphoria2_var = var


logOR_prior_elicitation = c(SocialSupport_mean, 
                            SocialSupport1_mean,
                            SocialSupport2_mean,
                            
                            NegativeAttitude_mean, 
                            NegativeAttitude1_mean,
                            NegativeAttitude2_mean, 
                            
                            PositiveAttitude_mean,
                            PositiveAttitude1_mean,
                            PositiveAttitude2_mean, 
                            
                            Symptoms_distress_mean,
                            Symp_Dis_Emotionbrrier_mean, 
                            
                            SelfEfficacy_mean, 
                            SE_BaCapbrrier_mean, 
                            
                            fewerPerceivedSymptoms_mean, 
                            fewerPerceivedSymptoms1_mean, 
                            fewerPerceivedSymptoms2_mean,
                            
                            Dysphoria_mean, 
                            Dysphoria1_mean, 
                            Dysphoria2_mean) 

variance_prior_elicitation = c(SocialSupport_var, 
                               SocialSupport1_var,
                               SocialSupport2_var,
                               
                               NegativeAttitude_var, 
                               NegativeAttitude1_var,
                               NegativeAttitude2_var, 
                               
                               PositiveAttitude_var,
                               PositiveAttitude1_var,
                               PositiveAttitude2_var, 
                               
                               Symptoms_distress_var,
                               Symp_Dis_Emotionbrrier_var, 
                               
                               SelfEfficacy_var, 
                               SE_BaCapbrrier_var, 
                               
                               fewerPerceivedSymptoms_var, 
                               fewerPerceivedSymptoms1_var, 
                               fewerPerceivedSymptoms2_var,
                               
                               Dysphoria_var,
                               Dysphoria1_var, 
                               Dysphoria2_var)


Construct =c("SocialSupport",   
             "SocialSupport1",  
             "SocialSupport2",  
             
             "NegativeAttitude",  
             "NegativeAttitude1",  
             "NegativeAttitude2",  
             
             "PositiveAttitude", 
             "PositiveAttitude1",
             "PositiveAttitude2",
              
             "Symptoms_distress",
             'Symp_Dis_Emotionbrrier', 
             
             "SelfEfficacy",
             'SE_BaCapbrrier_var', 
             
             "fewerPerceivedSymptoms",
             "fewerPerceivedSymptoms1",
             "fewerPerceivedSymptoms2",
             
             "Dysphoria", 
             "Dysphoria1", 
             "Dysphoria2") 


##### participant_source: 

participant_source = rep("ChatGPT", times = 19)



input_ChatGPT_prior_from_mean_quotes_merge_BS = data.frame(Construct,
                                                           logOR_prior_elicitation, 
                                                           variance_prior_elicitation,
                                                           participant_source) 

write.csv(input_ChatGPT_prior_from_mean_quotes_merge_BS, paste(DATA_ROOT, "input_ChatGPT_prior_from_mean_quotes_merge_BS_cosntant_var_from_SMD.csv", sep="")) # new qualitative data

#added priod distributions based on the corresponding BS where we add up the counts for BSs before calculating the OR (active vs inactive), now we set the var to constant = 0.1 
#added priod distributions based on the corresponding BS where we add up the counts for BSs before calculating the OR (active vs inactive), now we set the var to constant = 0.1 
#added priod distributions based on the corresponding BS where we add up the counts for BSs before calculating the OR (active vs inactive), now we set the var to constant = 0.1 
