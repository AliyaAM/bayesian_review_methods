

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


Construct =c("SocialSupport",          
             "NegativeAttitude",      
             "PositiveAttitude",      
             "Symptoms_distress",
             "SelfEfficacy",
             "fewerPerceivedSymptoms",
             "Dysphoria") 


##### participant_source: 

participant_source = rep("ChatGPT", times = 7)


print("rerun below if data for the CHatGPT prior changes (ie., belief statements and counts")

#source(paste(SOURCE_ROOT, "Prior_elicitation_ChatGPT_mean_percent_quote.R", sep = ""))

########## DATA FOR THE PRIOR: OR FOR BEING PRESENT IN CHARACTERS THAT ARE ACTIVE: 

OR_df_ChatGPT_prior = read.csv(paste(directory, "proj/bayesian_review_methods/RESULTS/OR_df_ChatGPT_prior.csv", sep="")) 


#############
#ChatGPT social support:  this is based on the examination of the scale used in the likelihood (Galagher et al., 2011)
# SI+_social_support_practical_compan
# SI+_social_support_emotional

print("try with different constructs, if mulptiple BS/cosntruscts are present for the same construct in likelihood")



SocialSupport1_mean = mean(OR_df_ChatGPT_prior$SIenblr_social_support_emotional) 
SocialSupport1_var = var(OR_df_ChatGPT_prior$SIenblr_social_support_emotional) 

SocialSupport2_mean = mean(OR_df_ChatGPT_prior$SIenblr_social_support_practical_compan) 
SocialSupport2_var = var(OR_df_ChatGPT_prior$SIenblr_social_support_practical_compan) 


#ChatGPT negative attitude: this is based on the examination of the scale used in the likelihood (Pozehl et al., 2018)
# BaCon-_neg_expctncy_sympcomrbd

NegativeAttitude_mean = mean(OR_df_ChatGPT_prior$BaConbrrier_neg_expctncy_sympcomrbd)
NegativeAttitude_var = var(OR_df_ChatGPT_prior$BaConbrrier_neg_expctncy_sympcomrbd)

#ChatGPT Positive attitude: this is based on the examination of the scale used in the likelihood (Pozehl et al., 2018)
#ChatGPT: BaCon+_pos_expctncy_health, 

PositiveAttitude_mean = mean(OR_df_ChatGPT_prior$BaConenblr_pos_expctncy_health)
PositiveAttitude_var = var(OR_df_ChatGPT_prior$BaConenblr_pos_expctncy_health)

#ChatGPT symptom distress
#ChatGPT: Emotion-_fear
Symptoms_distress_mean = mean(OR_df_ChatGPT_prior$Emotionbrrier_fear)
Symptoms_distress_var = var(OR_df_ChatGPT_prior$Emotionbrrier_fear)


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
OR_df_ChatGPT_prior$BaCapenblr_selfEfficacy
OR_df_ChatGPT_prior$BaCapbrrier_selfEfficacy_.hlth_cndtns
OR_df_ChatGPT_prior$BaCapbrrier_selfEfficacy_heart
OR_df_ChatGPT_prior$BaCapbrrier_selfEfficacy_HF
OR_df_ChatGPT_prior$BaCapbrrier_selfEfficacy_hlth_cndtns_typePA
OR_df_ChatGPT_prior$BaCapbrrier_selfEfficacy_prcvd_smptms
OR_df_ChatGPT_prior$BaCapbrrier_selfEfficacy_prcvd_smptmsHF
OR_df_ChatGPT_prior$BaCapbrrier_selfEfficacy_older_age
OR_df_ChatGPT_prior$BaCapbrrier_perceived_exertion


SelfEfficacy_mean = mean(OR_df_ChatGPT_prior$BaCapenblr_selfEfficacy)
SelfEfficacy_var = var(OR_df_ChatGPT_prior$BaCapenblr_selfEfficacy)
  
#ChatGPT perceived symptoms
#ChatGPT: BaCap-_selfEfficacy_prcvd_smptmsHF, BaCap-_selfEfficacy_prcvd_smptms
OR_df_ChatGPT_prior$BaCapbrrier_selfEfficacy_prcvd_smptmsHF

fewerPerceivedSymptoms_mean = mean(1/OR_df_ChatGPT_prior$BaCapbrrier_selfEfficacy_prcvd_smptmsHF)
fewerPerceivedSymptoms_var = var(1/OR_df_ChatGPT_prior$BaCapbrrier_selfEfficacy_prcvd_smptmsHF)

#ChatGPT Symptom dysphoria 
#ChatGPT: Emotion-_negative_emotions, Emotion-_mood
OR_df_ChatGPT_prior$Emotionbrrier_negative_emotions
OR_df_ChatGPT_prior$Emotionbrrier_mood

Dysphoria_mean = mean(OR_df_ChatGPT_prior$Emotionbrrier_negative_emotions)
Dysphoria_var = var(OR_df_ChatGPT_prior$Emotionbrrier_negative_emotions)


logOR_prior_elicitation = c(SocialSupport1_mean,
                            NegativeAttitude_mean, 
                            PositiveAttitude_mean, 
                            Symptoms_distress_mean,
                            SelfEfficacy_mean, 
                            fewerPerceivedSymptoms_mean, 
                            Dysphoria_mean) 

variance_prior_elicitation = c(SocialSupport1_var,
                               NegativeAttitude_var, 
                               PositiveAttitude_var, 
                               Symptoms_distress_var,
                               SelfEfficacy_var, 
                               fewerPerceivedSymptoms_var, 
                               Dysphoria_mean)


input_ChatGPT_prior_from_mean_quotes = data.frame(Construct,
                                                  logOR_prior_elicitation, 
                                                  variance_prior_elicitation,
                                                  participant_source) 

write.csv(input_ChatGPT_prior_from_mean_quotes, paste(DATA_ROOT, "input_ChatGPT_prior_from_mean_quotes.csv", sep="")) # new qualitative data

