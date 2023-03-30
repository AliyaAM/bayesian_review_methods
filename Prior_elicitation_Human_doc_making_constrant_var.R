

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
print("plot prior from Human and human prior next to each other")
print("compare Human and human prior using prior–data conflict determination using data agreement criterion")
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


###### old prior (using mta-analysis to pool varius belief statements underlying the construct): 
#x = read.csv(paste(DATA_ROOT, "input_Human_prior.csv", sep="")) # new qualitative data
#x = read.csv(paste(DATA_ROOT, "input_Human_prior.csv", sep="")) # new qualitative data

#x: names for the variables that are used throughout the rest of the code: Construct, logOR_prior_elicitation, variance_prior_elicitation, participant_source.
#the cosntruct names that are used throughout the rest of the code: 




print("rerun below if data for the Human prior changes (ie., belief statements and counts")

#source(paste(SOURCE_ROOT, "Prior_elicitation_Human_mean_percent_quote.R", sep = ""))

########## DATA FOR THE PRIOR: OR FOR BEING PRESENT IN CHARACTERS THAT ARE ACTIVE: 

#OR_df_merged_BS = read.csv(paste(directory, "proj/bayesian_review_methods/RESULTS/OR_df_merged_BS_Human_prior.csv", sep="")) 
#OR_df_merged_BS = read.csv(paste(directory, "proj/bayesian_review_methods/RESULTS/OR_df_merged_BS_Human_prior_fromSMD.csv", sep="")) 

OR_df_merged_BS = read.csv(paste(directory, "proj/bayesian_review_methods/RESULTS/OR_df_merged_BS_Human_prior_fromSMD.csv", sep="")) 




#############
#Human social support:  this is based on the examination of the scale used in the likelihood (Galagher et al., 2011)
# SI+_social_support_practical_compan
# SI+_social_support_emotional

print("try with different constructs, if mulptiple BS/cosntruscts are present for the same construct in likelihood")

var = 0.1

ls(OR_df_merged_BS)
SocialSupport_mean = OR_df_merged_BS$SocialSupport2
SocialSupport_var = var
SocialSupport1_mean = OR_df_merged_BS$SocialSupport1
SocialSupport1_var = var
SocialSupport2_mean = OR_df_merged_BS$SocialSupport2
SocialSupport2_var = var


NegativeAttitude_mean = OR_df_merged_BS$NegativeAttitude
NegativeAttitude_var = var
NegativeAttitude1_mean = OR_df_merged_BS$NegativeAttitude1
NegativeAttitude1_var = var
NegativeAttitude2_mean = OR_df_merged_BS$NegativeAttitude2
NegativeAttitude2_var = var


PositiveAttitude_mean = OR_df_merged_BS$PositiveAttitude
PositiveAttitude_var = var
PositiveAttitude1_mean = OR_df_merged_BS$PositiveAttitude1
PositiveAttitude1_var = var
PositiveAttitude2_mean = OR_df_merged_BS$PositiveAttitude2
PositiveAttitude2_var = var

Symptoms_distress_mean = OR_df_merged_BS$SymptomsDistress
Symptoms_distress_var = var
Symp_Dis_Emotionbrrier_mean = OR_df_merged_BS$Symp_Dis_Emotionbrrier
Symp_Dis_Emotionbrrier_var = var


SelfEfficacy_mean = OR_df_merged_BS$SelfEfficacy
SelfEfficacy_var = var
SelfEfficacy1_mean = OR_df_merged_BS$SelfEfficacy1
SelfEfficacy1_var = var
SE_BaCapbrrier_mean = OR_df_merged_BS$SE_BaCapbrrier
SE_BaCapbrrier_var = var


fewerPerceivedSymptoms_mean = OR_df_merged_BS$PerceivedSymptoms *-1
fewerPerceivedSymptoms_var = var




Dysphoria_mean = OR_df_merged_BS$Dysphoria
Dysphoria_var = var
Dysphoria1_mean = OR_df_merged_BS$Dysphoria1
Dysphoria1_var = var
Emotionbrrier_mean = OR_df_merged_BS$Emotionbrrier
Emotionbrrier_var = var


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
                            SelfEfficacy1_mean,
                            SE_BaCapbrrier_mean, 
                            
                            fewerPerceivedSymptoms_mean, 
                          
                            
                            Dysphoria_mean, 
                            Dysphoria1_mean, 
                            Emotionbrrier_mean) 

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
                               SelfEfficacy1_var, 
                               SE_BaCapbrrier_var, 
                               
                               fewerPerceivedSymptoms_var, 
                               
                               Dysphoria_var,
                               Dysphoria1_var, 
                               Emotionbrrier_var)


Construct =c('SocialSupport', 
             'SocialSupport1',
             'SocialSupport2',
             
             'NegativeAttitude', 
             'NegativeAttitude1',
             'NegativeAttitude2', 
             
             'PositiveAttitude',
             'PositiveAttitude1',
             'PositiveAttitude2', 
             
             'Symptoms_distress',
             'Symp_Dis_Emotionbrrier', 
             
             'SelfEfficacy', 
             'SelfEfficacy1', 
             'SE_BaCapbrrier', 
             
             'fewerPerceivedSymptoms', 
             
             'Dysphoria',
             'Dysphoria1', 
             'Emotionbrrier')


##### participant_source: 

participant_source = rep("Human", times = 18)



input_Human_prior_from_mean_quotes_merge_BS = data.frame(Construct,
                                                           logOR_prior_elicitation, 
                                                           variance_prior_elicitation,
                                                           participant_source) 

write.csv(input_Human_prior_from_mean_quotes_merge_BS, paste(DATA_ROOT, "input_Human_prior_from_mean_quotes_merge_BS_cosntant_var_from_SMD.csv", sep="")) # new qualitative data

#added priod distributions based on the corresponding BS where we add up the counts for BSs before calculating the OR (active vs inactive), now we set the var to constant = 0.1 
#added priod distributions based on the corresponding BS where we add up the counts for BSs before calculating the OR (active vs inactive), now we set the var to constant = 0.1 
#added priod distributions based on the corresponding BS where we add up the counts for BSs before calculating the OR (active vs inactive), now we set the var to constant = 0.1 
