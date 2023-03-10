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
library(bayesplot)
library(ggplot2)
library(ggridges)
library(rstan) 
library(coda)
library(bayestestR)
library(HDInterval)
library(assertthat)
library(RColorBrewer)

## Set the root directory to look for source code.
#SOURCE_ROOT = "/Users/aliyaamirova/proj/bayesian_meta_analysis/"

print("include mean quote as the statitic for the prior contingency table")
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
directory = "/Users/aliya/my_docs/"

###########  source root 
SOURCE_ROOT = paste(directory, "proj/bayesian_review_methods/", sep = "")

###########  data root
DATA_ROOT = paste(directory, "proj/bayesian_review_methods/DATA/", sep = "")

########### Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/", sep = "")

#x = read.csv(paste(DATA_ROOT, "Test_data_bayes_chatGPT.csv", sep="")) # new qualitative data
#x = read.csv(paste(DATA_ROOT, "input_chatGPT.csv", sep="")) # new qualitative data


Active_inactive_ChatGPT = read.csv(paste(DATA_ROOT, "Active_inactive_ChatGPT_March06_FINAL.csv", sep =""))
Active_inactive_ChatGPT = read.csv(paste(DATA_ROOT, "Active_inactive_ChatGPT_March06_FINAL.csv", sep =""))


Active_inactive_ChatGPT$active_n #construct present in active PA_X
Active_inactive_ChatGPT$sedentary_n #construct present in sedentary noPA_X
Active_inactive_ChatGPT$total_n #construct present X

#total active = 7
#total sedentary = 9

Active_inactive_ChatGPT$active_noX_n = 16 - Active_inactive_ChatGPT$active_n
Active_inactive_ChatGPT$sedentary_noX_n = 16 - Active_inactive_ChatGPT$sedentary_n
Active_inactive_ChatGPT$noX_n =   32 - Active_inactive_ChatGPT$total_n



#############

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


#  The first versio included: 

# Active_inactive_ChatGPT$construct_item = case_when(Active_inactive_ChatGPT$construct == "BaCap+_selfEfficacy" ~ "SelfEfficacy", 
#                                             Active_inactive_ChatGPT$construct ==  "BaCap-_selfEfficacy_ hlth_cndtns" ~ "Comorbidity", 
#                                             Active_inactive_ChatGPT$construct ==  "BaCap-_selfEfficacy_older_age" ~ "Age", 
#                                             #Active_inactive_ChatGPT$construct ==  "NA" ~ "PositiveAttitude", # no data from ChatGPT prior, in likelihood assessed using a scale in study see: (Pozehl, Mcguire, et al., 2018), 
#                                             Active_inactive_ChatGPT$construct ==  "SI+_social_support_emotional" ~ "SocialSupport",  # perceived social support score: (Gallagher, Luttik, & Jaarsma, 2011)
#                                             Active_inactive_ChatGPT$construct ==  "Emotion-" ~ "NegativeAttitude") #in likelihood assessed using a scale in study see: (Pozehl, Mcguire, et al., 2018), 



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

Active_inactive_ChatGPT_self_efficacy = data.frame(Active_inactive_ChatGPT)

Active_inactive_ChatGPT_self_efficacy$construct_item = case_when(Active_inactive_ChatGPT$construct == "BaCap+_selfEfficacy" ~ "BaCap+_selfEfficacy",
                                                                 
                                                                 ### add this three below:
                                                                 Active_inactive_ChatGPT$construct == "BaCap-_selfEfficacy_HF" ~ "BaCap-_selfEfficacy_HF",
                                                                 Active_inactive_ChatGPT$construct == "BaCap-_selfEfficacy_prcvd_smptms" ~ "BaCap-_selfEfficacy_prcvd_smptms",
                                                                 Active_inactive_ChatGPT$construct == "BaCap-_selfEfficacy_hlth_cndtns_typePA" ~ "BaCap-_selfEfficacy_hlth_cndtns_typePA",
                                                                 
                                                                 Active_inactive_ChatGPT$construct ==  "BaCap-_selfEfficacy_ hlth_cndtns" ~ "BaCap+_selfEfficacy_ hlth_cndtns", # we are inverting the OR for these below therefore changing them from BaCap- to BaCap+
                                                                 Active_inactive_ChatGPT$construct ==  "BaCap-_selfEfficacy_older_age" ~ "BaCap+_selfEfficacy_older_age", # we are inverting the OR for these below therefore changing them from BaCap- to BaCap+
                                                                 Active_inactive_ChatGPT$construct ==  "BaCap-_selfEfficacy_heart" ~ "BaCap+_selfEfficacy_heart", # we are inverting the OR for these below therefore changing them from BaCap- to BaCap+
                                                                 Active_inactive_ChatGPT$construct ==  "BaCap-_selfEfficacy_prcvd_smptmsHF" ~ "BaCap+_selfEfficacy_prcvd_smptmsHF")  # we are inverting the OR for these below therefore changing them from BaCap- to BaCap+


x_ChatGPT_self_efficacy = Active_inactive_ChatGPT_self_efficacy[complete.cases(Active_inactive_ChatGPT_self_efficacy), ]
print(x_ChatGPT_self_efficacy)


x_ChatGPT_self_efficacy$participant_source = rep("ChatGPT", times = nrow(x_ChatGPT_self_efficacy))

x_ChatGPT_self_efficacy$PriorExpert_N_PA_X   =   x_ChatGPT_self_efficacy$active_n + 0.5
x_ChatGPT_self_efficacy$PriorExpert_N_PA_noX =   x_ChatGPT_self_efficacy$active_noX_n + 0.5
x_ChatGPT_self_efficacy$PriorExpert_N_noPA_X =   x_ChatGPT_self_efficacy$sedentary_n + 0.5
x_ChatGPT_self_efficacy$PriorExpert_N_noPA_noX = x_ChatGPT_self_efficacy$sedentary_noX_n + 0.5

#print("decide on the variance for the prior")
#x_ChatGPT$variance = 0.1

print(x_ChatGPT_self_efficacy)

# calculate the OR and variance for self-efficacy prior from ChatGPT

self_efficacy_logOR <- escalc(measure="OR", ai=PriorExpert_N_PA_X, bi=PriorExpert_N_PA_noX , ci=PriorExpert_N_noPA_X, di=PriorExpert_N_noPA_noX, data=x_ChatGPT_self_efficacy, replace=FALSE)

self_efficacy_logOR$yi
# we need to invert the OR from belief statements that are phrased negatively 
# self_efficacy_logOR$OR = c(exp(self_efficacy_logOR$yi[1]), 1/exp(self_efficacy_logOR$yi[2]), 1/exp(self_efficacy_logOR$yi[3]), 1/exp(self_efficacy_logOR$yi[4]), 1/exp(self_efficacy_logOR$yi[5]))
# self_efficacy_logOR$logOR = c(log(self_efficacy_logOR$OR[1]), log(self_efficacy_logOR$OR[2]), log(self_efficacy_logOR$OR[3]), log(self_efficacy_logOR$OR[4]), log(self_efficacy_logOR$OR[5]))

self_efficacy_logOR$logOR = c(self_efficacy_logOR$yi[1], 
                              self_efficacy_logOR$yi[2]/-1,
                              self_efficacy_logOR$yi[3]/-1,
                              self_efficacy_logOR$yi[4]/-1, 
                              self_efficacy_logOR$yi[5]/-1, 
                              self_efficacy_logOR$yi[6]/-1, 
                              self_efficacy_logOR$yi[7]/-1, 
                              self_efficacy_logOR$yi[8]/-1)

self_efficacy_logOR$variance = self_efficacy_logOR$vi

self_efficacy_logOR$belief_statement
self_efficacy_logOR$construct_item 
self_efficacy_logOR$participant_source
self_efficacy_logOR$variance
self_efficacy_logOR$logOR

self_efficacy_logOR$pooled_construct_item = rep("self_efficacy_ma", times = nrow(self_efficacy_logOR))

##########
##########

# check: Active_inactive_ChatGPT$construct ==  "SI+_social_support_practical_compan" ~ "SI+_social_support_practical_compan",


Active_inactive_ChatGPT_social_support_practical = data.frame(Active_inactive_ChatGPT)


Active_inactive_ChatGPT_social_support_practical$construct_item = case_when(Active_inactive_ChatGPT$construct ==  "SI+_social_support_emotional" ~ "SI+_social_support_emotional") 


x_ChatGPT_social_support_practical = Active_inactive_ChatGPT_social_support_practical[complete.cases(Active_inactive_ChatGPT_social_support_practical), ]
print(x_ChatGPT_social_support_practical)


x_ChatGPT_social_support_practical$participant_source = rep("ChatGPT", times = nrow(x_ChatGPT_social_support_practical))

x_ChatGPT_social_support_practical$PriorExpert_N_PA_X   =   x_ChatGPT_social_support_practical$active_n + 0.5
x_ChatGPT_social_support_practical$PriorExpert_N_PA_noX =   x_ChatGPT_social_support_practical$active_noX_n + 0.5
x_ChatGPT_social_support_practical$PriorExpert_N_noPA_X =   x_ChatGPT_social_support_practical$sedentary_n + 0.5
x_ChatGPT_social_support_practical$PriorExpert_N_noPA_noX = x_ChatGPT_social_support_practical$sedentary_noX_n + 0.5

#print("decide on the variance for the prior")
#x_ChatGPT$variance = 0.1

print(x_ChatGPT_social_support_practical)

# calculate the OR and variance for self-efficacy prior from ChatGPT

social_support_practical_logOR <- escalc(measure="OR", ai=PriorExpert_N_PA_X, bi=PriorExpert_N_PA_noX , ci=PriorExpert_N_noPA_X, di=PriorExpert_N_noPA_noX, data=x_ChatGPT_social_support_practical, replace=FALSE)

social_support_practical_logOR$yi
# we need to invert the OR from belief statements that are phrased negatively 
social_support_practical_logOR$logOR = social_support_practical_logOR$yi
social_support_practical_logOR$variance = social_support_practical_logOR$vi


social_support_practical_logOR$belief_statement
social_support_practical_logOR$construct_item 
social_support_practical_logOR$participant_source
social_support_practical_logOR$variance
social_support_practical_logOR$logOR

social_support_practical_logOR$pooled_construct_item = rep("social_support_ma", times = nrow(social_support_practical_logOR))


#######
#######

#ChatGPT negative attitude: this is based on the examination of the scale used in the likelihood (Pozehl et al., 2018)
# BaCon-_neg_expctncy_sympcomrbd



Active_inactive_ChatGPT_negative_attitude = data.frame(Active_inactive_ChatGPT)


Active_inactive_ChatGPT_negative_attitude$construct_item = case_when(Active_inactive_ChatGPT$construct ==  "BaCon-_neg_expctncy_sympcomrbd" ~ "BaCon-_neg_expctncy_sympcomrbd") 


x_ChatGPT_negative_attitude = Active_inactive_ChatGPT_negative_attitude[complete.cases(Active_inactive_ChatGPT_negative_attitude), ]
print(x_ChatGPT_negative_attitude)


x_ChatGPT_negative_attitude$participant_source = rep("ChatGPT", times = nrow(x_ChatGPT_negative_attitude))

x_ChatGPT_negative_attitude$PriorExpert_N_PA_X   =   x_ChatGPT_negative_attitude$active_n + 0.5
x_ChatGPT_negative_attitude$PriorExpert_N_PA_noX =   x_ChatGPT_negative_attitude$active_noX_n + 0.5
x_ChatGPT_negative_attitude$PriorExpert_N_noPA_X =   x_ChatGPT_negative_attitude$sedentary_n + 0.5
x_ChatGPT_negative_attitude$PriorExpert_N_noPA_noX = x_ChatGPT_negative_attitude$sedentary_noX_n + 0.5

#print("decide on the variance for the prior")
#x_ChatGPT$variance = 0.1

print(x_ChatGPT_negative_attitude)

# calculate the OR and variance for self-efficacy prior from ChatGPT

negative_attitude_logOR <- escalc(measure="OR", ai=PriorExpert_N_PA_X, bi=PriorExpert_N_PA_noX , ci=PriorExpert_N_noPA_X, di=PriorExpert_N_noPA_noX, data=x_ChatGPT_negative_attitude, replace=FALSE)

negative_attitude_logOR$yi
# we need to invert the OR from belief statements that are phrased negatively 
negative_attitude_logOR$logOR = negative_attitude_logOR$yi
negative_attitude_logOR$variance = negative_attitude_logOR$vi


negative_attitude_logOR$belief_statement
negative_attitude_logOR$construct_item 
negative_attitude_logOR$participant_source
negative_attitude_logOR$variance
negative_attitude_logOR$logOR

negative_attitude_logOR$pooled_construct_item = rep("negative_attitude_ma", times = nrow(negative_attitude_logOR))



#x_ChatGPT$logOR = log(x_ChatGPT$PriorExpert_N_PA_X*x_ChatGPT$PriorExpert_N_noPA_noX)/(x_ChatGPT$PriorExpert_N_noPA_X*x_ChatGPT$PriorExpert_N_PA_noX)

#x_ChatGPT$logOR_selfefficacy = c(x_ChatGPT$logOR[1],  1/x_ChatGPT$logOR[2], 1/x_ChatGPT$logOR[3], 1/x_ChatGPT$logOR[4], 1/x_ChatGPT$logOR[5])

######
######

Active_inactive_ChatGPT_positive_attitude = data.frame(Active_inactive_ChatGPT)

#ChatGPT Positive attitude: this is based on the examination of the scale used in the likelihood (Pozehl et al., 2018)
#ChatGPT: BaCon+_pos_expctncy_health, 


Active_inactive_ChatGPT_positive_attitude$construct_item = case_when(Active_inactive_ChatGPT$construct ==  "BaCon+_pos_expctncy_health" ~ "BaCon+_pos_expctncy_health")


x_ChatGPT_positive_attitude = Active_inactive_ChatGPT_positive_attitude[complete.cases(Active_inactive_ChatGPT_positive_attitude), ]
print(x_ChatGPT_positive_attitude)


x_ChatGPT_positive_attitude$participant_source = rep("ChatGPT", times = nrow(x_ChatGPT_positive_attitude))

x_ChatGPT_positive_attitude$PriorExpert_N_PA_X   =   x_ChatGPT_positive_attitude$active_n + 0.5
x_ChatGPT_positive_attitude$PriorExpert_N_PA_noX =   x_ChatGPT_positive_attitude$active_noX_n + 0.5
x_ChatGPT_positive_attitude$PriorExpert_N_noPA_X =   x_ChatGPT_positive_attitude$sedentary_n + 0.5
x_ChatGPT_positive_attitude$PriorExpert_N_noPA_noX = x_ChatGPT_positive_attitude$sedentary_noX_n + 0.5

#print("decide on the variance for the prior")
#x_ChatGPT$variance = 0.1

print(x_ChatGPT_positive_attitude)

# calculate the OR and variance for self-efficacy prior from ChatGPT

positive_attitude_logOR <- escalc(measure="OR", ai=PriorExpert_N_PA_X, bi=PriorExpert_N_PA_noX , ci=PriorExpert_N_noPA_X, di=PriorExpert_N_noPA_noX, data=x_ChatGPT_positive_attitude, replace=FALSE)

positive_attitude_logOR$yi
# we need to invert the OR from belief statements that are phrased negatively 
positive_attitude_logOR$logOR = positive_attitude_logOR$yi
positive_attitude_logOR$variance = positive_attitude_logOR$vi



positive_attitude_logOR$belief_statement
positive_attitude_logOR$construct_item 
positive_attitude_logOR$participant_source
positive_attitude_logOR$variance
positive_attitude_logOR$logOR


positive_attitude_logOR$pooled_construct_item = rep("positive_attitude_ma", times = nrow(positive_attitude_logOR))


#######
#######
#######

#ChatGPT symptom distress
#ChatGPT: Emotion-_fear


Active_inactive_ChatGPT_symptom_distress = data.frame(Active_inactive_ChatGPT)


Active_inactive_ChatGPT_symptom_distress$construct_item = case_when(Active_inactive_ChatGPT$construct == "Emotion-_fear" ~ "Emotion-_fear")



x_ChatGPT_symptom_distress = Active_inactive_ChatGPT_symptom_distress[complete.cases(Active_inactive_ChatGPT_symptom_distress), ]
print(x_ChatGPT_symptom_distress)


x_ChatGPT_symptom_distress$participant_source = rep("ChatGPT", times = nrow(x_ChatGPT_symptom_distress))

x_ChatGPT_symptom_distress$PriorExpert_N_PA_X   =   x_ChatGPT_symptom_distress$active_n + 0.5
x_ChatGPT_symptom_distress$PriorExpert_N_PA_noX =   x_ChatGPT_symptom_distress$active_noX_n + 0.5
x_ChatGPT_symptom_distress$PriorExpert_N_noPA_X =   x_ChatGPT_symptom_distress$sedentary_n + 0.5
x_ChatGPT_symptom_distress$PriorExpert_N_noPA_noX = x_ChatGPT_symptom_distress$sedentary_noX_n + 0.5

#print("decide on the variance for the prior")
#x_ChatGPT$variance = 0.1

print(x_ChatGPT_symptom_distress)

# calculate the OR and variance for self-efficacy prior from ChatGPT

symptom_distress_logOR <- escalc(measure="OR", ai=PriorExpert_N_PA_X, bi=PriorExpert_N_PA_noX , ci=PriorExpert_N_noPA_X, di=PriorExpert_N_noPA_noX, data=x_ChatGPT_symptom_distress, replace=FALSE)

symptom_distress_logOR$yi
# we need to invert the OR from belief statements that are phrased negatively 
symptom_distress_logOR$logOR = symptom_distress_logOR$yi
symptom_distress_logOR$variance = symptom_distress_logOR$vi



symptom_distress_logOR$belief_statement
symptom_distress_logOR$construct_item 
symptom_distress_logOR$participant_source
symptom_distress_logOR$variance
symptom_distress_logOR$logOR

symptom_distress_logOR$pooled_construct_item = rep("symptom_distress_ma", times = nrow(symptom_distress_logOR))

#######
#######
#######

#ChatGPT perceived symptoms
#ChatGPT: BaCap-_selfEfficacy_prcvd_smptmsHF, BaCap-_selfEfficacy_prcvd_smptms



Active_inactive_ChatGPT_perceived_symptoms = data.frame(Active_inactive_ChatGPT)


Active_inactive_ChatGPT_perceived_symptoms$construct_item = case_when(Active_inactive_ChatGPT$construct == "BaCap-_selfEfficacy_prcvd_smptmsHF" ~ "BaCap-_prcvd_smptmsHF", 
                                                                      Active_inactive_ChatGPT$construct == "BaCap-_selfEfficacy_prcvd_smptms" ~ "BaCap-_selfEfficacy_prcvd_smptms") #changing this one here so we do not confuse it with the self-efficacy coding above

x_ChatGPT_perceived_symptoms = Active_inactive_ChatGPT_perceived_symptoms[complete.cases(Active_inactive_ChatGPT_perceived_symptoms), ]
print(x_ChatGPT_perceived_symptoms)


x_ChatGPT_perceived_symptoms$participant_source = rep("ChatGPT", times = nrow(x_ChatGPT_perceived_symptoms))

x_ChatGPT_perceived_symptoms$PriorExpert_N_PA_X   =   x_ChatGPT_perceived_symptoms$active_n + 0.5
x_ChatGPT_perceived_symptoms$PriorExpert_N_PA_noX =   x_ChatGPT_perceived_symptoms$active_noX_n + 0.5
x_ChatGPT_perceived_symptoms$PriorExpert_N_noPA_X =   x_ChatGPT_perceived_symptoms$sedentary_n + 0.5
x_ChatGPT_perceived_symptoms$PriorExpert_N_noPA_noX = x_ChatGPT_perceived_symptoms$sedentary_noX_n + 0.5

#print("decide on the variance for the prior")
#x_ChatGPT$variance = 0.1

print(x_ChatGPT_perceived_symptoms)

# calculate the OR and variance for self-efficacy prior from ChatGPT

perceived_symptoms_logOR <- escalc(measure="OR", ai=PriorExpert_N_PA_X, bi=PriorExpert_N_PA_noX , ci=PriorExpert_N_noPA_X, di=PriorExpert_N_noPA_noX, data=x_ChatGPT_perceived_symptoms, replace=FALSE)

perceived_symptoms_logOR$yi
# we need to invert the OR from belief statements that are phrased negatively 
perceived_symptoms_logOR$logOR = c(perceived_symptoms_logOR$yi[1]/-1, 
                                   perceived_symptoms_logOR$yi[2]/-2)
perceived_symptoms_logOR$variance = perceived_symptoms_logOR$vi



perceived_symptoms_logOR$belief_statement
perceived_symptoms_logOR$construct_item 
perceived_symptoms_logOR$participant_source
perceived_symptoms_logOR$variance
perceived_symptoms_logOR$logOR


perceived_symptoms_logOR$pooled_construct_item = rep("fewer_perceived_symptoms_ma", times = nrow(perceived_symptoms_logOR))

#######
#######
#######

#ChatGPT Symptom dysphoria 
#ChatGPT: Emotion-_negative_emotions, Emotion-_mood

Active_inactive_ChatGPT_dysphoria = data.frame(Active_inactive_ChatGPT)


Active_inactive_ChatGPT_dysphoria$construct_item = case_when(Active_inactive_ChatGPT$construct == "Emotion-_negative_emotions" ~ "Emotion-_negative_emotions", 
                                                             Active_inactive_ChatGPT$construct == "Emotion-_mood" ~ "Emotion-_mood")


x_ChatGPT_dysphoria = Active_inactive_ChatGPT_dysphoria[complete.cases(Active_inactive_ChatGPT_dysphoria), ]
print(x_ChatGPT_dysphoria)


x_ChatGPT_dysphoria$participant_source = rep("ChatGPT", times = nrow(x_ChatGPT_dysphoria))

x_ChatGPT_dysphoria$PriorExpert_N_PA_X   =   x_ChatGPT_dysphoria$active_n + 0.5
x_ChatGPT_dysphoria$PriorExpert_N_PA_noX =   x_ChatGPT_dysphoria$active_noX_n + 0.5
x_ChatGPT_dysphoria$PriorExpert_N_noPA_X =   x_ChatGPT_dysphoria$sedentary_n + 0.5
x_ChatGPT_dysphoria$PriorExpert_N_noPA_noX = x_ChatGPT_dysphoria$sedentary_noX_n + 0.5

#print("decide on the variance for the prior")
#x_ChatGPT$variance = 0.1

print(x_ChatGPT_dysphoria)

# calculate the OR and variance for self-efficacy prior from ChatGPT

dysphoria_logOR <- escalc(measure="OR", ai=PriorExpert_N_PA_X, bi=PriorExpert_N_PA_noX , ci=PriorExpert_N_noPA_X, di=PriorExpert_N_noPA_noX, data=x_ChatGPT_dysphoria, replace=FALSE)

dysphoria_logOR$yi
# we need to invert the OR from belief statements that are phrased negatively 
dysphoria_logOR$logOR = dysphoria_logOR$yi
dysphoria_logOR$variance = dysphoria_logOR$vi



dysphoria_logOR$belief_statement
dysphoria_logOR$construct_item 
dysphoria_logOR$participant_source
dysphoria_logOR$variance
dysphoria_logOR$logOR

dysphoria_logOR$pooled_construct_item = rep("dysphoria_ma", times = nrow(dysphoria_logOR))


#######
#######
#######


ChatGPT_input_data_for_prior =  rbind(perceived_symptoms_logOR, 
                                      dysphoria_logOR, 
                                      symptom_distress_logOR, 
                                      positive_attitude_logOR, 
                                      negative_attitude_logOR, 
                                      social_support_practical_logOR, 
                                      self_efficacy_logOR)


self_efficacy_ma <- rma(yi = logOR, vi = variance, 
                        data=ChatGPT_input_data_for_prior, 
                        method = "REML", 
                        subset=(pooled_construct_item=="self_efficacy_ma"))





perceived_symptoms_ma <- rma(yi = logOR, vi = variance, 
                             data=ChatGPT_input_data_for_prior, 
                             method = "REML", 
                             subset=(pooled_construct_item=="fewer_perceived_symptoms_ma"))





dysphoria_ma <- rma(yi = logOR, vi = variance, 
                    data=ChatGPT_input_data_for_prior, 
                    method = "REML", 
                    subset=(pooled_construct_item=="dysphoria_ma"))




symptom_distress_ma <- rma(yi = logOR, vi = variance, 
                           data=ChatGPT_input_data_for_prior, 
                           method = "REML", 
                           subset=(pooled_construct_item=="symptom_distress_ma"))






positive_attitude_ma <- rma(yi = logOR, vi = variance, 
                            data=ChatGPT_input_data_for_prior, 
                            method = "REML", 
                            subset=(pooled_construct_item=="positive_attitude_ma"))




negative_attitude_ma <- rma(yi = logOR, vi = variance, 
                            data=ChatGPT_input_data_for_prior, 
                            method = "REML", 
                            subset=(pooled_construct_item=="negative_attitude_ma"))



social_support_ma <- rma(yi = logOR, vi = variance, 
                         data=ChatGPT_input_data_for_prior, 
                         method = "REML", 
                         subset=(pooled_construct_item=="social_support_ma"))


Construct = c("SocialSupport",
              "NegativeAttitude",
              "PositiveAttitude", 
              "Symptoms_distress",
              "SelfEfficacy",
              "fewerPerceivedSymptoms", 
              "Dysphoria")


input_ChatGPT_prior = data.frame(Construct)


input_ChatGPT_prior$logOR_prior_elicitation = c(social_support_ma$beta,
                                                negative_attitude_ma$beta,
                                                positive_attitude_ma$beta,
                                                symptom_distress_ma$beta,
                                                self_efficacy_ma$beta,
                                                perceived_symptoms_ma$beta,
                                                dysphoria_ma$beta)



input_ChatGPT_prior$variance_prior_elicitation = c(social_support_ma$pval,
                                                   negative_attitude_ma$pval,
                                                   positive_attitude_ma$pval,
                                                   symptom_distress_ma$pval,
                                                   self_efficacy_ma$pval,
                                                   perceived_symptoms_ma$pval,
                                                   dysphoria_ma$pval)


input_ChatGPT_prior$participant_source = rep("ChatGPT", times = nrow(input_ChatGPT_prior))

write.csv(input_ChatGPT_prior, file = paste(DATA_ROOT, "input_ChatGPT_prior.csv", sep="")) # new qualitative data ChatGPT







