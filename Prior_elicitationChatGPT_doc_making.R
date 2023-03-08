

#############
#############
#############

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


# Active_inactive_ChatGPT$Construct = case_when(Active_inactive_ChatGPT$construct == "BaCap+_selfEfficacy" ~ "SelfEfficacy", 
#                                             Active_inactive_ChatGPT$construct ==  "BaCap-_selfEfficacy_ hlth_cndtns" ~ "Comorbidity", 
#                                             Active_inactive_ChatGPT$construct ==  "BaCap-_selfEfficacy_older_age" ~ "Age", 
#                                             #Active_inactive_ChatGPT$construct ==  "NA" ~ "PositiveAttitude", # no data from human prior, in likelihood assessed using a scale in study see: (Pozehl, Mcguire, et al., 2018), 
#                                             Active_inactive_ChatGPT$construct ==  "SI+_social_support_emotional" ~ "SocialSupport",  # perceived social support score: (Gallagher, Luttik, & Jaarsma, 2011)
#                                             Active_inactive_ChatGPT$construct ==  "Emotion-" ~ "NegativeAttitude") #in likelihood assessed using a scale in study see: (Pozehl, Mcguire, et al., 2018), 




x_ChatGPT = Active_inactive_ChatGPT[complete.cases(Active_inactive_ChatGPT), ]


x_ChatGPT$participant_source = rep("ChatGPT", times = nrow(x))

x_ChatGPT$PriorExpert_N_PA_X   =   x_ChatGPT$active_n + 0.5
x_ChatGPT$PriorExpert_N_PA_noX =   x_ChatGPT$active_noX_n + 0.5
x_ChatGPT$PriorExpert_N_noPA_X =   x_ChatGPT$sedentary_n + 0.5
x_ChatGPT$PriorExpert_N_noPA_noX = x_ChatGPT$sedentary_noX_n + 0.5

print("decide on the variance for the prior")
x_ChatGPT$variance = 0.1

print(x_ChatGPT)


x_ChatGPT$logOR = log(x_ChatGPT$PriorExpert_N_PA_X*x_ChatGPT$PriorExpert_N_noPA_noX)/(x_ChatGPT$PriorExpert_N_noPA_X*x_ChatGPT$PriorExpert_N_PA_noX)


print(x_ChatGPT)

#x = read.csv(paste(DATA_ROOT, "input_chatGPT.csv", sep="")) # new qualitative data



