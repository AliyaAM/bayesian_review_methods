


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


Active_inactive_Human = read.csv(paste(DATA_ROOT, "Active_inactive_Human_March06_FINAL.csv", sep =""))
Active_inactive_Human = subset(Active_inactive_Human, belief_statement !="Total")

Active_inactive_ChatGPT = read.csv(paste(DATA_ROOT, "Active_inactive_ChatGPT_March06_FINAL.csv", sep =""))
Active_inactive_ChatGPT = subset(Active_inactive_ChatGPT, belief_statement !="Total")

Active_inactive_Human$belief_statement

tail(Active_inactive_Human, 20)
#list of constructs 


construct = c(Active_inactive_Human$construct, Active_inactive_Human$construct)
data_human = data.frame(construct)
data_human$quotes = c(Active_inactive_Human$active_k, Active_inactive_Human$sedentary_k)

active_status = rep(1, times= nrow(Active_inactive_Human))
sedentary_status = rep(0, times= nrow(Active_inactive_Human))

data_human$PA_status = as.factor(c(active_status, sedentary_status))

data_human$human = rep(1, times= nrow(data_human))
data_human = data_human[complete.cases(data_human), ]


construct = c(Active_inactive_ChatGPT$construct, Active_inactive_ChatGPT$construct)
data_GPT = data.frame(construct)
data_GPT$quotes = c(Active_inactive_ChatGPT$active_k, Active_inactive_ChatGPT$sedentary_k)

active_status = rep(1, times= nrow(Active_inactive_ChatGPT))
sedentary_status = rep(0, times= nrow(Active_inactive_ChatGPT))

data_GPT$PA_status = as.factor(c(active_status, sedentary_status))

data_GPT$human = rep(0, times = nrow(data_GPT))

data_GPT = data_GPT[complete.cases(data_GPT), ]

data = rbind(data_human, data_GPT)




construct_list_enablers = c("BR+", 
                           "BaCap+", 
                           "BaCon+", 
                           "ECR+", 
                           "Emotion+", 
                           "Goals+", 
                           "Intention+", 
                           "Knowledge+", 
                           "MADP+", 
                           "Optimism+", 
                           "Reinforcement+", 
                           "SI+", 
                           "\nSPR+", 
                           "Skills+")

data_domains = subset(data, data$construct %in% c(construct_list_enablers))
#cols: quotes, PA_status, human
# rows: domain

 #for enablers 



ggplot(data_domains, aes(y= reorder(construct, +quotes), x = quotes)) +
 geom_col(aes(fill = PA_status), position = "dodge") + 
facet_wrap(~human, nrow=1)



# 
# 
# # for barriers 

construct_list_barriers = c("BR-", 
                            "BaCap-", 
                            "BaCon-", 
                            "ECR-", 
                            "Emotion-", 
                            "Goals-", 
                            "Intention-", 
                            "Knowledge-", 
                            "MADP-", 
                            "Optimism-", 
                            "Reinforcement-", 
                            "SI-", 
                            "SPR-", 
                            "Skills-")

data_domains_barriers = subset(data, data$construct %in% c(construct_list_barriers))
#cols: quotes, PA_status, human
# rows: domain

#for enablers 



ggplot(data_domains_barriers, aes(y= reorder(construct, +quotes), x = quotes)) +
  geom_col(aes(fill = PA_status), position = "dodge") + 
  facet_wrap(~human, nrow=1)