


library(ggplot2)
library(dplyr)
library(survminer)
########### DIRECTORY

#directory = "/Users/aliyaamirova/"
directory = "/Users/aliya/my_docs/"

###########  source root 
SOURCE_ROOT = paste(directory, "proj/bayesian_review_methods/", sep = "")

###########  data root
DATA_ROOT = paste(directory, "proj/bayesian_review_methods/DATA/", sep = "")

########### Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/CHatGPTvsHuman/Mean_percentage_quote/", sep = "")



#DATA: 
per_participant_Human = read.csv(paste(DATA_ROOT, "Human_perBS_per_participant_FINAL.csv", sep =""))
#per_participant_Human = subset(per_participant_Human, belief_statement !="Total")

per_bot_ChatGPT = read.csv(paste(DATA_ROOT, "ChatGPT_perBS_perBot_FINAL.csv", sep =""))
#per_bot_ChatGPT = subset(per_bot_ChatGPT, belief_statement !="Total")


#list of constructs 
per_participant_Human$human = rep("Human participants", times= nrow(per_participant_Human))
per_bot_ChatGPT$human = rep("Chat GPT characters", times = nrow(per_bot_ChatGPT))


per_participant_Human_domain =  select(per_participant_Human, c("Cases", 
                                                                
                                                                "human", 
                                                                
                                                                "PA_status", 
                                                                
                                                                "BRenblr", 
                                                                "BaCapenblr",
                                                                "BaConenblr",
                                                                "ECRenblr",
                                                                "Emotionenblr",
                                                                "Goalsenblr",
                                                                "Intentionenblr",
                                                                "Knowledgeenblr",
                                                                "MADPenblr",
                                                                "Optimismenblr",
                                                                "Reinforcementenblr",
                                                                "SIenblr",
                                                                "X.SPRenblr",
                                                                "Skillsenblr",
                                                                
                                                                "BRbrrier", 
                                                                "BaCapbrrier",
                                                                "BaConbrrier",
                                                                "ECRbrrier",
                                                                "Emotionbrrier",
                                                                "Goalsbrrier",
                                                                "Intentionbrrier",
                                                                "Knowledgebrrier",
                                                                "MADPbrrier",
                                                                "Optimismbrrier",
                                                                "Reinforcementbrrier",
                                                                "SIbrrier",
                                                                "SPRbrrier",
                                                                "Skillsbrrier"))



per_bot_ChatGPT_domain =  select(per_bot_ChatGPT, c("Cases", 
                                                    
                                                    "human",
                                                    
                                                    "PA_status",
                                                    
                                                    "BRenblr", 
                                                    "BaCapenblr",
                                                    "BaConenblr",
                                                    "ECRenblr",
                                                    "Emotionenblr",
                                                    "Goalsenblr",
                                                    "Intentionenblr",
                                                    "Knowledgeenblr",
                                                    "MADPenblr",
                                                    "Optimismenblr",
                                                    "Reinforcementenblr",
                                                    "SIenblr",
                                                    "X.SPRenblr",
                                                    "Skillsenblr",
                                                    
                                                    "BRbrrier", 
                                                    "BaCapbrrier",
                                                    "BaConbrrier",
                                                    "ECRbrrier",
                                                    "Emotionbrrier",
                                                    "Goalsbrrier",
                                                    "Intentionbrrier",
                                                    "Knowledgebrrier",
                                                    "MADPbrrier",
                                                    "Optimismbrrier",
                                                    "Reinforcementbrrier",
                                                    "SIbrrier",
                                                    "SPRbrrier",
                                                    "Skillsbrrier"))




#DATA: #DATA: #DATA: #DATA: #DATA: 
#DATA: #DATA: #DATA: #DATA: #DATA: 
#DATA: #DATA: #DATA: #DATA: #DATA: 
#DATA: #DATA: #DATA: #DATA: #DATA: 
#DATA: #DATA: #DATA: #DATA: #DATA: 
#DATA: #DATA: #DATA: #DATA: #DATA: 

data_domain = rbind(per_participant_Human_domain, per_bot_ChatGPT_domain)
data_human = subset(data_domain, human == "Human participants")
data_ChatGPT = subset(data_domain, human == "Chat GPT characters")


########### # # # # # # perform t-test for each category
########### BR (Barrier)
# # human
compare_means(BRbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(BRbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### BaCap (Barrier)
# # human
compare_means(BaCapbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(BaCapbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### BaCon (Barrier)
# # human
compare_means(BaConbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(BaConbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### ECR (Barrier)
# # human
compare_means(ECRbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(ECRbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### Emotion (Barrier)

compare_means(Emotionbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # human
#### Emotion Barrier domain had 0 quotes for humans

# # ChatGPT
compare_means(Emotionbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### Goals (Barrier)
# # human
compare_means(Goalsbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(Goalsbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### Intention (Barrier)
# # human
compare_means(Intentionbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
########### # # # # # # perform t-test for each category
########### Intention (Barrier)
per_participant_Human_domain$Intentionbrrier



########### # # # # # # perform t-test for each category
########### Knowledge (Barrier)
# # human
compare_means(Knowledgebrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(Knowledgebrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### MADP (Barrier)
# # human
compare_means(MADPbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(MADPbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### Optimism (Barrier)
# # human
compare_means(Optimismbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(Optimismbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### Reinforcement (Barrier)
# # human
compare_means(Reinforcementbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(Reinforcementbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### SI (Barrier)
# # human
compare_means(SIbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(SIbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### SPR (Barrier)
# # human

compare_means(SPRbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(SPRbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### Skills (Barrier)
# # human
compare_means(Skillsbrrier ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(Skillsbrrier ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




