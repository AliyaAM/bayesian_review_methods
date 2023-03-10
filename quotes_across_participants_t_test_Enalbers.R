

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
########### BR (Enabler)
# # human
compare_means(BRenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(BRenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### BaCap (Enabler)
# # human
compare_means(BaCapenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(BaCapenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### BaCon (Enabler)
# # human
compare_means(BaConenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(BaConenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### ECR (Enabler)
# # human
compare_means(ECRenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(ECRenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### Emotion (Enabler)
per_participant_Human_domain$Emotionenblr

# # human
#### Emotion enabler domain had 0 quotes for humans

# # ChatGPT
compare_means(Emotionenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### Goals (Enabler)
# # human
compare_means(Goalsenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(Goalsenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### Intention (Enabler)
# # human
compare_means(Intentionenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(Intentionenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### Knowledge (Enabler)
# # human
compare_means(Knowledgeenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(Knowledgeenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### MADP (Enabler)
# # human
compare_means(MADPenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(MADPenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### Optimism (Enabler)
# # human
compare_means(Optimismenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(Optimismenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### Reinforcement (Enabler)
# # human
compare_means(Reinforcementenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(Reinforcementenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




########### # # # # # # perform t-test for each category
########### SI (Enabler)
# # human
compare_means(SIenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(SIenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### SPR (Enabler)
# # human
compare_means(X.SPRenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(X.SPRenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")



########### # # # # # # perform t-test for each category
########### Skills (Enabler)
# # human
compare_means(Skillsenblr ~ PA_status,  data = data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(Skillsenblr ~ PA_status,  data = data_ChatGPT,
              ref.group = "0", method = "t.test")




# "BRbrrier", 
# "BaCapbrrier",
# "BaConbrrier",
# "ECRbrrier",
# "Emotionbrrier",
# "Goalsbrrier",
# "Intentionbrrier",
# "Knowledgebrrier",
# "MADPbrrier",
# "Optimismbrrier",
# "Reinforcementbrrier",
# "SIbrrier",
# "SPRbrrier",
# "Skillsbrrier"



# ########
# ########
# 
# 
# ######### data enablers 
# 
# per_participant_Human_enblr =  select(per_participant_Human, c("Cases", 
#                                                                
#                                                                "human", 
#                                                                
#                                                                "PA_status", 
#                                                                
#                                                                "BRenblr", 
#                                                                "BaCapenblr",
#                                                                "BaConenblr",
#                                                                "ECRenblr",
#                                                                "Emotionenblr",
#                                                                "Goalsenblr",
#                                                                "Intentionenblr",
#                                                                "Knowledgeenblr",
#                                                                "MADPenblr",
#                                                                "Optimismenblr",
#                                                                "Reinforcementenblr",
#                                                                "SIenblr",
#                                                                "X.SPRenblr",
#                                                                "Skillsenblr"))
# 
# 
# 
# per_bot_ChatGPT_enblr =  select(per_bot_ChatGPT, c("Cases", 
#                                                    
#                                                    "human",
#                                                    
#                                                    "PA_status",
#                                                    
#                                                    "BRenblr", 
#                                                    "BaCapenblr",
#                                                    "BaConenblr",
#                                                    "ECRenblr",
#                                                    "Emotionenblr",
#                                                    "Goalsenblr",
#                                                    "Intentionenblr",
#                                                    "Knowledgeenblr",
#                                                    "MADPenblr",
#                                                    "Optimismenblr",
#                                                    "Reinforcementenblr",
#                                                    "SIenblr",
#                                                    "X.SPRenblr",
#                                                    "Skillsenblr"))
# 
# 
# head(per_participant_Human_enblr)
# head(per_bot_ChatGPT_enblr)
# 
# 
# data_enblr = rbind(per_participant_Human_enblr, per_bot_ChatGPT_enblr)
# 
# 
# 
# 
# #########
# #########
# #########
# #########
# 
# ######### data barriers 
# 
# 
# 
# per_participant_Human_brrier =  select(per_participant_Human, c("Cases", 
#                                                                 
#                                                                 "human", 
#                                                                 
#                                                                 "PA_status", 
#                                                                 
#                                                                 
#                                                                 "BRbrrier", 
#                                                                 "BaCapbrrier",
#                                                                 "BaConbrrier",
#                                                                 "ECRbrrier",
#                                                                 "Emotionbrrier",
#                                                                 "Goalsbrrier",
#                                                                 "Intentionbrrier",
#                                                                 "Knowledgebrrier",
#                                                                 "MADPbrrier",
#                                                                 "Optimismbrrier",
#                                                                 "Reinforcementbrrier",
#                                                                 "SIbrrier",
#                                                                 "SPRbrrier",
#                                                                 "Skillsbrrier"))
# 
# 
# 
# per_bot_ChatGPT_brrier =  select(per_bot_ChatGPT, c("Cases", 
#                                                     
#                                                     "human",
#                                                     
#                                                     "PA_status",
#                                                     
#                                                     
#                                                     
#                                                     "BRbrrier", 
#                                                     "BaCapbrrier",
#                                                     "BaConbrrier",
#                                                     "ECRbrrier",
#                                                     "Emotionbrrier",
#                                                     "Goalsbrrier",
#                                                     "Intentionbrrier",
#                                                     "Knowledgebrrier",
#                                                     "MADPbrrier",
#                                                     "Optimismbrrier",
#                                                     "Reinforcementbrrier",
#                                                     "SIbrrier",
#                                                     "SPRbrrier",
#                                                     "Skillsbrrier"))
# 
# 
# 
# head(per_participant_Human_brrier)
# head(per_bot_ChatGPT_brrier)
# 
# 
# data_brrier = rbind(per_participant_Human_brrier, per_bot_ChatGPT_brrier)

