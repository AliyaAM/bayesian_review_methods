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


############### BR enabler

data_domain_BRenblr = select(data_domain, c("Cases", 
                                            "human", 
                                            "PA_status",  
                                            "BRenblr"))


data_domain_BRenblr$quotes = data_domain_BRenblr$BRenblr

data_domain_BRenblr$construct = rep("BR+", times = nrow(data_domain_BRenblr))

data_domain_BRenblr = select(data_domain_BRenblr, -c("BRenblr"))



###############
############### BaCapenblr


data_domain_BaCapenblr = select(data_domain, c("Cases", 
                                               "human", 
                                               "PA_status",  
                                               "BaCapenblr"))


data_domain_BaCapenblr$quotes = data_domain_BaCapenblr$BaCapenblr

data_domain_BaCapenblr$construct = rep("BaCap+", times = nrow(data_domain_BaCapenblr))

data_domain_BaCapenblr = select(data_domain_BaCapenblr, -c("BaCapenblr"))


###############
###############



###############
############### BaCon enblr


data_domain_BaConenblr = select(data_domain, c("Cases", 
                                               "human", 
                                               "PA_status",  
                                               "BaConenblr"))


data_domain_BaConenblr$quotes = data_domain_BaConenblr$BaConenblr

data_domain_BaConenblr$construct = rep("BaCon+", times = nrow(data_domain_BaConenblr))

data_domain_BaConenblr = select(data_domain_BaConenblr, -c("BaConenblr"))


###############
###############


###############
############### ECRenblr enblr


data_domain_ECRenblr = select(data_domain, c("Cases", 
                                               "human", 
                                               "PA_status",  
                                               "ECRenblr"))


data_domain_ECRenblr$quotes = data_domain_ECRenblr$ECRenblr

data_domain_ECRenblr$construct = rep("ECR+", times = nrow(data_domain_ECRenblr))

data_domain_ECRenblr = select(data_domain_ECRenblr, -c("ECRenblr"))


###############
###############


###############
############### "Emotionenblr",
data_domain_Emotionenblr = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Emotionenblr"))


data_domain_Emotionenblr$quotes = data_domain_Emotionenblr$Emotionenblr

data_domain_Emotionenblr$construct = rep("Emotion+", times = nrow(data_domain_Emotionenblr))

data_domain_Emotionenblr = select(data_domain_Emotionenblr, -c("Emotionenblr"))


###############
############### "Goalsenblr",

data_domain_Goalsenblr = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Goalsenblr"))


data_domain_Goalsenblr$quotes = data_domain_Goalsenblr$Goalsenblr

data_domain_Goalsenblr$construct = rep("Goals+", times = nrow(data_domain_Goalsenblr))

data_domain_Goalsenblr = select(data_domain_Goalsenblr, -c("Goalsenblr"))


###############
############### "Intentionenblr",
data_domain_Intentionenblr = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Intentionenblr"))


data_domain_Intentionenblr$quotes = data_domain_Intentionenblr$Intentionenblr

data_domain_Intentionenblr$construct = rep("Intention+", times = nrow(data_domain_Intentionenblr))

data_domain_Intentionenblr = select(data_domain_Intentionenblr, -c("Intentionenblr"))


###############
############### "Knowledgeenblr",

data_domain_Knowledgeenblr = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Knowledgeenblr"))


data_domain_Knowledgeenblr$quotes = data_domain_Knowledgeenblr$Knowledgeenblr

data_domain_Knowledgeenblr$construct = rep("Knowledge+", times = nrow(data_domain_Knowledgeenblr))

data_domain_Knowledgeenblr = select(data_domain_Knowledgeenblr, -c("Knowledgeenblr"))


###############
############### "MADPenblr",
data_domain_MADPenblr = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "MADPenblr"))


data_domain_MADPenblr$quotes = data_domain_MADPenblr$MADPenblr

data_domain_MADPenblr$construct = rep("MADP+", times = nrow(data_domain_MADPenblr))

data_domain_MADPenblr = select(data_domain_MADPenblr, -c("MADPenblr"))


###############
############### "Optimismenblr",

data_domain_Optimismenblr = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Optimismenblr"))


data_domain_Optimismenblr$quotes = data_domain_Optimismenblr$Optimismenblr

data_domain_Optimismenblr$construct = rep("Optimism+", times = nrow(data_domain_Optimismenblr))

data_domain_Optimismenblr = select(data_domain_Optimismenblr, -c("Optimismenblr"))


###############
############### "Reinforcementenblr",

data_domain_Reinforcementenblr = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Reinforcementenblr"))


data_domain_Reinforcementenblr$quotes = data_domain_Reinforcementenblr$Reinforcementenblr

data_domain_Reinforcementenblr$construct = rep("Reinforcement+", times = nrow(data_domain_Reinforcementenblr))

data_domain_Reinforcementenblr = select(data_domain_Reinforcementenblr, -c("Reinforcementenblr"))


###############
############### "SIenblr",

data_domain_SIenblr = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "SIenblr"))


data_domain_SIenblr$quotes = data_domain_SIenblr$SIenblr

data_domain_SIenblr$construct = rep("SI+", times = nrow(data_domain_SIenblr))

data_domain_SIenblr = select(data_domain_SIenblr, -c("SIenblr"))


###############
############### "X.SPRenblr",

data_domain_X.SPRenblr = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "X.SPRenblr"))


data_domain_X.SPRenblr$quotes = data_domain_X.SPRenblr$X.SPRenblr

data_domain_X.SPRenblr$construct = rep("SPR+", times = nrow(data_domain_X.SPRenblr))

data_domain_X.SPRenblr = select(data_domain_X.SPRenblr, -c("X.SPRenblr"))


###############
############### "Skillsenblr",

data_domain_Skillsenblr = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Skillsenblr"))


data_domain_Skillsenblr$quotes = data_domain_Skillsenblr$Skillsenblr

data_domain_Skillsenblr$construct = rep("Skills+", times = nrow(data_domain_Skillsenblr))

data_domain_Skillsenblr = select(data_domain_Skillsenblr, -c("Skillsenblr"))



###############
############### "BRbrrier", 

data_domain_BRbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "BRbrrier"))


data_domain_BRbrrier$quotes = data_domain_BRbrrier$BRbrrier

data_domain_BRbrrier$construct = rep("BR-", times = nrow(data_domain_BRbrrier))

data_domain_BRbrrier = select(data_domain_BRbrrier, -c("BRbrrier"))


###############
############### "BaCapbrrier",

data_domain_BaCapbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "BaCapbrrier"))


data_domain_BaCapbrrier$quotes = data_domain_BaCapbrrier$BaCapbrrier

data_domain_BaCapbrrier$construct = rep("BaCap-", times = nrow(data_domain_BaCapbrrier))

data_domain_BaCapbrrier = select(data_domain_BaCapbrrier, -c("BaCapbrrier"))


###############
############### "BaConbrrier",

data_domain_BaConbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "BaConbrrier"))


data_domain_BaConbrrier$quotes = data_domain_BaConbrrier$BaConbrrier

data_domain_BaConbrrier$construct = rep("BaCon-", times = nrow(data_domain_BaConbrrier))

data_domain_BaConbrrier = select(data_domain_BaConbrrier, -c("BaConbrrier"))


###############
############### "ECRbrrier",
###############
data_domain_ECRbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "ECRbrrier"))


data_domain_ECRbrrier$quotes = data_domain_ECRbrrier$ECRbrrier

data_domain_ECRbrrier$construct = rep("ECR-", times = nrow(data_domain_ECRbrrier))

data_domain_ECRbrrier = select(data_domain_ECRbrrier, -c("ECRbrrier"))


############### "Emotionbrrier",

data_domain_Emotionbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Emotionbrrier"))


data_domain_Emotionbrrier$quotes = data_domain_Emotionbrrier$Emotionbrrier

data_domain_Emotionbrrier$construct = rep("Emotion-", times = nrow(data_domain_Emotionbrrier))

data_domain_Emotionbrrier = select(data_domain_Emotionbrrier, -c("Emotionbrrier"))


###############
############### "Goalsbrrier",

data_domain_Goalsbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Goalsbrrier"))


data_domain_Goalsbrrier$quotes = data_domain_Goalsbrrier$Goalsbrrier

data_domain_Goalsbrrier$construct = rep("Goals-", times = nrow(data_domain_Goalsbrrier))

data_domain_Goalsbrrier = select(data_domain_Goalsbrrier, -c("Goalsbrrier"))

###############
############### "Intentionbrrier",

data_domain_Intentionbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Intentionbrrier"))


data_domain_Intentionbrrier$quotes = data_domain_Intentionbrrier$Intentionbrrier

data_domain_Intentionbrrier$construct = rep("Intention-", times = nrow(data_domain_Intentionbrrier))

data_domain_Intentionbrrier = select(data_domain_Intentionbrrier, -c("Intentionbrrier"))


###############
############### "Knowledgebrrier",

data_domain_Knowledgebrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Knowledgebrrier"))


data_domain_Knowledgebrrier$quotes = data_domain_Knowledgebrrier$Knowledgebrrier

data_domain_Knowledgebrrier$construct = rep("Knowledge-", times = nrow(data_domain_Knowledgebrrier))

data_domain_Knowledgebrrier = select(data_domain_Knowledgebrrier, -c("Knowledgebrrier"))


############### "MADPbrrier",

data_domain_MADPbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "MADPbrrier"))


data_domain_MADPbrrier$quotes = data_domain_MADPbrrier$MADPbrrier

data_domain_MADPbrrier$construct = rep("MADP-", times = nrow(data_domain_MADPbrrier))

data_domain_MADPbrrier = select(data_domain_MADPbrrier, -c("MADPbrrier"))


###############
############### "Optimismbrrier",
###############

data_domain_Optimismbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Optimismbrrier"))


data_domain_Optimismbrrier$quotes = data_domain_Optimismbrrier$Optimismbrrier

data_domain_Optimismbrrier$construct = rep("Optimism-", times = nrow(data_domain_Optimismbrrier))

data_domain_Optimismbrrier = select(data_domain_Optimismbrrier, -c("Optimismbrrier"))


############### "Reinforcementbrrier",
###############

data_domain_Reinforcementbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Reinforcementbrrier"))


data_domain_Reinforcementbrrier$quotes = data_domain_Reinforcementbrrier$Reinforcementbrrier

data_domain_Reinforcementbrrier$construct = rep("Reinforcement-", times = nrow(data_domain_Reinforcementbrrier))

data_domain_Reinforcementbrrier = select(data_domain_Reinforcementbrrier, -c("Reinforcementbrrier"))


############### "SIbrrier",

data_domain_SIbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "SIbrrier"))


data_domain_SIbrrier$quotes = data_domain_SIbrrier$SIbrrier

data_domain_SIbrrier$construct = rep("SI-", times = nrow(data_domain_SIbrrier))

data_domain_SIbrrier = select(data_domain_SIbrrier, -c("SIbrrier"))


###############
############### "SPRbrrier",
###############

data_domain_SPRbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "SPRbrrier"))


data_domain_SPRbrrier$quotes = data_domain_SPRbrrier$SPRbrrier

data_domain_SPRbrrier$construct = rep("SPR-", times = nrow(data_domain_SPRbrrier))

data_domain_SPRbrrier = select(data_domain_SPRbrrier, -c("SPRbrrier"))


############### "Skillsbrrier"))

data_domain_Skillsbrrier = select(data_domain, c("Cases", 
                                             "human", 
                                             "PA_status",  
                                             "Skillsbrrier"))


data_domain_Skillsbrrier$quotes = data_domain_Skillsbrrier$Skillsbrrier

data_domain_Skillsbrrier$construct = rep("Skills-", times = nrow(data_domain_Skillsbrrier))

data_domain_Skillsbrrier = select(data_domain_Skillsbrrier, -c("Skillsbrrier"))




###########
###########


data_per_domain_per_participant = rbind(data_domain_BRenblr, 
                                        data_domain_BaCapenblr, 
                                        data_domain_BaConenblr, 
                                        data_domain_ECRenblr, 
                                        data_domain_Emotionenblr, 
                                        data_domain_Goalsenblr, 
                                        data_domain_Intentionenblr, 
                                        data_domain_Knowledgeenblr, 
                                        data_domain_MADPenblr, 
                                        data_domain_Optimismenblr, 
                                        data_domain_Reinforcementenblr, 
                                        data_domain_SIenblr, 
                                        data_domain_X.SPRenblr ,
                                        data_domain_Skillsenblr,
                                        
                                        data_domain_BRbrrier, 
                                        data_domain_BaCapbrrier, 
                                        data_domain_BaConbrrier, 
                                        data_domain_ECRbrrier, 
                                        data_domain_Emotionbrrier,
                                        data_domain_Goalsbrrier,
                                        data_domain_Intentionbrrier, 
                                        data_domain_Knowledgebrrier, 
                                        data_domain_MADPbrrier, 
                                        data_domain_Optimismbrrier, 
                                        data_domain_Reinforcementbrrier, 
                                        data_domain_SIbrrier, 
                                        data_domain_SPRbrrier, 
                                        data_domain_Skillsbrrier)


write.csv(data_per_domain_per_participant, file = paste(DATA_ROOT, "data_per_domain_per_participant.csv", sep =""))

          
# 
# 
# Enablers_plot = ggplot(data_domain, aes(y= reorder(construct, +quotes), x = quotes)) +
#   geom_violin(aes(fill = PA_status), position = "dodge") + 
#   facet_wrap(~human, nrow=1)
# 
# 
# Enablers_plot = Enablers_plot + scale_y_discrete(labels=c("BR+" = "Behavioural regulation", 
#                                                           "BaCap+" = "Beliefs about capabilities", 
#                                                           "BaCon+" = "Beliefs about consequences", 
#                                                           "ECR+" = "Environmental context and resources", 
#                                                           "Emotion+" = "Emotion", 
#                                                           "Goals+" = "Goals", 
#                                                           "Intention+" = "Intentions", 
#                                                           "Knowledge+" = "Knowledge", 
#                                                           "MADP+" = "Memory, attention, and decision processes", 
#                                                           "Optimism+" = "Optimism", 
#                                                           "Reinforcement+" = "Reinforcement", 
#                                                           "SI+" = "Social influences", 
#                                                           "\nSPR+" = "Social, professional role and identity", 
#                                                           "Skills+" = "Skills"
# )) 
# 
# 
# # changing palette 
# Enablers_plot = Enablers_plot + scale_fill_brewer(palette = "Set1")+ scale_color_brewer(palette = "Set1")
# 
# #labeling axis
# Enablers_plot = Enablers_plot + ggtitle("Enablers") + xlab("quotes, k") + ylab("Domain")
# 
# #labeling legend
# Enablers_plot = Enablers_plot + guides(fill=guide_legend(title=""))
