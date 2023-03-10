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


Enablers_plot = ggplot(data_domain, aes(y= reorder(construct, +quotes), x = quotes)) +
  geom_violin(aes(fill = PA_status), position = "dodge") + 
  facet_wrap(~human, nrow=1)


Enablers_plot = Enablers_plot + scale_y_discrete(labels=c("BR+" = "Behavioural regulation", 
                                                          "BaCap+" = "Beliefs about capabilities", 
                                                          "BaCon+" = "Beliefs about consequences", 
                                                          "ECR+" = "Environmental context and resources", 
                                                          "Emotion+" = "Emotion", 
                                                          "Goals+" = "Goals", 
                                                          "Intention+" = "Intentions", 
                                                          "Knowledge+" = "Knowledge", 
                                                          "MADP+" = "Memory, attention, and decision processes", 
                                                          "Optimism+" = "Optimism", 
                                                          "Reinforcement+" = "Reinforcement", 
                                                          "SI+" = "Social influences", 
                                                          "\nSPR+" = "Social, professional role and identity", 
                                                          "Skills+" = "Skills"
)) 


# changing palette 
Enablers_plot = Enablers_plot + scale_fill_brewer(palette = "Set1")+ scale_color_brewer(palette = "Set1")

#labeling axis
Enablers_plot = Enablers_plot + ggtitle("Enablers") + xlab("quotes, k") + ylab("Domain")

#labeling legend
Enablers_plot = Enablers_plot + guides(fill=guide_legend(title=""))
