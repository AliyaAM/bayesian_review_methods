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
data_per_domain_per_case = read.csv(paste(DATA_ROOT, "data_quote_percentage.csv", sep =""))




############## plotting enablers ############## 
############## plotting enablers ############## 
############## plotting enablers ############## 
############## plotting enablers ############## 
############## plotting enablers ############## 

enablers_data = subset(data_per_domain_per_case, valence == "Enabler")


enablers_data$construct_name = case_when(enablers_data$construct == "BR+" ~ "Behavioural regulation", 
                                         enablers_data$construct ==  "BaCap+" ~ "Beliefs about capabilities", 
                                         enablers_data$construct ==  "BaCon+" ~ "Beliefs about consequences", 
                                         enablers_data$construct ==   "ECR+" ~ "Environmental context and resources", 
                                         enablers_data$construct ==  "Emotion+"  ~ "Emotion", 
                                         enablers_data$construct ==  "Goals+" ~ "Goals", 
                                         enablers_data$construct ==  "Intention+" ~ "Intentions", 
                                         enablers_data$construct == "Knowledge+" ~ "Knowledge", 
                                         enablers_data$construct ==  "MADP+" ~ "Memory, attention, and decision processes", 
                                         enablers_data$construct ==  "Optimism+" ~ "Optimism", 
                                         enablers_data$construct == "Reinforcement+" ~ "Reinforcement", 
                                         enablers_data$construct ==  "SI+" ~ "Social influences", 
                                         enablers_data$construct == "SPR+" ~ "Social, professional role and identity", 
                                         enablers_data$construct ==  "Skills+" ~ "Skills") 

############## 
############## t-test 

enablers_data_human = subset(enablers_data, human == "Human participants")
enablers_ChatGPT = subset(enablers_data, human == "Chat GPT characters")

# # human
compare_means(percent_quotes ~ PA_status,  data = enablers_data_human,
              ref.group = "0", method = "t.test")

# # ChatGPT
compare_means(percent_quotes ~ PA_status,  data = enablers_ChatGPT,
              ref.group = "0", method = "t.test")



enablers_data$PA_status_factor = case_when(enablers_data$PA_status == 1 ~ "Active", 
                                           enablers_data$PA_status == 0 ~ "Sedentary")



############## 
############## plotting enablers: 

wrap_text <- function(x, chars = 10) {
   x <- gsub("_", " ", x)
   stringr::str_wrap(x, chars)
}


 Enablers_plot = ggplot(enablers_data, aes(y= human, x = percent_quotes, fill = PA_status_factor)) +
   geom_violin() +
   facet_wrap(~reorder(construct_name, -quotes), ncol= 5, shrink = TRUE, labeller = as_labeller(wrap_text))
 
 Enablers_plot = Enablers_plot +  scale_x_continuous(limits=c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30))

 # changing palette 
 Enablers_plot = Enablers_plot + scale_fill_brewer(palette = "Set1")+ scale_color_brewer(palette = "Set1")
 
 #labeling axis
 Enablers_plot = Enablers_plot + ggtitle("Enablers") + xlab("% of quotes per case, k") + ylab("")
 
 #labeling legend
 Enablers_plot = Enablers_plot + guides(fill=guide_legend(title=""))
 
 
 Enablers_plot = Enablers_plot + theme(panel.grid.minor = element_blank(), #removes minor grid lines
                                       panel.grid.major = element_blank(),
                                       strip.text = element_text(size=9)) #change the font of the facets titles eg., Behavioural regulation
 
 

 # Enablers_plot +  scale_x_discrete(limits=c("1", "2","3","4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "20", "21", "22", "23", "24", "25"))
 # 
 # 
 # Enablers_plot = Enablers_plot + stat_summary(fun.data=mean_sdl, 
 #                  geom="pointrange")
                  



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
# Enablers_plot = Enablers_plot + ggtitle("Enablers") + xlab("percent_quotes, k") + ylab("Domain")
# 
# #labeling legend
# Enablers_plot = Enablers_plot + guides(fill=guide_legend(title=""))
