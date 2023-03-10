

library(ggplot2)
library(dplyr)
library(survminer)
library(ggpubr)
library(rstatix)

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



enablers_data$PA_status_factor = case_when(enablers_data$PA_status == 1 ~ "Active", 
                                           enablers_data$PA_status == 0 ~ "Sedentary")


enablers_data$PA_status_factor = as.factor(enablers_data$PA_status_factor)

enablers_data$human_factor = case_when(enablers_data$human == "Human participants" ~ "Human", 
                                       enablers_data$human == "Chat GPT characters" ~ "ChatGPT")


enablers_data$human_factor = as.factor(enablers_data$human_factor)


wrap_text <- function(x, chars = 10) {
  x <- gsub("_", " ", x)
  stringr::str_wrap(x, chars)
}


stat.test <- enablers_data %>%
  group_by(human_factor, construct_name) %>%
  t_test(percent_quotes ~ PA_status_factor) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test 


stat.test <- stat.test %>% add_xy_position(x = "PA_status_factor")
enablers_data$percent_quotes
enablers_data$PA_status_factor

plot = ggboxplot(enablers_data, x = "PA_status_factor", y = "percent_quotes", fill = "PA_status_factor", 
  facet = c("human_factor", "construct_name"), short.panel.labs = FALSE) +
  stat_pvalue_manual(stat.test, hide.ns = TRUE)


Enablers_plot = plot +  scale_y_continuous(limits=c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30))
Enablers_plot = plot +  scale_x_discrete( breaks = "")
# changing palette 
#Enablers_plot = Enablers_plot + scale_fill_brewer(palette = "Set1")+ scale_color_brewer(palette = "Set1")

#labeling axis
Enablers_plot = Enablers_plot + ggtitle("Enablers") + ylab("% of quotes per case, k") + xlab("")

#labeling legend
Enablers_plot = Enablers_plot + guides(fill=guide_legend(title=""))

Enablers_plot = Enablers_plot + scale_fill_brewer(palette = "Set1")+ scale_color_brewer(palette = "Set1")

# Enablers_plot = Enablers_plot + theme(panel.grid.minor = element_blank(), #removes minor grid lines
#                                       panel.grid.major = element_blank(),
#                                       strip.text = element_text(size=9)) 



