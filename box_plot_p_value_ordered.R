

library(ggplot2)
library(dplyr)
library(survminer)
library(ggpubr)
library(rstatix)

########### DIRECTORY

#directory = "/Users/aliyaamirova/"
directory = "/Users/aliya/my_docs/"
directory = "/Users/k2147340/OneDrive - King's College London/Documents/"


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


################ 
################ 
################ 
################ 
################ 
################ Add t-test between Active/sedentary per domains: 


stat.test <- enablers_data %>%
  group_by(human_factor, construct_name) %>%
  t_test(percent_quotes ~ PA_status_factor) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test 


stat.test <- stat.test %>% add_xy_position(x = "PA_status_factor")
enablers_data$percent_quotes
enablers_data$PA_status_factor

################ 
################ 
################ 
################ 
################  plot Enablers: 

plot = ggboxplot(enablers_data, x = "PA_status_factor", y = "percent_quotes", fill = "PA_status_factor") 
plot = plot + stat_pvalue_manual(stat.test, hide.ns = TRUE)        

# adding ticks of the axis
plot = plot +  scale_y_continuous(limits=c(0, 35), breaks = c(0, 5, 10, 15, 20, 25, 30, 35))
plot = plot +  scale_x_discrete( breaks = "")

#labeling axis
plot = plot + ggtitle("Enablers") + ylab("% of quotes per case, k") + xlab("")

#labeling legend
plot = plot + guides(fill=guide_legend(title=""))

# changing palette 
plot = plot + scale_fill_brewer(palette = "Set1")+ scale_color_brewer(palette = "Set1")



# arrange in a grid and also make sure the labels' text is wrapped 

wrap_text <- function(x, chars = 5) {
  x <- gsub("_", "", x)
  stringr::str_wrap(x, chars)
}



Enablers_plot = plot + facet_grid(vars(human_factor), 
                                  vars(factor(construct_name, levels = c("Beliefs about consequences", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Behavioural regulation", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Social influences", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Goals",  # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Environmental context and resources", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Reinforcement", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Optimism", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Social, professional role and identity", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Emotion", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Beliefs about capabilities", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Knowledge", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Skills", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Intentions", # order the grid columns by percent quote (from largest to smallest)
                                                                                              "Memory, attention, and decision processes"))), # order the grid columns by percent quote (from largest to smallest)
                                 
                                  labeller = as_labeller(wrap_text), as.table = TRUE)


                                 
Enablers_plot = Enablers_plot + theme(strip.text = element_text(size=5.4))

print(Enablers_plot)                    

 ggsave(file = paste(OUTPUT_ROOT, "/Enablers_plot.pdf",  sep=""), Enablers_plot, 
        #width=6, height=2, units="in", 
        scale=1)

 ggsave(file = paste(OUTPUT_ROOT, "/Enablers_plot.eps",  sep=""), Enablers_plot, 
        #width=6, height=2, units="in", 
        scale=1)
 


###########
###########
###########



############## plotting barriers ############## 
############## plotting barriers ############## 
############## plotting barriers ############## 
############## plotting barriers ############## 
############## plotting barriers ############## 

barriers_data = subset(data_per_domain_per_case, valence == "Barrier")


barriers_data$construct_name = case_when(barriers_data$construct == "BR-" ~ "Behavioural regulation", 
                                         barriers_data$construct ==  "BaCap-" ~ "Beliefs about capabilities", 
                                         barriers_data$construct ==  "BaCon-" ~ "Beliefs about consequences", 
                                         barriers_data$construct ==   "ECR-" ~ "Environmental context and resources", 
                                         barriers_data$construct ==  "Emotion-"  ~ "Emotion", 
                                         barriers_data$construct ==  "Goals-" ~ "Goals", 
                                         barriers_data$construct ==  "Intention-" ~ "Intentions", 
                                         barriers_data$construct == "Knowledge-" ~ "Knowledge", 
                                         barriers_data$construct ==  "MADP-" ~ "Memory, attention, and decision processes", 
                                         barriers_data$construct ==  "Optimism-" ~ "Optimism", 
                                         barriers_data$construct == "Reinforcement-" ~ "Reinforcement", 
                                         barriers_data$construct ==  "SI-" ~ "Social influences", 
                                         barriers_data$construct == "SPR-" ~ "Social, professional role and identity", 
                                         barriers_data$construct ==  "Skills-" ~ "Skills") 



barriers_data$PA_status_factor = case_when(barriers_data$PA_status == 1 ~ "Active", 
                                           barriers_data$PA_status == 0 ~ "Sedentary")


barriers_data$PA_status_factor = as.factor(barriers_data$PA_status_factor)

barriers_data$human_factor = case_when(barriers_data$human == "Human participants" ~ "Human", 
                                       barriers_data$human == "Chat GPT characters" ~ "ChatGPT")


barriers_data$human_factor = as.factor(barriers_data$human_factor)


################ 
################ 
################ 
################ 
################ 
################ Add t-test between Active/sedentary per domains: 


stat_test_barrier <- barriers_data %>%
  group_by(human_factor, construct_name) %>%
  t_test(percent_quotes ~ PA_status_factor) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat_test_barrier 


stat_test_barrier <- stat_test_barrier %>% add_xy_position(x = "PA_status_factor")
barriers_data$percent_quotes
barriers_data$PA_status_factor

################ 
################ 
################ 
################ 
################  plot barriers: 

plot_barrier = ggboxplot(barriers_data, x = "PA_status_factor", y = "percent_quotes", fill = "PA_status_factor") 
plot_barrier = plot_barrier + stat_pvalue_manual(stat_test_barrier, hide.ns = TRUE)        

# adding ticks of the axis
plot_barrier = plot_barrier +  scale_y_continuous(limits=c(0, 50), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50))
plot_barrier = plot_barrier +  scale_x_discrete( breaks = "")

#labeling axis
plot_barrier = plot_barrier + ggtitle("Barriers") + ylab("% of quotes per case, k") + xlab("")

#labeling legend
plot_barrier = plot_barrier + guides(fill=guide_legend(title=""))

# changing palette 
plot_barrier = plot_barrier + scale_fill_brewer(palette = "Set1")+ scale_color_brewer(palette = "Set1")



# arrange in a grid and also make sure the labels' text is wrapped 

wrap_text <- function(x, chars = 5) {
  x <- gsub("_", "", x)
  stringr::str_wrap(x, chars)
}



Barriers_plot = plot_barrier + facet_grid(vars(human_factor), 
                                  vars(factor(construct_name, levels = c("Beliefs about capabilities", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Beliefs about consequences", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Environmental context and resources", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Goals",  # order the grid columns by percent quote (from largest to smallest)
                                                                         "Memory, attention, and decision processes", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Emotion", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Skills", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Behavioural regulation", # order the grid columns by percent quote (from largest to smallest) 
                                                                         "Social, professional role and identity", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Social influences", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Optimism", # order the grid columns by percent quote (from largest to smallest) 
                                                                         "Knowledge", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Reinforcement", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Intentions"))), # order the grid columns by percent quote (from largest to smallest)
                                                  
                                  
                                  labeller = as_labeller(wrap_text), as.table = TRUE)





Barriers_plot = Barriers_plot + theme(strip.text = element_text(size=5.4))

print(Barriers_plot)

ggsave(file = paste(OUTPUT_ROOT, "/Barriers_plot.pdf",  sep=""), Barriers_plot, 
       #width=6, height=2, units="in", 
       scale=1)


ggsave(file = paste(OUTPUT_ROOT, "/Barriers_plot.eps",  sep=""), Barriers_plot, 
       #width=6, height=2, units="in", 
       scale=1)
#######
#######
#######
#######

################ Add t-test between HUMAN PARTICIPANTS AND CHATGPT per domains: 


stat.test_HumanvsChatGPT <- enablers_data %>%
  group_by(PA_status_factor, construct_name) %>%
  t_test(percent_quotes ~ human_factor) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test_HumanvsChatGPT 


stat.test_HumanvsChatGPT <- stat.test_HumanvsChatGPT %>% add_xy_position(x = "human_factor")

stat.test_HumanvsChatGPT 

################ 
################ 
################ 
################ 



plot_HumanvsChatGPT = ggboxplot(enablers_data, x = "human_factor", y = "percent_quotes", fill = "human_factor") 
plot_HumanvsChatGPT = plot_HumanvsChatGPT + stat_pvalue_manual(stat.test_HumanvsChatGPT, hide.ns = TRUE)        

# adding ticks of the axis
plot_HumanvsChatGPT = plot_HumanvsChatGPT +  scale_y_continuous(limits=c(0, 35), breaks = c(0, 5, 10, 15, 20, 25, 30, 35))
plot_HumanvsChatGPT = plot_HumanvsChatGPT +  scale_x_discrete( breaks = "")

#labeling axis
plot_HumanvsChatGPT = plot_HumanvsChatGPT + ggtitle("Enablers") + ylab("% of quotes per case, k") + xlab("")

#labeling legend
plot_HumanvsChatGPT = plot_HumanvsChatGPT + guides(fill=guide_legend(title=""))

# changing palette 
plot_HumanvsChatGPT = plot_HumanvsChatGPT + scale_fill_brewer(palette = "Set2")+ scale_color_brewer(palette = "Set2")



# arrange in a grid and also make sure the labels' text is wrapped 

wrap_text <- function(x, chars = 5) {
  x <- gsub("_", "", x)
  stringr::str_wrap(x, chars)
}



Enablers_plot_HumanvsChatGPT = plot_HumanvsChatGPT + facet_grid(vars(PA_status_factor), 
                                  vars(factor(construct_name, levels = c("Beliefs about consequences", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Behavioural regulation", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Social influences", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Goals",  # order the grid columns by percent quote (from largest to smallest)
                                                                         "Environmental context and resources", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Reinforcement", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Optimism", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Social, professional role and identity", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Emotion", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Beliefs about capabilities", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Knowledge", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Skills", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Intentions", # order the grid columns by percent quote (from largest to smallest)
                                                                         "Memory, attention, and decision processes"))), # order the grid columns by percent quote (from largest to smallest)
                                  
                                  labeller = as_labeller(wrap_text), as.table = TRUE)



Enablers_plot_HumanvsChatGPT = Enablers_plot_HumanvsChatGPT + theme(strip.text = element_text(size=5.4))

print(Enablers_plot_HumanvsChatGPT)                    

ggsave(file = paste(OUTPUT_ROOT, "/Enablers_plot_HumanvsChatGPT.pdf",  sep=""), Enablers_plot_HumanvsChatGPT, 
       #width=6, height=2, units="in", 
       scale=1)

ggsave(file = paste(OUTPUT_ROOT, "/Enablers_plot_HumanvsChatGPT.eps",  sep=""), Enablers_plot_HumanvsChatGPT, 
       #width=6, height=2, units="in", 
       scale=1)

################ 
################ 
################ 
################ 
################ 
################ Add t-test between Human and ChatGPT per domains (BARRIERS): 


stat.test_HumanvsChatGPT_barrier <- barriers_data %>%
  group_by(PA_status_factor, construct_name) %>%
  t_test(percent_quotes ~ human_factor) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test_HumanvsChatGPT_barrier 


stat.test_HumanvsChatGPT_barrier <- stat.test_HumanvsChatGPT_barrier %>% add_xy_position(x = "human_factor")
barriers_data$percent_quotes
barriers_data$PA_status_factor

################ 
################ 
################ 
################ 
################  plot barriers: 

plot_HumanvsChatGPT_barrier= ggboxplot(barriers_data, x = "human_factor", y = "percent_quotes", fill = "human_factor") 
plot_HumanvsChatGPT_barrier = plot_HumanvsChatGPT_barrier + stat_pvalue_manual(stat.test_HumanvsChatGPT_barrier, hide.ns = TRUE)        

# adding ticks of the axis
plot_HumanvsChatGPT_barrier = plot_HumanvsChatGPT_barrier +  scale_y_continuous(limits=c(0, 50), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50))
plot_HumanvsChatGPT_barrier = plot_HumanvsChatGPT_barrier +  scale_x_discrete( breaks = "")

#labeling axis
plot_HumanvsChatGPT_barrier = plot_HumanvsChatGPT_barrier + ggtitle("Barriers") + ylab("% of quotes per case, k") + xlab("")

#labeling legend
plot_HumanvsChatGPT_barrier = plot_HumanvsChatGPT_barrier + guides(fill=guide_legend(title=""))

# changing palette 
plot_HumanvsChatGPT_barrier = plot_HumanvsChatGPT_barrier + scale_fill_brewer(palette = "Set2")+ scale_color_brewer(palette = "Set2")



# arrange in a grid and also make sure the labels' text is wrapped 

wrap_text <- function(x, chars = 5) {
  x <- gsub("_", "", x)
  stringr::str_wrap(x, chars)
}



Barriers_plot_HumanvsChatGPT = plot_HumanvsChatGPT_barrier + facet_grid(vars(PA_status_factor), 
                                          vars(factor(construct_name, levels = c("Beliefs about capabilities", # order the grid columns by percent quote (from largest to smallest)
                                                                                 "Beliefs about consequences", # order the grid columns by percent quote (from largest to smallest)
                                                                                 "Environmental context and resources", # order the grid columns by percent quote (from largest to smallest)
                                                                                 "Goals",  # order the grid columns by percent quote (from largest to smallest)
                                                                                 "Memory, attention, and decision processes", # order the grid columns by percent quote (from largest to smallest)
                                                                                 "Emotion", # order the grid columns by percent quote (from largest to smallest)
                                                                                 "Skills", # order the grid columns by percent quote (from largest to smallest)
                                                                                 "Behavioural regulation", # order the grid columns by percent quote (from largest to smallest) 
                                                                                 "Social, professional role and identity", # order the grid columns by percent quote (from largest to smallest)
                                                                                 "Social influences", # order the grid columns by percent quote (from largest to smallest)
                                                                                 "Optimism", # order the grid columns by percent quote (from largest to smallest) 
                                                                                 "Knowledge", # order the grid columns by percent quote (from largest to smallest)
                                                                                 "Reinforcement", # order the grid columns by percent quote (from largest to smallest)
                                                                                 "Intentions"))), # order the grid columns by percent quote (from largest to smallest)
                                          
                                          
                                          labeller = as_labeller(wrap_text), as.table = TRUE)





Barriers_plot_HumanvsChatGPT = Barriers_plot_HumanvsChatGPT + theme(strip.text = element_text(size=5.4))

print(Barriers_plot_HumanvsChatGPT)

ggsave(file = paste(OUTPUT_ROOT, "/Barriers_plot_HumanvsChatGPT.pdf",  sep=""), Barriers_plot_HumanvsChatGPT, 
       #width=6, height=2, units="in", 
       scale=1)

ggsave(file = paste(OUTPUT_ROOT, "/Barriers_plot_HumanvsChatGPT.eps",  sep=""), Barriers_plot_HumanvsChatGPT, 
       #width=6, height=2, units="in", 
       scale=1)

#######
#######
#######
#######

