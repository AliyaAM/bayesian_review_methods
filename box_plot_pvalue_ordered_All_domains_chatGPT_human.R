


library(ggplot2)
library(dplyr)
library(survminer)
library(ggpubr)
library(rstatix)

########### DIRECTORY

#directory = "/Users/aliyaamirova/"
directory = "/Users/aliya/my_docs/"
#directory = "/Users/k2147340/OneDrive - King's College London/Documents/"

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


enablers_data$construct_name = case_when(enablers_data$construct == "BR+" ~ "BR", 
                                         enablers_data$construct ==  "BaCap+" ~ "BCap", 
                                         enablers_data$construct ==  "BaCon+" ~ "BCon", 
                                         enablers_data$construct ==   "ECR+" ~ "ECR", 
                                         enablers_data$construct ==  "Emotion+"  ~ "Emtns", 
                                         enablers_data$construct ==  "Goals+" ~ "Gs", 
                                         enablers_data$construct ==  "Intention+" ~ "Is", 
                                         enablers_data$construct == "Knowledge+" ~ "Knls", 
                                         enablers_data$construct ==  "MADP+" ~ "MADP", 
                                         enablers_data$construct ==  "Optimism+" ~ "Optm", 
                                         enablers_data$construct == "Reinforcement+" ~ "Rnfrt", 
                                         enablers_data$construct ==  "SI+" ~ "SI", 
                                         enablers_data$construct == "SPR+" ~ "SPR", 
                                         enablers_data$construct ==  "Skills+" ~ "Skls") 


enablers_data$PA_status_factor = case_when(enablers_data$PA_status == 1 ~ "Active", 
                                           enablers_data$PA_status == 0 ~ "Sedentary")


enablers_data$PA_status_factor = as.factor(enablers_data$PA_status_factor)

enablers_data$human_factor = case_when(enablers_data$human == "Human participants" ~ "Human", 
                                       enablers_data$human == "Chat GPT characters" ~ "LLM")


enablers_data$human_factor = as.factor(enablers_data$human_factor)


#######
#######
#######
#######



############## plotting enablers ############## 
############## plotting enablers ############## 
############## plotting enablers ############## 
############## plotting enablers ############## 
############## plotting enablers ############## 

barriers_data = subset(data_per_domain_per_case, valence == "Barrier")


barriers_data$construct_name = case_when(barriers_data$construct == "BR-" ~ "BR", 
                                         barriers_data$construct ==  "BaCap-" ~ "BCap", 
                                         barriers_data$construct ==  "BaCon-" ~ "BCon", 
                                         barriers_data$construct ==   "ECR-" ~ "ECR", 
                                         barriers_data$construct ==  "Emotion-"  ~ "Emtns", 
                                         barriers_data$construct ==  "Goals-" ~ "Gs", 
                                         barriers_data$construct ==  "Intention-" ~ "Is", 
                                         barriers_data$construct == "Knowledge-" ~ "Knls", 
                                         barriers_data$construct ==  "MADP-" ~ "MADP", 
                                         barriers_data$construct ==  "Optimism-" ~ "Optm", 
                                         barriers_data$construct == "Reinforcement-" ~ "Rnfrt", 
                                         barriers_data$construct ==  "SI-" ~ "SI", 
                                         barriers_data$construct == "SPR-" ~ "SPR", 
                                         barriers_data$construct ==  "Skills-" ~ "Skls") 




barriers_data$PA_status_factor = case_when(barriers_data$PA_status == 1 ~ "Active", 
                                           barriers_data$PA_status == 0 ~ "Sedentary")


barriers_data$PA_status_factor = as.factor(barriers_data$PA_status_factor)

barriers_data$human_factor = case_when(barriers_data$human == "Human participants" ~ "Human", 
                                       barriers_data$human == "Chat GPT characters" ~ "LLM")


barriers_data$human_factor = as.factor(barriers_data$human_factor)


barriers_data$human_factor = as.factor(barriers_data$human_factor)


################ Add t-test between HUMAN PARTICIPANTS AND CHATGPT per domains: 


stat.test_HumanvsChatGPT <- enablers_data %>%
  group_by(construct_name) %>%
  t_test(percent_quotes ~ human_factor) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() 
stat.test_HumanvsChatGPT 


#stat.test_HumanvsChatGPT <- stat.test_HumanvsChatGPT %>% add_xy_position(x = "human_factor")

write.csv(stat.test_HumanvsChatGPT, file = paste(OUTPUT_ROOT, "stat.test_HumanvsChatGPT_enabler.csv", sep = ""))



summary_stats <- enablers_data %>%
  group_by(construct_name, human_factor) %>%
  summarise(
    mean_percent_quotes = mean(percent_quotes, na.rm = TRUE),
    sd_percent_quotes = sd(percent_quotes, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)

write.csv(summary_stats, file = paste(OUTPUT_ROOT, "summary_HumanvsChatGPT_enabler.csv", sep = ""))


################ 
################ 

################ Add t-test between HUMAN PARTICIPANTS AND CHATGPT per domains (grouped by activity status)


stat.test_HumanvsChatGPT__grouped_by_activity_status <- enablers_data %>%
  group_by(construct_name, PA_status_factor) %>%
  t_test(percent_quotes ~ human_factor) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() 
stat.test_HumanvsChatGPT__grouped_by_activity_status 

print(stat.test_HumanvsChatGPT__grouped_by_activity_status)


#stat.test_HumanvsChatGPT__grouped_by_activity_status <- stat.test_HumanvsChatGPT__grouped_by_activity_status %>% add_xy_position(x = "human_factor")

write.csv(stat.test_HumanvsChatGPT__grouped_by_activity_status, file = paste(OUTPUT_ROOT, "stat.test_HumanvsChatGPT_enabler_grouped_by_activity_status.csv", sep = ""))



summary_stats__grouped_by_activity_status <- enablers_data %>%
  group_by(construct_name, human_factor, PA_status_factor) %>%
  summarise(
    mean_percent_quotes = mean(percent_quotes, na.rm = TRUE),
    sd_percent_quotes = sd(percent_quotes, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats__grouped_by_activity_status)

write.csv(summary_stats__grouped_by_activity_status, file = paste(OUTPUT_ROOT, "summary_HumanvsChatGPT_enabler_grouped_by_activity_status.csv", sep = ""))

################ 
################ 



plot_HumanvsChatGPT = ggboxplot(enablers_data, x = "human_factor", y = "percent_quotes", fill = "human_factor") 
plot_HumanvsChatGPT = plot_HumanvsChatGPT + stat_pvalue_manual(stat.test_HumanvsChatGPT)        

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



Enablers_HumanvsChatGPT = plot_HumanvsChatGPT + facet_grid(cols = vars(factor(construct_name, levels = c("BCon", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "BR", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "SI", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "Gs",  # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "ECR", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "Rnfrt", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "Optm", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "SPR", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "Emtns", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "BCap", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "Knls", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "Skls", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "Is", # order the grid columns by percent quote (from largest to smallest)
                                                                                                       "MADP"))), # order the grid columns by percent quote (from largest to smallest)
                                                                
                                                                labeller = as_labeller(wrap_text), as.table = TRUE)+
  theme(strip.text.x = element_text(size = 9.5, face = "bold"), # Adjust size and face here
  axis.text.x = element_blank())




Enablers_HumanvsChatGPT = Enablers_HumanvsChatGPT + theme(strip.text = element_text(size=5.4))

print(Enablers_HumanvsChatGPT)                    

ggsave(file = paste(OUTPUT_ROOT, "/Enablers_HumanvsChatGPT.pdf",  sep=""), Enablers_HumanvsChatGPT, 
       #width=6, height=2, units="in", 
       scale=1)



################ 
################ 
################ 
################ 
################ 
################ Add t-test between Human and ChatGPT per domains (BARRIERS): 


stat.test_HumanvsChatGPT_barrier <- barriers_data %>%
  group_by(construct_name) %>%
  t_test(percent_quotes ~ human_factor) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test_HumanvsChatGPT_barrier 


#stat.test_HumanvsChatGPT_barrier <- stat.test_HumanvsChatGPT_barrier %>% add_xy_position(x = "human_factor")

write.csv(stat.test_HumanvsChatGPT_barrier, file = paste(OUTPUT_ROOT, "stat.test_HumanvsChatGPT_barrier.csv", sep = ""))

barriers_data$percent_quotes
barriers_data$PA_status_factor


summary_stats_barrier <- barriers_data %>%
  group_by(construct_name, human_factor) %>%
  summarise(
    mean_percent_quotes = mean(percent_quotes, na.rm = TRUE),
    sd_percent_quotes = sd(percent_quotes, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats_barrier)
write.csv(summary_stats_barrier, file = paste(OUTPUT_ROOT, "summary_HumanvsChatGPT_barrier.csv", sep = ""))


################ 
################ 

################ Add t-test between Human and ChatGPT per domains (BARRIERS) grouped by activity status: 


stat.test_HumanvsChatGPT_barrier_grouped_by_activity_status <- barriers_data %>%
  group_by(construct_name, PA_status_factor) %>%
  t_test(percent_quotes ~ human_factor) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test_HumanvsChatGPT_barrier_grouped_by_activity_status 


#stat.test_HumanvsChatGPT_barrier_grouped_by_activity_status <- stat.test_HumanvsChatGPT_barrier_grouped_by_activity_status %>% add_xy_position(x = "human_factor")

write.csv(stat.test_HumanvsChatGPT_barrier_grouped_by_activity_status, file = paste(OUTPUT_ROOT, "stat.test_HumanvsChatGPT_barrier_grouped_by_activity_status.csv", sep = ""))

barriers_data$percent_quotes
barriers_data$PA_status_factor


summary_stats_barrier_grouped_by_activity_status <- barriers_data %>%
  group_by(construct_name, human_factor, PA_status_factor) %>%
  summarise(
    mean_percent_quotes = mean(percent_quotes, na.rm = TRUE),
    sd_percent_quotes = sd(percent_quotes, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats_barrier_grouped_by_activity_status)
write.csv(summary_stats_barrier_grouped_by_activity_status, file = paste(OUTPUT_ROOT, "summary_stats_barrier_grouped_by_activity_status.csv", sep = ""))


################ 
################ 
################  plot barriers: 

plot_HumanvsChatGPT_barrier= ggboxplot(barriers_data, x = "human_factor", y = "percent_quotes", fill = "human_factor") 
plot_HumanvsChatGPT_barrier = plot_HumanvsChatGPT_barrier + stat_pvalue_manual(stat.test_HumanvsChatGPT_barrier)        

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



Barriers_HumanvsChatGPT = plot_HumanvsChatGPT_barrier + facet_grid(cols = vars(factor(construct_name, levels = c("BCap", # order the grid columns by percent quote (from largest to smallest)
                                                                                                               "BCon", # order the grid columns by percent quote (from largest to smallest)
                                                                                                               "ECR", # order the grid columns by percent quote (from largest to smallest)
                                                                                                               "Gs",  # order the grid columns by percent quote (from largest to smallest)
                                                                                                               "MADP", # order the grid columns by percent quote (from largest to smallest)
                                                                                                               "Emtns", # order the grid columns by percent quote (from largest to smallest)
                                                                                                               "Skls", # order the grid columns by percent quote (from largest to smallest)
                                                                                                               "BR", # order the grid columns by percent quote (from largest to smallest) 
                                                                                                               "SPR", # order the grid columns by percent quote (from largest to smallest)
                                                                                                               "SI", # order the grid columns by percent quote (from largest to smallest)
                                                                                                               "Optm", # order the grid columns by percent quote (from largest to smallest) 
                                                                                                               "Knls", # order the grid columns by percent quote (from largest to smallest)
                                                                                                               "Rnfrt", # order the grid columns by percent quote (from largest to smallest)
                                                                                                               "Is"))), # order the grid columns by percent quote (from largest to smallest)
                                                                        
                                                                        
labeller = as_labeller(wrap_text), as.table = TRUE)+
  theme(strip.text.x = element_text(size = 9.5, face = "bold"), # Adjust size and face here
        axis.text.x = element_blank())






Barriers_HumanvsChatGPT = Barriers_HumanvsChatGPT + theme(strip.text = element_text(size=5.4))

print(Barriers_HumanvsChatGPT)

ggsave(file = paste(OUTPUT_ROOT, "/Barriers_HumanvsChatGPT.pdf",  sep=""), Barriers_HumanvsChatGPT, 
       #width=6, height=2, units="in", 
       scale=1)


#######
#######
#######
#######