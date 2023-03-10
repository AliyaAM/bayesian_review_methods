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








########### perform t-test for each category
########### BR (Enabler)

t_test_results = compare_means(percent_quotes ~ PA_status_factor, data = enablers_data, 
                               group.by = c("construct_name", "human"))




############## 
############## plotting enablers: 

wrap_text <- function(x, chars = 10) {
   x <- gsub("_", " ", x)
   stringr::str_wrap(x, chars)
}


 violin_enablers_plot = ggplot(enablers_data, aes(y=human_factor, x = percent_quotes, fill = PA_status_factor)) +
   geom_violin()  +
    facet_wrap(~reorder(construct_name, -quotes), ncol= 5, shrink = TRUE, labeller = as_labeller(wrap_text))

              
 
 violin_enablers_plot = violin_enablers_plot +  scale_x_continuous(limits=c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30))

 # changing palette 
 violin_enablers_plot = violin_enablers_plot + scale_fill_brewer(palette = "Set1")+ scale_color_brewer(palette = "Set1")
 
 #labeling axis
 violin_enablers_plot = violin_enablers_plot + ggtitle("Enablers") + xlab("% of quotes per case, k") + xlab("")
 
 #labeling legend
 violin_enablers_plot = violin_enablers_plot + guides(fill=guide_legend(title=""))
 
 
 violin_enablers_plot = violin_enablers_plot + theme(panel.grid.minor = element_blank(), #removes minor grid lines
                                       panel.grid.major = element_blank(),
                                       strip.text = element_text(size=9)) #change the font of the facets titles eg., Behavioural regulation

 
 
 
 violin_enablers_plot 
 print(violin_enablers_plot)
 
 
 
 ggsave(file = paste(OUTPUT_ROOT, "/violin_enablers_plot.pdf",  sep=""), violin_enablers_plot, 
        #width=6, height=2, units="in", 
        scale=1)
 
 
 ############## plotting enablers ############## 
 ############## plotting enablers ############## 
 ############## plotting enablers ############## 
 ############## plotting enablers ############## 
 ############## plotting enablers ############## 
 
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
 
 
 
 
 
 
 
 
 ########### perform t-test for each category
 ########### BR (Enabler)
 
 t_test_results_barrier = compare_means(percent_quotes ~ PA_status_factor, data = barriers_data, 
                                group.by = c("construct_name", "human"))
 
 
 
 
 ############## 
 ############## plotting barriers: 
 
 wrap_text <- function(x, chars = 10) {
    x <- gsub("_", " ", x)
    stringr::str_wrap(x, chars)
 }
 
 
 violin_barriers_plot = ggplot(barriers_data, aes(y=human_factor, x = percent_quotes, fill = PA_status_factor)) +
    geom_violin()  +
    facet_wrap(~reorder(construct_name, -quotes), ncol= 5, shrink = TRUE, labeller = as_labeller(wrap_text))
 
 
 
 violin_barriers_plot = violin_barriers_plot +  scale_x_continuous(limits=c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30))
 
 # changing palette 
 violin_barriers_plot = violin_barriers_plot + scale_fill_brewer(palette = "Set1")+ scale_color_brewer(palette = "Set1")
 
 #labeling axis
 violin_barriers_plot = violin_barriers_plot + ggtitle("Barriers") + xlab("% of quotes per case, k") + xlab("")
 
 #labeling legend
 violin_barriers_plot = violin_barriers_plot + guides(fill=guide_legend(title=""))
 
 
 violin_barriers_plot = violin_barriers_plot + theme(panel.grid.minor = element_blank(), #removes minor grid lines
                                       panel.grid.major = element_blank(),
                                       strip.text = element_text(size=9)) #change the font of the facets titles eg., Behavioural regulation
 
 
 
 
 violin_barriers_plot 
 print(violin_barriers_plot)
 
 
 ggsave(file = paste(OUTPUT_ROOT, "/violin_barriers_plot.pdf",  sep=""), violin_barriers_plot, 
        #width=6, height=2, units="in", 
        scale=1)
 
 
 
