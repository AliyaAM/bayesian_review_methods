

library(ggplot2)
library(survminer)
########### DIRECTORY

#directory = "/Users/aliyaamirova/"
directory = "/Users/aliya/my_docs/"

###########  source root 
SOURCE_ROOT = paste(directory, "proj/bayesian_review_methods/", sep = "")

###########  data root
DATA_ROOT = paste(directory, "proj/bayesian_review_methods/DATA/", sep = "")

########### Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/CHatGPTvsHuman/", sep = "")



#DATA: 
Active_inactive_Human = read.csv(paste(DATA_ROOT, "Active_inactive_Human_March06_FINAL.csv", sep =""))
Active_inactive_Human = subset(Active_inactive_Human, belief_statement !="Total")

Active_inactive_ChatGPT = read.csv(paste(DATA_ROOT, "Active_inactive_ChatGPT_March06_FINAL.csv", sep =""))
Active_inactive_ChatGPT = subset(Active_inactive_ChatGPT, belief_statement !="Total")





#list of constructs 
construct = c(Active_inactive_Human$construct, Active_inactive_Human$construct)
data_human = data.frame(construct)
data_human$quotes = c(Active_inactive_Human$active_k, Active_inactive_Human$sedentary_k)
active_status = rep("Active", times= nrow(Active_inactive_Human))
sedentary_status = rep("Sedentary", times= nrow(Active_inactive_Human))
data_human$PA_status = c(active_status, sedentary_status)
data_human$human = rep("Human participants", times= nrow(data_human))
data_human = data_human[complete.cases(data_human), ]

construct = c(Active_inactive_ChatGPT$construct, Active_inactive_ChatGPT$construct)
data_GPT = data.frame(construct)
data_GPT$quotes = c(Active_inactive_ChatGPT$active_k, Active_inactive_ChatGPT$sedentary_k)
active_status = rep("Active", times= nrow(Active_inactive_ChatGPT))
sedentary_status = rep("Sedentary", times= nrow(Active_inactive_ChatGPT))
data_GPT$PA_status = c(active_status, sedentary_status)
data_GPT$human = rep("Chat GPT characters", times = nrow(data_GPT))
data_GPT = data_GPT[complete.cases(data_GPT), ]
data = rbind(data_human, data_GPT)


#for enablers
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


Enablers_plot = ggplot(data_domains, aes(y= reorder(construct, +quotes), x = quotes)) +
  geom_col(aes(fill = PA_status), position = "dodge") + 
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


# #perform t-test for each category
# 
# # human
# data_human = subset(data_domains, human == "Human participants")
# 
# compare_means(quotes ~ PA_status,  data = data_human,
#               ref.group = "Sedentary", method = "t.test")
# 
# 
# # ChatGPT
# data_ChatGPT = subset(data_domains, human == "Chat GPT characters")
# 
# compare_means(quotes ~ PA_status,  data = data_ChatGPT,
#               ref.group = "Sedentary", method = "t.test")


# compare_means(quotes ~ PA_status,  data = data_domains,
#               ref.group = "Sedentary", method = "t.test")
# 
# 
# Enablers_plot = Enablers_plot +   
#   stat_compare_means(label = "p.signif", method = "t.test",
#                      ref.group = ".all.")   
# 
# Enablers_plot
# print(Enablers_plot)

# #adding sig diff test 
# Enablers_plot = Enablers_plot + geom_signif(stat="identity",
#                 data=data.frame(x=c(0.875, 1.875), xend=c(1.125, 2.125),
#                                   y=c(5.8, 8.5), annotation=c("**", "NS")),
#                 aes(x=x,xend=xend, y=y, yend=y, annotation=annotation))



ggsave(file = paste(OUTPUT_ROOT, "/Enablers_plot.pdf",  sep=""), Enablers_plot, 
       #width=6, height=2, units="in", 
       scale=1)






#########
#########
#########




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


Barriers_plot = ggplot(data_domains_barriers, aes(y= reorder(construct, +quotes), x = quotes)) +
  geom_col(aes(fill = PA_status), position = "dodge") + 
  facet_wrap(~human, nrow=1)


# The TDF domains and their descriptors are outlined in Table 1; the 14 domains are 1) knowledge, 2) skills, 3) social/professional role and identity, 4) beliefs about capabilities, 5) optimism, 6) beliefs about consequences, 7) reinforcement, 8) intentions, 9) goals, 10) memory, attention, and decision processes, 11) environment context and resources, 12) social influences, 13) emotion, and 14) behavioral regulation

Barriers_plot = Barriers_plot + scale_y_discrete(labels=c("BR-" = "Behavioural regulation", 
                                                          "BaCap-" = "Beliefs about capabilities", 
                                                          "BaCon-" = "Beliefs about consequences", 
                                                          "ECR-" = "Environmental context and resources", 
                                                          "Emotion-" = "Emotion", 
                                                          "Goals-" = "Goals", 
                                                          "Intention-" = "Intentions", 
                                                          "Knowledge-" = "Knowledge", 
                                                          "MADP-" = "Memory, attention, and decision processes", 
                                                          "Optimism-" = "Optimism", 
                                                          "Reinforcement-" = "Reinforcement", 
                                                          "SI-" = "Social influences", 
                                                          "SPR-" = "Social, professional role and identity", 
                                                          "Skills-" = "Skills"
))
Barriers_plot = Barriers_plot + scale_fill_brewer(palette = "Set1")+ scale_color_brewer(palette = "Set1")
Barriers_plot = Barriers_plot + ggtitle("Barriers") + xlab("quotes, k") + ylab("Domain")
Barriers_plot = Barriers_plot + guides(fill=guide_legend(title=""))

ggsave(file = paste(OUTPUT_ROOT, "/Barriers_plot.pdf",  sep=""), Barriers_plot, 
       #width=6, height=2, units="in", 
       scale=1)


