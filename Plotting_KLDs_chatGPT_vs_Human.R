
library(dplyr)
library(ggthemes) # Load


#directory = "/Users/aliyaamirova/"
#directory = "/Users/aliya/my_docs/"
directory = "/Users/k2147340/OneDrive - King's College London/Documents/"

OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/CHatGPTvsHuman/Mean_percentage_quote/from_SMD_and_n_contingency/", sep = "")


CHatGPTvsHuman_prior = read.csv(paste(OUTPUT_ROOT, "KLD_human_vs_ChatGPT.csv", sep = ""))

data = CHatGPTvsHuman_prior

#making sure the KLD value is allighned with the respective bar 

# m3 <- aggregate(data$KLD, by=list(participant_source=data$participant_source), FUN = sum)
# m3 <- as.data.frame(m3)
# names(m3) <- c("Constructs", "KLD", "participant_source")

#data$Constructs <- as.Constructs.POXIXct(data$Constructs)

#data$participant_source <- as.factor(data$participant_source)

data$new_Constructs =  case_when(data$Constructs ==" SocialSupport" ~ "Social Support", 
                                 data$Constructs == " Dysphoria" ~ "Dysphoria", 
                                 data$Constructs == " NegativeAttitude" ~ "Negative Attitude",
                                 data$Constructs ==" PositiveAttitude" ~ "Positive Attitude",
                                 data$Constructs == " Symptoms_distress" ~ "Symptoms distress", 
                                 data$Constructs == " fewerPerceivedSymptoms" ~ "Fewer Perceived Symptoms",
                                 data$Constructs ==" SelfEfficacy" ~ "Self-efficacy")


#plotting
KLD_plot_divergene_from_data <- ggplot(data, aes(x = KLD, y = reorder(new_Constructs, - KLD))) + 
  geom_col(aes(), position = "dodge") +
  geom_text(aes(label = round(KLD, digits = 2), x = 755), 
            show.legend = FALSE, 
            fontface="bold",
            # y = round(KLD, digits = 2) + 1, 
            position = position_dodge(width = 0.9), size = 3)

#labeling axis
KLD_plot_divergene_from_data = KLD_plot_divergene_from_data + ggtitle("KL divergence between prior distributions: Human and CHatGPT") + ylab("") + xlab("KLD")

#labeling legend
KLD_plot_divergene_from_data = KLD_plot_divergene_from_data + guides(fill=guide_legend(title=""))



KLD_plot_divergene_from_data = KLD_plot_divergene_from_data + scale_x_continuous(limits = c(0, 765))

#KLD_plot_divergene_from_data = KLD_plot_divergene_from_data +  theme_calc()+ scale_colour_calc()



print(KLD_plot_divergene_from_data)

ggsave(file = paste(OUTPUT_ROOT, "/KLD_plot_divergence_ChatGPTvsHuman.pdf",  sep=""),KLD_plot_divergene_from_data, scale=1)
ggsave(file = paste(OUTPUT_ROOT, "/KLD_plot_divergence_ChatGPTvsHuman.eps",  sep=""),KLD_plot_divergene_from_data, scale=1)


##### plot KL between chatgpt and humans 
