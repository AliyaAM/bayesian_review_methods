

#directory = "/Users/aliyaamirova/"
directory = "/Users/aliya/my_docs/"

OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/CHatGPTvsHuman", sep = "")


KLDChatGPT_prior_data = read.csv("/Users/aliya/my_docs/proj/bayesian_review_methods/RESULTS/ChatGPT/KLD_ChatGPT/.csv")

KLDHuman_prior_data = read.csv("/Users/aliya/my_docs/proj/bayesian_review_methods/RESULTS/Human/KLD_human.csv")


Human_distributions = read.csv("/Users/aliya/my_docs/proj/bayesian_review_methods/RESULTS/Human/Results_BayesianMeta_Analysis_data_qual_quant.csv")

ChatGPT_distributions  = read.csv("/Users/aliya/my_docs/proj/bayesian_review_methods/RESULTS/ChatGPT/Results_BayesianMeta_Analysis_data_qual_quant.csv")


list_of_included_constructs = unique(ChatGPT_distributions$Construct)

SocialSupport_ChatGPT_distributions = subset(ChatGPT_distributions, ChatGPT_distributions$Construct == "SocialSupport")
Dysphoria_ChatGPT_distributions = subset(ChatGPT_distributions, ChatGPT_distributions$Construct == "Dysphoria")
NegativeAttitude_ChatGPT_distributions = subset(ChatGPT_distributions, ChatGPT_distributions$Construct == "NegativeAttitude")
PositiveAttitude_ChatGPT_distributions = subset(ChatGPT_distributions, ChatGPT_distributions$Construct == "PositiveAttitude")
Symptoms_distress_ChatGPT_distributions = subset(ChatGPT_distributions, ChatGPT_distributions$Construct == "Symptoms_distress")
fewerPerceivedSymptoms_ChatGPT_distributions = subset(ChatGPT_distributions, ChatGPT_distributions$Construct == "fewerPerceivedSymptoms")
SelfEfficacy_ChatGPT_distributions = subset(ChatGPT_distributions, ChatGPT_distributions$Construct == "SelfEfficacy")




SocialSupport_Human_distributions = subset(Human_distributions, Human_distributions$Construct == "SocialSupport")
Dysphoria_Human_distributions = subset(Human_distributions, Human_distributions$Construct == "Dysphoria")
NegativeAttitude_Human_distributions = subset(Human_distributions, Human_distributions$Construct == "NegativeAttitude")
PositiveAttitude_Human_distributions = subset(Human_distributions, Human_distributions$Construct == "PositiveAttitude")
Symptoms_distress_Human_distributions = subset(Human_distributions, Human_distributions$Construct == "Symptoms_distress")
fewerPerceivedSymptoms_Human_distributions = subset(Human_distributions, Human_distributions$Construct == "fewerPerceivedSymptoms")
SelfEfficacy_Human_distributions = subset(Human_distributions, Human_distributions$Construct == "SelfEfficacy")






SocialSupport_data_divergence = rbind(SocialSupport_ChatGPT_distributions$Prior_qual_density, SocialSupport_Human_distributions$Prior_qual_density)
SocialSupport_KL = KL(SocialSupport_data_divergence)

Dysphoria_data_divergence = rbind(Dysphoria_ChatGPT_distributions$Prior_qual_density, Dysphoria_Human_distributions$Prior_qual_density)
Dysphoria_KL = KL(Dysphoria_data_divergence)

NegativeAttitude_data_divergence  = rbind(NegativeAttitude_ChatGPT_distributions$Prior_qual_density, NegativeAttitude_Human_distributions$Prior_qual_density)
NegativeAttitude_KL = KL(NegativeAttitude_data_divergence)

PositiveAttitude_data_divergence  = rbind(PositiveAttitude_ChatGPT_distributions$Prior_qual_density, PositiveAttitude_Human_distributions$Prior_qual_density)
PositiveAttitude_KL = KL(PositiveAttitude_data_divergence)

Symptoms_distress_data_divergence  = rbind(Symptoms_distress_ChatGPT_distributions$Prior_qual_density, Symptoms_distress_Human_distributions$Prior_qual_density)
Symptoms_distress_KL = KL(Symptoms_distress_data_divergence)

fewerPerceivedSymptoms_data_divergence  = rbind(fewerPerceivedSymptoms_ChatGPT_distributions$Prior_qual_density, fewerPerceivedSymptoms_Human_distributions$Prior_qual_density)
fewerPerceivedSymptoms_KL = KL(fewerPerceivedSymptoms_data_divergence)

SelfEfficacy_data_divergence  = rbind(SelfEfficacy_ChatGPT_distributions$Prior_qual_density, SelfEfficacy_Human_distributions$Prior_qual_density)
SelfEfficacy_KL = KL(SelfEfficacy_data_divergence)

participant_source = rep("human_ChatGPTcomparison", times = 7)


Constructs = c("SelfEfficacy", 
               "NegativeAttitude", 
               "Symptoms_distress", 
               "Dysphoria",
               "PositiveAttitude", 
               "fewerPerceivedSymptoms",
               "SocialSupport")


KLD = c(SelfEfficacy_KL, 
        NegativeAttitude_KL, 
        Symptoms_distress_KL, 
        Dysphoria_KL, 
        PositiveAttitude_KL, 
        fewerPerceivedSymptoms_KL, 
        SocialSupport_KL) 

KLD_human_vs_ChatGPT = data.frame(participant_source, Constructs, KLD)

write.table(KLD_human_vs_ChatGPT, file = paste(OUTPUT_ROOT, "KLD_human_vs_ChatGPT.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )


