
# 
 

########### DIRECTORY

#directory = "/Users/aliyaamirova/"
directory = "/Users/aliya/my_docs/"

###########  source root 
SOURCE_ROOT = paste(directory, "proj/bayesian_review_methods/", sep = "")

###########  data root
DATA_ROOT = paste(directory, "proj/bayesian_review_methods/DATA/", sep = "")



print("important to source Bayesian_MA_Quant_and_Qual.R if we update data before running the code below, to make sure we are using the most up to date data file of probability distributions")
 ### important to source Bayesian_MA_Quant_and_Qual.R if we update data before running the code below: 

source(paste(SOURCE_ROOT, "MAIN.R", sep=""))

data = Results_BayesianMeta_Analysis

list_of_included_constructs = unique(data$Construct)

SocialSupport_data = subset(data, data$Construct == "SocialSupport")
SocialSupport_data_divergence = rbind(SocialSupport_data$Prior_qual_density, SocialSupport_data$Likelihood)
Human_SocialSupport_KL = KL(SocialSupport_data_divergence)


Dysphoria_data = subset(data, data$Construct == "Dysphoria")
Dysphoria_data_divergence = rbind(Dysphoria_data$Prior_qual_density, Dysphoria_data$Likelihood)
Human_Dysphoria_KL = KL(Dysphoria_data_divergence)


NegativeAttitude_data = subset(data, data$Construct == "NegativeAttitude")
NegativeAttitude_data_divergence  = rbind(NegativeAttitude_data$Prior_qual_density, NegativeAttitude_data$Likelihood)
Human_NegativeAttitude_KL = KL(NegativeAttitude_data_divergence )


PositiveAttitude_data = subset(data, data$Construct == "PositiveAttitude")
PositiveAttitude_data_divergence  = rbind(PositiveAttitude_data$Prior_qual_density, PositiveAttitude_data$Likelihood)
Human_PositiveAttitude_KL = KL(PositiveAttitude_data_divergence)


Symptoms_distress_data = subset(data, data$Construct == "Symptoms_distress")
Symptoms_distress_data_divergence  = rbind(Symptoms_distress_data$Prior_qual_density, Symptoms_distress_data$Likelihood)
Human_Symptoms_distress_KL = KL(Symptoms_distress_data_divergence )


fewerPerceivedSymptoms_data = subset(data, data$Construct == "fewerPerceivedSymptoms")
fewerPerceivedSymptoms_data_divergence  = rbind(fewerPerceivedSymptoms_data$Prior_qual_density, fewerPerceivedSymptoms_data$Likelihood)
Human_fewerPerceivedSymptoms_KL = KL(fewerPerceivedSymptoms_data_divergence)


SelfEfficacy_data = subset(data, data$Construct == "SelfEfficacy")
SelfEfficacy_data_divergence  = rbind(SelfEfficacy_data$Prior_qual_density, SelfEfficacy_data$Likelihood)
Human_SelfEfficacy_KL = KL(SelfEfficacy_data_divergence)

participant_source = rep("human", times = 7)


Constructs = c("SelfEfficacy", 
               "NegativeAttitude", 
               "Symptoms_distress", 
               "Dysphoria",
               "PositiveAttitude", 
               "fewerPerceivedSymptoms",
               "SocialSupport")


KLD = c(Human_SelfEfficacy_KL, 
      Human_NegativeAttitude_KL, 
      Human_Symptoms_distress_KL, 
      Human_Dysphoria_KL, 
      Human_PositiveAttitude_KL, 
      Human_fewerPerceivedSymptoms_KL, 
      Human_SocialSupport_KL) 

KLD_human = data.frame(participant_source, Constructs, KLD)

write.table(KLD_human, file = paste(OUTPUT_ROOT, "KLD_human.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )








