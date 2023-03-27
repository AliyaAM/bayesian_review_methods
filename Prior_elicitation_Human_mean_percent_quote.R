#install.packages("RColorBrewer")

library(tidyverse)
library(dplyr)
library(assertthat)
library(ggplot2)
library(filenamer)
library(reshape2)  
library(tibble)
library(compute.es)
library(metafor)
#library(bayesplot)
library(ggplot2)
library(ggridges)
#library(rstan) 
library(coda)
library(bayestestR)
library(HDInterval)
library(assertthat)
library(RColorBrewer)

## Set the root directory to look for source code.
#SOURCE_ROOT = "/Users/aliyaamirova/proj/bayesian_meta_analysis/"


print("IMPORTANT NEXT STEP: IMPORTANT NEXT STEP IMPORTANT NEXT STEP IMPORTANT NEXT STEP IMPORTANT NEXT STEP change the code below, so we can go over all constructs or atleast those that were measured in the likelihood (detailed in the comments below) if we change BaCapenblr_selfEfficacy to generic construct or index it somehow then we can run a function that goes through all constructs") 



print("check carefully for quotes on how much physical activty each participant did")
print("we should reduce to those constructs that are measured using perceived latent variables (self-efficacy, positive attitude, negative attitude, perceived social support (CHECK THIS ONE), symptom distress")
print("consider how to deal with constructs with more than one belief statements (aggregate/average...?)")
print("decide on the variance for the prior")
print("systematically include all constructs from quant and from qual")
print("plot prior from Human and Human prior next to each other")
print("compare Human and Human prior using prior–data conflict determination using data agreement criterion")
print("plot violin distributions for belief quotes etc...")
print("check overleaf for to do list")


########### DIRECTORY

#directory = "/Users/aliyaamirova/"
#directory = "/Users/aliya/my_docs/"
directory = "/Users/k2147340/OneDrive - King's College London/Documents/"


###########  source root 
SOURCE_ROOT = paste(directory, "proj/bayesian_review_methods/", sep = "")

###########  data root
DATA_ROOT = paste(directory, "proj/bayesian_review_methods/DATA/", sep = "")

########### Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/", sep = "")


############ DATA

Human_perBS_perBot = read.csv(paste(DATA_ROOT, "Human_perBS_per_participant_FINAL.csv", sep = "")) 


#############

#fraction of quotes concerning 'construct' when 'participant' was active 
#fraction of quotes concerning 'construct' when 'participant' was sedentary

#1 - fraction of quotes concerning 'construct' when 'participant' was active  
#1 - fraction of quotes concerning 'construct' when 'participant' was sedentary

#OR = from the contingency table of the above 

#############

#Human self-efficacy: #BaCapenblr_selfEfficacy


sedentary = subset(Human_perBS_perBot, Human_perBS_perBot$PA_status == 0)
nrow(sedentary)
  
unique(Human_perBS_perBot$Cases)
Human_perBS_perBot[rowSums(is.na(Human_perBS_perBot)) > 0,]
ls(Human_perBS_perBot)
ncol(Human_perBS_perBot)

Human_perBS_perBot = tibble(Human_perBS_perBot) %>% 
  replace(is.na(.), 0) %>%
  mutate(sum_quotes_per_participant = rowSums(across(where(is.numeric)))) %>%
  mutate(name = Cases)


matrix_empty = matrix(nrow = 16, ncol = 0)
OR_df = data.frame(matrix_empty) 

for (i in colnames(Human_perBS_perBot[,c(-1, -127, -128)])){
  
  OR_vector = c()
  print(i)
  
  x = unlist(as.vector(Human_perBS_perBot[,i]))
  
  
  
  Human_perBS_perBot$sum_quotes_per_participant
  
  Human_perBS_perBot = Human_perBS_perBot  %>% 
    mutate(x_fraction = x/sum_quotes_per_participant + 0.001)
  
  print('got here')
  
  
  long_data = Human_perBS_perBot %>%
    select(name, PA_status, x_fraction) %>%
    gather("key", 'x_fraction', -name, -PA_status) %>%
    mutate(fraction_not_present = 1 - x_fraction + 0.001) %>%
    arrange(PA_status) %>%
    group_by(PA_status) %>%
    group_split(PA_status)
  
  
  for (j in 1:length(long_data)){
    
    print(j)
    
    print(long_data[[j]][2,4])
    print(long_data[[j]][1,5])
    print(long_data[[j]][2,5])
    print(long_data[[j]][1,4])
    OR_temp = (long_data[[j]][2,4]*long_data[[j]][1,5])/(long_data[[j]][2,5]*long_data[[j]][1,4])
    OR_vector = c(OR_vector, OR_temp$x_fraction[1])
    
  }
  
  OR_df = cbind(OR_df, OR_vector)
  
  colnames(OR_df) = names_columns_OR_df
  names_columns_OR_df = colnames(Human_perBS_perBot[,c(-1, -157, -158, -159)])
  class(names_columns_OR_df)
  
  
}

# 
write.table(OR_df, file = paste(OUTPUT_ROOT, "OR_df_Human_prior.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )



