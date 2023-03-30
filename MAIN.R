

#library(tidyverse)
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
library(philentropy)



## Set the root directory to look for source code.
#SOURCE_ROOT = "/Users/aliyaamirova/proj/bayesian_meta_analysis/"

print("decide on how to obtain variance for ORs for each construct in the human prior because it is only a single number")

print("include mean quote as the statitic for the prior contingency table")
print("check carefully for quotes on how much physical activty each participant did")

print("we should reduce to those constructs that are measured using perceived latent variables (self-efficacy, positive attitude, negative attitude, perceived social support (CHECK THIS ONE), symptom distress")
print("consider how to deal with constructs with more than one belief statements (aggregate/average...?)")
print("systematically include all constructs from quant and from qual")
print("plot prior from chatGPT and human prior next to each other")
print("compare chatGPT and human prior using prior–data conflict determination using data agreement criterion")
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
#OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/Human/", sep = "")
#OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/ChatGPT/", sep = "")

OUTPUT_ROOT = paste(directory, "proj/bayesian_review_methods/RESULTS/Human/BS_merged_for_constructs/constant_var/from_n_contingency_tbl/", sep = "")

###### old prior (using mta-analysis to pool varius belief statements underlying the construct): 
#x = read.csv(paste(DATA_ROOT, "input_Human_prior.csv", sep="")) #  qualitative data (human)
#x = read.csv(paste(DATA_ROOT, "input_ChatGPT_prior_from_mean_quotes.csv", sep="")) # new qualitative data from mean quote 
#x = read.csv(paste(DATA_ROOT, "input_Human_prior_from_mean_quotes_merge_BS_cosntant_var_from_SMD.csv", sep="")) # new qualitative data from mean quote merged BS if more than one is present with constant var
#x = read.csv(paste(DATA_ROOT, "input_Human_prior_from_mean_quotes_merge_BS_cosntant_var_from_meanquote.csv", sep="")) # new qualitative data from mean quote merged BS if more than one is present with constant var
x = read.csv(paste(DATA_ROOT, "input_Human_prior_constant_var.csv", sep="")) # new qualitative data from mean quote merged BS if more than one is present with constant var


#if(x$PriorExpert_N_PA_X == 0 || x$PriorExpert_N_PA_noX == 0 || x$PriorExpert_N_noPA_X == 0 || x$PriorExpert_N_noPA_noX == 0)
#{

# x %>% dplyr::transmute(PriorExpert_N_PA_X = PriorExpert_N_PA_X + 0.5, 
#                 PriorExpert_N_PA_noX = PriorExpert_N_PA_noX + 0.5, 
#                 PriorExpert_N_noPA_X = PriorExpert_N_noPA_X + 0.5, 
#                 PriorExpert_N_noPA_noX = PriorExpert_N_noPA_noX + 0.5)

#}





print(x)


#x = read.csv(paste(DATA_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 
data = read.csv(paste(DATA_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from  the quantitative studies, the file lists all data including the data that was not used for the meta-analysis. the data not included in the meta-anslysis is for the cases when insufficient data was reported in the article for it to be pooled in the meta-analysis (for example mean but no SD or variance etc)
JaarsmaInternationalStudy = read.csv(paste(DATA_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)



source(paste(SOURCE_ROOT, "ConvertEffectsizes.R", sep="")) #### convert effect sizes from individual studies  (F-value, Binary (Absolute numbers and proportions), r coeffcient and SMD) into log odds ratios. All quantitative results are converted to log OR in order to be comptable with qualitative evidence, we treated all results as binary. 
likelihood_data =  ConvertEffectsizes(data = data)


#run Bayesian meta-analysis for two procedures separately: 

#on the constructs that were present in both qualitative and quantitative studies (the main plot is outputed from the function below): 
source(paste(SOURCE_ROOT, "Bayesian_MA_Quant_and_Qual.R", sep=""))


#constructs that were present in quantitative studies only (the main plot is outputed from the function below): 
source(paste(SOURCE_ROOT, "BayesianMA_Quant_only.R", sep=""))



#plot the fidings with and without qualitative evidence next to each other for comparison

source(paste(SOURCE_ROOT, "Compare_Qual_Quant_posterior.R", sep=""))
