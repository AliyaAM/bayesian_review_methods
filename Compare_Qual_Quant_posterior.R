
library(dplyr)
library(RColorBrewer)


density_ALL_Construct
head(density_ALL_Construct)


All_constructs_prior = select(density_ALL_Construct, logOddsRatio, Construct, Prior_qual_density) 

All_constructs_likelihood = select(density_ALL_Construct, logOddsRatio, Construct, Likelihood)

All_constructs_posterior = select(density_ALL_Construct, logOddsRatio, Construct, posterior_QualplusQuant) 




SocialSupport_density_prior =  All_constructs_prior  %>% filter(Construct == "SocialSupport")
Dysphoria_density_prior =  All_constructs_prior  %>% filter(Construct == "Dysphoria")
NegativeAttitude_density_prior = All_constructs_prior  %>% filter(Construct == "NegativeAttitude")
PositiveAttitude_density_prior = All_constructs_prior  %>% filter(Construct == "PositiveAttitude")
Symptoms_distress_density_prior = All_constructs_prior  %>% filter(Construct== "Symptoms_distress")
Symptoms_density_prior = All_constructs_prior  %>% filter(Construct== "fewerPerceivedSymptoms")
SelfEfficacy_density_prior = All_constructs_prior  %>% filter(Construct == "SelfEfficacy")


SocialSupport_density_likelihood  =  All_constructs_likelihood  %>% filter(Construct == "SocialSupport")
Dysphoria_density_likelihood  =  All_constructs_likelihood  %>% filter(Construct == "Dysphoria")
NegativeAttitude_density_likelihood  = All_constructs_likelihood  %>% filter(Construct == "NegativeAttitude")
PositiveAttitude_density_likelihood  = All_constructs_likelihood  %>% filter(Construct == "PositiveAttitude")
Symptoms_distress_density_likelihood  = All_constructs_likelihood  %>% filter(Construct== "Symptoms_distress")
Symptoms_density_likelihood  = All_constructs_likelihood  %>% filter(Construct== "fewerPerceivedSymptoms")
SelfEfficacy_density_likelihood  = All_constructs_likelihood  %>% filter(Construct == "SelfEfficacy")


SocialSupport_density_posterior =  All_constructs_posterior  %>% filter(Construct == "SocialSupport")
Dysphoria_density_posterior =  All_constructs_posterior  %>% filter(Construct == "Dysphoria")
NegativeAttitude_density_posterior = All_constructs_posterior  %>% filter(Construct == "NegativeAttitude")
PositiveAttitude_density_posterior  = All_constructs_posterior  %>% filter(Construct == "PositiveAttitude")
Symptoms_distress_density_posterior = All_constructs_posterior  %>% filter(Construct== "Symptoms_distress")
Symptoms_density_posterior = All_constructs_posterior  %>% filter(Construct== "fewerPerceivedSymptoms")
SelfEfficacy_density_posterior  = All_constructs_posterior  %>% filter(Construct == "SelfEfficacy")



prior_name = rep("Qualitative evidence", times = 1000)

likelihood_name = rep("Quantitative evidence", times = 1000)

posterior_name = rep("Posterior (Qual + QUANT)", times = 1000)


distribution = c(prior_name, likelihood_name, posterior_name)


height = c(rep(10, 1000),
           rep(20, 1000), 
           rep(30, 1000), 
           rep(40, 1000), 
           rep(50, 1000), 
           rep(60, 1000), 
           rep(70, 1000), 
           rep(80, 1000), 
           rep(90, 1000),
           rep(100, 1000), 
           rep(110, 1000),
           rep(120, 1000), 
           rep(130, 1000), 
           rep(140, 1000), 
           rep(150, 1000),
           rep(160, 1000),
           rep(170, 1000),
           rep(180, 1000),
           rep(190, 1000),
           rep(200, 1000),
           rep(210, 1000))
#  rep(220, 1000), 
#  rep(230, 1000), 
#  rep(240, 1000), 
#  rep(250, 1000), 
#  rep(260, 1000), 
#  rep(270, 1000), 
#  rep(280, 1000), 
#  rep(290, 1000),
#  rep(300, 1000))

min(density_ALL_Construct$logOddsRatio)


d <- data.frame(
  logOddsRatio = density_ALL_Construct$logOddsRatio, 
  
  Construct = c(SocialSupport_density_prior$Construct,
                SocialSupport_density_likelihood$Construct,
                SocialSupport_density_posterior$Construct,
                
                
                Dysphoria_density_prior$Construct,
                Dysphoria_density_likelihood$Construct,
                Dysphoria_density_posterior$Construct,
                
                NegativeAttitude_density_prior$Construct,
                NegativeAttitude_density_likelihood$Construct,
                NegativeAttitude_density_posterior$Construct, 
                
                PositiveAttitude_density_prior$Construct,
                PositiveAttitude_density_likelihood$Construct,
                PositiveAttitude_density_posterior$Construct,
                
                Symptoms_distress_density_prior$Construct,
                Symptoms_distress_density_likelihood$Construct,
                Symptoms_distress_density_posterior$Construct,
                
                Symptoms_density_prior$Construct,
                Symptoms_density_likelihood$Construct,
                Symptoms_density_posterior$Construct,
                
                
                SelfEfficacy_density_prior$Construct,
                SelfEfficacy_density_likelihood$Construct,
                SelfEfficacy_density_posterior$Construct),
  
  
  
  y = c(SocialSupport_density_prior$Prior_qual_density,
        SocialSupport_density_likelihood$Likelihood,
        SocialSupport_density_posterior$posterior_QualplusQuant,
        
        
        Dysphoria_density_prior$Prior_qual_density,
        Dysphoria_density_likelihood$Likelihood,
        Dysphoria_density_posterior$posterior_QualplusQuant,
        
        
        NegativeAttitude_density_prior$Prior_qual_density,
        NegativeAttitude_density_likelihood$Likelihood,
        NegativeAttitude_density_posterior$posterior_QualplusQuant, 
        
        PositiveAttitude_density_prior$Prior_qual_density,
        PositiveAttitude_density_likelihood$Likelihood,
        PositiveAttitude_density_posterior$posterior_QualplusQuant,
        
        Symptoms_distress_density_prior$Prior_qual_density,
        Symptoms_distress_density_likelihood$Likelihood,
        Symptoms_distress_density_posterior$posterior_QualplusQuant,
        
        
        Symptoms_density_prior$Prior_qual_density,
        Symptoms_density_likelihood$Likelihood,
        Symptoms_density_posterior$posterior_QualplusQuant,
        
        SelfEfficacy_density_prior$Prior_qual_density,
        SelfEfficacy_density_likelihood$Likelihood,
        SelfEfficacy_density_posterior$posterior_QualplusQuant),
  
  
  
  
  distribution = distribution, 
  
  height = height)

d$group_name = paste0(as.character(d$Construct)," ", as.character(d$distribution))
# colors for the intermediate plots were as follows: 
#prior: "#CC79A7"
#posterior: "#D55E00"
# likelihood: "#009E73"

#we want the distributions to be same color as they are in the intermediate plots so prior should be "#CC79A7" and so on. 
# however, they do not look nice together so I will pick the same colors but more pastel, Set2 is nice: 
#display.brewer.all(colorblindFriendly = TRUE)
#colors = display.brewer.pal(c(1, 2, 4), "Set2")
#print the codes for the colors: 
#brewer.pal(n = 5, name = "Set2")

##############

#colors from the set2: "#66C2A5" "#FC8D62" "#E78AC3"
Compare_distributions_plot = ggplot(d, aes(x = logOddsRatio, 
                                           y = Construct,
                                           height = y, 
                                           group = group_name, 
                                           color = distribution,
                                           fill = distribution)) +
  
  scale_x_continuous(name = "log OR", 
                     
                     # breaks = c(-3.0,
                     # 
                     #                             -2.8,
                     #                             -2.6,
                     #                             -2.4,
                     #                             -2.2,
                     #                             -2.0,
                     #                             -1.8,
                     #                             -1.6,
                     #                             -1.4,
                     #                             -1.2,
                     # 
                     #                             -1,
                     #                             -0.8,
                     #                             -0.6,
                     #                             -0.4,
                     #                             -0.2,
                     #                             0,
                     # 
                     #                             0.2,
                     #                             0.4,
                     #                             0.6,
                     #                             0.8,
                     #                             1,
                     #                             1.2,
                     #                             1.4,
                     #                             1.6,
                     #                             1.8,
                     #                             2),
                     limits = c(-2, 3)) +
  
  
  geom_density_ridges(stat = "identity",
                      scale = 2) +
  
  #prior: "#CC79A7"
  #posterior: "#D55E00"
  # likelihood: "#009E73"
  
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  #scale_fill_manual(values = c("#FC8D62" , "#E78AC3" ,"#66C2A5"))+
  #scale_color_manual(values = c("#FC8D62" , "#E78AC3" ,"#66C2A5"))+
  
  #xlim(-3,3) +
  
  scale_y_discrete(labels=c(#"Age" =  "Age",
    #"Comorbidity" =  "Comorbidity",
    "SocialSupport"=  "Social Support",
    "Dysphoria" = "Dysphoria", 
    "NegativeAttitude"=  "Negative Attitude",
    "PositiveAttitude"=  "Positive Attitude",
    "Symptoms_distress" = "Symptoms distress", 
    # "6MWT"= "6MWT",
    # "PhysicalFunctioning"="Physical Functioning",
    "fewerPerceivedSymptoms"= "Fewer Perceived Symptoms",
    # "LVEF"="LVEF",
    "SelfEfficacy"="Self-efficacy"
    
  ))   + 
  
  theme(legend.position = c("top"),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1), 
        
        legend.text = element_text(size = 8), 
        legend.title = element_text(face = "bold", size = 8), 
        
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        panel.grid.minor = element_line(colour = "grey", size = 0.1))+
  theme(text = element_text(size = 10))   





print(Compare_distributions_plot)



ggsave(file = paste(OUTPUT_ROOT, "/Compare_distributions_plot.pdf",  sep=""),Compare_distributions_plot, width=6, height=2, units="in", scale=1)

ggsave(file = paste(OUTPUT_ROOT, "/Compare_distributions_plot.eps",  sep=""),Compare_distributions_plot, width=6, height=2, units="in", scale=1)


