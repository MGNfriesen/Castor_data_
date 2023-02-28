

#load packages
require(tidyverse) || {install.packages("tidyverse"); library(tidyverse)}
library(sjPlot)
library(HLMdiag)
library(dplyr)
library(readxl)






############ (2) MULTILEVEL MODELS: GENERATE APA RESULTS TABLE ############

#run the model
summary(Arm_TD_bd)

#export a APA summary table of your model into word
sjPlot::tab_model(Arm_TD_bd,
                  show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Day of ultrasound", "Diagnosis: Others", "Diagnosis: SVP","Diagnosis: TGA", "Brain damage", "Interaction timing:Diagnose(Other)","Interaction timing:Diagnose(SVP)","Interaction timing:Diagnose(TGA)"), #specify how you'd like to name your predictors 
                  dv.labels= "Flow pattern", #specify how you would like to name your criterion
                  file = "MixedModel_md.doc") #specify the name you'd like to name your word document; the 


