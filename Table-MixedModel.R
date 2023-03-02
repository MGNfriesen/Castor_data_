

#load packages
require(tidyverse) || {install.packages("tidyverse"); library(tidyverse)}
library(sjPlot)
library(HLMdiag)
library(dplyr)
library(readxl)






############ (2) MULTILEVEL MODELS: GENERATE APA RESULTS TABLE ############

#run the model
summary(Arm_TD)

#export a APA summary table of your model into word
sjPlot::tab_model(Arm_TD,
                  show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Day of ultrasound", "Diagnosis(TGA)", "Day of Ultrasound: Diagnosis(TGA)"),
                  dv.labels= "Flow pattern", #specify how you would like to name your criterion
                  file = "MixedModel_ps.doc") #specify the name you'd like to name your word document; the 


