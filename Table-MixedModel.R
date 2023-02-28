

#load packages
require(tidyverse) || {install.packages("tidyverse"); library(tidyverse)}
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(lme4)
library(HLMdiag)
library(car)
library(dplyr)
library(reshape2)
library(tidyr)
library(lmerTest)
library(quantmod)
library(multilevelTools)
install.packages("ggeffects",dependencies = TRUE) # if you run into issues, install the package(s) manually


library(readxl)
library(dplyr)
library(ggplot2)
library(nlme)
library(reshape)
library(ggeffects) 


# Load data from Castor
#flow data (repeated measures)
df <- read_excel("~/Downloads/NECTAR_Necrotizing_Enterocolitis_excel_export_20230114104407.xlsx")
#clinical data
timing_castor <- read_excel("~/Downloads/NECTAR_Necrotizing_Enterocolitis_excel_export_20230208011705.xlsx")
#diagnosis 
Diagnosegroep <- read_excel("~/Downloads/Diagnoses en OKs Morgan tijdelijk.xlsx")


# Merge dataframes based on Participant Id
Diagnosegroep$`Participant Id`<- as.character(Diagnosegroep$`Participant Id`)
total_data <- df %>%
  left_join(timing_castor, by = "Participant Id") %>%
  left_join(Diagnosegroep, by = "Participant Id") %>%
  select(-matches(".*\\.(x|y)$"))

total_data$Participant.Id<-total_data$`Participant Id`

# Add flow measurements
total_data <- total_data %>%
  mutate(
    pi = ifelse(ultr_aca_angle == 1, ultr_aca_pi_no_ac_right_angle, ultr_aca_pi_with_ac),
    ri = ifelse(ultr_aca_angle == 1, ultr_aca_ri_no_ac_right_angle, ultr_aca_ri_with_ac),
    ps = ifelse(ultr_aca_angle == 1, ultr_aca_ps_no_ac_right_angle, ultr_aca_ps_with_ac),
    md = ifelse(ultr_aca_angle == 1, ultr_aca_md_no_ac_right_angle, ultr_aca_md_with_ac),
    pi = as.numeric(pi),
    ri = as.numeric(ri),
    ps = as.numeric(ps),
    md = as.numeric(md)
  )

# Add brain injury information
total_data <- total_data %>%
  mutate(
    bd_pre = ifelse(preop_mri_avail == 1, preop_bd_mri, preop_bd_echo),
    bd_post = ifelse(postop_mri_available == 1, postop_bd_mri, NA),
    bd_total = as.numeric(bd_pre == 1 | bd_post == 1)
  )

# Delete excluded patients
excluded_ids <- c("110007", "110011", "110012", "110019")
total_data <- total_data %>%
  filter(!Participant.Id %in% excluded_ids)

# Prepare data for mixed effect analyses
total_data$age_time_ultr<-as.numeric(total_data$age_time_ultr)
total_data$surg_age<-as.numeric(total_data$surg_age)
total_data$tim_ultr <- total_data$age_time_ultr-total_data$surg_age
total_data <- total_data %>% filter(tim_ultr %in% c("-6","-5", "-4","-3","-2","-1","0","1","2","3","4"))


#Intercept
intercept <-gls(md ~ 1, data = total_data, method =
                  "ML", na.action = na.exclude)

#vary intercept accross patients
randomIntercept <- lme(md ~ 1, data = total_data,
                       random = ~1|Participant.Id, method = "ML", na.action = na.exclude, 
                       control = list(opt="optim"))


#adding random slopes: which means that intercepts and the effect of time (~Time) vary across people
timeRS<-update(randomIntercept, random = ~tim_ultr|Participant.Id)

#add covariance time
ARModel<-update(timeRS, correlation = corAR1(value=0, form = ~tim_ultr|Participant.Id))
summary(ARModel)

#fixed effects time and diagnosis
Arm_time_<- update(ARModel, md ~ tim_ultr * Diagnosegroep)
Arm_time_bd<-update(Arm_time_, .~. + bd_total)

Arm_time_Diagnos<-update(Arm_time_, .~. + Diagnosegroep)
Arm_time_Diagnos_bd<-update(Arm_time_Diagnos, .~. + bd_total)

#PLOT RESULTS
#Expand palette;  ggpredict has only 9 colors in its default setting. If you want to plot uniquely colored regression lines for more than 9 clusters the code won't run if you don't expand its color palette 
library(RColorBrewer) #load package to expand the color palette
nb.cols100 <- 100 #Create a color palette (here: the new color palette includes 100 colors)
mycolors100 <- colorRampPalette(brewer.pal(8, "RdYlBu"))(nb.cols100)

#Plot
PlotSimple <- ggpredict(Arm_time_Diagnos_bd, terms = c("tim_ultr", "bd_total","Diagnosegroep"), type = "re") %>% #create a 'plot object', select the predictor variable (here: CN_GMC) and the clustering variable (here: Country)
  plot() +
  labs(x = "Days around surgery", y = "Min. diastolic flow (cm/s)", title = "") + #name your axes and add a title (if you want to)
  scale_fill_manual(values = mycolors100) #specify that you want to use the expanded color palette that you just created (here: mycolors100)
PlotSimple #generate and display the plot






total_data$bd_total<-as.factor(total_data$bd_total)
#plot
pred <- ggpredict(Arm_time_bd, terms = c("tim_ultr", "bd_total","Diagnosegroep"), type = "re") %>% 
  plot() +
  labs(x = "Days around surgery", y = "Min. diastolic flow (cm/s)", title = "") + 
  scale_fill_manual(values = mycolors100) +
geom_point(data = total_data, inherit.aes = FALSE,
            aes(x = tim_ultr, y = md, group=Participant.Id, colour=bd_total),     
            alpha = 0.3) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        strip.text = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(hjust = 0.5)) +           
  ylim(0,35) +                     
  xlim(-6,4)                      
pred + p #generate and display the plot

total_data$bd_total<-as.factor(total_data$bd_total)


library(ggplot2)
p = total_data %>% 
  ggplot(aes(x = tim_ultr, y = md, group = Participant.Id, color= bd_total)) + 
  geom_line()

p = p + facet_wrap(~Diagnosegroep)
p


library(ggplot2)
library(ggeffects)

total_data$tim_ultr<-as.numeric(total_data$tim_ultr)
total_data$bd_total<-as.factor(total_data$bd_total)
total_data$Diagnosegroep<-as.factor(total_data$Diagnosegroep)




# Generate predicted values and confidence intervals for the interaction model
predict_data <- ggpredict(Arm_time_bd, terms = c("tim_ultr", "bd_total","Diagnosegroep"), type = "re")

# Plot the predicted values and confidence intervals
ggplot(predict_data, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  labs(x = "Days around surgery", y = "Min. diastolic flow (cm/s)", title = "") +
  scale_color_manual(values = mycolors100) +
  scale_fill_manual(values = mycolors100) +
  facet_wrap(~Diagnosegroep)

# Plot the original data with colored lines by bd_total and grouped by Participant.Id
p = total_data %>% 
  ggplot(aes(x = tim_ultr, y = md, group = Participant.Id, color= bd_total)) + 
  geom_line() +
  labs(x = "Days around surgery", y = "Min. diastolic flow (cm/s)", title = "") +
  scale_color_manual(values = mycolors100) +
  facet_wrap(~Diagnosegroep)

# Combine the two plots using the patchwork package
library(patchwork)
p + plot_spacer() + plot_layout(ncol = 1, heights = c(3, 1)) + predict_data + 
  plot_layout(ncol = 1, heights = c(1, 1))





############ (2) MULTILEVEL MODELS: GENERATE APA RESULTS TABLE ############

#run the model
summary(Arm_time_Diagnos_bd)

#export a APA summary table of your model into word
sjPlot::tab_model(Arm_time_Diagnos_bd,
                  show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Day of ultrasound", "Diagnosis: Others", "Diagnosis: SVP","Diagnosis: TGA", "Brain damage", "Interaction timing:Diagnose(Other)","Interaction timing:Diagnose(SVP)","Interaction timing:Diagnose(TGA)"), #specify how you'd like to name your predictors 
                  dv.labels= "Flow pattern", #specify how you would like to name your criterion
                  file = "baljsq.doc") #specify the name you'd like to name your word document; the 


