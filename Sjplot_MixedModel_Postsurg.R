library(readxl)
library(dplyr)
library(lme4)
library(car)
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


# Delete excluded patients
excluded_ids <- c("110007", "110011", "110012", "110019")
total_data <- total_data %>%
  filter(!Participant.Id %in% excluded_ids)

# Prepare data for mixed effect analyses
total_data$age_time_ultr<-as.numeric(total_data$age_time_ultr)
total_data$surg_age<-as.numeric(total_data$surg_age)
total_data$tim_ultr <- total_data$age_time_ultr-total_data$surg_age
total_data <- total_data %>% filter(tim_ultr %in% c("0","1","2","3"))
total_data <- total_data %>% filter(Diagnosegroep %in% c("TGA","LVOTO"))




####Plot mixed effects
# Load the required packages
library(sjPlot)

# Set the theme to "scientific"
theme_set(theme_sjplot())

##Plot PS 
pi<-total_data$pi
ri<-total_data$ri
md<-total_data$md
ps<-total_data$ps
tim_ultr<-total_data$tim_ultr


#write out mixedmodel
##Plot PS
Arm_TD_ps <- lme(ps ~ tim_ultr * Diagnosegroep, data = total_data, random = ~tim_ultr|Participant.Id, 
                 correlation = corCAR1(value=0.9, form = ~tim_ultr|Participant.Id), 
                 method = "ML", na.action = na.exclude, control = list(opt="optim"))

# Create the predicted values plot with confidence interval
plot_model(Arm_TD_ps, 
           type = "pred", 
           terms = c("tim_ultr", "Diagnosegroep"), 
           show.se = TRUE,) +
  labs(title = "Predicted values for peak diastolic velocity by age, diagnosis and time",
       subtitle = "with 95% confidence intervals",
       x = "Days around surgery", y = "Peak systolic velocity (cm/s)",color = "CHD Diagnosis") +
geom_point(data = total_data, inherit.aes = FALSE,
            aes(x= tim_ultr, y = ps, color=Diagnosegroep),alpha = 0.7) +
    labs(title = "Predicted values for peak diastolic velocity by age, diagnosis and time",
         x = "Days post-surgery", y = "Peak systolic velocity (cm/s)")
 

##Plot MD
Arm_TD_md <- lme(md ~ tim_ultr * Diagnosegroep, data = total_data, random = ~tim_ultr|Participant.Id, correlation = corCAR1(value=0.9, form = ~tim_ultr|Participant.Id), method = "ML", na.action = na.exclude, control = list(opt="optim"))

# Create the predicted values plot with confidence interval
plot_model(Arm_TD_md, 
           type = "pred", 
           terms = c("tim_ultr", "Diagnosegroep"), 
           show.se = TRUE,) +
  labs(title = "Predicted values for peak diastolic velocity by age, diagnosis and time",
       subtitle = "with 95% confidence intervals",
       x = "Days around surgery", y = "Peak systolic velocity (cm/s)",color = "CHD Diagnosis") +
  geom_point(data = total_data, inherit.aes = FALSE,
             aes(x= tim_ultr, y = md, color=Diagnosegroep),alpha = 0.7) +
  labs(title = "Predicted values for peak diastolic velocity by age, diagnosis and time",
       x = "Days post-surgery", y = "Peak systolic velocity (cm/s)")

##Plot PI
Arm_TD_pi <- lme(pi ~ tim_ultr * Diagnosegroep, data = total_data,  random = ~tim_ultr|Participant.Id, 
                 correlation = corCAR1(value=0.9, form = ~tim_ultr|Participant.Id), method = "ML", 
                 na.action = na.exclude, control = list(opt="optim"))

# Create the predicted values plot with confidence interval
plot_model(Arm_TD_pi, 
           type = "pred", 
           terms = c("tim_ultr", "Diagnosegroep"), 
           show.se = TRUE,) +
  labs(title = "Predicted values for pulsatility indexes by age, diagnosis and time",
       subtitle = "with 95% confidence intervals",
       x = "Days around surgery", y = "Peak systolic velocity (cm/s)",color = "CHD Diagnosis") +
  geom_point(data = total_data, inherit.aes = FALSE,
             aes(x= tim_ultr, y = pi, color=Diagnosegroep),alpha = 0.7) +
  labs(title = "Predicted values for peak diastolic velocity by age, diagnosis and time",
       x = "Days post-surgery", y = "Pulsatility Index")

##Plot RI
Arm_TD_ri <- lme(ri ~ tim_ultr * Diagnosegroep, data = total_data, random = ~tim_ultr|Participant.Id, correlation = corCAR1(value=0.9, form = ~tim_ultr|Participant.Id), method = "ML", na.action = na.exclude, control = list(opt="optim"))

# Create the predicted values plot with confidence interval
plot_model(Arm_TD_ri, 
           type = "pred", 
           terms = c("tim_ultr", "Diagnosegroep"), 
           show.se = TRUE,) +
  labs(title = "Predicted values for peak diastolic velocity by age, diagnosis and time",
       subtitle = "with 95% confidence intervals",
       x = "Days around surgery", y = "Resistivity Index",color = "CHD Diagnosis") +
  geom_point(data = total_data, inherit.aes = FALSE,
             aes(x= tim_ultr, y = ri, color=Diagnosegroep),alpha = 0.7) +
  labs(title = "Predicted values for Resistivity Index by age, diagnosis and time",
       x = "Days post-surgery", y = "Resistivity Index")


  


