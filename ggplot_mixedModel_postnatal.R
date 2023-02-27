library(readxl)
library(dplyr)
library(lme4)
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
total_data <- total_data %>%
  filter(age_time_ultr < 5)

#load in packages for mixed effect analyses
library(car); library(ggplot2); library(nlme); library(reshape);library(lme4)

#Intercept
intercept <-gls(ps ~ 1, data = total_data, method =
                  "ML", na.action = na.exclude)

#vary intercept accross patients
randomIntercept <- lme(ps ~ 1, data = total_data,
                       random = ~1|Participant.Id, method = "ML", na.action = na.exclude, 
                       control = list(opt="optim"))


#adding random slopes: which means that intercepts and the effect of time (~Time) vary across people
timeRS<-update(randomIntercept, random = ~age_time_ultr|Participant.Id)

#add covariance time
ARModel<-update(timeRS, correlation = corAR1(value=0, form = ~age_time_ultr|Participant.Id))
summary(ARModel)

#fixed effects time and diagnosis
Arm_time_<- update(ARModel, ps ~ age_time_ultr * Diagnosegroep)
Arm_time_Diagnos<-update(Arm_time_, .~. + Diagnosegroep)
Arm_time_Diagnos_bd<-update(Arm_time_Diagnos, .~. + bd_total)


anova( ARModel,Arm_time_)
anova( ARModel,Arm_time_Diagnos )




####Plot mixed effects

(mm_plot <- ggplot(total_data, aes(x = age_time_ultr, y = ps, colour = Participant.Id)) +
    facet_wrap(~Diagnosegroep, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(total_data, pred = predict(Arm_time_)), aes(y = pred), linewidth = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))+
    labs(x = "day of ultrasound", y = "ps", 
         title = ""))


# Extract the prediction data frame
total_data$age_time_ultr<-as.numeric(total_data$age_time_ultr)
mydf <- ggpredict(Arm_time_, terms = c("age_time_ultr","Diagnosegroep"))  # this gives overall means for the model


(ggplot(mydf) + 
    geom_line(aes(x = x, y = predicted)) +
    facet_wrap(~Diagnosegroep, nrow=2)+
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = total_data,                      # adding the raw data (scaled values)
               aes(x = age_time_ultr, y = ps, colour = Participant.Id))  + xlim(0,4)+ylim(0,100)+
    labs(x = "Day of ultrasound", y = "Peak systolic velocity (cm/s)", 
         title = "") + 
    theme_minimal()
)



