library(readxl)
library(dplyr)
library(lme4)
library(car)
library(ggplot2)
library(nlme)
library(reshape)
library(ggeffects) 


# Load data from Castor
df <- read_excel("~/Downloads/NECTAR_Necrotizing_Enterocolitis_excel_export_20230114104407.xlsx")
timing_castor <- read_excel("~/Downloads/NECTAR_Necrotizing_Enterocolitis_excel_export_20230208011705.xlsx")
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
Arm_time_Diagnos<-update(Arm_time_, .~. + Diagnosegroep)
Arm_time_Diagnos_bd<-update(Arm_time_Diagnos, .~. + bd_total)


anova( ARModel,Arm_time_)
anova( ARModel,Arm_time_Diagnos )




####Plot mixed effects
# Load the required packages
library(sjPlot)


# Set the theme to "scientific"
theme_set(theme_sjplot())

# Create the predicted values plot with variance ranges
plot_model(Arm_time_, type = "pred", terms = c("tim_ultr", "Diagnosegroep"), show.se = TRUE) +
  labs(title = "Predicted values of minimal diastolic velocity by age, diagnosis and time",
       subtitle = "with 95% confidence intervals",
       x = "Days around surgery", y = "Min. diastolic velocity (cm/s)") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        strip.text = element_text(face = "bold", size = 12))

summary(Arm_time_)


