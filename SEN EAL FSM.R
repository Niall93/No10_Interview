Key_Absence_Data

spc_school_level_underlying_data <- read_csv("data/spc_school_level_underlying_data.csv")

FSM_ESL <- spc_school_level_underlying_data %>%
  select(time_period,urn,admissions_policy,FSM_perc =`% of pupils known to be eligible for free school meals (Performance Tables)`,nonESL = `% of pupils whose first language is known or believed to be English`) %>%
  mutate(ESL_perc = 100-as.numeric(nonESL))

SEN_all <- read_csv("data/sen_school_level_ud.csv") 

SEN <- SEN_all %>%
  select(time_period,URN,tot_pup=`Total pupils`,SEN=`SEN support`,EHCP=`EHC plan`) %>%
  mutate(SEN_perc = as.numeric(SEN)/as.numeric(tot_pup),
         EHCP_perc = as.numeric(EHCP)/as.numeric(tot_pup),
         total_needs = SEN_perc+EHCP_perc)

LSOA_lookup <- SEN_all %>%
  select(URN,LSOA11)

IMD2019 <- read_csv("data/societal-wellbeing_imd2019_indices.csv",skip = )

School_IMD <- left_join(LSOA_lookup,IMD2019,by=c("LSOA11"="LSOA11CD"))

Joined_data <- Key_Absence_Data %>%
  left_join(FSM_ESL,by=c("time_period","school_urn"="urn")) %>%
  left_join(SEN,by=c("time_period","school_urn"="URN")) %>%
  left_join(School_IMD,by=c("school_urn"="URN"))

library(ggpmisc)
Joined_data %>%
  filter(time_period==202324&education_phase!="Special") %>%
  ggplot(aes(x = as.numeric(FSM_perc),y=sess_unauthorised_percent)) +
  geom_point(size = 0.01) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  facet_wrap(~education_phase)+
  custom_ggplot_theme() +
  scale_y_continuous(expand=c(0,0),limits = c(0,15)) +
  labs(title="Comparing absence rates vs free school meals rates 2023-24",
       x="Free school meals %",y="Absence %")

Joined_data %>%
  filter(time_period==202324&education_phase!="Special") %>%
  ggplot(aes(x = as.numeric(FSM_perc),y=enrolments_pa_10_exact_percent)) +
  geom_point(size = 0.01) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2"))) +
  facet_wrap(~education_phase)+
  custom_ggplot_theme() +
  scale_y_continuous(expand=c(0,0)) +
  labs(title="Comparing percentage of students absent for 10% or more vs free school meals rates 2023-24",
       x="Free school meals %",y="Proportion of students absent for more than 10%") +
  theme(axis.title.y = element_text(angle = 90))

Joined_data %>%
  filter(time_period==202324&education_phase!="Special") %>%
  ggplot(aes(x = 100*as.numeric(total_needs),y=enrolments_pa_10_exact_percent)) +
  geom_point(size = 0.01) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2"))) +
  facet_wrap(~education_phase)+
  custom_ggplot_theme() +
  scale_y_continuous(expand=c(0,0)) +
  labs(title="Comparing percentage of students absent for 10% or more vs SEN/EHCP rates 2023-24",
       x="Special Educational Needs %",y="Proportion of students absent for more than 10%") +
  theme(axis.title.y = element_text(angle = 90))

Joined_data %>%
  filter(time_period==202324&education_phase!="Special") %>%
  ggplot(aes(x = as.numeric(FSM_perc),y=enrolments_pa_50_exact_percent)) +
  geom_point(size = 0.01) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2"))) +
  facet_wrap(~education_phase)+
  custom_ggplot_theme() +
  scale_y_continuous(expand=c(0,0)) +
  labs(title="Comparing percentage of students absent for 50% or more vs free school meals rates 2023-24",
       x="Free school meals %",y="Proportion of students absent for more than 10%") +
  theme(axis.title.y = element_text(angle = 90))

Joined_data %>%
  filter(time_period==202324) %>%
  ggplot(aes(x = as.numeric(ESL_perc),y=sess_unauthorised_percent)) +
  geom_point(size=0.1) +
  stat_poly_line() +
  facet_wrap(~education_phase)+
  custom_ggplot_theme() +
  scale_y_continuous(expand=c(0,0)) +
  labs(title="Comparing absence rates vs EAL rates 2023-24",
       x="English as additional Language %",y="Absence %")

Joined_data %>%
  filter(time_period==202324&education_phase!="Special") %>%
  ggplot(aes(x = 100*as.numeric(total_needs),y=sess_unauthorised_percent)) +
  geom_point(size=0.1) +
  stat_poly_line() +
  stat_poly_eq()+
  facet_wrap(~education_phase)+
  custom_ggplot_theme() +
  scale_y_continuous(expand=c(0,0)) +
  labs(title="Comparing absence rates vs SEN rates 2023-24",
       x="Students with Special Educaional Needs or EHCP %",y="Absence %")

Joined_data %>%
  filter(time_period==202324&education_phase!="Special") %>%
  ggplot(aes(x = as.numeric(IMD),y=sess_unauthorised_percent)) +
  geom_point(size=0.1) +
  stat_poly_line() +
  stat_poly_eq()+
  facet_wrap(~education_phase)+
  custom_ggplot_theme() +
  scale_y_continuous(expand=c(0,0)) +
  labs(title="Comparing absence rates 2023-24 vs IMD 2019",
       x="Index of Multiple Deprivation",y="Absence %")

Joined_data %>%
  mutate(FSM_bins = cut(as.numeric(FSM_perc),breaks = c(0,10,20,30,40,100))) %>%
  filter(time_period==202324&education_phase=="State-funded secondary") %>%
  ggplot(aes(x = 100*as.numeric(total_needs),y=sess_unauthorised_percent)) +
  geom_point(size=0.1) +
  stat_poly_line() +
  stat_poly_eq()+
  facet_wrap(~FSM_bins)+
  custom_ggplot_theme() +
  scale_y_continuous(expand=c(0,0))


Joined_data %>%
  mutate(SEN_bins = cut(100*as.numeric(total_needs),breaks = c(0,10,15,20,30,100))) %>%
  filter(time_period==202324&education_phase=="State-funded secondary") %>%
  ggplot(aes(x = as.numeric(FSM_perc),y=sess_unauthorised_percent)) +
  geom_point(size=0.1) +
  stat_poly_line() +
  stat_poly_eq()+
  facet_wrap(~SEN_bins)+
  custom_ggplot_theme() +
  scale_y_continuous(expand=c(0,0))

Joined_data$FSM_perc <- as.numeric(Joined_data$FSM_perc)

Primary <- Joined_data %>% filter(education_phase=="State-funded primary")
Secondary <- Joined_data %>% filter(education_phase=="State-funded secondary"&time_period==202324&!is.na(total_needs)&!is.na(FSM_perc))


model <- lm(sess_unauthorised_percent ~ FSM_perc ,data= Primary)
summary(model)

model2 <- lm(sess_unauthorised_percent ~ FSM_perc+total_needs ,data= Secondary)
summary(model2)

residuals <- residuals(model2)

largest_residuals_indices <- order(residuals, decreasing = TRUE)[1:2]

smallest_residuals_indices <- order(residuals, decreasing = FALSE)[1:2]

plot(Secondary$FSM_perc, Secondary$sess_unauthorised_percent, main = "",xlab = "Free School Meal %",ylab="Unauthorised Absence %")
abline(model2, col = "blue")  # Model line
points(Secondary$FSM_perc[largest_residuals_indices], Secondary$sess_unauthorised_percent[largest_residuals_indices], 
       col = "red", pch = 19, cex = 1.5)  # Highlight outliers
points(Secondary$FSM_perc[smallest_residuals_indices], Secondary$sess_unauthorised_percent[smallest_residuals_indices], 
       col = "red", pch = 19, cex = 1.5)  # Highlight outliers

bad <- Secondary[largest_residuals_indices,]
good <- Secondary[smallest_residuals_indices,]


Improver <- Key_Absence_Data %>%
  filter(time_period==202324)%>%
  filter(education_phase!="Special") %>%
  group_by(education_phase) %>%
  mutate(mean_unauth = mean(sess_unauthorised_percent)) %>%
  mutate(highunauth = ifelse(sess_unauthorised_percent>quantile(sess_unauthorised_percent,0.95),1,0)) %>%
  mutate(new_unauth = ifelse(highunauth==1,mean_unauth,sess_unauthorised_percent)) %>%
  mutate(old_abs = enrolments*sess_unauthorised_percent/100,
         new_abs = enrolments*new_unauth/100)

(sum(Improver$new_abs)/sum(Improver$enrolments))/(sum(Improver$old_abs)/sum(Improver$enrolments))-1

hist(Improver$old_abs)
hist(Improver$new_abs)
