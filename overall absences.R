#Absence data
library(tidyverse)
library(ggrepel)

Absences_Raw <- read_csv("data/1_absence_2term_school.csv")
View(Absences_Raw)

boxplot(Absences_Raw$sess_overall_percent)

hist(Absences_Raw$sess_overall_percent)

unique(Absences_Raw$time_period)

Key_Absence_Data <- Absences_Raw %>%
  select(time_period,school_urn,education_phase,num_schools,enrolments,sess_overall_percent,sess_authorised_percent,sess_unauthorised_percent,enrolments_pa_10_exact_percent,enrolments_pa_50_exact_percent) %>%
  mutate(Dateyear = as.Date(paste0(substr(as.character(time_period),1,4),"-12-31")))

Key_Absence_Data %>%
  mutate(year = as.numeric(substr(as.character(time_period),3,6))) %>%
  ggplot() +
  geom_boxplot(outlier.size =0.1,mapping = aes(x=year,group=year,y=sess_overall_percent)) +
  facet_wrap(~education_phase) +
  custom_ggplot_theme() +
  scale_x_continuous(breaks=c(1617,1718,1819,1920,2021,2122,2223,2324)) +
  labs(x="",y="",title= "Overall absence rate, by type of school")

Absence_barchart <- Key_Absence_Data %>%
  mutate(year = paste0(substr(as.character(time_period),3,4),"-",substr(as.character(time_period),5,6))) %>%
  group_by(Dateyear,year,education_phase) %>%
  summarise(absence = mean(sess_overall_percent),
            unauth = mean(sess_unauthorised_percent))

covid <- data.frame(Dateyear = rep(as.Date("2019-12-31"),4),
                    year = rep("19-20",4),
                    education_phase = c("Special","State-funded secondary","State-funded primary","Overall"),
                    absence = c(NA,NA,NA,NA),
                    unauth = c(NA,NA,NA,NA))

overall <- Key_Absence_Data %>%
  mutate(year = paste0(substr(as.character(time_period),3,4),"-",substr(as.character(time_period),5,6))) %>%
  group_by(Dateyear,year) %>%
  summarise(absence = mean(sess_overall_percent),
            unauth = mean(sess_unauthorised_percent)) %>%
  mutate(education_phase = "Overall")

Absence_barchart <- bind_rows(Absence_barchart,covid,overall) %>%
  ungroup() %>%
  mutate(label = if_else(Dateyear == max(Dateyear)&as.character(education_phase)!="Overall", as.character(education_phase), NA_character_))


Absence_barchart$education_phase = factor(Absence_barchart$education_phase,levels=c("Special","State-funded secondary","State-funded primary"))

barchart(Absence_barchart,year,education_phase,absence,type="cluster",flipped=F) +
  labs(fill="",x="",y="%",title = "Average absence by school type, by academic year") 
  
multi_line_timeseries(Absence_barchart,Dateyear,absence,education_phase) +
  scale_y_continuous(expand=c(0,0),limits = c(0,20)) +
  scale_x_date(limits=c(as.Date("2016-10-01"),as.Date("2026-11-11")),breaks = sort(unique(Absence_barchart$Dateyear)),labels = function(x){paste0(year(x),"-",year(x)-1999)}) +
  labs(y="%",x="",colour="",title = "Average total absence by school type, by academic year") +
  geom_label_repel(aes(label = label,colour=education_phase),
                   nudge_x = c(100,100,100),
                   na.rm = TRUE) +
  theme(legend.position = "none")

multi_line_timeseries(Absence_barchart,Dateyear,unauth,education_phase) +
  scale_y_continuous(expand=c(0,0),limits = c(0,20)) +
  scale_x_date(limits=c(as.Date("2016-10-01"),as.Date("2026-11-11")),breaks = sort(unique(Absence_barchart$Dateyear)),labels = function(x){paste0(year(x),"-",year(x)-1999)}) +
  labs(y="%",x="",colour="",title = "Average unauthorised absence by school type, by academic year") +
  geom_label_repel(aes(label = label,colour=education_phase),
                   nudge_x = c(100,100,100),
                   na.rm = TRUE) +
  theme(legend.position = "none")
