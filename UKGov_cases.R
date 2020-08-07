library(dslabs)
library(tidyverse)
library(dplyr)
library(ggplot2)
#### UKGov confirmed cases ####
cases <- read.csv("~/Desktop/uk_covid19/2020-07-31/data_2020-Jul-31.csv")
View(cases)

#Latest total and total 1 week ago
cases[c(1,8),5]

#Sum past week
sum(cases[(1:7),5])

#Sum week before
sum(cases[(8:14),5])

####Local welsh cases####
w_cases <- read.csv("~/Desktop/uk_covid19/2020-07-31/Rapid COVID-19 surveillance data-8.csv", header=TRUE)
w_cases$Specimen.date <- 
  as.Date(w_cases$Specimen.date, format = "%d/%m/%Y")
str(w_cases)

#Weekly cases anglesey
a_cases <- filter(w_cases, Local.Authority == "Isle of Anglesey")
a_cases


#Latest week
latest_a <- sum(a_cases$Cases..new.[(6:12)])
latest_a
#Previous week
previous_a <- sum(a_cases$Cases..new.[(13:19)])
previous_a

#Weekly cases Gwynedd
g_cases <- filter(w_cases, Local.Authority == "Gwynedd")
g_cases
#Latest week
latest_g <- sum(g_cases$Cases..new.[(6:12)])
latest_g
#Previous week
previous_g <- sum(g_cases$Cases..new.[(13:19)])
previous_g

#Weekly cases Conwy
c_cases <- filter(w_cases, Local.Authority == "Conwy")
c_cases
#Latest week
latest_c <- sum(c_cases$Cases..new.[(6:12)])
latest_c
#Previous week
previous_c <- sum(c_cases$Cases..new.[(13:19)])
previous_c

#Latest AGC
latest_a + latest_g + latest_c
(latest_a + latest_g + latest_c)*0.32

#Previous AGC
previous_a + previous_g + previous_c
(previous_a + previous_g + previous_c)*0.32


#Combined AGC plot
newdf2 <- data.frame("Specimen date" = c_cases$Specimen.date[6:157],
                    "Conwy" = c_cases$Cases..new.[6:157],
                    "Anglesey" = a_cases$Cases..new.[6:157],
                    "Gwynedd" = g_cases$Cases..new.[6:157],
                    "Conwy_episodes" = c_cases$Testing.episodes..new.[6:157],
                    "Anglesey_episodes" = a_cases$Testing.episodes..new.[6:157],
                    "Gwynedd_episodes" = g_cases$Testing.episodes..new.[6:157])
library(ggthemes)
library(zoo)
#Add mutated vectors for AGC totals and rollmeans#
newdf2 <- newdf2 %>% 
  mutate("AGC" = (Conwy+Anglesey+Gwynedd))
newdf2 <- newdf2 %>% 
  mutate("roll_mean" = rollmean(AGC, 7, fill = NA))
newdf2 <- newdf2 %>% 
  mutate("AGC_episodes" = (Conwy_episodes +
                             Anglesey_episodes+
                             Gwynedd_episodes))
newdf2 <- newdf2 %>% 
  mutate("roll_mean_episodes" = rollmean(AGC_episodes, 7, fill = NA))
newdf2 <- newdf2 %>% 
  mutate("Per_Positivity" = ((AGC/AGC_episodes)*100))
newdf2 <- newdf2 %>% 
  mutate("roll_mean_positivity" = rollmean(Per_Positivity, 7, fill = NA))
View(newdf2)


agc_plot <- ggplot(data = newdf2, aes(x=Specimen.date, y=AGC))+
  geom_bar(stat="identity", fill="steelblue", color = "steelblue",
           width = 0.6)+
  labs(x = "Specimen date",
       y = "Anglesey/Gwynedd/Conwy")+
  geom_line(data = newdf2, aes(x=Specimen.date, y=roll_mean), 
            color = "royalblue4",
            size = 1)+
  ggtitle("Local cases by specimen date")

agc_plot
#Corresponding plot for testing episodes#
agc_episodes_plot <- ggplot(data = newdf2, aes(x=Specimen.date, y=AGC_episodes))+
  geom_bar(stat="identity", fill="steelblue", color = "steelblue",
           width = 0.6)+
  labs(x = "Episode date",
       y = "Anglesey/Gwynedd/Conwy")+
  geom_line(data = newdf2, aes(x=Specimen.date, y=roll_mean_episodes), 
            color = "royalblue4",
            size = 1)+
  ggtitle("Testing episodes by date")

agc_episodes_plot
#Plot for test positivity
agc_positivity <- ggplot(data = newdf2, aes(x=Specimen.date, y= Per_Positivity))+
  geom_bar(stat="identity", fill="steelblue", color = "steelblue",
           width = 0.6)+
  labs(x = "Episode date",
       y = "Anglesey/Gwynedd/Conwy")+
  geom_line(data = newdf2, aes(x=Specimen.date, y=roll_mean_positivity), 
            color = "royalblue4",
            size = 1)+
  ggtitle("Test % Positivity by date")

agc_positivity
####Zoe app####
(464+852+287)/3
(534.3333/1000000)*311200
311200/166.2845

####ONS####
ONS <- read.csv("~/Desktop/Coronavirus briefings/ons_infections.csv")
View(ONS)
ONS$date <- 
  as.Date(ONS$date, format = "%Y/%m/%d")
str(ONS)

ons_plot <- ggplot(data = ONS, aes(x = date, y = new_per_day))+
  geom_errorbar(aes(ymin = low_ci, ymax= high_ci), width=1)+
  geom_line()+
  geom_point(size = 3)
ons_plot
  
  

