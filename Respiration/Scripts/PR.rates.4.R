library("ggplot2")
library("dplyr")
library('janitor')
library("tidyr")
library("emmeans")

#load in data
full_data_clean<- read.csv("Respiration/Output/full.data.cleaned.csv")
############### RESPIRATION FIGURES ############################################ #####
#day 0
R_data <- full_data_clean%>% 
  filter(Rate%in% c("Respiration"))%>%
  filter(timepoint == "0")%>%
  mutate(wound = as.factor(wound),
         timepoint = as.factor(timepoint))

shapiro.test(R_data$umol.cm2.hr)
qqnorm(R_data$umol.cm2.hr)
qqline(R_data$umol.cm2.hr)

quartz()


ggplot(data = R_data, aes(x = wound_status, y = umol.cm2.hr, shape = temp)) +
  stat_summary(
    fun = "mean", 
    geom = "point", 
    col = "black", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2),
    col = "black"
  ) +
  stat_summary(
    fun = "mean", 
    geom = "line", 
    aes(group = temp), 
    linetype = "dashed", 
    col = "black", 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 0 (pre-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')


ggplot(data = R_data, aes(x = wound, y = umol.cm2.hr, shape = temp)) +
  stat_summary(
    fun = "mean", 
    geom = "point", 
    col = "black", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2),
    col = "black"
  ) +
  stat_summary(
    fun = "mean", 
    geom = "line", 
    aes(group = temp), 
    linetype = "dashed", 
    col = "black", 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 0 (pre-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')

#day 1
R_data1<- full_data_clean%>%
  filter(Rate%in% c("Respiration"))%>%
  filter(timepoint == "1")%>%
  mutate(wound = as.factor(wound),
         timepoint = as.factor(timepoint))
quartz()
ggplot(data = R_data1, aes(x = wound, y = umol.cm2.hr, shape = temp)) +
  stat_summary(
    fun = "mean", 
    geom = "point", 
    col = "black", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2),
    col = "black"
  ) +
  stat_summary(
    fun = "mean", 
    geom = "line", 
    aes(group = temp), 
    linetype = "dashed", 
    col = "black", 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 1 (post-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')
#day 10
R_data2<- full_data_clean%>%
  filter(Rate%in% c("Respiration"))%>%
  filter(timepoint == "10")%>%
  mutate(wound = as.factor(wound),
         timepoint = as.factor(timepoint))
quartz()
ggplot(data = R_data2, aes(x = wound, y = umol.cm2.hr, shape = temp)) +
  stat_summary(
    fun = "mean", 
    geom = "point", 
    col = "black", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2),
    col = "black"
  ) +
  stat_summary(
    fun = "mean", 
    geom = "line", 
    aes(group = temp), 
    linetype = "dashed", 
    col = "black", 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 10 (post-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')

#day 19
R_data3<- full_data_clean%>%
  filter(Rate%in% c("Respiration"))%>%
  filter(timepoint == "19")%>%
  mutate(wound = as.factor(wound),
         timepoint = as.factor(timepoint))
quartz()
ggplot(data = R_data3, aes(x = wound, y = umol.cm2.hr, shape = temp)) +
  stat_summary(
    fun = "mean", 
    geom = "point", 
    col = "black", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2),
    col = "black"
  ) +
  stat_summary(
    fun = "mean", 
    geom = "line", 
    aes(group = temp), 
    linetype = "dashed", 
    col = "black", 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 19 (post-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')





ggplot(data = R_data, aes(x = wound, y = umol.cm2.hr, col= temp)) + geom_point()+
  stat_summary(
    fun = "mean", 
    geom = "point", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 0 (pre-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')

ggplot(data = R_data1, aes(x = wound, y = umol.cm2.hr, col= temp)) + geom_point()+
  stat_summary(
    fun = "mean", 
    geom = "point", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 1 (post-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')

ggplot(data = R_data2, aes(x = wound, y = umol.cm2.hr, col= temp)) + geom_point()+
  stat_summary(
    fun = "mean", 
    geom = "point", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 10 (post-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')



ggplot(data = R_data3, aes(x = wound, y = umol.cm2.hr, col= temp)) + geom_point()+
  stat_summary(
    fun = "mean", 
    geom = "point", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 19 (post-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')








R_data <- full_data_clean %>%
  filter(Rate %in% c("Respiration"))
filteredR_data$date <- as.factor(filteredR_data$date)
filteredR_data$wound <- as.factor(filteredR_data$wound)
filteredR_data$timepoint <- factor(filteredR_data$timepoint, levels = c("0", "1", "10", "19"))

summary(filteredR_data)

control_mean <- filteredR_data %>% 
  filter(temp == "A" & wound == 0) %>% 
  group_by(timepoint) %>% 
  reframe(respiration = mean(umol.cm2.hr))

difference_from_control <- filteredR_data %>% 
  mutate(treatment = paste(temp, wound, sep = "_")) %>% 
  filter(treatment != "A_0") %>% 
  left_join(control_mean, by = "timepoint") %>% 
  mutate(resp_difference = umol.cm2.hr - respiration)

quartz()

ggplot(filteredR_data, aes(x = timepoint, y = umol.cm2.hr, shape = wound, col = temp))+
  geom_point(size = .5, position = position_dodge(width = 0.5))+
  stat_summary(fun = mean, geom = "point", size = 5 , position = position_dodge(width = 0.5))+
  ggtitle("Mean Respiration Rate")+
  ylab('Rate (umol.cm2.hr)') +
  xlab('Days Post Wounding')

ggplot(filteredR_data, aes(x = timepoint, y = umol.cm2.hr, shape = wound, col = temp))+
  stat_summary(fun = mean, geom = "point", size = 3 , position = position_dodge(width = 0.7))+
  ggtitle("Mean Respiration Rate")+
  ylab('Rate (umol.cm2.hr)') +
  xlab('Days Post Wounding') 

ggplot(difference_from_control, aes(x = timepoint, y = resp_difference, shape = wound, col = temp))+
  geom_point(aes(x = timepoint, y = resp_difference, shape = wound, col = temp), size = .5, position = position_dodge(width = 0.5))+
  stat_summary(fun = mean, geom = "point", size = 5 , position = position_dodge(width = 0.5))+
  ggtitle("Difference in Mean Respiration Rate from Control Corals")+
  ylab('Rate (umol.cm2.hr)') +
  xlab('Days Post Wounding')

ggplot(difference_from_control, aes(x = timepoint, y = resp_difference, shape = wound, col = temp)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.7)) +
  ggtitle("Difference in Mean Respiration Rate from Control Corals")+
  ylab('Rate (umol.cm2.hr)') +
  xlab('Days Post Wounding') 
facet_wrap(~temp)
############### BOX PLOTS ###################################################### #####
quartz()
#plots
ggplot(filteredR_data, aes(x = wound, y = umol.cm2.hr, color = temp)) +
  geom_boxplot() +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type') +
  scale_x_discrete(labels = c("0" = "No Wound", "1" = "Fragment", "2" = "Abrasion")) +
  facet_wrap(~ Rate + timepoint, ncol = 2) # scales = "free_y"

ggplot(filteredGP_data, aes(x = wound, y = umol.cm2.hr, color = temp)) +
  geom_boxplot() +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type') +
  scale_x_discrete(labels = c("0" = "No Wound", "1" = "Fragment", "2" = "Abrasion")) +
  facet_wrap(~ Rate + timepoint, ncol = 2)

ggplot(filtered_data, aes(x = wound, y = umol.cm2.hr, color = temp))+
  geom_boxplot()+
  ylab('Rate (umol.cm2.hr)')+
  xlab('Wound Type')+
  scale_x_discrete(labels = c("0" = "No Wound", "1" = "Fragment", "2" = "Abrasion"))+
  facet_wrap(~Rate)


############### PHOTOSYNTHESIS FIGURES ######################################### #####
P_data <- full_data_clean%>% 
  filter(Rate%in% c("Gross Photosynthesis"))%>%
  filter(timepoint == "0")%>%
  mutate(wound = as.factor(wound),
         timepoint = as.factor(timepoint))

shapiro.test(P_data$umol.cm2.hr)
qqnorm(R_data$umol.cm2.hr)
qqline(R_data$umol.cm2.hr)

quartz()

ggplot(data = P_data, aes(x = wound, y = umol.cm2.hr, shape = temp)) +
  stat_summary(
    fun = "mean", 
    geom = "point", 
    col = "black", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2),
    col = "black"
  ) +
  stat_summary(
    fun = "mean", 
    geom = "line", 
    aes(group = temp), 
    linetype = "dashed", 
    col = "black", 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 0 (pre-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')

#day 1
P_data1<- full_data_clean%>%
  filter(Rate%in% c("Gross Photosynthesis"))%>%
  filter(timepoint == "1")%>%
  mutate(wound = as.factor(wound),
         timepoint = as.factor(timepoint))
quartz()
ggplot(data = P_data1, aes(x = wound, y = umol.cm2.hr, shape = temp)) +
  stat_summary(
    fun = "mean", 
    geom = "point", 
    col = "black", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2),
    col = "black"
  ) +
  stat_summary(
    fun = "mean", 
    geom = "line", 
    aes(group = temp), 
    linetype = "dashed", 
    col = "black", 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 1 (post-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')
#day 10
P_data2<- full_data_clean%>%
  filter(Rate%in% c("Gross Photosynthesis"))%>%
  filter(timepoint == "10")%>%
  mutate(wound = as.factor(wound),
         timepoint = as.factor(timepoint))
quartz()
ggplot(data = P_data2, aes(x = wound, y = umol.cm2.hr, shape = temp)) +
  stat_summary(
    fun = "mean", 
    geom = "point", 
    col = "black", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2),
    col = "black"
  ) +
  stat_summary(
    fun = "mean", 
    geom = "line", 
    aes(group = temp), 
    linetype = "dashed", 
    col = "black", 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 10 (post-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')

#day 19
P_data3<- full_data_clean%>%
  filter(Rate%in% c("Gross Photosynthesis"))%>%
  filter(timepoint == "19")%>%
  mutate(wound = as.factor(wound),
         timepoint = as.factor(timepoint))
quartz()
ggplot(data = P_data3, aes(x = wound, y = umol.cm2.hr, shape = temp)) +
  stat_summary(
    fun = "mean", 
    geom = "point", 
    col = "black", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2),
    col = "black"
  ) +
  stat_summary(
    fun = "mean", 
    geom = "line", 
    aes(group = temp), 
    linetype = "dashed", 
    col = "black", 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 19 (post-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')



ggplot(data = P_data3, aes(x = wound, y = umol.cm2.hr, col= temp)) + geom_point()+
  stat_summary(
    fun = "mean", 
    geom = "point", 
    size = 5, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2)
  ) +
  ggtitle("Day 19 (post-wounding)") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Wound Type')











filteredP_data <- full_data_clean %>%
  filter(Rate %in% c("Gross Photosynthesis"))
filteredP_data$date <- as.factor(filteredP_data$date)
filteredP_data$wound<- as.factor(filteredP_data$wound)
filteredP_data$timepoint <- factor(filteredP_data$timepoint, levels = c("0", "1", "10", "19"))

control_mean1 <- filteredP_data %>% 
  filter(temp == "A" & wound == 0) %>% 
  group_by(timepoint) %>% 
  reframe(photosynthesis = mean(umol.cm2.hr))

difference_from_control1 <- filteredP_data %>% 
  mutate(treatment = paste(temp, wound, sep = "_")) %>% 
  filter(treatment != "A_0") %>%
  left_join(control_mean1, by = "timepoint") %>% 
  mutate(photo_difference = umol.cm2.hr - photosynthesis)

ggplot(filteredP_data, aes(x = timepoint, y = umol.cm2.hr, shape = wound, col = temp))+
  stat_summary(fun = mean, geom = "point", size = 3 , position = position_dodge(width = 0.7))+
  ggtitle("Mean Photosynthesis Rate") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Days Post Wounding') 

ggplot(difference_from_control1, aes(x = timepoint, y = photo_difference, shape = wound, col = temp))+
  geom_point(aes(x = timepoint, y = photo_difference, shape = wound, col = temp), size = .5, position = position_dodge(width = 0.5))+
  stat_summary(fun = mean, geom = "point", size = 5 , position = position_dodge(width = 0.5))+
  ggtitle("Difference in Mean Photosynthesis Rate from Control Corals")+
  ylab('Rate (umol.cm2.hr)') +
  xlab('Days Post Wounding')

ggplot(difference_from_control1, aes(x = timepoint, y = photo_difference, shape = wound, col = temp)) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.7)) +
  ggtitle("Difference in Mean Photosynthesis Rate from Control Corals") +
  ylab('Rate (umol.cm2.hr)') +
  xlab('Days Post Wounding') +
  facet_wrap(~temp)
quartz()



############### P:R ########################################################### #####
ratio<- read.csv("Respiration/Output/PR_Ratio.csv")%>%
  mutate(wound = as.factor(wound),
         timepoint = as.factor(timepoint))

ggplot(ratio, aes(x = timepoint, y = PR_Ratio, col = temp))+ #geom_point(position = position_dodge(width = 0.5))+
  stat_summary(fun = mean, geom = "point", size = 3 , position = position_dodge(width = 0.2))+
  stat_summary(
    fun = "mean", 
    geom = "line", 
    aes(group = temp), 
    linetype = "dashed", 
    col = "black", 
    position = position_dodge(width = 0.2)
  )+
  ggtitle("Mean P:R") +
  ylab('P:R') +
  xlab('Days Post Wounding')+
  facet_wrap("wound")
