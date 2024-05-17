#Pulse Amplitude Modulation (PAM) measurement analysis for photosynthetic efficiency (fv/fm)
#load packages
library(tidyverse)
library(readxl)
library(janitor)
library(ggplot2)
library(ggpubr)
library(broom)
library(AICcmodavg)
library(car)
library(lme4)
library(lmerTest)
library(nlme)
library(rstatix)

setwd("/Users/ninahmunk/Desktop/Projects/Acropora_Regeneration-main/")
#load initial PAM datasheet 
initial<- read_xlsx("PAM/Data/20230601_initial.xlsx", sheet= "20230601")%>%clean_names()%>%
  select(c(date,genotype, id, f0,fm,fv_fm))%>%
  rename("coral_id" = "id")
head(initial)

#use aggregate function to find the mean of values in other column(s) (f0, fm, fv/fm) based on factor levels in coral_id column 
initial_mean<-aggregate(.~coral_id, data = initial, mean)%>%mutate(timepoint = "initial")%>%select(-genotype)
head(initial_mean)

#load day 10 PAM datasheets - now separated by groups/date bc of how data was collected
day10_g1<- read_xlsx("PAM/Data/day10_PAM.xlsx", sheet= "20230622")%>%clean_names()%>%
  select(c(date, coral_id, f0,fm,fv_fm))
head(day10_g1)

day10_g2<- read_xlsx("PAM/Data/day10_PAM.xlsx", sheet= "20230623")%>%clean_names()%>%
  select(c(date, coral_id, f0,fm,fv_fm))
head(day10_g2)

day10_g3<- read_xlsx("PAM/Data/day10_PAM.xlsx", sheet= "20230624")%>%clean_names()%>%
  select(c(date, coral_id, f0,fm,fv_fm))%>%
  fill(coral_id) #Fills missing values in selected columns using the next or previous entry. This is useful in the common output format where values are not repeated, and are only recorded when they change
head(day10_g3)

#bind three dataframes with data from day 10 timepoint together - append 'add' rows
day10_bind <- bind_rows(day10_g1, day10_g2, day10_g3)

#get mean values of (f0, fm, fv/fm) for each coral id 
day10_mean<-aggregate(.~coral_id,data=day10_bind,mean)%>%mutate(timepoint = "day10")


final<- read_xlsx("PAM/Data/final_PAM.xlsx", sheet = "Sheet1")%>%clean_names()%>%
  select(c(date, coral_id, f0,fm,fv_fm))
final_mean<-aggregate(.~coral_id, data = final, mean)%>%mutate(timepoint = "final")

all.data<- rbind(initial_mean, day10_mean, final_mean)

samp.info<- read.csv("Respiration/Data/samp_info.csv")

full.data<- left_join(all.data, samp.info, by = "coral_id")

full.data<- full.data%>%mutate(wound = as.factor(wound))%>%
  mutate(genotype = as.factor(genotype))%>%
  mutate(temp = as.factor(temp))%>%
  mutate(timepoint = case_when(timepoint == "initial" ~ "0",
                               timepoint == "day10" ~ "10",
                               timepoint == "final" ~ "19"))

full.data$timepoint<- factor(full.data$timepoint)
full.data$genotype <- factor(full.data$genotype)
full.data$temp <- factor(full.data$temp)

quartz()
ggplot(full.data, aes(x = timepoint, y = fv_fm, shape = wound, col = temp))+
  stat_summary(fun = mean, geom = "point", size = 3 , position = position_dodge(width = 0.1))+
  ggtitle("ACR Photosynthetic Efficiency")+
  ylab('Photosynthetic Efficiency (Fv/Fm)') +
  xlab('Days')

ggplot(full.data, aes(x = timepoint, y = fv_fm, shape = wound, col = temp))+
  geom_point(size = .5, position = position_dodge(width = 0.5))+
  stat_summary(fun = mean, geom = "point", size = 5 , position = position_dodge(width = 0.5))+
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.5)
  )+
  ylab('Photosynthetic Efficiency (Fv/Fm)') +
  xlab('Days')

#linear models
pam.lmer<- lmer(fv_fm ~  timepoint + temp * wound + (1|coral_id), full.data ) # lower AIC
summary(pam.lmer)
pam.lmer2<- lmer(fv_fm ~  timepoint + temp + wound + (1|coral_id), full.data ) 
summary(pam.lmer2)

AIC(pam.lmer, pam.lmer2)

#ANOVA MODELS

one.way<- aov(fv_fm ~ temp, data = full.data)
summary(one.way)

two.way <- aov(fv_fm ~ temp + wound, data = full.data)
summary(two.way)

interaction <- aov(fv_fm ~ temp * wound, data = full.data)
summary(interaction)

block <- aov(fv_fm ~ wound * temp + timepoint, data = full.data)
summary(block)

block2 <- aov(fv_fm ~ wound * temp + timepoint + genotype, data = full.data) #this is the best fit model according to AIC
summary(block2)

#checking AIC
model.set <- list(one.way, two.way, interaction, block, block2)
model.names <- c("one.way", "two.way", "interaction", "block", "block2")

aictab(model.set, modnames = model.names) #block 2 model has the best fit according to lowest AIC value

#checking for normality
shapiro.test(full.data$fv_fm) #data is normal p=0.1379
#checking for homoscedasticity
par(mfrow=c(2,2))
plot(block2)
par(mfrow=c(1,1))
# Levene's Test for Homogeneity of Variance
leveneTest(fv_fm ~ temp, data = full.data) #variance does not significantly differ between groups

#tukey test, post hoc test 
tukey.block2<-TukeyHSD(block2)
tukey.block2

tukey.plot.aov<-aov(fv_fm ~ temp * wound + genotype, data=full.data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

ggplot(full.data%>%filter(!timepoint == "0"), aes(x = timepoint, y = fv_fm, shape = wound, col = temp))+
  geom_point(size = .5, position = position_dodge(width = 0.5))+
  stat_summary(fun = mean, geom = "point", size = 5 , position = position_dodge(width = 0.5))+
  stat_summary(
    fun.data = "mean_cl_normal", 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.5)
  )+
  ylab('Photosynthetic Efficiency (Fv/Fm)') +
  xlab('Days')

ggplot(data = full.data, aes(x = timepoint, y = fv_fm, col = temp))+geom_jitter(size = .5, alpha = .5)+
  stat_summary(
    fun.data = mean_se, 
    geom = "errorbar", 
    width = 0.2, 
    position = position_dodge(width = 0.2)
  ) +
  stat_summary( fun = "mean", geom = "point", position = position_dodge(width = 0.2))+ 
  stat_summary( fun = "mean", geom = "line", position = position_dodge(width = 0.2), linetype = "dashed", aes(group = temp))+
  ylab("Photosynthetic Efficiency (Fv/Fm)")+
  xlab("Timepoint (Day)")+
  scale_x_discrete(labels = c('0','10', '19'))+
  facet_wrap(~wound, labeller = labeller(wound = c("0" = "No Injury", "1" = "Fragmentation", "2" = "Abrasion")))+ 
  theme_classic()+
  scale_color_discrete(labels = c("Ambient (~28)", "Elevated (~30)"))+
  labs(color = "Temperature")


# REPEATED MEASURES TWO WAY ANOVA 

# summary statistics 
full.data %>%
  group_by(temp, wound, timepoint) %>%
  get_summary_stats(fv_fm, type = "mean_sd")

#check outliers - no extreme outliers
full.data %>%
  group_by(temp, wound, timepoint) %>%
  identify_outliers(fv_fm)

#check normality - 1 within group subject is not normal
full.data %>%
  group_by(temp, wound, timepoint) %>%
  shapiro_test(fv_fm)

ggqqplot(full.data, "fv_fm", ggtheme = theme_bw()) +
  facet_grid(temp + wound ~ timepoint, labeller = "label_both")

#change timepoint to numeric 

# Two-way ANOVA with repeated measures using aov
aov_result <- aov(fv_fm ~ temp * wound *as.numeric(timepoint) + Error(as.numeric(timepoint)/(coral_id)), data=full.data)
summary(aov_result)



# Two-way ANOVA with repeated measures using lme
lme_result <- lme(fv_fm ~ temp * wound, random = ~1 | timepoint/temp/wound, data = full.data)
summary(lme_result)




