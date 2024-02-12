library(lme4)
library(tidyverse)
library(Matrix)
library(ggplot2)
library("emmeans")

#read in the data
data<- read.csv("Respiration/Output/full.data.cleaned.csv")
############### Filtering data by R and P   #################################### #####
Rdata <- data%>% 
  filter(Rate%in% c("Respiration"))%>%
  mutate(wound = as.factor(wound), 
         temp = as.factor(temp),
         genotype = as.factor(genotype),
         date = as.factor(date))
Rdata$timepoint <- factor(Rdata$timepoint, levels = c("0", "1", "10", "19"))
# log transform umol.cm2.hr to fit assumptions of normality
Rdata$log_umol <- log(Rdata$umol.cm2.hr)

Pdata <- data%>% 
  filter(Rate%in% c("Gross Photosynthesis"))%>%
  mutate(wound = as.factor(wound), 
         temp = as.factor(temp),
         genotype = as.factor(genotype),
         date = as.factor(date))
Pdata$timepoint <- factor(Pdata$timepoint, levels = c("0", "1", "10", "19"))
# log transform umol.cm2.hr to fit assumptions of normality
Pdata$log_umol <- log(Pdata$umol.cm2.hr)
############### Linear Mixed Effects Model  #################################### #####

Rmodel <- lmer(log_umol ~ wound + temp + (1 | timepoint), data = Rdata)

summary(Rmodel)
# checking distribution of residuals
residuals <- resid(Rmodel)
hist(residuals)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

# Visual inspection of heteroscedasticity
plot(fitted(Rmodel), resid(Rmodel), xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# Alternatively, you can use the ggplot2 package for a smoother plot
ggplot(data.frame(fitted = fitted(Rmodel), residuals = resid(Rmodel)), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess") +
  labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs. Fitted")

# run Chi-Squared Type III ANOVA
anovaIII.results<-Anova(Rmodel, type="III")
print(anovaIII.results)
# Obtain  estimated marginal means (EMMs) for all levels of the predictors
emmeans_results <- emmeans(Rmodel, ~ wound + temp)
# Display the EMMs
print(emmeans_results)
# Pairwise comparisons for the levels within each predictor
pairwise_results <- pairs(emmeans_results)
# Display the pairwise comparisons
print(pairwise_results)


################## anova w/o model fitting ##################################### #####
one.way<- aov(umol.cm2.hr ~ wound, data = Rdata)
summary(one.way)

two.way <- aov(umol.cm2.hr ~ temp + wound, data = Rdata)
summary(two.way)

interaction <- aov(umol.cm2.hr ~ temp * wound, data = Rdata)
summary(interaction)

block <- aov(umol.cm2.hr ~ wound * temp + timepoint, data = Rdata)
summary(block)

block2 <- aov(umol.cm2.hr ~ wound * temp + timepoint + genotype, data = Rdata) #this is the best fit model according to AIC
summary(block2)

#checking AIC
model.set <- list(one.way, two.way, interaction, block, block2)
model.names <- c("one.way", "two.way", "interaction", "block", "block2")
aictab(model.set, modnames = model.names) #block 2 model has the best fit according to lowest AIC value

#checking for normality
shapiro.test(Rdata$umol.cm2.hr) #data is NOT normal p=5.129e-09

# Levene's Test for Homogeneity of Variance
leveneTest(umol.cm2.hr ~ temp, data = Rdata) #variance does not significantly differ between groups

#tukey test, post hoc test 
tukey.block2<-TukeyHSD(block2)
tukey.block2

tukey.plot.aov<-aov(fv_fm ~ temp, data=full.data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

#respiration data is not normal so trying non-parametric test 

#Kruskal-Wallis test
kw_result <- kruskal.test(umol.cm2.hr ~ temp, data = Rdata)

# Pairwise comparisons (adjust method as needed)
pairwise_result <- pairwise.wilcox.test(Rdata$umol.cm2.hr, Rdata$temp, p.adjust.method = "holm")
# Print results
print(kw_result)

# subsetting for wound 
kw_result_wound1 <- kruskal.test(umol.cm2.hr ~ temp, data = subset(Rdata, wound == "1"))
print(kw_result_wound1)
kw_result_wound2 <- kruskal.test(umol.cm2.hr ~ temp, data = subset(Rdata, wound == "2"))
print(kw_result_wound2)

# Fit linear regression model
R.lm <- lm(umol.cm2.hr ~ temp + wound + timepoint + genotype, data = Rdata)

# Display model summary
summary(R.lm)



### lily helps with some stats

library(stargazer)

resp<- filtered_data %>% 
  filter(Rate=="Respiration")


#resp model

#note temp is significant only when we also consider genotype also significant

full.model <- lm(umol.cm2.hr ~ temp*wound_status + genotype, data = full_data_clean)
summary(full.model)
stargazer(full_model, title = "Linear Regression Results", type = "text")

full_model <- lm(umol.cm2.hr ~ temp * wound_status + genotype, data = resp)

# Display the summary using stargazer
stargazer(full_model, title = "Linear Regression Results", type = "text")

# Using a multiple linear regression model, I explored how umol.cm2.hr is influenced by temperature (temp), wound status (wound_status), and genotype (genotype). I also considered the potential interaction between temperature and wound status (temp * wound_status)


