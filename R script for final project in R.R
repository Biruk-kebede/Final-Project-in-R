library(tidyverse)
library(broom)
library(psych)
library(stargazer)
library(car)
library(knitr)
library(ggfortify)

#working directory adjusted
setwd("/Users/biruk/Dropbox/Final Project in R")

#importing the data
given_dataset<- read_csv(file="/Users/biruk/Dropbox/Final Project in R/xAPI-Edu-Data.csv")


#Exploratory Data Analysis -----------------------------------------------

# study the data using summary function 
summary(given_dataset)

## Exclude variables not to be used in this project       
performance_data<-given_dataset %>% 
 select(-PlaceofBirth, -StageID, -GradeID, -SectionID, -Semester, -ParentAnsweringSurvey, -StudentAbsenceDays)

#missing values  
sapply(performance_data,function(x) sum(is.na(x)))
# no missing value in the dataset


# Examine the possible scores for qualitative variables in the given dataset   
performance_data %>% distinct(gender)
performance_data %>% distinct(Topic)
performance_data %>% distinct(Relation)
performance_data %>% distinct(ParentschoolSatisfaction)
performance_data %>% distinct(Class)
performance_data %>% distinct(NationalITy)


##Inform the nature of the qualitative variables to R
subject <-factor(performance_data$Topic, levels = c("English", "Spanish", "French", "Arabic", "IT", "Math", "Chemistry", "Biology", "Science", "History", "Quran", "Geology"))
resp_parent <- factor(performance_data$Relation, levels = c("Mum", "Father"))
Parent_Sats <- ordered(performance_data$ParentschoolSatisfaction, levels = c("Bad", "Good"))
final_grade <- ordered(performance_data$Class, levels = c("L", "M", "H"))
sex <- factor(performance_data$gender, levels = c("F", "M"))
citizenship <- factor(performance_data$NationalITy,levels = c("KW", "lebanon", "Egypt", "SaudiArabia", "USA", "Jordan", "venzuela", "Iran", "Tunis", "Morocco", "Syria", "Palestine", "Iraq", "Lybia"))


## Transformed the dataset so that qualitavie variables will be changed to numberc form
stud_performance_data<-performance_data %>% 
  mutate (subject = as.integer(factor(Topic, levels = c("English", "Spanish", "French", "Arabic", "IT", "Math", "Chemistry", "Biology", "Science", "History", "Quran", "Geology"))), 
          resp_parent = as.integer(factor(Relation, levels = c("Mum", "Father"))),
          Parent_Sats = as.integer(factor(ParentschoolSatisfaction, levels = c("Bad", "Good"))), 
          final_grade = as.integer(factor(Class, levels = c("L", "M", "H"))),
          citizenship = as.integer(factor(NationalITy, levels = c("KW", "lebanon", "Egypt", "SaudiArabia", "USA", "Jordan", "venzuela", "Iran", "Tunis", "Morocco", "Syria", "Palestine", "Iraq", "Lybia"))),
          sex = as.integer(factor(gender, levels = c("F", "M")))) %>% 
           as_tibble()


## save the transformed data
write_csv(stud_performance_data, 'stud_performance_data.csv')
View(stud_performance_data)


## check outliers for all the four continous variables using boxplot
boxplot(stud_performance_data$Discussion, stud_performance_data$raisedhands, stud_performance_data$VisITedResources, stud_performance_data$AnnouncementsView, horizontal=TRUE)

#As can be clearly seen from the boxplots above all of the four continous variables in the dataset donot have outliers

#Besides the boxplots, boxplot statistics were also checked that no outliers found in all the four variables mentioned above. Just to demostrate one of them 
boxplot.stats(stud_performance_data$Discussion)


# visualization of the variables in the dataset
## students vs subjects
pie(table(stud_performance_data$Topic))

## Participants citizenship composition 
barplot(table(stud_performance_data$NationalITy), xlab = 'participants citizenship', ylab = 'Number Of Participants')

## check for normality of quantitative variables in the dataset
plot (density(stud_performance_data$raisedhands))
plot(density(stud_performance_data$Discussion))
hist(stud_performance_data$VisITedResources)
hist(stud_performance_data$AnnouncementsView)
#As can be seen from the plots above, the variables "raisedhands", "Discussion", "VisITedResources" and "AnnouncementsView" are not normally distributed  

## raising hands vs discussion 
with(stud_performance_data,plot(Discussion, raisedhands))

# Test for association  ---------------------------------------------------

## is there association between responsible parent and their satisfaction?
table(resp_parent, Parent_Sats)
chisq.test(table(resp_parent, Parent_Sats))
### there is significant association between responsible parent and their satisfaction 

# plot the result 
plot(resp_parent, Parent_Sats, ylab = "Parents' satisfaction", xlab = "Responsible parent for students education")
##larger proportion of participants from mum group have good satisfaction than fathers group


# is there association between parents' satisfaction and their children's final grade score?
table(Parent_Sats, final_grade)
chisq.test(table(Parent_Sats, final_grade))
# stastically significant association is found between parents' satisfaction and their chilfen's final grade 

# show the result in plot
plot(Parent_Sats, final_grade, ylab = "final_grade", xlab = "Parent_Sats")
## the result depicted that there is relationship between parents satisfaction and their childen's final grade score
###larer proporsion of students with good parental satisfaction scored better grade than students whose parents have bad satisfaction. 


#is there association between responsible parent and students' final grade?
table(resp_parent, final_grade)
chisq.test(table(resp_parent, final_grade))
## chisqured test indicated that there is a significant association between responsible parent and their children's  

 ## result plotted 
plot(resp_parent, final_grade, ylab = "final_grade", xlab = "resp_parent")
## relatively speaking larger proporstion of students followed by their mums scored high in the final grade compare with their counterpars 
## in addition, students who scored lower grade came from families whose education is followed by their fathers than their mums 


# is there association between subject taught and final grade scored?
table(subject, final_grade)
chisq.test(table(subject, final_grade))
## chisqured test showed that there is difference in final grade scored as a function of subjects. 

###plot the result
plot(subject, final_grade, ylab = "final_grade", xlab = "subject")
### the plot clearly depicted that no students from Geograph class scored lower grade
### bigger proporsion of stdents scored low final grade in IT class compared to proporsion of students who scored same in other subject group
### bigger proporsion of students from bilogy group scored high grade compared with proporsion of students who scored same in other subject group

# Test for group difference---------------------------------------------------
 #Does sex make difference in students' handraising? 
    # Ho: mean of handraising of female is equal to male

## Examine the assumptions for parametric test 
# Independent observations is automatic as the two observations are completely independent (male and female)
#Based on the central limit theorem if the sample size is large enough (n > 30), we can ignore the distribution of the data and use parametric tests.
  ## Do box plot to examine the equivalence of variance between the two groups
boxplot(stud_performance_data$raisedhands~stud_performance_data$sex) 

# the boxplot indicates that the variances of the two groups are almost the same. This can also be verified usng Levene's Test
leveneTest(stud_performance_data$raisedhands~sex) 

# it is confirmed that the difference in variance is not significant  

## Now conduct t-test to verify/refute the hypothesis
  t.test(stud_performance_data$raisedhands~sex, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
## to include SD statistics in the report calculate it 
sd(stud_performance_data$raisedhands[sex=="F"])
sd(stud_performance_data$raisedhands[sex=="M"])
   
  
##correlation--------------------------------------------------------

# Is there a statistically significant relationship between announcement view and participation in discussion?

## check for the density distribution of the variables to check for normality to select appropriate technique of correlation
plot((density(stud_performance_data$AnnouncementsView)), main="Kernel Density of announcement view distribution")
plot((density(stud_performance_data$Discussion)), main="Kernel Density of Discussion distribution")
## the plots indicated that both variables are not normally distributed therefore the case cannot be handled by Pearson product-moment corr under this condition

## transform the scores if it helps to get normal
log_Discussion<-log(stud_performance_data$Discussion)
qqnorm(log_Discussion, col='blue')
qqline(log_Discussion, col ="red")

log_AnnouncementsView<-log(stud_performance_data$AnnouncementsView + 1) # there is score 0 in log_AnnouncementsView variable
qqnorm(log_AnnouncementsView, col='blue')
qqline(log_AnnouncementsView, col ="red")
## still the transformation of the scores in both variable indicated that they are normally distributed after transformation

## plot to examine monotonic relationship between the two variables to use Kendall's tau. 
plot(stud_performance_data$Discussion, stud_performance_data$AnnouncementsView, main="scatterplot", las=1)
# the plot shows that it fulfilled the assumption of monotonic relationship 

# conduct correlation test
cor.test(stud_performance_data$Discussion,  stud_performance_data$AnnouncementsView, method="kendall")

## correlation test depicted that there is signficant correlation between the two variables (tau=0.2851579, p<.0001)

## plot the result by using different color for gender just for the sake of informativity of the plot

stud_performance_data %>% 
  ggplot() +
  aes(x = AnnouncementsView, y = Discussion) +
  geom_point(aes(color = gender), size = 3) +
  geom_smooth(method = "lm")



# Regression Analysis Part ------------------------------------------------
  
# Do resource visiting and announcement viewing significantly predict students hand raising behavior? 

## Before conducting linear regression check the assumptions of it
#the relationship among the variables used in the hypothesis was studied using plot matrix (used to check the linearity of relationship between IVs with DV)
  plot(stud_performance_data[5:7], pch=16, col="blue", main="Matrix Scatterplot of raisedhands, VisITedResources, AnnouncementsView")

##As can be seen from the plot matrix the assumption of linear relationship between IVS and DV is fulfilled.

## The next step could be to check for multicollinearity of independent variables. 
## It would be better to start this process by looking at correlation matrix among the variables 
library(corrplot)
check_cor = cor(stud_performance_data[5:7])
corrplot(check_cor, method = "number")
## As can be seen from the correlation matrix the correlation between the two independent variables is 0.59, which is below the cutoff (.8) 


#Fit a Linear model and continue testing its assumptions  
lm1<-lm(raisedhands~VisITedResources + AnnouncementsView, data=stud_performance_data)
summary(lm1)

# now assess multicollinearity using the variance inflaction factor (VIF)
car::vif(lm1)
# The values look ok as they are not very large. 

# However as the average of VIF is larger than 1, it seems that multicollineratity is a bit biasing the model
mean(car::vif(lm1))

#So, look at the tolerance.
1/car::vif(lm1)
# The tolerance value seems it is good as it is in the range of 0 and 1.

# now, assess the independence of residuals
car::dwt(lm1)
# The result indicates that there seems autocorrelation between the the two independent variables, so the residuals may not be independent.

# To check heteroscetasticity inspect the residual diagnostic plots
autoplot(lm1, which = 1:6, label.size = 3)

#Examine the outliers if there is concrete reason to eliminate theme. There is no concrete reason to eliminate them. 
stud_performance_data %>% 
  slice(c(96, 178, 187, 345, 382))


#Now, fit the second model which consists of both predictors and their interaction together
lm2<-lm(raisedhands~VisITedResources*AnnouncementsView, data=stud_performance_data)
summary(lm2)
# the p value indicates that the interaction between the two independent variables didn't significantly contribute to the model.


# The VIF of the model indicated that the interaction between the two variables has a score of 15.007, which is larger than 10. So, multicollinearity is an issue here.  
car::vif(lm2)

# In addition, the the average of VIF score (9.273) confirms that multicollineratity is biasing the model.
mean(car::vif(lm2))

## The tolerance also indicates that except visiting resources the other two variables (announcement and interaction between announcement view and visiting resource) have not tolerable.1/car::vif(lm2)

# Measuring the independence of residuals
car::dwt(lm2)
# It seems that the model has some significant autocorrelation, so the residuals are not independent

# To check heteroscetasticity inspect the residual diagnostic plots

autoplot(lm2, which = 1:6, label.size = 3)

# Fit the third model which includes only the interaction between the two predictors and continue testing linear regression model assumptions 
lm3<-lm(raisedhands~VisITedResources:AnnouncementsView, data=stud_performance_data)
summary(lm3)

# In this model the p value indicates that the interaticon of the independent variables significantly contribute to the model.

# Assess the independence of residuals in the model
car::dwt(lm3)
# The result indicates that there seems autocorrelation between the the two independent variables, so the residuals may not be independent again.

# To check heteroscetasticity inspect the residual diagnostic plots

autoplot(lm3, which = 1:6, label.size = 3)


# confidence intervals for parameters
confint(lm1, level = 0.95)
confint(lm2, level = 0.95)
confint(lm3, level = 0.95)

## Model selection
#### comparing models using broom::glance()
glance(lm1)
glance(lm2)
glance(lm3)
# based on the principle to choose a model with the smallest logLik, AIC, and BIC with the same df
## the first model (lm1) is selected


#### to cross check and confirm model selection annova function can be used
anova(lm1, lm2)
anova(lm1, lm3)
### anova test for model comparision showed that there is not signifivcant difference between the first and the seond model(F=0.5674, df=1)
### F value indicated that there is significant difference between the first and third model in favor of the first.
### threfore, anova analysis aslo comfirmed that the first model is the winner. 

#normal distribution of resuduals is checked for the wining model
stud_performance_data %>% 
  augment(lm(raisedhands~VisITedResources + AnnouncementsView, data = .), .) %>% 
  ggplot() +
  aes(.resid) +
  geom_histogram(bins = 10)
# the residuals of the selected model are approximatelly normally distributed!!

# this can be confirmed using statistical method
stud_performance_data %>% 
  augment(lm(raisedhands ~ VisITedResources*AnnouncementsView, data = .), .) %>% 
  pull(.resid) %>% 
  shapiro.test(.)
# The Shapiro-Wilks test confirmed that the residuals are normally distributed (p=0.07819)


# Explore the jointed effect of resources visiting and announcement vewing on handrising using plot
coplot(raisedhands~VisITedResources|AnnouncementsView, panel = panel.smooth, stud_performance_data)
coplot(raisedhands ~AnnouncementsView|VisITedResources, panel = panel.smooth, stud_performance_data)










##By loading important packages plot the result of the model in 3D.

library(plotly)
plot_ly(stud_performance_data, y= ~stud_performance_data$raisedhands, x= ~stud_performance_data$AnnouncementsView, z= ~stud_performance_data$VisITedResources)

install.packages("scatterplot3d")
library(scatterplot3d)
install.packages("rgl")
library(rgl)
library(car)

scatter3d(y=stud_performance_data$raisedhands, x=stud_performance_data$AnnouncementsView, z=stud_performance_data$VisITedResources)











# report the results of regression in table
Models_table_html <-
  stargazer(lm1,
            lm2,
            lm3,
            coef = list(lm1$standardized.coefficients,
                        lm2$standardized.coefficients,
                        lm3$standardized.coefficients),
            
            title = "Model comparison",
            dep.var.labels = "Raising Hand",
            align = TRUE,
            ci = TRUE,
            df = TRUE,
            digits = 2,
            type = "html")

write_lines(Models_table_html, "Models_table_html")


stargazer(lm1, lm2, lm3, title = "Model comparison", align = TRUE, type = "text")

# also add standardized coefficients
# transform the non-standardized values using the lm.beta package
install.packages("lm.beta")
library(lm.beta)

# Create standardized versions from all objects
lm1_std <- lm.beta(lm1)
lm2_std <- lm.beta(lm2)
lm3_std <- lm.beta(lm3)

# explicitly tell stargazer which coefficients we want to see
results_table_html <-
  stargazer(lm1_std,
            lm2_std,
            lm3_std,
            coef = list(lm1_std$standardized.coefficients,
                        lm2_std$standardized.coefficients,
                        lm3_std$standardized.coefficients),
                        
            title = "Model comparison",
            dep.var.labels = "Raising Hand",
            align = TRUE,
            ci = TRUE,
            df = TRUE,
            digits = 2,
            type = "html")

write_lines(results_table_html, "results_table.html")


