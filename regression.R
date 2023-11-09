library(gridExtra)
library(ggplot2)
library(corrplot)

data <- read.csv("./insurance.csv")
head(data)

dim(data)
str(data)
summary(data)
colSums(is.na(data))
table(duplicated(data))
data <- unique(data)
table(duplicated(data))

#plot 1 : Total Charges by Region

charges <- aggregate(data$charges, by=list(data$region), FUN=sum)
colnames(charges) <- c("region", "charges")
charges <- charges[order(charges$charges),]
ggplot(data=charges[1:4,], aes(x=reorder(region, charges), y=charges, fill=region)) + 
  geom_bar(stat='identity') +
  labs(x="Region", y="Total charges", title="Total Charges by Region") +
  scale_fill_brewer(palette="Blues") +
  theme_minimal()

#Plot 2: bmi distribution

plot1 <- ggplot(data, aes(x=bmi)) +
  geom_histogram(fill="orange", color="black") +
  labs(x="bmi", y="Frequency") +
  ggtitle("bmi histogram") +
  theme(plot.title = element_text(color="darkred"))
plot2 <- ggplot(data, aes(x=bmi)) +
  geom_histogram(aes(y=after_stat(density)), fill="orange", color="black") +
  geom_density(color="darkred") +
  labs(x="bmi", y="Density") +
  ggtitle("bmi Distplot") +
  theme(plot.title = element_text(color="darkred"))
grid.arrange(plot1, plot2, ncol=2)

#plot 3:Charges distribution

p1 <- ggplot(data, aes(x = charges)) + geom_histogram(binwidth = 5000, color = "black", fill = "maroon", alpha = 0.8) + theme_bw() + labs(title = "Charges - Actual values", x = "Charges", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
p2 <- ggplot(data, aes(x = log10(charges))) + geom_histogram(binwidth = 0.2, color = "black", fill = "maroon", alpha = 0.8) + theme_bw() + labs(title = "Charges - log. Transformed", x = "log(Charges)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5), axis.line = element_line(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
grid.arrange(p1, p2, ncol = 2, widths = c(1, 1.2))

data['log_charges']=log10(data['charges'])

#plot 4:Charges by age range

ageFirstInterval <- mean(data$charges[data$age >= 18 & data$age <= 28])
ageSecondInterval <- mean(data$charges[data$age >= 29 & data$age <= 39])
ageThirdInterval <- mean(data$charges[data$age >= 40 & data$age <= 50])
ageFourthInterval <- mean(data$charges[data$age >= 51 & data$age <= 64])
data_means <- data.frame(Age_Range = c("18-28 Age", "29-39 Age", "40-50 Age", "51-64 Age"),
                       Charges_Mean = c(ageFirstInterval, ageSecondInterval, 
                                        ageThirdInterval, ageFourthInterval))
ggplot(data_means, aes(x=Age_Range, y=Charges_Mean, fill=Age_Range)) +
  geom_bar(stat="identity", color="black") +
  labs(x="Age Range", y="Charges Mean") +
  ggtitle("Charges by age range") +
  theme(plot.title = element_text(color="darkgreen")) +
  scale_fill_manual(values=c("#96bb7c", "#74a063", "#4c7e4d", "#255f38"))  

#plot 5:Smoking Status by Gender

ggplot(data, aes(x = smoker, fill = smoker)) + 
  geom_bar() +
  labs(title = "Smoking Status by Gender", x = "Smoker") +
  scale_fill_manual(values = c("darkblue", "lightblue"), 
                    name = "Smoker", 
                    labels = c("Non-Smoker", "Smoker")) +
  facet_grid(. ~ sex) +
  theme_minimal()

#plot 6: Distribution of Charges by Smoking Status and Number of Children

ggplot(data, aes(x = smoker, y = charges, fill = factor(children))) +
  geom_boxplot(linetype = 1, size = 1.2, outlier.size = 3.2) +
  labs(title = "Distribution of Charges by Smoking Status and Number of Children",
       x = "Smoker",
       y = "Charges",
       fill = "Children") 
  theme_minimal()
  
#Correlation
  
unique(data['sex'])
unique(data['smoker'])
unique(data['region'])
dummy_data=data.frame(model.matrix(~data$region - 1, data = data))
df<-cbind(data,dummy_data)
df<- subset(df, select = -region)
df$sex <- as.numeric(factor(df$sex, levels = c("male", "female")))
df$smoker <- as.numeric(factor(df$smoker, levels = c("no", "yes")))
head(df)
corr_matrix <- cor(df)
corrplot(corr_matrix, method = "color")

#ANOVA 1-way
anovasmoker <- aov(log_charges ~ smoker, data = data)
summary(anovasmoker)      #reject

anovaage <- aov(log_charges ~ age, data = data)
summary(anovaage)         #reject

anovabmi <- aov(log_charges ~ bmi, data = data)
summary(anovabmi)         #reject

anovasex <- aov(log_charges ~ factor(sex), data = data)
summary(anovasex)        #fail to reject

anovaregion <- aov(log_charges ~ factor(region), data = data)
summary(anovaregion)     #fail to reject

anovachildren <- aov(log_charges ~ children, data = data)
summary(anovachildren)  #reject

#ANOVA 2-way

anova<-aov(log_charges~age*smoker,data=data)
summary(anova)

anova<-aov(log_charges~age*children,data=data)
summary(anova)

anova<-aov(log_charges~smoker*bmi,data=data)
summary(anova)

anova<-aov(log_charges~children*smoker,data=data)
summary(anova)

#linear regression
model1=lm(formula = log_charges ~ age, data = data)
summary(model1)
confint(model1,level=0.99)

model2=lm(formula = log_charges ~ factor(smoker), data = data)
summary(model2)
confint(model2,level=0.99)

#multi-linear regression
model3=lm(formula = log_charges ~ age+factor(smoker), data = data)
summary(model3)
confint(model3,level=0.99)

model4=lm(formula = log_charges ~ bmi+age+factor(smoker), data = data)
summary(model4)
confint(model4,level=0.99)

model5=lm(formula = log_charges ~ factor(smoker)+bmi+ age+factor(sex)+children+factor(region), data = data)
summary(model5)
confint(model5,level = 0.99)
anova(model5)

#try to use the fitted model to predict the charges
predictiondf<-subset(data, select = -c(charges,log_charges))
predictiondf
new_y <- predict(model5, newdata = predictiondf)
new_y

#plotting predicted vs actual charges
ggplot(data, aes(x = new_y, y = data$log_charges)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  xlab("Predicted Y") +
  ylab("Actual Y") +
  ggtitle("Predicted vs Actual Y")

