#POL342 Final Project Code: Media Fragmentation (IV) and Political Polarization (DV).
#Date Started: March 23, 2026
#Due: April 8, 2026
#Name: Lama Ammane

rm(list=ls())

######## Part 1: Set up dataset and clean variables ########

#Load Libraries
library(tidyverse)
install.packages("broom")
library(broom)
install.packages("modelsummary")
library(modelsummary)
install.packages("pandoc")
install.packages("sjPlot")
library(sjPlot)
install.packages("modelsummary") # regression table
install.packages("pandoc") # to save regression table to Word doc output
install.packages("modelsummary", dependencies = TRUE)


#Load variables from V-Dem dataset
vars <- c("year", "country_name", "country_id",
          "v2smmefra", "v2cacamps", "v2x_regime", "e_gdppc",
          "v2mecenefi", "v2expathhg")

df <- read.csv("Project/VDem/vdem2000_2023.csv") |>
  select(vars)|>
  filter(year==2019)

#Prepare to drop NAs.

#See unique values of each variable to see if any NAs appear.
table(is.na(df$v2smmefra)) #No.
table(is.na(df$v2cacamps)) #No.
table(is.na(df$v2x_regime)) #No.
table(is.na(df$e_gdppc)) #5 NAs, drop.
table(is.na(df$v2mecenefi)) #No.
table(is.na(df$v2expathhg)) #65 NAs to drop.

#Drop all rows with NA.
df2 <- df |> na.omit()
table(is.na(df2$v2expathhg)) #There are now 113 observations/ countries in the analysis.

# Treat this variable (head of government type of appointment) as nominal:
df2$v2expathhg <- as.factor(df2$v2expathhg)

######## Part 2: Descriptive Statistics of Variables ########

#1: Central tendency and variance of all variables
summary(df2$v2smmefra) #Interval variable.
sd(df$v2smmefra)

summary(df2$v2cacamps) #Interval variable.
sd(df$v2cacamps)

summary(df2$v2x_regime) #Ordinal variable for regime type.
IQR(df2$v2x_regime) #Use interquartile range instead of standard deviation.

summary(df2$e_gdppc) #Interval variable.
sd(df2$e_gdppc)

summary(df2$v2mecenefi) #Interval variable.
sd(df2$v2mecenefi)

summary(df2$v2expathhg) #Nominal variable. Most common category is 6.
IQR(df2$v2expathhg)

#Univariate statistics: Frequency (Histograms)

#Histogram of media fractionalization.
mediahis <- ggplot(data=df2,aes(x=v2smmefra)) +
  geom_histogram() +
  xlab("Media Fractionalization (v2smmefra)") +
  labs(
  title="Media Fractionalization Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

mediahis

ggsave(filename="mediahis.png", plot=mediahis)

#Histogram of polarization.
polhis <- ggplot(data=df2,aes(x=v2cacamps)) +
  geom_histogram() +
  xlab("Political Polarization (v2cacamps)") +
  labs(
    title="Political Polarization Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

polhis

ggsave(filename="polhis.png", plot=polhis)

#Bar graph of regime types (essentially a histogram for categories)
regimebar <- ggplot(data = df2, aes(x=factor(v2x_regime), fill=factor(v2x_regime))) +
  geom_bar() +
  labs(
    title = "Regimes of the World (RoW) Frequency",
    subtitle = "Measuring the frequency of different regime types in the 2019 sample.",
    fill = "Regimes of the World Measure") +
  xlab("Regime Type") +
  ylab("Number of Countries") +
  scale_fill_manual(
    values = c("#FF746C", "#F5B74C", "#87D691", "#9EB9F7"),
    labels = c("Closed Autocracy", "Electoral Autocracy", "Electoral Democracy", "Liberal Democracy")) +
    scale_y_continuous(breaks=c(seq(-5,100,by=5)))

regimebar

ggsave(filename="regimebar.png", plot=regimebar)

table(df2$v2x_regime)

######## Part 3: Crosstab of polarization and media fractionalization ########

crosstab1 <- df2 |>
  mutate(v2smmefra = cut(v2smmefra, 
                         breaks = 4),
         v2cacamps = cut(v2cacamps, 
                         breaks = 4))

tab_xtab(var.row = crosstab1$v2smmefra,
         var.col = crosstab1$v2cacamps,
         show.col.prc = TRUE,
         file="IVandDVcrosstab.html")

######## Part 4: Data Visualization ########

#Plot independent variable (media fractionalization - v2smmefra)
#and dependent variable (v2cacamps).
#Both are interval according to codebook, so use a scatter plot.

scatter <- ggplot(data=df2,aes(x=v2smmefra,y=v2cacamps)) +
  geom_point((aes(color=factor(v2x_regime))),size=1,alpha=.7) +
  geom_smooth(method="lm",size=.5) +
  theme_bw() +
  labs(
    title="How Media Fragmentation Affects Political Polarization in 2019",
    subtitle = "Political polarization ranks to which degree political differences
affect social relationships beyond political discussions.") +
  xlab("Media Fractionalization") +
  ylab("Political Polarization") +
  scale_colour_manual(name=" Regimes of the World Measure",
                      values = c("red","orange","green","blue"),
                      labels = c("Closed Autocracy","Electoral Autocracy", "Electoral Democracy", "Liberal Democracy"))

scatter

ggsave(filename = "MediaFrag_Polarization_Scatter.png",
       plot = obj1,
       width=8, height=5)

#Box plot of polarization levels across regime types.

boxplot <- ggplot(df2, aes(x=factor(v2x_regime), y=v2cacamps)) +
  geom_boxplot(width=0.3, fill="orange") +
  geom_point(stat="summary", fun=mean, shape=21, fill="blue",size=2) +
  scale_x_discrete(labels=c("Closed Autocracy","Electoral Autocracy","Electoral Democracy","Liberal Democracy")) +
  theme_bw() +
  xlab("Regime Type") +
  ylab("Political Polarization") +
  labs(
    title="Box Plot of Political Polarization and Regime Type") +
  theme(plot.title = element_text(hjust = 0.5))

boxplot

ggsave(filename = "BoxPlot.png",
       plot = boxplot)

######## Part 5: Regression ########

#3 Different regression models.

#Regression model 1: no controls
model1 <- lm(data=df2,v2cacamps ~ v2smmefra)

summary(model1)

modelsummary(model1, 
             stars=TRUE, 
             gof_map=c("nobs","r.squared","adj.r.squared"),
             coef_rename=c("v2smmefra"="Media Fractionalization"), #Renaming coefficient.
             output="Regression1.docx")

#Model 2: All controls included:
df2$v2expathhg <- as.factor(df2$v2expathhg)

model2 <- lm(data=df2, v2cacamps ~ v2smmefra  + e_gdppc + as.factor(v2x_regime) 
          + v2mecenefi + as.factor(v2expathhg))

summary(model2)

modelsummary(model2, 
             stars=TRUE, 
             gof_map=c("nobs","r.squared","adj.r.squared"),
             coef_rename=c("v2smmefra"="Media Fractionalization",
                          "e_gdppc"="GDP per capita",
                          "as.factor(v2x_regime)1"="Regime Type: Electoral autocracy",
                          "as.factor(v2x_regime)2"="Regime Type: Electoral democracy",
                          "as.factor(v2x_regime)3"="Regime Type: Liberal democracy",
                          "v2mecenefi"="Internet Censorship Effort",
                          "as.factor(v2expathhg)2"="Head of State Appointment: By Ruling Party",
                          "as.factor(v2expathhg)6"="Head of State Appointment: By Head of State",
                          "as.factor(v2expathhg)7"="Head of State Appointment: By Legislature",
                          "as.factor(v2expathhg)8"="Head of State Appointment: By Popular Election",
                          "as.factor(v2expathhg)9"="Head of State Appointment: Other"),
             output="Regression2.docx")

#Visualize coefficients of model 2#

Coefplot <- lm(data = df2, v2cacamps ~ v2smmefra  + e_gdppc + as.factor(v2x_regime) + v2mecenefi + as.factor(v2expathhg)) |>
  tidy(conf.int = TRUE, conf.level = .95) |>
  filter(term!= "(Intercept)") |>
  ggplot(aes(x=estimate, y=term)) + 
  geom_pointrange(aes(xmax = conf.high, xmin = conf.low)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Coefficient Estimate") +
  ylab("") +
  labs(
    title="Coefficient Plot for Regression Model 2",
    subtitle="Plotting the regression coefficients for the independent variable
(media fractionalization) on political polarization, and all other control
variables, with their confidence intervals.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_discrete(labels = c(
    "v2smmefra"="Media Fractionalization",
    "e_gdppc"="GDP per capita",
    "as.factor(v2x_regime)1"="Regime Type: Electoral autocracy",
    "as.factor(v2x_regime)2"="Regime Type: Electoral democracy",
    "as.factor(v2x_regime)3"="Regime Type: Liberal democracy",
    "v2mecenefi"="Internet Censorship Effort",
    "as.factor(v2expathhg)2"="Head of State Appointment: By Ruling Party",
    "as.factor(v2expathhg)6"="Head of State Appointment: By Head of State",
    "as.factor(v2expathhg)7"="Head of State Appointment: By Legislature",
    "as.factor(v2expathhg)8"="Head of State Appointment: By Popular Election",
    "as.factor(v2expathhg)9"="Head of State Appointment: Other"))

Coefplot

ggsave(filename = "CoefPlot.png",
       plot = Coefplot,
       width=8, height=5.5)

#Model 3: Interaction included#

model3interaction <- lm(data=df2, v2cacamps ~ v2smmefra * factor(v2x_regime) + e_gdppc + v2mecenefi + as.factor(v2expathhg))
summary(model3interaction)

modelsummary(model3interaction, 
             stars=TRUE, 
             coef_rename=c("v2smmefra"="Media Fractionalization",
                           "e_gdppc"="GDP per capita",
                           "factor(v2x_regime)1"="Regime Type: Electoral autocracy",
                           "factor(v2x_regime)2"="Regime Type: Electoral democracy",
                           "factor(v2x_regime)3"="Regime Type: Liberal democracy",
                           "v2smmefra:factor(v2x_regime)1"="Electoral autocracy interaction with media fractionalization",
                           "v2smmefra:factor(v2x_regime)2"="Electoral democracy interaction with media fractionalization",
                           "v2smmefra:factor(v2x_regime)3"="Liberal democracy interaction with media fractionalization",
                           "v2mecenefi"="Internet Censorship Effort",
                           "as.factor(v2expathhg)2"="Head of State Appointment: By Ruling Party",
                           "as.factor(v2expathhg)6"="Head of State Appointment: By Head of State",
                           "as.factor(v2expathhg)7"="Head of State Appointment: By Legislature",
                           "as.factor(v2expathhg)8"="Head of State Appointment: By Popular Election",
                           "as.factor(v2expathhg)9"="Head of State Appointment: Other"),
             gof_map=c("nobs","r.squared","adj.r.squared"),
             output="Regression3.docx")

