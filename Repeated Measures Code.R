###################
###################
# Clear workspace #
###################
###################

# Clearing the workspace removes all current loaded data, functions and so on

rm(list = ls())

###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function
suppressPackageStartupMessages(
  libraries("reshape",
            "ez", # for ezANOVA
            "nlme",
            "multcomp", # for posthoc tests
            "WRS2", # for robust repeated measures ANOVA
            "ggplot2",
            "pastecs" # for stat.desc()
            ))

#############
# Functions #
#############

# function to calculate r contrast
rcontrast <- function(t, df){
  r <- sqrt(t^2/(t^2 + df))
  print(paste("r = ", r))
}

#############
#############
# Load Data #
#############
#############

setwd("C:/R Portfolio/Repeated Measures")
bushData <- read.delim("Bushtucker.dat", header = T)

###################
###################
# Data Formatting #
###################
###################

longBush <- melt(bushData, id = "participant", measured = c("stick insect", "kangaroo_testicle", "fish_eye", "witchetty_grub"))
names(longBush) <- c("Participant", "Animal", "Retch")
longBush$Animal <- factor(longBush$Animal, labels = c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub")) # relabel animal variable
longBush

####################
####################
# Data Exploration #
####################
####################

######################
######################
# Choosing Contrasts #
######################
######################

PartvsWhole <- c(1, -1, -1, 1)
TesticlevsEye <- c(0, -1, 1, 0)
StickvsGrub <- c(-1, 0, 0, 1)
contrasts(longBush$Animal) <- cbind(PartvsWhole, TesticlevsEye, StickvsGrub)

###############################
# Analysing Repeated Measures #
###############################
###############################

bushModel <- ezANOVA(longBush, 
                     dv = .(Retch),
                     wid = .(Participant),
                     within = .(Animal), 
                     detailed = T,
                     type = 3)
bushModel

# The output for Mauchly's test is statistically significant, which violates the assumption. If it had been insignificant, then we can use the p value from the ANOVA table.
# the Greenhouse - Geisser GGe value of .553  is closer to the limit of .33, which is calculated by: 1/(4-1), rather than 1, so it is a substantial deviation from sphericity. If Greenhouse-Geisser is greater than 0.75 or there is a small sample size (e.g. 10), Huynh-Feldt should be used.
# sphericity only becomes an issue where there are at least three conditions or variables.
# However, if one considers the HFe value, it is .66, which is closer to 1 than .33
# The p-value next to animal effect, indicates there was a significant difference between the four animals in their capacity to induce retching when eaten.
# A more mixed picture emerges when one considers the p values of the GG and HF respectively. The GG p-value, which is more conservative is not significant, while the HF p value is significant. One way around this is to take the average of the two p-values and then let that value determine whether it is significant.

pairwise.t.test(longBush$Retch, longBush$Animal, paired = T, p.adjust.method = "bonferroni")

############################
# The multilevel approach  #
############################

bushModel <- lme(Retch ~ Animal, random = ~ 1 | Participant/Animal, longBush, method = "ML" )
# As we want to test whether Animal had an effect, it is necessary to compare it to a model where the predictor is absent

baselineModel <- lme(Retch ~ 1, random = ~ 1 | Participant/Animal, longBush, method = "ML" )

# To compare the models, use:

anova(baselineModel, bushModel)
# The AIC and BIC values are smaller in the full model over the baseline model, indicating the former is a better fit to the data. The p-value is also significant, which means that animal type had a significant effect on time take to retch.

summary(bushModel)

# The first contrast tells us that retching times were significantly different for whole animals compared to animal parts. The descriptive statistics indicate that people retched quicker after eating animal parts than whole animals. There is no significant difference after eating a testicle vs an eye.

##################
##################
# Post hoc tests #
##################
##################

postHocs <- glht(bushModel, linfct = mcp(Animal = "Tukey"))
summary(postHocs)
confint(postHocs)
# The summary indicates that the difference between testicle and stick insect is significant, as is fish eye and stick insect. The confint() function indicates that only the significant ones did not cross zero, which indicates they must be significant as the mean cannot be zero.

##########################################
# Robust One Way repeated measures ANOVA #
##########################################

rmanova(y = longBush$Retch, groups = longBush$Animal, blocks =
          longBush$Participant)
?rmanova
# There are no significant differences in retch times after eating different animals 

rmanovab(y = longBush$Retch, groups = longBush$Animal, blocks = longBush$Participant, nboot = 2000)
# This test indicates what the test statistic would be for it to be significant. The actual test statistic is less than the critical value, so it is not significant.
rmmcp(y = longBush$Retch, groups = longBush$Animal, blocks =
        longBush$Participant)
# If the p-values is smaller than the critical p-value and the confidence interval does not cross zero, then it is significant. None of the findings are significant as the p-values are more than the p-critical values and the confidence intervals all cross zero.

pairdepb(y = longBush$Retch, groups = longBush$Animal, blocks =
           longBush$Participant)
# If the value of test is larger than critical and then confidence interval does not cross zero, then the finding is significant. None of the findings are significant.

##############################################
# Effect Sizes for repeated measures designs #
##############################################

# function to calculate r contrast
rcontrast <- function(t, df){
  r <- sqrt(t^2/(t^2 + df))
print(paste("r = ", r))
}
summary(bushModel) # need t value and degrees of freedom from bushModel summary

# The difference between body parts and whole animals was a large effect (r = .57), while a medium effect was evident between stick insect and whitchetty grub (r=.39) and small effect between testicle and eye ball (r=.02)

rcontrast(3.149752, 21)
rcontrast(-0.101237, 21)
rcontrast(-1.923500, 21)

#######################################
#######################################
# Factorial Repeated Measures Designs #
#######################################
#######################################

#############
#############
# Load Data #
#############
#############

setwd("C:/R Portfolio/Repeated Measures")
attitudeData <- read.delim("Attitude.dat", header = T)

###################
###################
# Data Formatting #
###################
###################

longAttitude <- melt(attitudeData, id = "participant", 
                     measured = c( "beerpos", "beerneg", "beerneut", "winepos", "wineneg", "wineneut", "waterpos", "waterneg", "waterneu", "participant"))

names(longAttitude) <- c("participant", "groups", "attitude")

longAttitude$drink <- gl(3, 60, labels = c("Beer", "Wine", "Water"))
longAttitude$imagery <- gl(3, 20, 180, 
                           labels = c("Positive", "Negative", "Neutral"))

longAttitude <- longAttitude[order(longAttitude$participant), ]

####################
####################
# Data Exploration #
####################
####################

# Graphs #

attitudeBoxplot <- ggplot(longAttitude, 
                          aes(drink, attitude))

attitudeBoxplot + geom_boxplot() + facet_wrap(~imagery, nrow = 1) + 
  labs(x = "Type of Drink", y = "Mean Preference Score") + theme_classic()

imageFile <- paste(imageDirectory,"13 attitude Boxplot.png", sep = "/")
ggsave(file = imageFile)


drinkBar <- ggplot(longAttitude, aes(drink, attitude))
drinkBar + stat_summary(fun = mean, geom = "bar", 
                        fill = "White", colour = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
  labs(x = "Type of Drink", y = "Mean Attitude") 

imageFile <- paste(imageDirectory,"13 Drink Main Effect.png", sep = "/")
ggsave(file = imageFile)

imageryBar <- ggplot(longAttitude, aes(imagery, attitude))
imageryBar + stat_summary(fun = mean, geom = "bar", fill = "White", 
                          colour = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
  labs(x = "Type of Imagery", y = "Mean Attitude") 



options(digits = 3)
by(longAttitude$attitude, list(longAttitude$drink, longAttitude$imagery), 
   stat.desc, basic = FALSE)
by(longAttitude$attitude, longAttitude$drink, stat.desc, basic = FALSE)
by(longAttitude$attitude, longAttitude$imagery, stat.desc, basic = FALSE)
options(digits = 7)

#####################
# Setting Contrasts #
#####################

# We can set up contrasts to compare alcohol vs water, beer vs wine and positive vs neutral imagery.
# Such contrast answer questions like are effects different for alcoholic and non alcoholic drinks and are effects different for different types of alcoholic drink?

# Contrasts for drink #

AlcoholvsWater<-c(1, 1, -2)
BeervsWine<-c(-1, 1, 0)

contrasts(longAttitude$drink) <- cbind(AlcoholvsWater, BeervsWine)
longAttitude$drink

# Contrasts for imagery #

NegativevsOther <- c(1, -2, 1)
PositivevsNeutral <- c(-1, 0, 1)

contrasts(longAttitude$imagery) <- cbind(NegativevsOther, PositivevsNeutral)
longAttitude$imagery

# using ezAnova #

attitudeModel <- ezANOVA(data = longAttitude, dv = .(attitude), 
                         wid = .(participant),  within = .(drink, imagery), 
                         type = 3, detailed = TRUE)
options(digits = 3)
attitudeModel

# The ANOVA table reveals all effects are significant, which means they all violate the sphericity assumption. When the Mauchly's test output is consulted, drink and imagery effects are significant, but the interaction between drink and imagery is not. For drink and imagery individual effects then, it is necessary to use the GGe and HFe sphericity corrections, whereby they are significant. This means that drink type is significant, whereby if the type of imagery is ignored, people will still rate some drinks differently.
# The interaction (drink x imagery) is also significant, which means that imagery type has a different effect depending on which type of drink it was presented with.

# The Interaction Effect (drink x imagery) #

imageFile <- paste(imageDirectory,"13 Imagery Main Effect.png", sep = "/")
ggsave(file = imageFile)

attitudeInt <- ggplot(longAttitude, aes(drink, attitude, colour = imagery))
attitudeInt + stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun = mean, geom = "line", aes(group = imagery)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Type of Drink", y = "Mean Attitude", colour = "Type of Imagery") 
imageFile <- paste(imageDirectory,"13 Attitude Interaction Line.png",
                   sep = "/")
ggsave(file = imageFile)

# In the above plot, mean attitudes towards drink are similar for positive and neutral imagery types. In other words, for both positive and neutral, wine had the highest mean attitude, followed by beer and then water. With negative imagery, ratings were lower for wine and water, which means despite negative imagery, attitudes towards beer remained quite neutral. This could suggest negative imagery has a different effect than positive and neutral imagery as it decreases attitude ratings rather than increasing them

##################
# Post Hoc Tests #
##################

pairwise.t.test(longAttitude$attitude, 
                longAttitude$groups, 
                paired = TRUE, 
                p.adjust.method = "bonferroni")

# A snapshot of results indicates there are significant differences between beer positive and negative and neutral imagery for beer. For wine, there are significant differences between positive and negative and neutral.

options(digits = 7)

##############################################
# Factorial repeated measures designs as GLM #
##############################################

# To compare effects of any predictor, it is first necessary to generate a baseline moel with predictors in, so that we can make effective comparisons to later.

baseline <- lme(attitude ~ 1, 
                random = ~ 1|participant/drink/imagery, 
                data = longAttitude, 
                method = "ML") # maximum likelihood method


drinkmodel <- update(baseline, .~. + drink)
imagerymodel <- update(drinkmodel, .~. + imagery)
attitudemodel <- update(imagerymodel, .~. + drink:imagery)

# Compare the models #

anova(baseline, drinkmodel, imagerymodel, attitudemodel)

# The attitudemodel is the best fit, as it has the lowest (best fit) AIC and BIC scores and is statistically significant with a p-value of <.0001. Each model is a better fit than the previous one, which indicates that adding predictors helps explain the data.
summary(attitudemodel)
# effects show significant effect for alcohol vs water, but not when comparing beer with wine.In terms of negative imagery, it is significant when compared to other types and to neutral imagery.

# Effect sizes #

rcontrast(3.18, 38)
rcontrast(-1.47, 38)
rcontrast(17.26, 114)
rcontrast(-9.81, 114)
rcontrast(0.69, 114)
rcontrast(6.77, 114)
rcontrast(0.93, 114)
rcontrast(-0.80, 114)

