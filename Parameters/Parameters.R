library(dplyr)
library(lme4)
library(lmerTest)
library(multcomp)
source('R_function/summarySE.R')
source('R_function/gerrorbar.R')

# import fermentation parameters, nutrient intake and nutrient digestibility
Fermentation = read.csv(file.choose())
NutrientIntake = read.csv(file.choose(), colClasses = c(rep(c('factor', 'numeric'), each = 4), 'numeric'))
Digestibility = read.csv(file.choose(), colClasses = c(rep(c('factor', 'numeric'), each = 4), 'numeric'))


# calculat the anova of NutrientIntake and digestbility using linear mixed effect model
Nutrient = c('DM', 'OM', 'CP', 'NDF', 'ADF')
NutrientIntake_anova  = list()
NutrientIntake_posthoc = list()
Digestibility_anova = list()
Digestibility_posthoc = list()
for (i in 1:length(Nutrient)){
  Nutrient_index = Nutrient[i]
  formula = as.formula(paste(Nutrient_index, '~ Period*Diet'))
  NutrientIntake_lme = aov(formula, data = NutrientIntake)
  Digestibility_lme = aov(formula, data = Digestibility)

  # Store the anova result in NutrientIntake_anova
  NutrientIntake_anova[[Nutrient_index]] = summary(NutrientIntake_lme)
  Digestibility_anova[[Nutrient_index]] = summary(Digestibility_lme)

  # Store the post hoc result
  NutrientIntake_posthoc[[Nutrient_index]] = TukeyHSD(NutrientIntake_lme)[-3]
  Digestibility_posthoc[[Nutrient_index]] = TukeyHSD(Digestibility_lme)[-3]
}


# calculate standard error of means for rumen fermentation parameters
FermentationParameter = c('pH', 'NH', 'MCP', 'VFA')
Fermentation_SE = list()
for (i in 1:length(FermentationParameter)){
  FermentationParameter_Index = FermentationParameter[i]
  Fermentation_SE[[FermentationParameter_Index]] = summarySE(data = Fermentation,
                                                             measurevar = FermentationParameter_Index,
                                                             groupvars = c('Sequence', 'Day'),
                                                             na.rm = T)
}


# plotting error bar for rumen fermentation parameters
FermentationParameter_gerrorbar = list()
for (i in 1:length(FermentationParameter)){
  FermentationParameter_Index = FermentationParameter[i]
  FermentationParameter_gerrorbar[[FermentationParameter_Index]] = gerrorbar(data = Fermentation_SE[[FermentationParameter_Index]],
                                                                             xvars = 'Day',
                                                                             yvars = FermentationParameter_Index,
                                                                             se = 'se',
                                                                             group = 'Sequence',
                                                                             legendlabels = c('AH to CS to AH', 'CS to AH to CS'),
                                                                             legendnames = 'Treatment Sequence',
                                                                             xlab = 'Days After Transition',
                                                                             ylimits = c(6,8),
                                                                             legendjustification = c(1.1, -0.1),
                                                                             legendposition = c(1, 0),
                                                                             title = paste('Temporal varitaion of', FermentationParameter_Index, 'during forage transition'))
}


# multicomparison between different time points
FermentationParameter_ACA = FermentationParameter[FermentationParameter$Sequence == 'ACA',]
FermentationParameter_CAC = FermentationParameter[FermentationParameter$Sequence == 'CAC',]

# initialize parameters to store the posthoc result
FermentationParameter_CAC_posthoc = list()
FermentationParameter_ACA_posthoc = list()

for (i in 1:length(FermentationParameter)){

  FermentationParameter_Index = FermentationParameter[i]

  FermentationParameter_ACA_posthoc[FermentationParameter_Index] = TukeyHSD(aov(FermentationParameter_Index~Day, data = FermentationParameter_ACA))
  FermentationParameter_CAC_posthoc[FermentationParameter_Index] = TukeyHSD(aov(FermentationParameter_Index~Day, data = FermentationParameter_CAC))
}

