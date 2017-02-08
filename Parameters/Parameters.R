library('dplyr')
library('agricolae')
library('ggplot2')
library('gridExtra')
source('../R_function/summarySE.R')
source('../R_function/gerrorbar.R')

# ferment: Rumen fermentation parameters
# intake: Animal nutrients intake
# digest: Animal digestibility
# import fermentation parameters, nutrient intake and nutrient digestibility

# days and pdays were left as numeric for easier plotting
ferment <- read.csv('Parameters/para.csv')
ferment$Subject = as.factor(ferment$Subject)
# set subject, period, diet, group as factors; DM, CP, NDF, ADF, OM as numeric
intake <- read.csv('Parameters/Diet.csv', colClasses = c(rep(c('factor', 'numeric'), each = 4), 'numeric'))
digest <- read.csv('Parameters/Digestiblity.csv', colClasses = c(rep(c('factor', 'numeric'), each = 4), 'numeric'))


# calculat the anova of NutrientIntake and digestbility
nutrient <- c('DM', 'OM', 'CP', 'NDF', 'ADF')
intake_anova  <- list()
intake_posthoc <- list()
digest_anova <- list()
digest_posthoc <- list()

for (i in 1:length(nutrient)){
  nutrient_index <- nutrient[i]
  formula <- as.formula(paste(nutrient_index, '~ Period*Diet'))

  # do calculation
  intake_index_anova <- aov(formula, data = intake)
  digest_index_anova <- aov(formula, data = digest)

  # store the anova result
  intake_anova[[nutrient_index]] <- summary(intake_index_anova)
  digest_anova[[nutrient_index]] <- summary(digest_index_anova)

  # store the post hoc result
  intake_posthoc[[nutrient_index]] <- TukeyHSD(intake_index_anova)[-3]
  digest_posthoc[[nutrient_index]] <- TukeyHSD(digest_index_anova)[-3]
}


# calculate standard error of means for rumen fermentation parameters
ferment_variable <- c('pH', 'NH', 'MCP', 'VFA')
ferment_variable_extend = c(ferment_variable, 'acetate_pre', 'propionate_pre', 'butyrate_pre', 'isobutyrate_pre', 'valerate_pre', 'isovalerate_pre')
ferment_se <- list()
for (i in 1:length(ferment_variable_extend)){
  ferment_variable_index <- ferment_variable_extend[i]
  ferment_se[[ferment_variable_index]] = summarySE(data = ferment,
                                                   measurevar = ferment_variable_index,
                                                   groupvars = c('Sequence', 'Day'),
                                                   na.rm = T)
}


# we seperat the data according to two sequeneces, so we are able to do multicompasion between different time points within each sequence
ferment_ACA = ferment[ferment$Sequence == 'ACA',]
ferment_CAC = ferment[ferment$Sequence == 'CAC',]
ferment_ACA$Day = as.factor(ferment_ACA$Day)    #set days as factors
ferment_CAC$Day = as.factor(ferment_CAC$Day)

# initialize parameters to store the ANOVA result and post hoc tablet
ferment_ACA_anova = list()
ferment_CAC_anova = list()
ferment_ACA_posthoc = list()
ferment_CAC_posthoc = list()

# do ANOVA, post hoc calculation
# posthoc result were labled with letters, setting 0.05 as significant thresholed
for (i in 1:length(ferment_variable_extend)){
  ferment_variable_index = ferment_variable_extend[i]
  formula = as.formula(paste(ferment_variable_index, '~ Day'))

  ferment_ACA_anova[[ferment_variable_index]] <- aov(formula, data = ferment_ACA)
  ferment_ACA_posthoc[[ferment_variable_index]] <- HSD.test(y = ferment_ACA_anova[[ferment_variable_index]], trt = 'Day', group = T)$groups

  ferment_CAC_anova[[ferment_variable_index]] <- aov(formula, data = ferment_CAC)
  ferment_CAC_posthoc[[ferment_variable_index]] <- HSD.test(y = ferment_CAC_anova[[ferment_variable_index]], trt = 'Day', group = T)$groups
}


# plotting error bar for rumen fermentation parameters
# pH plotting
pH_plot <- gerrorbar(data = ferment_se[['pH']],
                    xvars = 'Day', yvars = 'pH', se = 'se', group = 'Sequence',
                    legendlabels = c('ACA sequence', 'CAC sequence'), legendnames = 'Treatment Seqeuences',
                    ylab = 'pH', xlab = 'Time, d', ylimits = c(6,7.8)) +
           geom_vline(color = 'grey40', linetype = 'dashed', xintercept = c(0.5, 14.5))

# VFA plotting
VFA_plot <- gerrorbar(data = ferment_se[['VFA']],
                     xvars = 'Day', yvars = 'VFA', se = 'se', group = 'Sequence',
                     legendlabels = c('ACA sequence', 'CAC sequence'), legendnames = 'Treatment Seqeuences',
                     ylab = 'VFA, mM/L', xlab = 'Time, d', ylimits = c(-10,110)) +
            geom_vline(color = 'grey40', linetype = 'dashed', xintercept = c(0.5, 14.5))

# MCP plotting
MCP_plot <- gerrorbar(data = ferment_se[['MCP']],
                      xvars = 'Day', yvars = 'MCP', se = 'se', group = 'Sequence',
                      legendlabels = c('ACA sequence', 'CAC sequence'), legendnames = 'Treatment Seqeuences',
                      ylab = 'MCP, mg/dL', xlab = 'Time, d', ylimits = c(0,10)) +
            geom_vline(color = 'grey40', linetype = 'dashed', xintercept = c(0.5, 14.5))

# NH plotting
NH_plot = gerrorbar(data = ferment_se[['NH']],
                    xvars = 'Day', yvars = 'NH', se = 'se', group = 'Sequence',
                    legendlabels = c('ACA sequence', 'CAC sequence'), legendnames = 'Treatment Sequences',
                    ylab = 'NH3-N, mg/dL', xlab = 'Time, d', ylimits = c(0,25),
                    legendposition = c(1,1), legendjustification = c(1,1))+
          geom_vline(color = 'grey40', linetype = 'dashed', xintercept = c(0.5, 14.5))


pdf(file = 'para.pdf', width = 14, height = 6)
grid.arrange(pH_plot, VFA_plot, MCP_plot, NH_plot, nrow = 2)
dev.off()
