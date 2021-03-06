library('agricolae')
library('ggplot2')
library('gridExtra')
library('car')
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
ferment_variable_extend = c(ferment_variable, 'acetate_pre', 'propionate_pre', 'butyrate_pre', 'isobutyrate_pre', 'valerate_pre', 'isovalerate_pre', 'ap')
ferment_se <- list()
for (i in 1:length(ferment_variable_extend)){
  ferment_variable_index <- ferment_variable_extend[i]
  ferment_se[[ferment_variable_index]] = summarySE(data = ferment,
                                                   measurevar = ferment_variable_index,
                                                   groupvars = c('Sequence', 'Day'),
                                                   na.rm = T)
}


# we seperat the data according to two sequeneces, so we are able to do multicompasion between different time points within each sequence
ferment_ACA <- ferment[ferment$Sequence == 'ACA',]
ferment_CAC <- ferment[ferment$Sequence == 'CAC',]
ferment_ACA$Day <- as.factor(ferment_ACA$Day)    #set days as factors
ferment_CAC$Day <- as.factor(ferment_CAC$Day)

ferment_ACA_P2 <- ferment_ACA[ferment_ACA$Period != 'R',]
ferment_ACA_P3 <- ferment_ACA[ferment_ACA$Period == 'R'|(ferment_ACA$Period == 'T'&ferment_ACA$Day == '14'),]
ferment_CAC_P2 <- ferment_CAC[ferment_CAC$Period != 'R',]
ferment_CAC_P3 <- ferment_CAC[ferment_CAC$Period == 'R'|(ferment_CAC$Period == 'T'&ferment_CAC$Day == '14'),]


# initialize parameters to store the ANOVA result and post hoc tablet
ferment_ACA_anova = list()
ferment_CAC_anova = list()
ferment_ACA_posthoc = list()
ferment_CAC_posthoc = list()

ferment_ACA_P2_anova = list()
ferment_ACA_P3_anova = list()
ferment_CAC_P2_anova = list()
ferment_CAC_P3_anova = list()
ferment_ACA_P2_posthoc = list()
ferment_CAC_P2_posthoc = list()
ferment_ACA_P3_posthoc = list()
ferment_CAC_P3_posthoc = list()

# do ANOVA, post hoc calculation
# posthoc result were labled with letters, setting 0.05 as significant thresholed
for (i in 1:length(ferment_variable_extend)){
  ferment_variable_index = ferment_variable_extend[i]
  formula = as.formula(paste(ferment_variable_index, '~ Day'))

  ferment_ACA_anova[[ferment_variable_index]] <- aov(formula, data = ferment_ACA)
  ferment_ACA_posthoc[[ferment_variable_index]] <- LSD.test(y = ferment_ACA_anova[[ferment_variable_index]], trt = 'Day', group = T, alpha = 0.05, p.adj = c("bonferroni"))$groups

  ferment_CAC_anova[[ferment_variable_index]] <- aov(formula, data = ferment_CAC)
  ferment_CAC_posthoc[[ferment_variable_index]] <- LSD.test(y = ferment_CAC_anova[[ferment_variable_index]], trt = 'Day', group = T, alpha = 0.05)$groups
}

for (i in 1:length(ferment_variable_extend)){
  ferment_variable_index <- ferment_variable_extend[i]
  formula <- as.formula(paste(ferment_variable_index, '~ Day'))
  
  ferment_ACA_P2_anova[[ferment_variable_index]] <-aov(formula, data = ferment_ACA_P2)
  ferment_ACA_P2_posthoc[[ferment_variable_index]] <- waller.test(y = ferment_ACA_P2_anova[[ferment_variable_index]], trt = 'Day', group = T)$groups
  
  ferment_ACA_P3_anova[[ferment_variable_index]] <-aov(formula, data = ferment_ACA_P3)
  ferment_ACA_P3_posthoc[[ferment_variable_index]] <- waller.test(y = ferment_ACA_P3_anova[[ferment_variable_index]], trt = 'Day', group = T)$groups
  
  ferment_CAC_P2_anova[[ferment_variable_index]] <-aov(formula, data = ferment_CAC_P2)
  ferment_CAC_P2_posthoc[[ferment_variable_index]] <- waller.test(y = ferment_CAC_P2_anova[[ferment_variable_index]], trt = 'Day', group = T)$groups
  
  ferment_CAC_P3_anova[[ferment_variable_index]] <-aov(formula, data = ferment_CAC_P3)
  ferment_CAC_P3_posthoc[[ferment_variable_index]] <- waller.test(y = ferment_CAC_P3_anova[[ferment_variable_index]], trt = 'Day', group = T)$groups
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
                     ylab = 'VFA, mM/L', xlab = 'Time, d', ylimits = c(0,120)) +
            geom_vline(color = 'grey40', linetype = 'dashed', xintercept = c(0.5, 14.5))

# MCP plotting
MCP_plot <- gerrorbar(data = ferment_se[['MCP']],
                      xvars = 'Day', yvars = 'MCP', se = 'se', group = 'Sequence',
                      legendlabels = c('ACA sequence', 'CAC sequence'), legendnames = 'Treatment Seqeuences',
                      ylab = 'MCP, mg/dL', xlab = 'Time, d', ylimits = c(1.5,11.5)) +
            geom_vline(color = 'grey40', linetype = 'dashed', xintercept = c(0.5, 14.5))

# NH plotting
NH_plot = gerrorbar(data = ferment_se[['NH']],
                    xvars = 'Day', yvars = 'NH', se = 'se', group = 'Sequence',
                    legendlabels = c('ACA sequence', 'CAC sequence'), legendnames = 'Treatment Sequences',
                    ylab = 'NH3-N, mg/dL', xlab = 'Time, d', ylimits = c(-5,25),
                    legendposition = c(1,1), legendjustification = c(1,1))+
          geom_vline(color = 'grey40', linetype = 'dashed', xintercept = c(0.5, 14.5))


# annotating significance using letters
annotation_ACA_y <- list('pH' = 7.5, 'VFA' = 100, 'MCP' = 10.5, 'NH' = 20)
annotation_CAC_y <- list('pH' = 6.2, 'VFA' = 23, 'MCP' = 3, 'NH' = 0)
ferment_plot <- list('pH' = pH_plot, 'VFA' = VFA_plot, 'MCP' = MCP_plot, 'NH' = NH_plot)

for (i in 1:length(ferment_variable)){
  ferment_variable_index <- ferment_variable[i]
  
  annotation_ACA <- ferment_ACA_posthoc[[ferment_variable_index]]$M
  annotation_ACA_x <- as.numeric(levels(ferment_ACA_posthoc[[ferment_variable_index]]$trt))[ferment_ACA_posthoc[[ferment_variable_index]]$trt]
  
  annotation_CAC <- ferment_CAC_posthoc[[ferment_variable_index]]$M
  annotation_CAC_x <- as.numeric(levels(ferment_CAC_posthoc[[ferment_variable_index]]$trt))[ferment_CAC_posthoc[[ferment_variable_index]]$trt]
  
  ferment_plot[[ferment_variable_index]] = ferment_plot[[ferment_variable_index]] + 
                                           annotate(geom = 'text', label = annotation_ACA,
                                                    x = annotation_ACA_x, y = annotation_ACA_y[[ferment_variable_index]],
                                                    angle = 30, color = 'DarkRed')+
                                           annotate(geom = 'text', label = annotation_CAC,
                                                    x = annotation_CAC_x, y = annotation_CAC_y[[ferment_variable_index]],
                                                    angle = 30, color = 'Turquoise4')
}


annotation_ACA_P2_y <- list('pH' = 7.5, 'VFA' = 100, 'MCP' = 9.5, 'NH' = 20)
annotation_CAC_P2_y <- list('pH' = 6.2, 'VFA' = 23, 'MCP' = 4, 'NH' = 0)
annotation_ACA_P3_y <- list('pH' = 7.7, 'VFA' = 110, 'MCP' = 10.5, 'NH' = 22.5)
annotation_CAC_P3_y <- list('pH' = 6, 'VFA' = 13, 'MCP' = 3, 'NH' = -2.5)
ferment_plot <- list('pH' = pH_plot, 'VFA' = VFA_plot, 'MCP' = MCP_plot, 'NH' = NH_plot)

for (i in 1:length(ferment_variable)){
  ferment_variable_index <- ferment_variable[i]
  
  annotation_ACA_P2 <- ferment_ACA_P2_posthoc[[ferment_variable_index]]$M
  annotation_ACA_P2_x <- as.numeric(levels(ferment_ACA_P2_posthoc[[ferment_variable_index]]$trt))[ferment_ACA_P2_posthoc[[ferment_variable_index]]$trt]
  
  annotation_CAC_P2 <- ferment_CAC_P2_posthoc[[ferment_variable_index]]$M
  annotation_CAC_P2_x <- as.numeric(levels(ferment_CAC_P2_posthoc[[ferment_variable_index]]$trt))[ferment_CAC_P2_posthoc[[ferment_variable_index]]$trt]
  
  annotation_ACA_P3 <- ferment_ACA_P3_posthoc[[ferment_variable_index]]$M
  annotation_ACA_P3_x <- as.numeric(levels(ferment_ACA_P3_posthoc[[ferment_variable_index]]$trt))[ferment_ACA_P3_posthoc[[ferment_variable_index]]$trt]
  
  annotation_CAC_P3 <- ferment_CAC_P3_posthoc[[ferment_variable_index]]$M
  annotation_CAC_P3_x <- as.numeric(levels(ferment_CAC_P3_posthoc[[ferment_variable_index]]$trt))[ferment_CAC_P3_posthoc[[ferment_variable_index]]$trt]
  
  
  ferment_plot[[ferment_variable_index]] = ferment_plot[[ferment_variable_index]] + 
    annotate(geom = 'text', label = annotation_ACA_P2,
             x = annotation_ACA_P2_x, y = annotation_ACA_P2_y[[ferment_variable_index]],
             angle = 30, color = 'DarkRed')+
    annotate(geom = 'text', label = annotation_CAC_P2,
             x = annotation_CAC_P2_x, y = annotation_CAC_P2_y[[ferment_variable_index]],
             angle = 30, color = 'Turquoise4')+
    annotate(geom = 'text', label = annotation_ACA_P3,
             x = annotation_ACA_P3_x, y = annotation_ACA_P3_y[[ferment_variable_index]],
             angle = 30, color = 'DarkRed')+
    annotate(geom = 'text', label = annotation_CAC_P3,
             x = annotation_CAC_P3_x, y = annotation_CAC_P3_y[[ferment_variable_index]],
             angle = 30, color = 'Turquoise4')
}


# export plotting result as pdf format
pdf(file = 'para3.pdf', width = 7, height = 12)
grid.arrange(ferment_plot[['pH']], ferment_plot[['VFA']], ferment_plot[['MCP']], ferment_plot[['NH']], nrow = 4)
dev.off()


# test
tmp = lmer(MCP~Diet*Day+(1|Sequence)+(1|Period)+(1|Subject), data = ferment, REML = F)
