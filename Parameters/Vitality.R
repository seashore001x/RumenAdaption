library('ggplot2')

# generate random variables according to logistic distribution
x <- rlogis(500)
x <- x[order(x)]

y_activate <- (plogis(x) + 0.5) * 2.5 - 0.5
y_depress <- 1 - plogis(x, location = -3) + 1
y_total <- y_activate + y_depress

data = data.frame(vitality = rep(c('Activated', 'Depressed', 'Total'), each = 500),
                  y = c(y_activate, y_depress, y_total),
                  x = rep(x, 3))

logis_curve <- ggplot(data, aes(x = x, y = y, group = vitality)) + 
              geom_line(aes(color = vitality, linetype = vitality)) +
              theme()+
              scale_linetype_manual(values = c(2,2,1), name = 'Microorganims') +
              scale_color_manual(values = c('DarkRed', 'Turquoise4', 'Black'), name = 'Microorganims') +
              xlab(NULL) + ylab('Microbial vitality')

pdf(file = 'vitality.pdf', width = 7, height = 3)
dev.off()
