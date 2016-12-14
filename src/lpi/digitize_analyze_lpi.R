
# digitize graphs from lpi report -----------------------------------------

# require(digitize)
# lpi_terr <- digitize(file.choose())
# lpi_fresh <- digitize(file.choose())
# lpi_marine <- digitize(file.choose())
# 
# lpi_terr$habitat <- 'terrestric'
# lpi_fresh$habitat <- 'freshwater'
# lpi_marine$habitat <- 'marine'
# 
# lpi <- rbind(lpi_fresh, lpi_marine, lpi_terr)
# 
# saveRDS(lpi, '/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/src/lpi/lpi.rds')



# plot --------------------------------------------------------------------

lpi <- readRDS('/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/src/lpi/lpi.rds')
library('ggplot2')
library('esmisc')
library('tikzDevice')
head(lpi)
lpi_plot <- ggplot(lpi) +
  geom_line(aes(x = x, y = y, col = habitat), size = 1) + 
  geom_hline(aes(yintercept = 1), linetype = 'dotted') +
  theme_edi() +
  labs(y = 'Index value (1970 = 1)', x = 'Year') +
  theme(panel.grid.major = element_blank(),
        legend.position = 'bottom') +
  ylim(c(0, 1.5)) +
  scale_color_manual('Habitat', 
                     values = c('steelblue', 'blue', "green4"),
                     breaks = c("terrestric", "marine", "freshwater"))
ggsave('/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/figs/tikz/lpi.tikz',
       lpi_plot, 
       device = tikz,
       width = 5)

