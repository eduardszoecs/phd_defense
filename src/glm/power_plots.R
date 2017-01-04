if(!exists("prj")){
  stop("You need to create a object 'prj' that points to the top folder, 
       e.g. prj <- '/home/edisz/Documents/work/research/projects/2016/6USETHEGLM/'!")
} else {
  source(file.path(prj, "src", "0-load.R"))
}

n_labeller <- function(string){
  value <- paste0('n = ', string)
}

require(tikzDevice)
figp <- '/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/figs/'
tikzp <- '/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/figs/tikz/'
require(esmisc)
# require(latex2exp)


# Plot one simulation -----------------------------------------------------

dat <- dosim2(N = 5, pC = 0.90, pE = 0.45, nsim = 100, n_animals = 5)
plot(dat$x, dat$y[,1])

pdat <- data.frame(x = dat$x, y = dat$y[,1])
pdat

p_sim <- ggplot(pdat, aes(x = x, y = y)) + 
  geom_count() +
  theme_edi() +
  labs(y = 'Number of surviving animals\n(out of 5)', 
       x = 'Treatment') +
  ggtitle('Binomial data', 
          subtitle = 'n = 5, p(Control,T1) = 0.9, p(T2-T6) = 0.45')
p_sim


ggsave(plot = p_sim, filename = file.path(tikzp, 'p_sim.tikz'), device = tikz,
       width = 6, height = 5)

### ----------------------------------------------------------------------------
### Results -  Count data
### Written by Eduard Szöcs
### ----------------------------------------------------------------------------

## Load results
source(file.path(prj, "src", "1-simulations.R"))
# Power 
res1_c <- readRDS(file.path(cachedir, 'res1_c.rds'))
# Type I error
res2_c <- readRDS(file.path(cachedir, 'res2_c.rds'))


### ----------------------------------------------------------------------------
### Global test 
### ------------------------
# Plot Power
pow_glob_c <- ldply(res1_c, p_glob1)
pow_glob_c$muc <- rep(todo_c$ctrl, each  = 6)
pow_glob_c$N <- rep(todo_c$N, each = 6)
pow_glob_c$variable <-  factor(pow_glob_c$variable, unique(pow_glob_c$variable)[1:6], 
                               labels = c('lm', 'glm_nb', 'glm_qp', 'glm_pb', 'glm_p', 'np'))

plot_pow_glob_c <- ggplot(pow_glob_c) +
  geom_line(aes(y = power, x = log2(muc), group = variable, linetype = variable)) +
  geom_point(aes(y = power, x = log2(muc), shape = variable), color = 'black', size = 4) +
  facet_grid( ~N, labeller = labeller(N = n_labeller)) + 
  # axes
  labs(x = expression(mu[C]), 
       y = expression(paste('Power (global test , ', alpha, ' = 0.05)'))) +
  scale_x_continuous(breaks = log2(round(unique(todo_c$ctrl), 0)), 
                     labels = round(unique(todo_c$ctrl), 0)) +
  # appearance
  theme_edi() +
  # legend title
  scale_shape_manual('Method', values = c(16,2,4,0, 1, 17), 
                     labels = c('LM', expression(GLM[nb]), expression(GLM[qp]), 
                                expression(GLM[npb]), expression(GLM[p]), 'KW')) +
  scale_linetype_discrete('Method', labels = c('LM', expression(GLM[nb]), expression(GLM[qp]), 
                                               expression(GLM[npb]), expression(GLM[p]), 'KW')) +
  ylim(c(0,1)) +
  theme(legend.position = 'bottom')
plot_pow_glob_c







### ------------------------
## Plot T1-error
t1_glob_c <- ldply(res2_c, p_glob1)
t1_glob_c$muc <- rep(todo_c$ctrl, each  = 6)
t1_glob_c$N <- rep(todo_c$N, each = 6)
t1_glob_c$variable  <-  factor(t1_glob_c$variable, unique(t1_glob_c$variable)[c(1, 2, 3, 4, 5, 6)], 
                               labels = c('lm', 'glm_nb', 'glm_pb', 'glm_qp', 'glm_p', 'np'))

plot_t1_glob_c  <- ggplot(t1_glob_c) +
  geom_line(aes(y = power, x = log2(muc), group = variable, linetype = variable)) +
  geom_point(aes(y = power, x = log2(muc), shape = variable), color = 'black', size = 4) +
  geom_segment(aes(x = -Inf, xend = Inf, y = 0.05, yend = 0.05), linetype = 'dashed') + 
  facet_grid( ~N, labeller = labeller(N = n_labeller)) + 
  # axes
  labs(x = expression(mu[C]), 
       y = expression(paste('Type 1 error (global test , ', alpha, ' = 0.05)'))) +
  scale_x_continuous(breaks = log2(round(unique(todo_c$ctrl), 0)), 
                     labels = round(unique(todo_c$ctrl), 0)) +
  # appearance
  theme_edi() + 
  # legend title
  scale_shape_manual('Method', values = c(16,2,4,0,1,17)) +
  theme(legend.position = "none") + labs(x = NULL) +
  scale_y_log10(breaks = c(0.01, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1))
# coord_cartesian(ylim = c(-0.005, 0.25))
plot_t1_glob_c

require(cowplot)
ap_t1_g <- plot_grid(plot_t1_glob_c, plot_pow_glob_c, nrow = 2, rel_heights = c(1, 1.4))

# ggsave(plot = ap_t1_g, filename = file.path(tikzp, 'ap_t1_g.tikz'), device = tikz, 
#        height = 10, width = 14)


### ---------------------------------------------------------------------------
### LOEC
### ------------------------
## Plot Power
pow_loec_c <- ldply(res1_c, p_loec1, type = 'power')
pow_loec_c$muc <- todo_c$ctrl
pow_loec_c$N <- todo_c$N
pow_loec_c  <- melt(pow_loec_c, id.vars = c('muc', 'N'), value.name = 'power')
pow_loec_c$variable  <-  factor(pow_loec_c$variable, unique(pow_loec_c$variable)[1:5], 
                                labels = c('lm', 'glm_nb', 'glm_qp', 'glm_p', 'np'))

plot_pow_loec_c <- ggplot(pow_loec_c) +
  geom_line(aes(y = power, x = log2(muc), group = variable, linetype = variable)) +
  geom_point(aes(y = power, x = log2(muc), shape = variable), color = 'black', size = 4) +
  facet_grid( ~N, labeller = labeller(N = n_labeller)) + 
  # axes
  labs(x = expression(mu[C]), 
       y = expression(paste('Power (LOEC)'))) + 
  scale_x_continuous(breaks = log2(round(unique(todo_c$ctrl), 0)), 
                     labels = round(unique(todo_c$ctrl), 0)) +
  # appearance
  theme_edi() + 
  # legend title
  scale_shape_manual('Method', values = c(16,2,4,1,17), 
                     labels = c('LM', expression(GLM[nb]), expression(GLM[qp]), 
                                expression(GLM[p]), 'WT')) +
  scale_linetype_discrete('Method',  labels = c('LM', expression(GLM[nb]), 
                                                expression(GLM[qp]), expression(GLM[p]), 'WT')) +
  ylim(c(0,1)) +
  theme(legend.position = 'bottom')
# +
#   scale_y_log10(breaks = c(0.01, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1), limit = c(0.01, 1))
plot_pow_loec_c


### ------------------------
## Plot T1-error
t1_loec_c <- ldply(res2_c, p_loec1, type = 't1')
t1_loec_c$muc <- todo_c$ctrl
t1_loec_c$N <- todo_c$N
t1_loec_c <- melt(t1_loec_c, id.vars = c('muc', 'N'), value.name = 't1')
t1_loec_c$variable  <-  factor(t1_loec_c$variable, unique(t1_loec_c$variable)[c(1, 2, 3, 4, 5)], 
                               labels = c('lm', 'glm_nb', 'glm_qp', 'glm_p', 'np'))

plot_t1_loec_c <- ggplot(t1_loec_c) +
  geom_line(aes(y = t1, x = log2(muc), group = variable, linetype = variable)) +
  geom_point(aes(y = t1, x = log2(muc), shape = variable), color = 'black', size = 4) +
  geom_segment(aes(x = -Inf, xend = Inf, y = 0.05, yend = 0.05), 
               linetype = 'dashed') + 
  facet_grid( ~N, labeller = labeller(N = n_labeller)) + 
  # axes
  labs(x = expression(mu[C]), 
       y = expression(paste('Type 1 error (LOEC)'))) + 
  scale_x_continuous(breaks = log2(round(unique(todo_c$ctrl), 0)), 
                     labels = round(unique(todo_c$ctrl), 0)) +
  # appearance
  theme_edi() + 
  # legend title
  scale_shape_manual('Method', values = c(16,2,4,1, 17)) +
  theme(legend.position = "none") + labs(x = NULL) +
  scale_y_log10(breaks = c(0.01, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1), limit = c(0.01, 1))
plot_t1_loec_c 

ap_t1_loec <- plot_grid(plot_t1_loec_c, plot_pow_loec_c, nrow = 2, rel_heights = c(1, 1.4))


# ggsave(plot = ap_t1_loec, filename = file.path(tikzp, 'ap_t1_loec.tikz'), device = tikz, 
#        height = 10, width = 14)




## ----------------------------------------------------------------------------
# XKCD-Plot for presentations

require(xkcd2)
# from https://github.com/EDiLD/xkcd2

# global power

pow_glob_c <- pow_glob_c[pow_glob_c$variable != 'glm_p', ]
# for the dataman
ratioxy <- 8
mapping <- aes(x = x,
               y = y,
               scale = scale,
               ratioxy = ratioxy,
               angleofspine = angleofspine,
               anglerighthumerus = anglerighthumerus,
               anglelefthumerus = anglelefthumerus,
               anglerightradius = anglerightradius,
               angleleftradius = angleleftradius,
               anglerightleg =  anglerightleg,
               angleleftleg = angleleftleg,
               angleofneck = angleofneck)
dataman <- data.frame(x = 4.5, y = 0.4, N = 9,
                       scale = 0.15 ,
                       ratioxy = ratioxy,
                       angleofspine = -pi/2 ,
                       anglerighthumerus = pi/2 + pi/2,
                       anglelefthumerus = pi/2 + pi/3,
                       anglerightradius = pi/2 +  pi/4,
                       angleleftradius = pi/2 +  pi/4,
                       angleleftleg = 3*pi/2 + pi / 12 ,
                       anglerightleg = 3*pi/2 - pi / 12,
                       angleofneck = runif(1, 3*pi/2 - pi/10, 3*pi/2 + pi/10)
)


p_pow_xkcd <- ggplot(pow_glob_c) +
  geom_smooth(aes(y = power, x = log2(muc), group = variable, color = variable),
              size = 1, se = FALSE, span = 0.8,
              position = position_jitter(width = 0.025),
              method = 'loess') +
  facet_grid( ~N, labeller = labeller(N = n_labeller)) +
  # axes
  labs(x = expression('Effect size'),
       y = 'Power') +
  scale_y_continuous(breaks = c(0, 0.5, 0.8, 1), labels = c('0', '50', '80', '100')) +
  scale_x_continuous(breaks = c(1.5, 6.5), labels = c('small', 'big')) +
  scale_color_manual('Model',
                     breaks = c("lm", "glm_nb", "glm_pb", "glm_qp", "np"),
                     values = c('#242F95', '#D8201B', '#D8821B', '#D8AF1B', '#139951'), 
                     labels = c("Normal", "Negative binomial", "Bootstrap", "Quasi Poisson", "Non parametric")
  ) +
  # decoration
  xkcdman(mapping, dataman) +
  geom_text(data = data.frame(x = 5.7, y = 0.55, N = 9, label = "Lo and behold!"),
            aes(x = x,y = y,label = label), size = 6,
            show.legend = F, family = "xkcd") +
  xkcdline(mapping = aes(xbegin = xb, ybegin = yb, xend = xe, yend = ye),
           data = data.frame(xb = 5.2, xe = 6, yb = 0.45, ye = 0.5, N = 9),
           xjitteramount = 0.4) + 
  theme_xkcd() +
  xkcdaxis(c(1,7), c(0, 1)) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0.01,1),
        legend.background = element_rect(fill = "gray90", size = .5, linetype = "dotted"),
        legend.text = element_text(size = 22),
        axis.text = element_text(size = 22, color = 'grey50'),
        axis.title = element_text(size = 25,face = "bold"),
        strip.text.x = element_text(size = 25))
p_pow_xkcd
ggsave(file.path(figp, 'p_pow_xkcd.pdf'), plot = p_pow_xkcd, 
       height = 10, width = 14)



# global t1 error

head(t1_glob_c)
t1_glob_c <- t1_glob_c[t1_glob_c$variable != 'glm_p', ]
# for the dataman
ratioxy <- 80
mapping <- aes(x = x,
               y = y,
               scale = scale,
               ratioxy = ratioxy,
               angleofspine = angleofspine,
               anglerighthumerus = anglerighthumerus,
               anglelefthumerus = anglelefthumerus,
               anglerightradius = anglerightradius,
               angleleftradius = angleleftradius,
               anglerightleg =  anglerightleg,
               angleleftleg = angleleftleg,
               angleofneck = angleofneck)
dataman <- data.frame(x = 6, y = 0.105, 
                      N = 3,
                      scale = 0.02,
                      ratioxy = ratioxy,
                      angleofspine = -pi/2 ,
                      anglerighthumerus = pi/6,
                      anglelefthumerus = pi/2 + pi/3,
                      anglerightradius = pi/2 +  pi/6,
                      angleleftradius = pi/4 + pi/16,
                      angleleftleg = 3*pi/2 + pi / 12 ,
                      anglerightleg = 3*pi/2 - pi / 12,
                      angleofneck = runif(1, 3*pi/2 - pi/10, 3*pi/2 + pi/10)
)

p_t1_xkcd <- ggplot(t1_glob_c) +
  geom_smooth(aes(y = power, x = log2(muc), group = variable, color = variable),
              size = 1, se = FALSE, span = 0.8,
              position = position_jitter(width = 0.025),
              method = 'loess') +
  facet_grid( ~N, labeller = labeller(N = n_labeller)) + 
  geom_hline(aes(yintercept = 0.05), linetype = 'dotted') +
  # axes
  scale_x_continuous(breaks = c(1.5, 6.5), labels = c('small', 'big')) +
  scale_y_continuous(breaks = c(0.01, 0.05, 0.15), limits = c(0.005, 0.16)) +
  labs(x = expression('Effect size'),
       y = 'Type I Error rate') +
  scale_color_manual('Model',
         breaks = c("lm", "glm_nb", "glm_pb", "glm_qp", "np"),
         values = c('#242F95', '#D8201B', '#D8821B', '#D8AF1B', '#139951'), 
         labels = c("Normal", "Negative binomial", "Bootstrap", "Quasi Poisson", "Non parametric")
         ) +
  # xkcd 
  xkcdman(mapping, dataman) +
  geom_text(data = data.frame(x = 5.5, y = 0.126, N = 3, 
                              label = "That was not expected..."),
            aes(x = x,y = y,label = label), size = 5,
            show.legend = F, family = "xkcd") +
  xkcdline(mapping = aes(xbegin = xb, ybegin = yb, xend = xe, yend = ye),
           data = data.frame(xb = 5.4, xe = 5.2, yb = 0.115, ye = 0.123, N = 3),
           xjitteramount = 0.4) +
  theme_xkcd() +
  xkcdaxis(c(1,7), c(0.01, 0.16)) +
  theme(legend.justification = c(1,1),
        legend.position = c(1,1),
        legend.background = element_rect(fill = "grey90", size = .5, linetype = "dotted"),
        legend.text = element_text(size = 22),
        axis.text = element_text(size = 22, color = 'grey50'),
        axis.title = element_text(size = 25, face = "bold"),
        strip.text.x = element_text(size = 25))
p_t1_xkcd
ggsave(file.path(figp, 'p_t1_xkcd.pdf'), plot = p_t1_xkcd, 
       height = 10, width = 14)
  
  
  
  
