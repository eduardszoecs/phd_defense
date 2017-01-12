# simulate data
set.seed(12345)
n <- 1000
x <- runif(n, min = -1, max = 1)
a <- 0.5
b <- 0.6
y_true <- exp(a + b * x)
shape <- 10
y <- rgamma(n, rate = shape / y_true, shape = shape)
df <- data.frame(x, y)



df$cens <- 1.5
df$iscens <- ifelse(df$y < df$cens, TRUE, FALSE)
df$y0 <- ifelse(df$iscens, 0, df$y)


plot(y ~ x, data = df, col = factor(df$iscens), ylim = c(0, 7))
points(df$x[df$iscens], df$y0[df$iscens], col = 'blue')
lines(sort(x), sort(y_true))

# plot raw data
library(ggplot2)
library(esmisc)
p_raw <- ggplot() +
  geom_point(data = df, aes(x = x, y = y, shape = iscens)) +
  scale_shape_manual('below LOQ?', values = c(16, 4)) +
  geom_hline(aes(yintercept = 1.5), col = 'orange', linetype = 'dashed', size = 1.5) +
  theme_edi()
p_raw


# prediction data.frame
pdat <- data.frame(x = df$x)

# lm on positive
mod_y <- lm(y ~ x, data = df[df$iscens == FALSE, ])
# pdat$y_lm_pos <- predict(mod_y, newdata = pdat, type = 'response')
  

# gamma glm on positive
mod_gy <- glm(y ~ x, data = df[df$iscens == FALSE, ], 
              family = Gamma(link = "log"))
pdat$y_glm_pos <- predict(mod_gy, newdata = pdat, type = 'response')

# true glm
mod_true <- glm(y ~ x, data = df, 
              family = Gamma(link = "log"))
pdat$true <- predict(mod_true, newdata = pdat, type = 'response')


# tobin regression (gaussian)
require(survival)
mod_tob <- survreg(Surv(df$y, df$iscens == FALSE, type = 'left') ~ x, 
                   data = df, dist = 'gaussian')
pdat$y_lm_tob <- predict(mod_tob, newdata = pdat, type = 'response')


#  tobin regression (gamma)
require(gamlss)
require(gamlss.cens)
require(gamlss.dist)
mod_gtob <- gamlss(Surv(df$y, df$iscens == FALSE, type = 'left') ~ x, data = df, 
                   family = cens(GA, type = 'left'))
pdat$y_glm_tob <- predict(mod_gtob, type = 'response')


# hurdle models
mod_zaga <- gamlss(y0 ~ x,                   # linear in mean
                   nu.formula = ~ x,         # nu linear
                   data = df, 
                   family = ZAGA)
pdat$y_zaga <- predict(mod_zaga, type = 'response') * 
  (1 - predict(mod_zaga, type = 'response', what = 'nu'))

pdatm <- melt(pdat, id.vars = 'x')
p_mod <- p_raw +
  geom_line(data = pdatm, aes(x = x, y = value, col = variable), size = 1.5) +
  scale_color_brewer('Model', palette = 'Dark2')
p_mod

pdat2 <- data.frame(x = df$x)
pdat2$loq <-  (1 - predict(mod_zaga, type = 'response', what = 'nu'))
pdat2$mean <-  predict(mod_zaga, type = 'response')

pdat2m <- melt(pdat2, id.vars = 'x')
pdat2m$variable <- mapvalues(pdat2m$variable, c('loq', 'mean'), c('p(>LOQ)', 'mu | > LOQ'))
p_zaga <- ggplot(pdat2m, aes(x = x, y = value)) +
  geom_line(size = 1.5, aes(color = '#66a61e')) +
  facet_grid(variable~., scales = 'free_y') +
  scale_color_identity() +
  theme_edi()
p_zaga

library(cowplot)
p_comb <- plot_grid(p_mod, p_zaga, rel_widths = c(2, 1))
p_comb
ggsave('/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/figs/zaga_mods.pdf', p_comb,
       width = 14, height = 7)
