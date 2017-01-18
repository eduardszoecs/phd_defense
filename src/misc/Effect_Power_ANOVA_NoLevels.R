


sim <- function(n_groups = 3, 
                n_rep = 3, 
                m = 100, # mean in treatments
                r = 80,  # mean in effect treatment (=last)
                sigma = 1){
  df <- expand.grid(trt = factor(seq_len(n_groups)), 
                    rep = seq_len(n_rep))
  betas <- c(rep(m, n_groups-1), r)
  
  # linear predictor
  X <- model.matrix(~0+trt, data = df)
  nu <- X %*% betas
  # identitiy link
  mu <- nu
  # sample from normal
  y <- rnorm(n = nrow(X), mean = mu, sd = sigma)
  
  df <- data.frame(df, y = y)
  # plot(y ~ trt, data = df)
  return(df)
}


ana <- function(df){
  # fit model
  mod <- lm(y ~ trt, data = df)
  # test for treatment
  ft <- drop1(mod, test = 'F')
  # return pvalue
  p <- ft$`Pr(>F)`[2]
  return(p)
}


# define scenarious

scen <- expand.grid(n_groups = seq(2, 50, by = 2), 
                    n_rep = c(3, 6, 9),
                    m = 100, 
                    r = 80,
                    sigma = 10)
nsim = 250

# run sims
library(pbapply)
res <- pblapply(seq_len(nrow(scen)), # for each scenariou
                FUN = function(i){
                  takedf <- scen[i , ]
                  # create data
                  sims <- replicate(n = nsim, sim(n_groups = takedf$n_groups,
                                            n_rep = takedf$n_rep,
                                            m = takedf$m,
                                            r = takedf$r,
                                            sigma = takedf$sigma),
                                      simplify = FALSE)
                  # run tests
                  res <- pbsapply(sims, ana)
                  # calc power 
                  out <- c(mean = mean(res < 0.05))
                  return(out)
                },
                cl = 7)

# compile & plot results
df <- data.frame(scen, pwr = unlist(res))
library(ggplot2)
p <- ggplot(df, aes(x = n_groups, y = pwr, col = factor(n_rep))) +
  geom_line() +
  labs(y = 'Power', x = 'Number of trt levels',
       col = 'Replicates') +
  scale_color_brewer(palette = 'Dark2') +
  ggtitle('Power analysis for different numbers of treatment levels.',
          subtitle = 'Treatment mean = 100, last treatment was reduced to 80.
          Distribution: Norman; Link: Identity; Sigma_res = 10;
          250 simulation runs per scenario') +
  theme_edi()
p

figp <- '/home/edisz/Documents/work/research/projects/2016/1PHD/phd_defense/figs/'
ggsave(file.path(figp, 'p_power_trt.pdf'), plot = p)
