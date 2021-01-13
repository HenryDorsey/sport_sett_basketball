stan_df <- roll_merged_df %>%
  arrange(team_code.ToI, date)

colnames(stan_df)

bt_data <-list(N = nrow(baked_train),
                     K = 12,
                     J = 30,
                     L = 3,
                     y = baked_train$Rdiff,
                     cauchy_mean= 0,
                     cuachy_sd = 2.5,
                     lkj_corr = 2,
                     gamma_mean = 0,
                     gamma_sd = 5
)

fit <- stan(
  file = '~/Fall2020/STAT6341/baseball.stan',  
  data = baseball_data,     # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 1,              # number of cores (could use one per chain)
  refresh = 1           # no progress shown
)

