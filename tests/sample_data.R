
set.seed(919)

param_list <- list(ER=.6, LR=.15, FR=.1, TR=.1, TV = .05, Tmax = 75, Tmin = 0.5, lambda = .5)

beta_timed <- initPCRparams(params = param_list, distribution = "beta", nItems = 20,
                            nSims = 1000, nFeatures = 100, time = 10)

binom_timed <- initPCRparams(params = param_list, distribution = "binomial",nItems = 20,
                             nSims = 1000, nFeatures = 100, time = 10)

beta_untimed <- initPCRparams(params = param_list, distribution = "beta", nItems = 20,
                              nSims = 1000, nFeatures = 100)

binom_untimed <- initPCRparams(params = param_list, distribution = "binomial",nItems = 20,
                               nSims = 1000, nFeatures = 100)

# Initial Study
beta_t_studied <- study(beta_timed, nCues = 2, tests_per_cue = c(1,1))
binom_t_studied <- study(binom_timed, nCues = 2, tests_per_cue = c(1,1))
beta_studied <- study(beta_untimed, nCues = 2, tests_per_cue = c(1,1))
binom_studied <- study(binom_untimed, nCues = 2, tests_per_cue = c(1,1))

# Restudy
beta_t_restudied <- study(beta_t_studied, cue = 1)
binom_t_restudied <- study(binom_t_studied, cue = 1)

# Test
beta_t_tested <- cuedRecall(beta_t_studied, cue = 1)
binom_t_tested <- cuedRecall(binom_t_studied, cue = 1)

# No Practice
beta_t_no_prac <- forget(beta_t_studied, cue = 1)
binom_t_no_prac <- forget(binom_t_studied, cue = 1)
