min_val <- 0
max_val <- 100

param_list <- list(ER=.6, LR=.15, FR=.1, TR=.1, TV = .05, Tmax = 75, Tmin = 0.5, lambda = .5)

# Memory data
mem <-structure(c(56L, 55L, 61L, 51L, 99L, 51L, 59L, 50L, 51L, 56L,
                  50L, 53L, 54L, 57L, 58L),
                .Dim = c(3L, 5L))
thresh <- structure(c(56L, 46L, 46L, 50L, 98L, 02L, 43L, 52L, 56L, 49L,
                      51L, 48L, 47L, 51L, 54L),
                    .Dim = c(3L, 5L))

mem_continuous <- structure(c(0.615723615968426, 0.589972537391328, 0.467912515995964,
                              0.522620685556245, 0.607614257503363, 0.980055358207064, 0.514484390626524,
                              0.576597597433024, 0.585887959352946, 0.580708899736991, 0.658038268679618,
                              0.562303542618935, 0.646057922583702, 0.002138009703025, 0.628875291380699),
                            .Dim = c(3L, 5L)) * max_val

thresh_continuous <- structure(c(0.482994295176146, 0.554661452924281, 0.992227641234684,
                                 0.556897972078241, 0.485085693043906, 0.558457860883398, 0.454974711341695,
                                 0.488205791059824, 0.529774619827727, 0.593478434672639, 0.449333432257201,
                                 0.484600716134854, 0.0365049940911, 0.442111441874386, 0.497987068879674),
                               .Dim = c(3L, 5L)) * max_val

beta_timed <- initPCRparams(params = param_list, distribution = "beta", nItems = 20,
                            nSims = 1000, nFeatures = 100, time = 10)

binom_timed <- initPCRparams(params = param_list, distribution = "binomial",nItems = 20,
                             nSims = 1000, nFeatures = 100, time = 10)

beta_untimed <- initPCRparams(params = param_list, distribution = "beta", nItems = 20,
                              nSims = 1000, nFeatures = 100)

binom_untimed <- initPCRparams(params = param_list, distribution = "binomial",nItems = 20,
                               nSims = 1000, nFeatures = 100)

# Initial Study
beta_t_studied <- study(beta_timed, nCues = 2)
binom_t_studied <- study(binom_timed, nCues = 2)
beta_studied <- study(beta_untimed, nCues = 2)
binom_studied <- study(binom_untimed, nCues = 2)

# Restudy
beta_t_restudied <- study(beta_t_studied, cue = 1)
binom_t_restudied <- study(binom_t_studied, cue = 1)

# Test
beta_t_tested <- cuedRecall(beta_t_studied, cue = 1)
binom_t_tested <- cuedRecall(binom_t_studied, cue = 1)

# No Practice
beta_t_no_prac <- forget(beta_t_studied, cue = 1)
binom_t_no_prac <- forget(binom_t_studied, cue = 1)
