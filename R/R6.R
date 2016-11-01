#' @import R6
#' @import parameters
#' @export
PCR <- R6Class("PCR",
               public = list(
                 ER = NULL,
                 LR = NULL,
                 TR = NULL,
                 FR = NULL,
                 nFeatures = NULL,
                 nSim = NULL,
                 nItems = NULL,
                 PR_strengths = NULL,
                 CR_thresholds = NULL,
                 recalled = NULL,
                 recall_order = NULL,
                 initialize = function(ER = .5, LR = .15, TR = .1, FR = .1,
                                       nSim = 1000, nItems = 15, nFeatures = 100,
                                       tests_per_cue = list(one = 1)) {

                   # Initialize the parameter fields
                   self$ER <- new("parameter", value = ER, name = "encoding_rate",
                                  upper_bound = 1, lower_bound = 0)
                   self$LR <- new("parameter", value = LR, name = "learning_rate",
                                  upper_bound = 1, lower_bound = 0)
                   self$TR <- new("parameter", value = TR, name = "threshold_reduction",
                                  upper_bound = 1, lower_bound = 0)
                   self$FR <- new("parameter", value = FR, name = "forgetting_rate",
                                  upper_bound = 1, lower_bound = 0)

                   # Initialize experiment characteristics
                   self$nSim <- nSim
                   self$nItems <- nItems
                   self$nFeatures <- nFeatures

                   # Initialize strengths and thresholds
                   self$PR_strengths <- replicate(length(tests_per_cue),
                                                  matrix(0L, nSim, nItems),
                                                  simplify = FALSE)
                   self$PR_strengths <- setNames(self$PR_strengths, nm = names(tests_per_cue))

                   self$CR_thresholds <- private$initialize_thresholds()

                   # Initialize Recall Outcomes Arrays
                   self$recalled <- lapply(tests_per_cue,
                                           function(tests) {
                                             if (tests > 0) {
                                               element <- array(dim = c(nSim, nItems, tests))
                                             } else {
                                               element <- NULL
                                             }
                                             return(element)
                                           })

                   self$recall_order <- self$recalled

                   private$tests_taken <- lapply(tests_per_cue, function(...) 0)

                 },

                 study = function(cue) {

                   # Calls PR_learning method with probability parameter ER
                   # and adds the result to the current activations
                   # for each item associated with the given cue
                   self$PR_strengths[[cue]] <-  self$PR_strengths[[cue]] +
                     private$PR_learning(cue = cue, p = value(self$ER))
                   invisible(self)

                 },

                 restudy = function(cue) {

                   # Calls PR_learning method with probability parameter LR
                   # and adds the result to the current activations
                   # for each item associated with the given cue
                   self$PR_strengths[[cue]] <- self$PR_strengths[[cue]] +
                     private$PR_learning(cue = cue, p = value(self$LR))
                   invisible(self)

                 },

                 forgetting = function(cue) {

                   # Calls PR_forgetting method with probability parameter FR
                   # and subtracts the result from the current activations
                   # for each item associated with the given cue
                   self$PR_strengths[[cue]] <- self$PR_strengths[[cue]] -
                     private$PR_forgetting(cue = cue, p = value(self$FR))
                   invisible(self)

                 },

                 cuedRecall = function(cue, increment = TRUE) {

                   # Keep track of what test we're on for this cue
                   test_number <- private$tests_taken[[cue]] + 1
                   # Record which items are recallable
                   private$score_corrects(cue, test_number)

                   # Record the correct output order
                   for (i in 1:self$nSim) {
                     x <- self$recalled[[cue]][i,,test_number]
                     self$recall_order[[cue]][i,x,test_number] <- 1:sum(x)
                   }

                   if (increment) {
                     private$testing_increments(cue)
                   }

                   private$tests_taken[[cue]] <- test_number
                   invisible(self)

                 },

                 freeRecall = function(cue, increment = TRUE) {


                   # Keep track of what test we're on for this cue
                   test_number <- private$tests_taken[[cue]] + 1
                   # Record which items are recallable
                   private$score_corrects(cue, test_number)

                   # Record the correct output order
                   for (i in 1:self$nSim) {
                     # Subset out a logical vector reporting whether each individual item
                     # was recalled (TRUE) or not recalled (FALSE). This vector is in
                     # simulation order
                     corrects <- self$recalled[[cue]][i,,test_number]

                     # Subset out a numeric vector reporting the position of memory stregnths,
                     # sort from largest to smallest. The first element in retrieval_order gives
                     # the *position* of the element with the largest PR activation, the second
                     # element gives the *position* of the element with the second
                     # largest PR activation, etc.
                     retrieval_order <- order(self$PR_strengths[[cue]][i,],
                                           decreasing = TRUE)

                     # Use the retrieval_order vector to sort the corrects vector from simulation
                     # order into retrieval order. So now the corrects vector tells you whether or not
                     # the first retrieval attempt was successful, whether or not the second
                     # retrieval attempt was successful, etc.
                     corrects <- corrects[retrieval_order]

                     # Subset the retrieval_order vector with the corrects vector,
                     # so as to fill in only elements in positions of retrieved items
                     # with an output order
                     self$recall_order[[cue]][i, retrieval_order[corrects], test_number] <- 1:sum(corrects)
                    }

                   # Incrementing memory strengths must be done after determining recall order,
                   # because the order of recall depends on the memory strengths. If it is done before,
                   # the rank order of memory strengths will not be the same as when accuracy was checked
                   if (increment) {
                     private$testing_increments(cue)
                   }

                   private$tests_taken[[cue]] <- test_number
                 }),

               private = list(

                 tests_taken = NULL,

                 toGo = function(cue) {
                   return(self$nFeatures - self$PR_strengths[[cue]])
                 },

                 distance_above = function() {
                   return(self$CR_thresholds)
                 },

                 PR_learning = function(cue, p) {
                   learned <- rbinom(n = length(self$PR_strengths[[cue]]),
                                     size = private$toGo(cue),
                                     prob = p)
                   return(learned)
                 },

                 CR_learning = function(corrects, p) {
                   lowered <- rbinom(n = length(corrects),
                                     size = private$distance_above()[corrects],
                                     prob = p)
                   return(lowered)
                 },

                 PR_forgetting = function(cue, p) {
                   forgotten <- rbinom(n = length(self$PR_strengths[[cue]]),
                                       size = self$PR_strengths[[cue]],
                                       prob = p)
                   return(forgotten)
                 },

                 initialize_thresholds = function() {
                   self$CR_thresholds <- matrix(self$nFeatures, self$nSim, self$nItems)
                   thresholds <- self$CR_thresholds -
                     private$CR_learning(matrix(TRUE, self$nSim, self$nItems),
                                         p = .5)
                 },

                 testing_increments = function(cue, test_number) {

                   # corrects is an index into the item/list matrices
                   # which tells you the linear position of the recalled items
                   corrects <- private$find_corrects(cue, test_number)

                   self$CR_thresholds[corrects] <- self$CR_thresholds[corrects] -
                     private$CR_learning(corrects = corrects,
                                         p = value(self$TR))
                   self$PR_strengths[[cue]][corrects] <- self$PR_strengths[[cue]][corrects] +
                     private$PR_learning(cue = cue, p = value(self$LR))[corrects]
                 },

                 above_threshold = function(cue) {

                   return(self$PR_strengths[[cue]] >= self$CR_thresholds)

                 },

                 score_corrects = function(cue, test_number) {

                   self$recalled[[cue]][,,test_number] <- private$above_threshold(cue)

                 },

                 find_corrects = function(cue, test_number) {

                   correct_index <- which(self$recalled[[cue]][,,test_number])
                   return(correct_index)
                 }
                )
)


PCRt <- R6Class("PCRt",
                inherit = PCR,
                public = list(
                  Tmin = NULL,
                  Tmax = NULL,
                  lambda = NULL,
                  Time = NULL,
                  RT = NULL,
                  initialize = function(Tmin = 1, Tmax = 30, lambda = 1, Time = 90, ...) {

                    self$Time <- Time

                    self$Tmin <- new("parameter", value = Tmin, name = "min_RT",
                                     upper_bound = Tmax, lower_bound = 0)
                    self$Tmax <- new("parameter", value = Tmax, name = "max_RT",
                                   upper_bound = self$Time, lower_bound = Tmin)
                    self$lambda <- new("parameter", value = lambda, name = "threshold_reduction",
                                   upper_bound = Inf, lower_bound = 0)

                    super$initialize(...)

                    self$RT <- self$recalled
                  },

                  cuedRecall = function(cue, increment = TRUE) {

                    test_number <- private$tests_taken[[cue]] + 1
                    private$calculate_RT(cue, test_number)
                    super$cuedRecall(cue, increment)
                    r <- self$recalled[[cue]][,,test_number]
                    self$RT[[cue]][[,,test_number]][r] <- NA
                  },

                  freeRecall = function(cue, increment = TRUE) {

                    # Keep track of what test we're on for this cue
                    test_number <- private$tests_taken[[cue]] + 1

                    private$calculate_RT(cue, test_number)

                    recallable <- private$above_threshold(cue)

                    all_lists_retrieval_orders <- t(apply(self$PR_strengths[[cue]], 1, order,
                                                          decreasing = TRUE))

                    # Record the correct output order
                    for (i in 1:self$nSim) {#

                      # Subset out a numeric vector reporting the position of memory stregnths,
                      # sort from largest to smallest. The first element in retrieval_order gives
                      # the *position* of the element with the largest PR activation, the second
                      # element gives the *position* of the element with the second
                      # largest PR activation, etc.
                      retrieval_order <- all_lists_retrieval_orders[i,]

                      # Use the retrieval_order vector to sort the RT vector for the current list
                      # from simulation order into retrieval order.
                      RTs <- cumsum(self$RT[[cue]][i, retrieval_order, test_number])

                      # Determine which items were recalled by considering if they were above threshold,
                      # and if the cummulative RT at the point of recall is less than the total alloted
                      # time for the recall episode
                      corrects <- recallable[i, retrieval_order] &  RTs <= self$Time

                      # Insert the logical vector into the correct row of the $recalled field arrays
                      # Importantly, it must be inserted in simulation order
                      self$recalled[[cue]][i, retrieval_order, test_number] <- corrects

                      # Subset the retrieval_order vector with the corrects vector,
                      # so as to fill in only elements in positions of retrieved items
                      # with an output order

                      self$RT[[cue]][i, retrieval_order[corrects], test_number] <- c(RTs[1],
                                                                                     diff(RTs[corrects]))
                      self$RT[[cue]][i, retrieval_order[!corrects], test_number] <- NA
                      self$recall_order[[cue]][i, retrieval_order[corrects], test_number] <- 1:sum(corrects)
                    }

                    # Incrementing memory strengths must be done after determining recall order,
                    # because the order of recall depends on the memory strengths. If it is done before,
                    # the rank order of memory strengths will not be the same as when accuracy was checked
                    if (increment) {
                      private$testing_increments(cue)
                    }

                    private$tests_taken[[cue]] <- test_number
                  }
                ),

                private = list(

                  calculate_RT = function(cue, test_number) {

                    self$RT[[cue]][,,test_number] <- value(self$Tmin) + (value(self$Tmax)-value(self$Tmin)) *
                      exp(-value(self$lambda) * abs(self$PR_strengths[[cue]]-self$CR_thresholds))

                  },

                  score_corrrects = function(cue, test_number) {

                    self$recalled[[cue]][,,test_number] <- private$above_threshold(cue) &
                      self$RT[[cue]] <= self$Time

}

                )
)
