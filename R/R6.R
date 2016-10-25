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
                                           function(cue) {
                                             if (cue > 0) {
                                               element <- array(dim = c(nSim, nItems, tests_per_cue[cue]))
                                             } else {
                                               element <- NULL
                                             }
                                             return(element)
                                           })

                   private$tests_taken <- lapply(tests_per_cue, function(...) 0)

                 },

                 toGo = function(cue) {
                   return(self$nFeatures - self$PR_strengths[[cue]])
                 },

                 above = function() {
                   return(self$CR_thresholds)
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
                   # Which items are recallable?
                   self$recalled[[cue]][,,test_number] <- self$PR_strengths[[cue]] > self$CR_thresholds
                   correct_index <- which(self$recalled[[cue]][,,test_number])

                   if (increment) {
                     self$CR_thresholds[correct_index] <- self$CR_thresholds[correct_index] -
                       private$CR_learning(corrects = correct_index,
                                           p = value(self$TR))
                     self$PR_strengths[[cue]][correct_index] <- self$PR_strengths[[cue]][correct_index] +
                       private$PR_learning(cue = cue, p = value(self$LR))[correct_index]

                   }

                   private$tests_taken[[cue]] <- test_number
                   invisible(self)
                 },

                 freeRecall = function(cue) {}),

               private = list(

                 tests_taken = NULL,

                 PR_learning = function(cue, p) {
                   learned <- rbinom(n = length(self$PR_strengths[[cue]]),
                                     size = self$toGo(cue),
                                     prob = p)
                   return(learned)
                 },

                 CR_learning = function(corrects, p) {
                   lowered <- rbinom(n = length(corrects),
                                     size = self$above()[corrects],
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
                 }
                )
)
