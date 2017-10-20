#' @include AllClasses.R
NULL

setMethod("ObservedNames", "MixtureSummary",
          function(object) c("x.val", "batch", "component"))

setMethod("TheoreticalNames", "MixtureSummary",
          function(object) c("batch", "component", "x", "y"))

setMethod("getObserved", "MixtureSummary", function(object) object@observed)

setMethod("getTheoretical", "MixtureSummary", function(object) object@theoretical)

setMethod("nBins", "MixtureSummary", function(object) object@nBins)

setValidity("MixtureSummary", function(object) {
  observed.names <- ObservedNames(object)
  theoretical.names <- TheoreticalNames(object)

  result <- c()

  observed.missing <- ! observed.names %in% names(getObserved(object))
  if (any(observed.missing)) {
    result <- sprintf("Column \"%s\" is missing from the observed mixture data frame.",
                      observed.names[observed.missing])
  }

  theoretical.missing <- ! theoretical.names %in% names(getTheoretical(object))
  if (any(theoretical.missing)) {
    result <- c(result,
                sprintf("Column \"%s\" is missing from the theoretical mixture data frame.",
                        theoretical.names[theoretical.missing]))
  }

  if (length(result)) {
    result
  } else {
    TRUE
  }
})
