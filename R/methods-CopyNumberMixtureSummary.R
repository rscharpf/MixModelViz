#' @include AllClasses.R
NULL

setMethod("ObservedNames", "CopyNumberMixtureSummary",
          function(object) c(callNextMethod(object), "copynumber"))

setMethod("TheoreticalNames", "CopyNumberMixtureSummary",
          function(object) c(callNextMethod(object), "copynumber"))
