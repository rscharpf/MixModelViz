#' @include methods-MixtureSummary.R
NULL


#' @include help.R
NULL


#' @rdname ObservedNames-method
#' @aliases ObservedNames,CopyNumberMixtureSummary-method
setMethod("ObservedNames", "CopyNumberMixtureSummary",
          function(object) c(callNextMethod(object), "copynumber"))


#' @rdname TheoreticalNames-method
#' @aliases TheoreticalNames,CopyNumberMixtureSummary-method
setMethod("TheoreticalNames", "CopyNumberMixtureSummary",
          function(object) c(callNextMethod(object), "copynumber"))
