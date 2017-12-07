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

#' @rdname addMarginalModel-method
#' @aliases addMarginalModel,CopyNumberMixtureSummary-method
setMethod("addMarginalModel", "CopyNumberMixtureSummary", function(summ) {
  result <- callNextMethod(summ)
  theor.df <- result@theoretical
  theor.df$copynumber <- factor(theor.df$copynumber,
                                c("marginal", levels(theor.df$copynumber)),
                                ordered=TRUE)
  theor.df$copynumber[is.na(theor.df$copynumber)] <- "marginal"
  result@theoretical <- theor.df
  result
})
