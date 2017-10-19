#' @include help.R
NULL

#' @export
setGeneric("ObservedNames", function(object) standardGeneric("ObservedNames"))

#' @export
setGeneric("TheoreticalNames", function(object) standardGeneric("TheoreticalNames"))

#' @export
setGeneric("getObserved", function(object) standardGeneric("getObserved"))

#' @export
setGeneric("getTheoretical", function(object) standardGeneric("getTheoretical"))

#' @export
setGeneric("nBins", function(object) standardGeneric("nBins"))

#' @export
setGeneric("summarizeObserved", function(model) standardGeneric("summarizeObserved"))

#' @export
setGeneric("summarizeTheoretical", function(model) standardGeneric("summarizeTheoretical"))

#' Summarize a MixtureModel into a set of data.frames that can be plotted
#'  (observed and predicted)
#'
#' @param model a MixtureModel-derived object
#' @return A MixtureSummary object of two data.frames (observed and predicted) that can be plotted
#' @export
#' @rdname summarize-method
setGeneric("summarize", function(model) standardGeneric("summarize"))

#' @export
setGeneric("getThetaMatrix", function(model) standardGeneric("getThetaMatrix"))

#' @export
setGeneric("getSigmaMatrix", function(model) standardGeneric("getSigmaMatrix"))
