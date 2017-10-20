#' @include AllGenerics.R
NULL

#' An object summarizing the observed mixture model and theoretical models as plottable data.frames
#'
#' @slot observed data.frame of observed values, with batch and component metadata. Suitable for histogram plotting. Must have the following columns: "batch" (bactor), "component" (factor), "x.val" (numeric).
#' @slot predicted data.frame of predicted densities plotted over the range of observed values, with batch and component metadata. Suitable for line plotting. Must have the following columns: "batch" (factor), "component" (factor), "x" (numeric), "y" (numeric).
#' @slot nBins integer value for reccomended number of bins when plotting over the exact range of `.Data@observed$x.val` . `.Data@predicted$y` has been scaled to bin width, e.g. `diff(range(.Data@observed$x.val))/nBins` .
setClass("MixtureSummary",
         representation(observed="data.frame",
                        theoretical="data.frame",
                        nBins="integer"))

#' An object summarizing the observed mixture model and theoretical models as plottable data.frames.
#'
#' @slot observed data.frame of observed values. Must have the following columns: "batch" (bactor), "component" (factor), "copynumber" (factor), "x.val" (numeric). Otherwise identical to `MixtureSummary` .
#' @slot predicted data.frame of predicted densities. Must have the following columns: "batch" (factor), "component" (factor), "copynumber" (factor), "x" (numeric), "y" (numeric). Otherwise identical to `MixtureSummary` .
#' @slot nBins integer value for reccomended number of bins when plotting over the exact range of `.Data@observed$x.val` . Identical to `MixtureSummary` .
setClass("CopyNumberMixtureSummary",
         contains="MixtureSummary")
