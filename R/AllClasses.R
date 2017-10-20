#' @include AllGenerics.R
NULL

#' A temporary `MixtureModel` superclass
#'
#' A temporary, abstract superclass for `MixtureModel` objects
setClass("TempMix", contains="MixtureModel")

#' A temporary `SingleBatchModel` superclass
#'
#' A temporary, abstract class for `SingleBatchModel` objects, may be coerced as follows: `as(singlebatchmodel.obj, "TempSBM")`
setClass("TempSBM", contains=c("SingleBatchModel", "TempMix"))

#' A temporary `SingleBatchPooled` superclass
#'
#' A temporary, abstract class for `SingleBatchPooled` objects
setClass("TempSBP", contains=c("SingleBatchPooled", "TempSBM"))

#' A temporary `SingleBatchCopyNumber` superclass
#'
#' A temporary, abstract class for `SingleBatchCopyNumber` objects
setClass("TempSBCN", contains=c("SingleBatchCopyNumber", "TempSBM"))

#' A temporary `MultiBatchModel` superclass
#'
#' A temporary, abstract class for `MultiBatchModel` objects
setClass("TempMBM", contains=c("MultiBatchModel", "TempMix"))

#' A temporary `MultiBatchPooled` superclass
#'
#' A temporary, abstract class for `MultiBatchPooled` objects
setClass("TempMBP", contains=c("MultiBatchPooled", "TempMBM"))

#' A temporary `MultiBatchCopyNumber` superclass
#'
#' A temporary, abstract class for `MultiBatchCopyNumber` objects
setClass("TempMBCN", contains=c("MultiBatchCopyNumber", "TempMBM"))

#' A temporary `MultiBatchCopyNumberPooled` superclass
#'
#' A temporary, abstract class for `MultiBatchCopyNumberPooled` objects
setClass("TempMBCNP", contains=c("MultiBatchCopyNumberPooled", "TempMBM"))



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
