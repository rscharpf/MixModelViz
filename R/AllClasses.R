#' @include AllGenerics.R
NULL

setClass("TempMix", contains="MixtureModel")
setClass("TempMixCN", contains="TempMix")
setClass("TempSBM", contains=c("SingleBatchModel", "TempMix"))
setClass("TempSBP", contains=c("SingleBatchPooled", "TempSBM"))
setClass("TempSBCN", contains=c("SingleBatchCopyNumber", "TempSBM", "TempMixCN"))
setClass("TempMBM", contains=c("MultiBatchModel", "TempMix"))
setClass("TempMBP", contains=c("MultiBatchPooled", "TempMBM"))
setClass("TempMBCN", contains=c("MultiBatchCopyNumber", "TempMBM", "TempMixCN"))
setClass("TempMBCNP", contains=c("MultiBatchCopyNumberPooled", "TempMBM", "TempMixCN"))




setClass("MixtureSummary",
         representation(observed="data.frame",
                        theoretical="data.frame",
                        nBins="integer"))

setClass("CopyNumberMixtureSummary",
         contains="MixtureSummary")
