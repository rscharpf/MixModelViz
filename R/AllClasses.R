#' @include AllGenerics.R
NULL

#' A plottable summary of a mixture model
#'
#' An object summarizing the observed mixture model and theoretical models as plottable data.frames
#' @slot observed data.frame of observed values, with batch and component metadata. Suitable for histogram plotting. Must have the following columns: "batch" (bactor), "component" (factor), "x.val" (numeric).
#' @slot predicted data.frame of predicted densities plotted over the range of observed values, with batch and component metadata. Suitable for line plotting. Must have the following columns: "batch" (factor), "component" (factor), "x" (numeric), "y" (numeric).
#' @slot nBins integer value for reccomended number of bins when plotting over the exact range of `.Data@observed$x.val` . `.Data@predicted$y` has been scaled to bin width, e.g. `diff(range(.Data@observed$x.val))/nBins` .
#' @export
setClass("MixtureSummary",
         representation(observed="data.frame",
                        theoretical="data.frame",
                        nBins="integer"))


#' A plottable summary of a mixture model and inferred copy number
#'
#' An object summarizing the observed mixture model and theoretical models as plottable data.frames.
#' @slot observed data.frame of observed values. Must have the following columns: "batch" (bactor), "component" (factor), "copynumber" (factor), "x.val" (numeric). Otherwise identical to `MixtureSummary` .
#' @slot predicted data.frame of predicted densities. Must have the following columns: "batch" (factor), "component" (factor), "copynumber" (factor), "x" (numeric), "y" (numeric). Otherwise identical to `MixtureSummary` .
#' @slot nBins integer value for reccomended number of bins when plotting over the exact range of `.Data@observed$x.val` . Identical to `MixtureSummary` .
#' @export
setClass("CopyNumberMixtureSummary",
         contains="MixtureSummary")


setClassUnion("vectorOrNULL", c("vector", "NULL"))


#' Default graphical parameters for plotting a MixtureSummary object.
#'
#' Default parameters for color and fill scales, fixed aesthetic for histogram and line plots. Set any value to NULL to remove a parameter from the plotting function call.
#' @slot fill.palette A vector of colors used for ggplot's fill aesthetic. Values may be any accepted value (including character or numeric) for `scale_fill_manual(value=)`. Set to NULL to remove the `scale_fill_manual` call from the `plot_model` output. The vector may optionally be named. Default value is a named vector for values 1:5.
#' @slot color.palette A vector of colors used for ggplot's color aesthetic. Values may be any accepted value (including character or numeric) for `scale_color_manual(value=)`. Set to NULL to remove the `scale_color_manual` call from the `plot_model` output. The vector may optionally be named. Default value is a named vector for values 1:5.
#' @slot histogram.alpha Numeric vector of length 1 or the number of observed data points. Values should be acceptable by scales::alpha(), that is, in the range [0,1]. Set to null to remove the alpha argument from the `geom_histogram` call. Default value is 0.5
#' @slot histogram.linetype Vector of length 1 or the number of observed data points. Value type is not checked, but should be an integer, a name, or a string. See help(ggplot2::aes_linetype_size_shape) for more details. Set to null to remove the linetype argument from the `geom_histogram` call. Default value is 0 (blank).
#' @slot histogram.size Numeric vector of length 1 or the number of observed data points. Set to null to remove the alpha argument from the `geom_histogram` call. Default value is 0.
#' @slot line.alpha Numeric vector of length 1 or the number of points representing the theoretical model. Values should be acceptable by scales::alpha(), that is, in the range [0,1]. Set to null to remove the alpha argument from the `geom_line` call. Default value is 1.
#' @slot line.linetype Vector of length 1 or the number of points representing the theoretical model. Value type is not checked, but should be an integer, a name, or a string. See help(ggplot2::aes_linetype_size_shape) for more details. Set to null to remove the linetype argument from the `geom_line` call. Default value is 1 (solid).
#' @slot line.size Numeric vector of length 1 or the number of points representing the theoretical model. Set to null to remove the alpha argument from the `geom_line` call. Default value is 2.
#' @export
setClass("GraphicalParameters",
         representation(
           fill.palette="vectorOrNULL",
           color.palette="vectorOrNULL",
           histogram.alpha="numeric",
           histogram.linetype="ANY",
           histogram.size="numeric",
           line.alpha="numeric",
           line.linetype="ANY",
           line.size="numeric"
         ),
         prototype(
           fill.palette=c("1"="#56B4E9", "2"="#E69F00", "3"="#009E73", "4"="#F0E442", "5"="#0072B2"),
           color.palette=c("1"="#56B4E9", "2"="#E69F00", "3"="#009E73", "4"="#F0E442", "5"="#0072B2"),
           histogram.alpha=0.5,
           histogram.linetype=0,
           histogram.size=0,
           line.alpha=1,
           line.linetype=1,
           line.size=2))
