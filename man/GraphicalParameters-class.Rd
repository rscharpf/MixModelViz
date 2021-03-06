% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{GraphicalParameters-class}
\alias{GraphicalParameters-class}
\title{Default graphical parameters for plotting a MixtureSummary object.}
\description{
Default parameters for color and fill scales, fixed aesthetic for histogram and line plots. Set any value to NULL to remove a parameter from the plotting function call.
}
\section{Slots}{

\describe{
\item{\code{fill.palette}}{A vector of colors used for ggplot's fill aesthetic. Values may be any accepted value (including character or numeric) for `scale_fill_manual(value=)`. Set to NULL to remove the `scale_fill_manual` call from the `plot_summary` output. The vector may optionally be named. Default value is a named vector for values 1:5.}

\item{\code{color.palette}}{A vector of colors used for ggplot's color aesthetic. Values may be any accepted value (including character or numeric) for `scale_color_manual(value=)`. Set to NULL to remove the `scale_color_manual` call from the `plot_summary` output. The vector may optionally be named. Default value is a named vector for values c("marginal", 1, 2, 3, 4, 5).}

\item{\code{histogram.alpha}}{Numeric vector of length 1 or the number of observed data points. Values should be acceptable by scales::alpha(), that is, in the range [0,1]. Set to null to remove the alpha argument from the `geom_histogram` call. Default value is 0.5}

\item{\code{histogram.linetype}}{Vector of length 1 or the number of observed data points. Value type is not checked, but should be an integer, a name, or a string. See help(ggplot2::aes_linetype_size_shape) for more details. Set to null to remove the linetype argument from the `geom_histogram` call. Default value is 0 (blank).}

\item{\code{histogram.size}}{Numeric vector of length 1 or the number of observed data points. Set to null to remove the alpha argument from the `geom_histogram` call. Default value is 0.}

\item{\code{line.alpha}}{Numeric vector of length 1 or the number of points representing the theoretical model. Values should be acceptable by scales::alpha(), that is, in the range [0,1]. Set to null to remove the alpha argument from the `geom_line` call. Default value is 1.}

\item{\code{line.linetype}}{Vector of length 1 or the number of points representing the theoretical model. Value type is not checked, but should be an integer, a name, or a string. See help(ggplot2::aes_linetype_size_shape) for more details. Set to null to remove the linetype argument from the `geom_line` call. Default value is 1 (solid).}

\item{\code{line.size}}{Numeric vector of length 1 or the number of points representing the theoretical model. Set to null to remove the alpha argument from the `geom_line` call. Default value is 2.}
}}

