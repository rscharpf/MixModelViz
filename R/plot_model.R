#  TODO: add force_singlebatch to collapse densities for multibatch/pooled models

##
## DM:  - Too many arguments to pass.
##      - plot_model is doing too much work
##
## Suggest the following pseudocode as a template, where the function summarize
## returns all the data that is needed for plotting ( a list of tibbles )
##
## sums <- summarize( model )  ## a list of tibbles: (observed and predicted)
## plotModel( sums )
##
## The only other argument (optional) would be a list of parameters for the
## plotting (fill_aes, etc).  i.e.,
##
## params <- graphicalParams() ## returns list with defaults
## plotModel( sums, params )
##
##

#' Plotting function for `MixtureSummary` objects
#'
#' Plots normal densities from theoretical mixture models over a histogram of observed data.
#'
#' @param summ a `MixtureSummary` object
#' @param copynumber_mapping An integer-vector of length `k` that indicates the
#'      predicted copy number for theoretical components `1:k`
#' @param fill_aes should be "copynumber" or "component", but can be any value
#'      that can be evaluated by `aes_(fill=as.name(fill_aes))` where `data=obs.df`
#' @param palette A vector of colors. To specify separate vectors for fill and
#    color aesthetics, provide a list named vectors, `fill` and `color`.
#' @param fixed_aes a list of alpha, linetype, and size values that will be
#'      passed on to geom_hist and geom_line as fixed aesthetic parameters.
#'      The named list should contain named vectors "hist" and "line".
#' @return An object of class `ggplot`
#' @examples
#' summ <- summarize(sbm.obj)
#' plot_model(summ, fill_aes="component")
#' summ.cn <- summarize(mbcn.obj)
#' plot_model(summ, fill_aes="copynumber")
#' @export
#'
plot_model <- function(summ, copynumber_mapping,
                       fill_aes="copynumber",
                       palette=c("1"="#56B4E9", "2"="#E69F00", "3"="#009E73",
                                 "4"="#F0E442", "5"="#0072B2"),
                       fixed_aes=list(hist=list(alpha=0.5, linetype=0,  size=0),
                                      line=list(alpha=1,   linetype=1,  size=1))
                       ) {

  if(is.list(palette)) {
    stopifnot(all(c("color", "fill") %in% names(palette)))
  } else {
    palette <- list(color=palette, fill=palette)
  }

  ggp <- ggplot(mapping=aes_(color=as.name(fill_aes), fill=as.name(fill_aes))) +
    geom_histogram(data=getObserved(summ), aes(x.val, ..count..), alpha=fixed_aes$hist$alpha,
                   linetype=fixed_aes$hist$linetype, size=fixed_aes$hist$size,
                   bins=nBins(summ), position=position_stack()) + #reverse=TRUE)) +
    geom_line(data=getTheoretical(summ), aes(x,y, group=component),
              alpha=fixed_aes$line$alpha, linetype=fixed_aes$line$linetype,
              size=fixed_aes$line$size) +
    scale_color_manual(name="Component", values=palette$color) +
    scale_y_sqrt()

  if(fill_aes=="component") {
    ggp <- ggp + scale_fill_manual(name="Component", values=palette$fill)
  } else {
    fill_name <- fill_aes
    if (fill_aes=="copynumber") fill_name <- "Copy Number"
    ggp <- ggp + scale_fill_manual(name=fill_name, values=palette$fill) +
      guides(color=guide_legend(override.aes=list(fill=NA)))
  }


  nBatches <- length(levels(getTheoretical(summ)$batch))
  # Faceting and legend positioning
  if (nBatches > 1) {
    batch_labels <- paste("batch", seq(nBatches))
    # if("batch.var" %in% names(obs.df)) {
    #   batch_names <- unique(obs.df[,c("batch", "batch.var")])
    #   batch_labels <- paste(batch_labels, ":",
    #                         batch_names$batch.var[order(batch_names$batch)])
    # }
    names(batch_labels) <- seq(nBatches)
    batch_labels["marginal"] = "marginal"

    ggp <- ggp + facet_wrap("batch", scales="free_y",
                            ncol=round(sqrt(nBatches + 1)),
                            labeller=labeller(batch=batch_labels))


    n_facets <- nBatches + 1
    if((n_facets / round(sqrt(n_facets))) %% 1 != 0) {
      ggp <- ggp + theme(legend.position = c(1, 0),
                         legend.justification = c(1, 0),
                         legend.box="horizontal")
    }
  }

  ggp
}
