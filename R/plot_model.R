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


setMethod("init_ggplot", c("MixtureSummary", "list"), function(summ) {
  ggplot(mapping=aes(color=component, fill=component)) +
  scale_color_manual(name="Component", values=plot.params$color) +
    scale_fill_manual(name="Component", values=plot.params$fill)
})

setMethod("init_ggplot", c("CopyNumberMixtureSummary", "list"), function(summ) {
  ggplot(mapping=aes(color=copynumber, fill=copynumber)) +
  scale_color_manual(name="Copy Number", values=plot.params$color) +
    scale_fill_manual(name="Copy Number", values=plot.params$fill) +
    guides(color=guide_legend(override.aes=list(fill=NA)))
})

add_batch_facetting <- function(summ) {
  result <- list()
  nBatches <- length(levels(getTheoretical(summ)$batch))
  if (nBatches > 1) {
    batch_labels <- paste("batch", seq(nBatches))
    # if("batch.var" %in% names(obs.df)) {
    #   batch_names <- unique(obs.df[,c("batch", "batch.var")])
    #   batch_labels <- paste(batch_labels, ":",
    #                         batch_names$batch.var[order(batch_names$batch)])
    # }
    names(batch_labels) <- seq(nBatches)
    batch_labels["marginal"] = "marginal"

    result <- facet_wrap("batch", scales="free_y",
                            ncol=round(sqrt(nBatches + 1)),
                            labeller=labeller(batch=batch_labels))


    n_facets <- nBatches + 1
    if((n_facets / round(sqrt(n_facets))) %% 1 != 0) {
      result <- result + theme(legend.position = c(1, 0),
                         legend.justification = c(1, 0),
                         legend.box="horizontal")
    }
  }
  result
}

#' Plotting function for `MixtureSummary` objects
#'
#' Plots normal densities from theoretical mixture models over a histogram of observed data.
#'
#' @param summ a `MixtureSummary` object
#' @param plot.params Currently a list of fixed aesthetics that can be passed to
#'      ggplot geom_ functions. Named list elements correspond to geom_* and
#'      scale_*_manual functions. Elements of geom_* elements are used as
#'      fixed aestheics. Elements of scale_*_manual elements are passed to the
#'      named argument `values`.
#' @return An object of class `ggplot`
#' @examples
#' summ <- summarize(sbm.obj)
#' plot_summary(summ, fill_aes="component")
#' summ.cn <- summarize(mbcn.obj)
#' plot_summary(summ, fill_aes="copynumber")
#' @export
#'
setMethod("plot_summary", c("MixtureSummary", "list"), function(summ, plot.params=list(
    color=c("1"="#56B4E9", "2"="#E69F00", "3"="#009E73", "4"="#F0E442", "5"="#0072B2"),
    fill=c("1"="#56B4E9", "2"="#E69F00", "3"="#009E73", "4"="#F0E442", "5"="#0072B2"),
    hist=list(alpha=0.5, linetype=0, size=0),
    line=list(alpha=1,   linetype=1,  size=1))) {

  init_ggplot(summ, plot.params) +
    geom_histogram(data=getObserved(summ),
                   aes(x.val, ..count..),
                   alpha=plot.params$hist$alpha,
                   linetype=plot.params$hist$linetype,
                   size=plot.params$hist$size,
                   bins=nBins(summ),
                   position=position_stack()) +
    geom_line(data=getTheoretical(summ),
              aes(x,y, group=component),
              alpha=plot.params$line$alpha,
              linetype=plot.params$line$linetype,
              size=plot.params$line$size) +
    scale_y_sqrt() +
    add_batch_facetting(summ)
})


