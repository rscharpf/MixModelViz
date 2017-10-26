#  TODO: refactor plot_model to plot_summary?
#  TODO: include links to GraphicalParameters in plot_model documentation

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


setMethod("init_plot", c("CopyNumberMixtureSummary"), function(summ) {
  ggplot(mapping=aes(color=copynumber, fill=copynumber))
})


setMethod("init_plot", c("MixtureSummary"), function(summ) {
  ggplot(mapping=aes(color=component, fill=component))
})

setMethod("init_scales", c("MixtureSummary", "GraphicalParameters"),
          function(summ, params) {
            result <- list()
            if(!is.null(color_palette(params))) {
              result <- append(result,
                               scale_color_manual(name="Component",
                                                  values=color_palette(params)))
            }

            if(!is.null(fill_palette(params))) {
              result <- append(result,
                               scale_fill_manual(name="Component",
                                                 values=fill_palette(params)))
            }
            result
})

setMethod("init_scales", c("CopyNumberMixtureSummary", "GraphicalParameters"),
          function(summ, params) {
            result <- list(guides(color=guide_legend(override.aes=list(fill=NA))))
            if(!is.null(color_palette(params))) {
              result <- append(result,
                               scale_color_manual(name="Component",
                                                  values=color_palette(params)))
            }

            if(!is.null(fill_palette(params))) {
              result <- append(result,
                               scale_fill_manual(name="CopyNumber",
                                                 values=fill_palette(params)))
            }
            result
})

setMethod("plot_model", c("MixtureSummary", "GraphicalParameters"),
          function(summ, params) {

        ggp <- init_plot(summ) +
          do.call("geom_histogram",
                  modifyList(
                    list(
                      data=getObserved(summ),
                      mapping=aes(x.val, ..count..),
                      bins=nBins(summ),
                      position=position_stack()),
                    getHistogramParams(params))) +
          do.call("geom_line",
                  modifyList(
                    list(
                      data=getTheoretical(summ),
                      mapping=aes(x, y, group=component)),
                    getLineParams(params))) +
          init_scales(summ, params) +
          scale_y_sqrt()


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
})


setMethod("plot_model", c("MixtureSummary", "missing"),
          function(summ, params) plot_model(summ, new("GraphicalParameters")))
