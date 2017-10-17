#' @export
plot_batch <- function(best.model, tiles.df, use.copy_number=TRUE, plot.title=NULL, plot.subtitle=NULL) {
  
  if(missing(tiles.df)) {
    obs.data <- oned(best.model)
    obs.batches <- batch(best.model)
    obs.component <- apply(as.matrix(probz(best.model)), 1, which.max)
  } else {
    obs.data <- tiles.df$logratio
    obs.batches <- tiles.df$batch
#    obs.batch_names <- tiles.df$batch.var

    obs_matched_to_model <- match(tiles.df$tile, tileSummaries(tiles.df)$tile)
    
    tiles.probz <- probz(best.model)
    obs.probz <- tiles.probz[obs_matched_to_model, ]
    obs.component <- apply(as.matrix(obs.probz), 1, which.max)
  }

  
  model.class <- class(best.model)
  cn.model <- switch(
    model.class,
    SingleBatchModel=SingleBatchCopyNumber(best.model),
    SingleBatchPooled=SingleBatchCopyNumber(best.model),
    MultiBatchModel=MultiBatchCopyNumber(best.model),
    MultiBatchPooled=MultiBatchCopyNumberPooled(best.model),
    {
      stop("Model class could not be converted to Copy Number model\n")
    })
  cn.mapping <- factor(mapping(mapComponents(cn.model)), ordered=TRUE)

  lrr.df <- data.frame(
    lrr.val=obs.data,
    model.component=obs.component,
    cn.component=cn.mapping[obs.component],
    batch=obs.batches
  )

  if(class(best.model) %in% c("SingleBatchModel", "SingleBatchPooled")) {
    lrr.df$batch <- factor(1)
    component_sizes <- matrix(table(lrr.df$model.component), ncol=1)
  }
  else if(class(best.model) %in% c("MultiBatchModel", "MultiBatchPooled")) {
    component_sizes <- matrix(
      table(lrr.df$model.component, lrr.df$batch),
      nrow=k(best.model))
    
    lrr.df <- rbind(lrr.df, transform(lrr.df, batch="marginal"))
    lrr.df$batch <- factor(lrr.df$batch, levels=c("marginal", seq(ncol(component_sizes)), ordered=TRUE))
    
  } else {
    stop("Unknown model class\n")
  }
  
  
  # Generate points for density plots
  nBins <- ceiling(sqrt(length(obs.data)))
  binSize <- diff(range(obs.data))/nBins  # used to scale density to histogram
  
  nBatches <- ncol(component_sizes)
  thetas <- unname(matrix(theta(best.model), nrow=nBatches))
  sigmas <- unname(matrix(sigma(best.model), nrow=nBatches))

  if(model.class %in% c("SingleBatchPooled", "MultiBatchPooled")) {
    sigmas <- matrix(replicate(k(best.model), c(sigmas)), ncol=k(best.model))
  }

  densities.df <- Reduce(rbind, lapply(seq(nBatches), function(b) {
    Reduce(rbind, lapply(seq(k(best.model)), function(k) {
      transform(
        data.frame(
          theta=thetas[b,k],
          sigma=sigmas[b,k],
          batch=b,
          model.component=k,
          cn.component=cn.mapping[k],
          x=seq(min(lrr.df$lrr.val), max(lrr.df$lrr.val), length=1000)),
        y=dnorm(x, theta, sigma)*binSize*component_sizes[k,b]
      )
    }))
  }))
  
  densities.df$batch <- factor(densities.df$batch, levels(lrr.df$batch), ordered=TRUE)
  
  # Aesthetic values
  palette.brighter <- c("1"="#56B4E9", "2"="#E69F00", "3"="#009E73", "4"="#F0E442", "5"="#0072B2")
  palette.darker <- apply(col2rgb(palette.brighter), 2,
                          function(x) do.call(rgb, as.list(pmax(x - 50, 0)/255)))

  # Facet values and legend positioning
  n_facets <- nBatches + 1
  if((n_facets / round(sqrt(n_facets))) %% 1 != 0) {
    legend_position <- theme(legend.position = c(1, 0),
                             legend.justification = c(1, 0))
  } else legend_position <- theme()

  batch_labels <- with(tiles.df, setNames(unique(batch), unique(batch.var)))
  batch_labels <- setNames(
    paste("batch", batch_labels, ":", names(batch_labels)),
    batch_labels)
  batch_labels["marginal"] = "marginal"
  
  
  ggplot(lrr.df, aes(fill=cn.component, color=cn.component)) +
    geom_histogram(aes(lrr.val, ..count..), alpha=0.5, linetype=0,
                   bins=nBins, position=position_stack(reverse=TRUE)) +
    geom_line(data=densities.df, aes(x,y, group=model.component)) +
    scale_fill_manual(values=palette.brighter) +
    scale_color_manual(values=palette.darker) +
    guides(fill=guide_legend(title="Component"),
           color = guide_legend(title="Component", override.aes = list(linetype=1)),
           alpha = guide_legend(title="Component")) +
    scale_y_sqrt() +
    facet_wrap("batch", scales="free_y", ncol=round(sqrt(n_facets)), labeller=labeller(batch=batch_labels)) + 
    legend_position + 
    labs(title=plot.title, subtitle=plot.subtitle)

}
