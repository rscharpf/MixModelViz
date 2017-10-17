# Todo - remove or infer filtered outliers discovered in `upsample.CopyNumberDataFrame`

library(CNPBayes)

# Proposed function for creating a data.frame from a MixtureModel object for plotting by `plot_model`
create.CopyNumberDataFrame <- function(model.obj) {
  if (hasMethod(copyNumber, class(model.obj))) {
    cn.model <- model.obj
  } else {
    cn.model <- CopyNumberModel(model.obj)
  }

  data.frame(
    x.val=oned(model.obj),
    batch=batch(model.obj),
    component=factor(map_z(model.obj), seq(k(model.obj)), ordered=TRUE),
    copynumber=factor(copyNumber(cn.model), levels=seq(max(mapping(cn.model))), ordered=TRUE)
  )
}

# Upsamples a data.frame using tile information in `ds.tib`, assumes identies of
#   rows in `cn.df` correspond to those of `ds.tib`.

upsample.CopyNumberDataFrame <- function(cn.df, ds.tib) {
  upsample_model <- match(ds.tib$tile, tileSummaries(ds.tib)$tile)
  upsampled_cn.df <- cn.df[upsample_model, ]
  if (max(cn.df$batch) > 1)
    stopifnot(ds.tib$batch == upsampled_cn.df$batch)  # Cannot be tested with `cn.df` derived from SingleBatch/Pooled models
  upsampled_cn.df$x.val <- ds.tib$logratio
  upsampled_cn.df$batch.var <- ds.tib$batch.var
  upsampled_cn.df
}
