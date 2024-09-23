cacheDataset = function(df, location = NULL, use_compression = TRUE) {

  if (is.null(location)) {
    location <- deparse(substitute(df))
    if (!(dir.exists("./data-cache"))) {
      dir.create("./data-cache")
    }
  }

  outPath <- paste0('./cached-data/', location, '.rds')

  saveRDS(df, outPath, compress = use_compression)

  if (file.exists(outPath)) {
    message(paste0('The R object ',
                   deparse(substitute(df)),
                   ' was saved to "',
                   outPath,
                   '"'))
  }

  }
