# library(plyr)

# when using this for factor-smooth interactions, we are actually looking
# at the observed vs. expected number of GROUPS not the number of
# individuals -- is this okay??
obs_exp <- function(model, term) {
  oe <- model$data
  oe$Npredict <- predict(model)


  oe <- plyr::ddply(oe, plyr::as.quoted(term), function(x) {
    data.frame(Observed = sum(x[[as.character(model$formula)[2]]]),
      Expected = sum(x$Npredict))
  })

  cn <- oe[, 1]
  oe <- t(oe[, 2:3])
  colnames(oe) <- cn

  return(oe)
}
