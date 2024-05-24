# to do calculation of total N and total CV, need to faff a bit...

# args:
#  vp         result from callinf dsm.varprop
#  distdata    data given to ddf
#  preddata    prediction data

est_N_var_ss <- function(vp, distdata, preddata){

  # first need average group sizes in each class
  ss_avg <- plyr::ddply(distdata, plyr::.(size_class), plyr::summarize, ss_mean=mean(save_size))
  # and their empirical variances
  ss_var <- plyr::ddply(distdata, plyr::.(size_class), plyr::summarize, ss_var=var(save_size))

  # abundance per size class
  preds <- cbind(preddata, N=vp$pred)
  N_by_ss <- plyr::ddply(preds, plyr::.(size_class), plyr::summarise, N=sum(N))

  # now construct the estimate
  sum_term <- merge(N_by_ss, ss_var, by="size_class")
  varN <- vp$var + sum(sum_term$N^2*sum_term$ss_var)

  # get total abundance by a weighted sum over the size class abundance ests
  sum_term <- merge(N_by_ss, ss_avg, by="size_class")
  totalN <-  sum(sum_term$N*sum_term$ss_mean)

  totalCV <- sqrt(varN)/totalN

  return(list(N    = totalN,
              varN = varN,
              cvN  = totalCV))
}
