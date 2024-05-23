# Automate the cutting and sticking of the data into the correct format
# for the "fs" basis to use

# assumes that the size of the groups is in a column called "size"
# it will create a new variable called "size_class" based on the cuts
# this will be a factor variable

# args:
#  obs         detection function (distance) data (to give to ddf()) AND observation data
#  segs        segment data (as for dsm)
#  cuts        cut points for the data (e.g. c(0, 1, max(df_data$size)))
#  cut_labels  labels to give the cuts

# Example
# fs_data <- make_ss_fs_data(obs, segs, c(0,1,10), c("one", "big"))
make_ss_fs_data <- function(obs, segs, cuts, cut_labels){

  # check everything makes sense
  n_cuts <- length(diff(cuts))
  if(!is.null(cut_labels)){
    if(n_cuts != length(cut_labels)){
      stop("Length of cuts and cut_labels doesn't make sense!")
    }
  }

  ## distance/observation data
  obs_ss <- obs
  obs_ss$size_class <- cut(obs_ss$size, cuts, labels=cut_labels)
  obs_ss$Sample.Label <- paste0(obs_ss$Sample.Label, "-",
                                obs_ss$size_class)
  obs_ss$save_size <- obs_ss$size
  obs_ss$size <- 1

  ## segment data
  # replicate the segments as many times as there are segments
  segs_ss <- segs[rep(1:nrow(segs), n_cuts),]

  # set a factor variable with the class names
  segs_ss$size_class <- as.factor(cut_labels[sort(rep(1:n_cuts, nrow(segs)))])
  # make a segment label that includes the size class
  segs_ss$Sample.Label <- paste0(segs_ss$Sample.Label, "-", segs_ss$size_class)

  return(list(obs=obs_ss, segs=segs_ss))
}
