#' plot_coh
#' @description Specialized plot function for the Coherence index.
#' @param df  data.frame in the format provided by 'coherence_index' (see deteails).
#' @param degree  charater - name of column containing the strings
#' @param invers boolean -
#' @param est_dens boolean - If TRUE performs a KDE and adds the contours to the plot. Default = FALSE
#' @param coh_th numeric - Threshold for the coherence index to use for KDE (only used if 'est_dens' = T). Default=NULL
#' @param bins numeric - Number of countour bins for KDE (only used if 'est_dens' = T). Default=10

#' @return A plot with the language item classes in different colors and the coherence index as alpha values.

#' @author Andreas Schönberg
#' @export plot_coh
#' @aliases plot_coh
#' @examples
#' # load librarys
#' require(LinguGeo)
#' require(dplyr)
#' # load data
#' coh <- read.csv(system.file("extdata","coh_utm.csv",package = "LinguGeo"))
#' # take a look
#' head(coh)
#' # plot the coherence index and language items
#' plot_coh(coh)
#' # adjust intensity
#' plot_coh(coh,degree=0.2)
#' # plot inverse
#' plot_coh(coh,inverse=T)
#' # plot with KDE
#' plot_coh(coh,est_dens=T,coh_th=0.8)



plot_coh <- function(df,degree = 1, inverse = F,est_dens=F, coh_th=NULL,bins=10) {

  # switch for inverse
  if(inverse == T) {
    # require plot for language items class in color and inverse coh as alpha
    plt <- ggplot(df, aes(xcord, ycord,
                              col = data,
                              alpha = 1-nrm)) +
      geom_point(size = 3) +
      labs(x="", y="") +
      theme(legend.position = "none") +
      scale_color_brewer(palette = "Set1")
  } else {
    # plot natural coherence index as adjustible intensity by alpha
    plt <- ggplot(df, aes(xcord, ycord,
                              col = data,
                              alpha = ifelse(nrm < degree, nrm, 1))) +
      geom_point(size = 3) +
      labs(x="", y="") +

      theme(legend.position = "none") +
      scale_color_brewer(palette = "Set1")
  }

  # optional KDE density display
  if(est_dens==T){

    # check input
    if(is.null(coh_th)){
      stop("coherence threshold value missing!")}
      if(coh_th>1){
        warning("coherence threshold value is > 1 ")
      }

    # plot coh + KDE density
    return(plt + geom_density_2d(data = filter(df, nrm < coh_th),
                                  #h = 0.08,
                                  alpha = 1,
                                  bins = bins,
                                  col = "black"))
  } # else plot coh only
  return(plt)
}


