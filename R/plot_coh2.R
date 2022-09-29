#' plot_coh
#' @description ggplot2 based plot function in order to plot the coherence values
#' @param df  data.frame - in the format provided by 'LinguGeo::coherence'
#' @param est_dens boolean - if TRUE performs a KDE and adds the contours to the plot. Default = FALSE
#' @param coh_th numeric - threshold for the coherence index to use for KDE (only used if 'est_dens' = T). Default=NULL
#' @param bins numeric - number of bins for KDE (only used if 'est_dens' = T). Default=10

#' @return A plot with the language item classes in different colors and the coherence index as alpha values.

#' @author Andreas Schönberg
#' @export plot_coh
#' @aliases plot_coh
#' @examples
#' # load librarys


plot_coh <- function(df,est_dens=F,coh_th=NULL,bins=10) {

  # check for NA
  if(any(is.na(df$coh))){
    # get subsets
    s1 <- subset(df,!is.na(df$coh)) # all non NA
    s2 <- subset(df,is.na(df$coh)  )# all NA

    # plot
    plt <- ggplot2::ggplot(s1, aes(xcord, ycord,
                   col = data,
                   alpha = 1-coh)) +
      geom_point(size = 3) +
      theme(legend.position="left") +
      viridis::scale_color_viridis(discrete = T)+
      labs(x="", y="", caption = ". NA") +
      geom_point(data=s2, size=0.5,color="black",alpha=0.90)


    # optional KDE density display
    if(est_dens==T){

      # check input
      if(is.null(coh_th)){
        stop("coherence threshold value missing!")}
      if(coh_th>1){
        warning("coherence threshold value is > 1 ")
      }

      # plot coh + KDE density
      return(plt + ggplot2::geom_density_2d(data = dplyr::filter(df, coh < coh_th),
                                   #h = 0.08,
                                   alpha = 1,
                                   bins = bins,
                                   col = "black"))
    }


  } else {
    # plot
    plt <- ggplot2::ggplot(df, aes(xcord, ycord,
                          col = data,
                          alpha = 1-coh)) +
      geom_point(size = 3) +
      labs(x="", y="") +
      theme(legend.position="left") +
      viridis::scale_color_viridis(discrete = T)
    # optional KDE density display
    if(est_dens==T){

      # check input
      if(is.null(coh_th)){
        stop("coherence threshold value missing!")}
      if(coh_th>1){
        warning("coherence threshold value is > 1 ")
      }

      # plot coh + KDE density
      return(plt + ggplot2::geom_density_2d(data = dplyr::filter(df, coh < coh_th),
                                   #h = 0.08,
                                   alpha = 1,
                                   bins = bins,
                                   col = "black"))
    }

  }

  return(plt)

} # end of function

