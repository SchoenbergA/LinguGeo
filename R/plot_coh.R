#' plot_coh
#' @description Specialized plot function for the Coherence index.
#' @param df  data.frame in the format provided by 'coherence_index' (see deteails).
#' @param degree  charater - name of column containing the strings
#' @param invers boolean -

#' @return A plot with the language item classes in different colors and the coherence index as alpha values.

#' @author Andreas Schönberg
#' @export plot_coh
#' @aliases plot_coh
#' @examples
#' # load data
#' coh <- read.csv(system.file("extdata","coh_utm.csv",package = "LinguGeo"))
#' # take a look
#' head(coh)
#' # plot the coherence index and laguage items
#' plot_coh(coh)
#' # adjust intensity
#' plot_coh(coh,degree=0.2)
#' # plot inverse
#' plot_coh(coh,inverse=T)



plot_coh <- function(df,degree = 1, inverse = F) {

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
  return(plt)
}
