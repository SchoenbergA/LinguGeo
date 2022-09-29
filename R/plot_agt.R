#' plot_agt
#' @description ggplot2 based plot function in order to visualize the aggregated coherence values
#' @param df_agt  data.frame - with aggregated coherence values computed by 'LinguGeo::aggregate_coh'
#' @return returns a ggplot2 based visualization for the aggregated coherence values
#' @author Andreas Schönberg
#' @export plot_agt
#' @aliases plot_agt
#' @examples
#' # load librarys
#' require(LinguGeo)

plot_agt <- function(df_agt){

  plt <- ggplot2::ggplot(df_agt, aes(xcord, ycord,
                   col = agt,
                   alpha = amount
)) +
  geom_point(size = 3) +
  theme(legend.position="left") +
  viridis::scale_color_viridis(discrete = F) +
  labs(x="", y="")

  return(plt)
} # end of function
