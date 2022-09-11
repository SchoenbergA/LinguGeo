#' plot_agt
#' @description Specialised ggplot2 based plot function to visualize the aggregated coh values.
#' @param agt  data.frame - with aggregated coh values computed by 'LinguGeo::aggregate_coh'
#' @return returns a ggplot2 based visualization for the aggregated coh values..
#' @author Andreas Schönberg
#' @export plot_agt
#' @aliases plot_agt
#' @examples
#' # load librarys
#' require(LinguGeo)

plot_agt <- function(agt){

  plt <- ggplot(df_agt, aes(xcord, ycord,
                   col = agt,
                   alpha = n
)) +
  geom_point(size = 3) +
  theme(legend.position="left") +
  scale_color_viridis(discrete = F)+
  labs(x="", y="")

  return(plt)
} # end of function
