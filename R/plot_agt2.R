#' plot_agt2
#' @description Specialized ggplot2 based plot function to visualize the aggregated coherence values.
#' @param df_agt  data.frame - with aggregated coherence values computed by 'LinguGeo::aggregate_coh'
#' @return returns a ggplot2 based visualization for the aggregated coherence values.
#' @author Andreas Schönberg
#' @export plot_agt2
#' @aliases plot_agt2
#' @examples
#' # load librarys
#' require(LinguGeo)

plot_agt2 <- function(df_agt){

  # check for NA
  if(any(df_agt$n==0)==T){

    # split NA
    df_agt_s1 <- subset(df_agt,df_agt$n!=0)
    df_agt_s2 <- subset(df_agt,df_agt$n==0)
    plt <- ggplot2::ggplot(df_agt_s1, aes(xcord, ycord,
                              col = agt,
                              alpha = amount
    )) +
      geom_point(size = 3) +
      theme(legend.position="left") +
      viridis::scale_color_viridis(discrete = F)+
      labs(x="", y="")+
      geom_point(data=df_agt_s2, size=0.5,color="black",alpha=0.90)

    return(plt)
  } else {
  plt <- ggplot2::ggplot(df_agt, aes(xcord, ycord,
                   col = agt,
                   alpha = amount
)) +
  geom_point(size = 3) +
  theme(legend.position="left") +
  viridis::scale_color_viridis(discrete = F)+
  labs(x="", y="")

  return(plt)
  }
} # end of function
