#' plot_shannon
#' @description ggplot2 based plot function in order to plot the local measure of Shannon entropy
#' @param df  data.frame - in the format provided by 'LinguGeo::calc_shannon' (see details)
#' @param add_data boolean - if FALSE will plot the Shannon entropy, if TRUE will plot the language item classes and use alpha based on Shannon entropy values. Default=FALSE

#' @return a plot with the Shannon entropy or the language item classes in different colors and the Shannon entropy as alpha

#' @author Andreas Schönberg
#' @export plot_shannon
#' @aliases plot_shannon
#' @examples
#' # load librarys


plot_shannon <- function(df,add_data=F){
# check input
if(any(colnames(df=="shn")==F)){
  stop("shannon entropy values are missing!")
}

# fork to display shannon only or data and shannon as alpha
if(add_data==F){
  ggplot2::ggplot(df, aes(xcord, ycord,
                  col = shn)) +
    geom_point(size = 3) +
    labs(x="", y="") +
    theme_bw() +
    theme(legend.position = "none") +
    scale_color_gradient(low = "black", high = "yellow")
} else {
  ggplot2::ggplot(df, aes(xcord, ycord,
                  col = data,
                  alpha = shn)) +
    geom_point(size = 3) +
    theme_bw() +
    labs(x="", y="") +
    theme(legend.position = "none") +
    scale_color_brewer(palette = "Set1")
}
} # end of function
