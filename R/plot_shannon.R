#' plot_shannon
#' @description Specialized plot function for the Shannon entropy.
#' @param df  data.frame in the format provided by 'LinguGeo::calc_shannon' (see details).
#' @param asd Boolean - If FALSE will plot the shannon entropy, if TRUE will plot the Language Item classes and use alpha based on shannon entropy values. Default=F

#' @return A plot with the shannon entropy or the language item classes in different colors and the shannon entropy as alpha.

#' @author Andreas Schönberg
#' @export plot_shannon
#' @aliases plot_shannon
#' @examples
#' # load librarys
#' require(LinguGeo)
#' require(ggplot2)
#' # load data
#' coh <- read.csv(system.file("extdata","coh_utm.csv",package = "LinguGeo"))
#' # take a look
#' head(coh)
#' # calculate shannon entropy
#' df_shn <- calc_shannon(coh)
#' # plot shannon entropy
#' plot_shannon(df_shn)
#' # plot add data
#' plot_shannon(df_shn,add_data=T)

plot_shannon <- function(df,add_data=F){
# check input
if(any(colnames(df=="shn"))==F){
  stop("shannon entropy values are missing!")
}

# fork to display shannon only or data and shannon as alpha
if(add_data==F){
  ggplot(coh, aes(xcord, ycord,
                  col = shn)) +
    geom_point(size = 3) +
    labs(x="", y="") +
    theme_bw() +
    theme(legend.position = "none") +
    scale_color_gradient(low = "black", high = "yellow")
} else {
  ggplot(coh, aes(xcord, ycord,
                  col = data,
                  alpha = shn)) +
    geom_point(size = 3) +
    theme_bw() +
    labs(x="", y="") +
    theme(legend.position = "none") +
    scale_color_brewer(palette = "Set1")
}
} # end of function
