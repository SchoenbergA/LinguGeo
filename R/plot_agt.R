#' plot_agt
#' @description ggplot2 based plot function in order to visualize the aggregated coherence values
#' @param df_agt  data.frame - with aggregated coherence values computed by 'LinguGeo::aggregate_coh'
#' @return returns a ggplot2 based visualization for the aggregated coherence values
#' @author Andreas Schönberg
#' @export plot_agt
#' @aliases plot_agt
#' @examples
#' # load data
#' mau_class <- read.csv(system.file("extdata","Classification_Maurer.csv",package = "LinguGeo"),row.names = 1)
#' head(mau_class)
#'
#' # set planar projection for correct euclidean distances
#'
#' # define source and target projection
#' src <- "+proj=longlat +datum=WGS84 +no_defs" # WGS 84 (Long / Lat)
#' trg <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # UTM 32
#'
#' # convert ellipsoidal coordinates to planar
#' mau_class_utm <- convertCRS(df = mau_class,
#'                             pos_x = 1,
#'                             pos_y = 2,
#'                             src_proj = src,
#'                             trg_proj = trg,
#'                             colnames_trg = c("utm_x","utm_y") )
#'
#' head(mau_class_utm)
#'
#' # merge multiple places and clean duplicates
#' merged_plc <- mergeMultiPlaces(df = mau_class_utm,pos_x = 8,pos_y = 9,
#'                                col_ls = 3:7,
#'                                class_col_ls = 3:7)
#'
#' head(merged_plc)
#'
#' # calculate coherence for each language items
#' coh_heute <- coherence(dat = merged_plc,cl = "heuteVok",xcord = 8,ycord = 9,nk = 5,na_value = NA)
#' coh_breit <- coherence(dat = merged_plc,cl = "breitVok",xcord = 8,ycord = 9,nk = 5,na_value = NA)
#' coh_VokStem <- coherence(dat = merged_plc,cl = "wachsenVokStem",xcord = 8,ycord = 9,nk = 5,na_value = NULL)
#' coh_VokEnd<- coherence(dat = merged_plc,cl = "wachsenVokEnd",xcord = 8,ycord = 9,nk = 5,na_value = NULL)
#' coh_butter <- coherence(dat = merged_plc,cl = "butter",xcord = 8,ycord = 9,nk = 5,na_value = NULL)
#'
#' # list coherence items
#' coh_ls <- list(coh_heute,
#'                coh_breit,
#'                coh_VokStem,
#'                coh_VokEnd,
#'                coh_butter)
#'
#' # aggregate coherence
#' mau_agt <- aggregate_coh(ls = coh_ls)
#'
#' head(mau_agt)
#'
#' # plot aggregated coherence
#' plot_agt(mau_agt)

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
