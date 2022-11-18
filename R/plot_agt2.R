#' plot_agt2
#' @description Specialized ggplot2 based plot function to visualize the aggregated coherence values.
#' @param df_agt  data.frame - with aggregated coherence values computed by 'LinguGeo::aggregate_coh'
#' @return returns a ggplot2 based visualization for the aggregated coherence values.
#' @author Andreas Schönberg
#' @export plot_agt2
#' @aliases plot_agt2
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
#' # calculate coherence
#' coh_breit <- coherence(dat = merged_plc,cl = "breitVok",xcord = 8,ycord = 9,nk = 5,na_value = NA)
#'
#' # get NA dummy data
#'
#' coh_NA <- coh_breit
#' coh_NA$coh[1:800] <- NA
#' coh_NA2 <- coh_breit
#' coh_NA$coh[1:200] <- NA
#'
#' # list coherence items
#' coh_ls <- list(coh_NA,
#'                coh_NA2)
#'
#' # aggregate coherence
#' mau_agt <- aggregate_coh(ls = coh_ls)
#'
#' head(mau_agt)
#'
#' # plot aggregated coherence
#' plot_agt2(mau_agt)

plot_agt2 <- function(df_agt){

  # check for NA
  if(any(df_agt$amount==0)==T){

    # split NA
    df_agt_s1 <- subset(df_agt,df_agt$amount!=0)
    df_agt_s2 <- subset(df_agt,df_agt$amount==0)
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
