#' aggregate_coh
#' @description aggregates the coherence values for each site
#' @param ls  list - list of dataframes computed by 'LinguGeo::coh'
#' @return returns the aggregated coherence values divided by n entries which are !=NA for each site
#' @author Andreas Schönberg
#' @export aggregate_coh
#' @aliases aggregate_coh
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


aggregate_coh <- function(ls){

  # get empty vector to check coordinates
  crs_check <- vector()
  # get ID based on coordinates
  for(i in 1:length(ls)){
    ls[[i]]$ID_cords <- ls[[i]]$xcord+ls[[i]]$ycord
    # sort by ID
    ls[[i]] <- ls[[i]][order(ls[[i]]$ID_cords),]
    crs_check[i] <- nchar(ls[[i]]$ID_cords[1])
  }

  # check for mixed CRS
  if(stats::var(crs_check)!=0){
    warning("Length of coordinate strings are not equal ! ")
  }


  # select xy coords and coh
  df_agt <- ls[[1]][,c(1,2,4)]
  colnames(df_agt)[3] <- paste0("coh_",1)
  for(i in 2:length(ls)){
    # cbind the coh column
    df_agt <- cbind(df_agt,ls[[i]][,4])
    colnames(df_agt)[2+i] <- paste0("coh_",i)
  }

  # calculate aggregation value agt
  nc <- ncol(df_agt)

  for(i in 1:nrow(df_agt)){
    df_agt$agt[i] <- sum(df_agt[i,c(3:nc)],na.rm = T)/
      (length(ls)-length(  which(is.na(df_agt[i,c(3:nc)])==T)))
    # add column with n entires !=NA
    df_agt$amount[i] <- length(which(is.na(df_agt[i,c(3:nc)])==F))

  }
  return(df_agt)
}# end of function
