#' aggregate_coh
#' @description aggregates the coherence values for each site
#' @param ls  list - list of dataframes computed by 'LinguGeo::coh'
#' @return returns the aggregated coherence values divided by n entries which are !=NA for each site
#' @author Andreas Schönberg
#' @export aggregate_coh
#' @aliases aggregate_coh
#' @examples
#' # load librarys


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
