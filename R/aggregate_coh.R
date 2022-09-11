#' aggregate_coh
#' @description aggregates the coh values for each place.
#' @param ls_coh  list - list of coh items
#' @return returns the aggregated coh values devided by n entries which are !=NA for each place.
#' @author Andreas Schönberg
#' @export aggregate_coh
#' @aliases aggregate_coh
#' @examples
#' # load librarys
#' require(LinguGeo)

aggregate_coh <- function(ls){

  # get ID based on coordinates
  for(i in 1:length(ls)){
    ls[[i]]$ID_cords <- paste0(ls[[i]]$xcord,ls[[i]]$ycord)
    # sort by ID
    ls[[i]] <- ls[[i]][order(ls[[i]]$ID_cords),]
  }

  # select xy coords and coh
  df_agt <- ls[[1]][,c(1,2,5)]
  colnames(df_agt)[3] <- paste0("coh_",1)
  for(i in 2:length(ls)){
    # cbind the coh column
    df_agt <- cbind(df_agt,ls[[i]][,5])
    colnames(df_agt)[2+i] <- paste0("coh_",i)
  }

  # calculate aggregation value agt
  nc <- ncol(df_agt)
  df_agt$n
  for(i in 1:nrow(df_agt)){
    df_agt$agt[i] <- sum(df_agt[i,c(3:nc)],na.rm = T)/
      (length(ls)-length(  which(is.na(df_agt[i,c(3:nc)])==T)))
    # add column with n entires !=NA
    df_agt$n[i] <- length(which(is.na(df_agt[i,c(3:nc)])==F))

  }
  return(df_agt)
}# end of function
