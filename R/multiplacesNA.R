#' mergeMultiPlaces
#' @description merges information of places with identical coordinates
#' @param df  data.frame - with at least x y coordinates and one data column
#' @param pos_x numeric - column position with x-coordinates
#' @param pos_y numeric - column position with y-coordinates
#' @param col numeric - column position with class information
#' @param col_ls list, numeric - list with numeric column position which should be merged in
#' addition to the "class" column. If col_ls==NULL only the class column will be merged. (see details). Default=NULL
#' @return returns the data.frame with only the unique places.
#' @note Only useful for classified data!
#' @details soon
#' @author Andreas Schönberg
#' @export mergeMultiPlaces
#' @aliases mergeMultiPlaces
#' @examples
#' # soon

mergeMultiPlaces <- function(df,pos_x,pos_y,col,col_ls=NULL){

  # check col_ls
  if(any(col_ls==col)){
    cat("Class column detect in 'col_ls'. Recycle dublicated class column")
    col_ls <- col_ls[-which(col_ls==col)]
  }
  if(is.null(col_ls)==T) {
    col_ls <- col
  } else {
    col_ls <- c(col_ls,col)
  }
  # paste x and y coorinates to get duplicates by table
  df$places <- paste0(df[,pos_x],".", df[,pos_y])
  occ <- data.frame(table(df$places))

  # merge occuriance with df
  df2 <- base::merge(df, occ, by.x = "places",by.y= "Var1",all=T)
  # rename column
  colnames(df2)[which(colnames(df2)=="Freq")] <- "n_places"
  # get unique coordinates
  uc <-unique(df2$places)

  # test every unique place
  for(n in 1:length(uc)){

    # check for duplicates
    if(df2$n_places[which(df2$places==uc[n])][1]>1) {

      # loop over n columns
      for (nc in 1:length(col_ls)){

        for(i in 2:length(df2$places[which(df2$places==uc[n])])) {

          # raw code for column
          df2[,col_ls[nc]+1][which(df2$places==uc[n])][1] <- paste0(df2[,col_ls[nc]+1][which(df2$places==uc[n])][1],
                                                                    ", ",
                                                                    df2[,col_ls[nc]+1][which(df2$places==uc[n])][i])



        } # end i loop
      } # end nc loop
    }# close if condition


  }# end loop 'n' over 1:nrows

  # delete rows after all dubplicates are pasted
  for(n in 1:length(uc)){
    # delete all dublicates
    if(df2$n_places[which(df2$places==uc[n])][1]>1) {
      df2 <-df2[-which(df2$places==uc[n])[2:length(df2$places[which(df2$places==uc[n])])],]
    }
  }# end loop 'n' over 1:nrows

  df2$new <- 999
  # clean up class column duplicates due to merging equal classes and handle NA
  for(k in 1:nrow(df2)){

    # get string in colum "class"
    k_string <- unlist(strsplit(as.character(df2[k,colnames(df2)[col+1]]), split=", "))

    # exact one NA <- NA
    if(any(is.na(k_string))==T && length(k_string)==1 ){
      df2$new[k] <- NA
    # multiple 'NA' without any other class  <- NA
    } else if(unique(k_string)=="NA" && length(unique(k_string))==1){
        df2$new[k] <- NA
        # 'NA' mixed with other classes <- remove NA
      } else if(any(k_string=='NA')&& length(k_string)>1){
        # get all non NA values
        k_string2 <-unique(k_string[-which(k_string=='NA')])
        df2$new[k]  <- paste(unique(sort(k_string2)), collapse = ', ')
      } else {
        # all entires !=NA
        df2$new[k] <-paste(unique(sort(k_string)), collapse = ', ')}# sort to avoid unequal order of elements
      }


  # clean up unneeded columns
  df2 <- df2[-which(colnames(df2)=="places")]

  # return
  return(df2)
} # end of function

