#' mergeMultiPlacesNA
#' @description Merges class information for places with multiple entires.
#' @param df  data.frame.
#' @param pos_x numeric - column with x-coordinates
#' @param pos_y numeric - column with y-coordinates
#' @param col numeric - column with "class"
#' @return returns only unique places.
#' @note Only useful for classified data!
#' @details uses pasted x and y coordinates to search for multiple entries and
#' merges all entires in class for dublicates
#' @author Andreas Schönberg
#' @export mergeMultiPlacesNA
#' @aliases mergeMultiPlacesNA
#' @examples
#' # load librarys
#' require(LinguGeo)
#' require(spatstat)
#' require(stringr)

mergeMultiPlacesNA <- function(df,pos_x,pos_y,col,col_ls){

  # paste x and y coorinates to get duplicates by table
  df$places <- paste0(df[,pos_x],".", df[,pos_y])
  occ <- data.frame(table(df$places))

  # merge occuriance with df
  df2 <- base::merge(df, occ, by.x = "places",by.y= "Var1",all=T)
  # rename column
  colnames(df2)[which(colnames(df2)=="Freq")] <- "n"
  # get unique coordinates
  uc <-unique(df2$places)

  # test every unique place
  for(n in 1:length(uc)){

    # check for duplicates
    if(df2$n[which(df2$places==uc[n])][1]>1) {

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
    if(df2$n[which(df2$places==uc[n])][1]>1) {
      df2 <-df2[-which(df2$places==uc[n])[2:length(df2$places[which(df2$places==uc[n])])],]
    }
  }# end loop 'n' over 1:nrows
  head(df2)

  df2$new <- 999
  # clean up class column duplicates due to merging equal classes and handle NA
  for(k in 1:nrow(df2)){

    # get string in colum "class"
    k_string <- unlist(strsplit(as.character(df2[k,colnames(df2)[col+1]]), split=", "))

    # exact one NA <- NA
    if(any(is.na(k_string))==T && length(k_string)==1 ){
      print("is NA")
      df2$new[k] <- NA
    # multiple 'NA' without any other class  <- NA
    } else if(unique(k_string)=="NA" && length(unique(k_string))==1){
      print("is NANANA")
        df2$new[k] <- NA
        # 'NA' mixed with other classes <- remove NA
      } else if(any(k_string=='NA')&& length(k_string)>1){
        print("is mixed")
        # get all non NA values
        k_string2 <-unique(k_string[-which(k_string=='NA')])
        df2$new[k]  <- paste(unique(sort(k_string2)), collapse = ', ')
      } else {
        # all entires !=NA
        print("nromal case")
        df2$new[k] <-paste(unique(sort(k_string)), collapse = ', ')}# sort to avoid unequal order of elements
      }


  # clean up unneeded columns
  df2 <- df2[-which(colnames(df2)=="places")]

  # return
  return(df2)
} # end of function

