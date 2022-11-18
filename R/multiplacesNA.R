#' mergeMultiPlaces
#' @description merges information of places with identical coordinates
#' @param df  data.frame - with at least x y coordinates and one data column
#' @param pos_x numeric - column position with x-coordinates
#' @param pos_y numeric - column position with y-coordinates
#' @param col_ls list, numeric - list with numeric column position which should be merged. Default=NULL
#' @param class_col_ls list, numeric - list with numeric column position where duplicates should be cleaned after merging. Typically columns with language items classification. Default=NULL
#' @return returns the data.frame with only the unique places and optional clean duplicated entires after merge.
#' @note Only useful for classified data!
#' @details soon
#' @author Andreas Schönberg
#' @export mergeMultiPlaces
#' @aliases mergeMultiPlaces
#' @examples
#' # load data
#' mau_class <- read.csv(system.file("extdata","Classification_Maurer.csv",package = "LinguGeo"),row.names = 1)
#' head(mau_class)
#'
#' # merge multiple places
#' merged_plc <- mergeMultiPlaces(df = mau_class,pos_x = 1,pos_y = 2,
#'                                 col_ls = 3:ncol(mau_class))
#'
#' View(merged_plc)
#'
#' # merge multiple places and clean duplicates
#' merged_plc <- mergeMultiPlaces(df = mau_class,pos_x = 1,pos_y = 2,
#'                                 col_ls = 3:ncol(mau_class),
#'                                 class_col_ls = 3:ncol(mau_class))
#'
#' merged_plc[which(merged_plc$n_places>9),]

mergeMultiPlaces <- function(df,pos_x,pos_y,col_ls=NULL,class_col_ls=NULL){

  # paste x and y coorinates to get duplicates by table
  df$places <- paste0(df[,pos_x],".", df[,pos_y])
  occ <- data.frame(table(df$places))

  # merge occuriance with df
  df2 <- base::merge(df, occ, by.x = "places",by.y= "Var1",all=T)
  # rename column
  colnames(df2)[which(colnames(df2)=="Freq")] <- "n_places"

  # get unique coordinates
  uc <-unique(df2$places)

  if(max(df2$n_places)==1){
    cat("No multiple places detected",sep = "\n")
  } else {

    cat(paste0(nrow(df)-length(uc)," dublicated places detected"),sep = "\n")
  }



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


  ### clean up classification columns duplicates due to merging equal classes and handle NA

  if(is.null(class_col_ls)==F){
    cat("Cleaning dublicates from selected columns",sep = "\n")
  # loop for all columns in 'class_col_ls
  for(c in 1:length(class_col_ls)){

  for(k in 1:nrow(df2)){

    # get string in colum "class"
    k_string <- unlist(strsplit(as.character(df2[k,colnames(df2)[class_col_ls[c]+1]]), split=", "))

    # exact one NA <- NA
    if(any(is.na(k_string))==T && length(k_string)==1 ){
      df2[k,class_col_ls[c]+1] <- NA
    # multiple 'NA' without any other class  <- NA
    } else if(unique(k_string)=="NA" && length(unique(k_string))==1){
      df2[k,class_col_ls[c]+1] <- NA
        # 'NA' mixed with other classes <- remove NA
      } else if(any(k_string=='NA')&& length(k_string)>1){
        # get all non NA values
        k_string2 <-unique(k_string[-which(k_string=='NA')])
        df2[k,class_col_ls[c]+1]  <- paste(unique(sort(k_string2)), collapse = ', ')
      } else {
        # all entires !=NA
        df2[k,class_col_ls[c]+1] <-paste(unique(sort(k_string)), collapse = ', ')}# sort to avoid unequal order of elements
      }

  }# end c loop

  }
  # clean up unneeded columns
  df2 <- df2[-which(colnames(df2)=="places")]

  # return
  return(df2)
} # end of function

