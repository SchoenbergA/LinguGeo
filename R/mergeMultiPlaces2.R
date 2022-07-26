#' mergeMultiPlaces2
#' @description Merges data and class information for places with multiple entires.
#' @param df  data.frame.
#' @param pos_x numeric - column with x-coordinates
#' @param pos_y numeric - column with y-coordinates
#' @return returns only unique places.
#' @note no notes
#' @author Andreas Schönberg
#' @export mergeMultiPlaces2
#' @aliases mergeMultiPlaces2
#' @examples
#' # load librarys
#' require(LinguGeo)
#' require(spatstat)
#' require(stringr)


mergeMultiPlaces2 <- function(df,pos_x,pos_y){

# get n occurence
df2 <-df %>%
  group_by(X_utm, Y_utm) %>%
  mutate(n = n()) %>%
  ungroup %>%
  distinct(X_utm, Y_utm, .keep_all = TRUE)

# merge with df to get places names
head(df)
head(df2)
class(df)
class(df2)
df2 <- as.data.frame(df2)
df3 <- base::merge(df, df2[,c(pos_x,pos_y,ncol(df2))]
                   , by=c("X_utm","Y_utm"),all.x=T)

#df3$n[is.na(df3$n)==T] <- 1 # fill NA values with 1
head(df3)



df <- df3
# get unique coordinates
uc <-unique(df$X_utm)
# loop over all unique x coordinates

for(n in 1:length(uc)){
  # if T select [1] item to avoid any()

  if(df$n[which(df$X_utm==uc[n])][1]>1) {

    # loop for all dublicates 2: amount of dublicates
    for(i in 2:length(df$X_utm[which(df$X_utm==uc[n])])) {
      # select 1st row and paste data from next row
      df$class[which(df$X_utm==uc[n])][1] <- paste0(df$class[which(df$X_utm==uc[n])][1],
                                                    ", ",
                                                    df$class[which(df$X_utm==uc[n])][i])
      # merge data
      df$data[which(df$X_utm==uc[n])][1] <- paste0(df$data[which(df$X_utm==uc[n])][1],
                                                   ", ",
                                                   df$data[which(df$X_utm==uc[n])][i])

    }# end loop for duplicates

  }

  print(n)
}# end loop 'n' over 1:nrows

# delete rows after all dubplicates are pasted
for(n in 1:length(uc)){
  # delete all dublicates
  if(df$n[which(df$X_utm==uc[n])][1]>1) {
    df <-df[-which(df$X_utm==uc[n])[2:length(df$X_utm[which(df$X_utm==uc[n])])],]



  }


}# end loop 'n' over 1:nrows

# clean up dublicates due to merging equal classes
for(k in 1:nrow(df)){
  d <- unlist(strsplit(as.character(df[k,7]), split=", "))
  df$new[k] <-paste(unique(sort(d)), collapse = ', ')}# sort to avoid unequal order of elements

# return
return(df)
} # end of function
