#' coherenceIndex2
#' @description Calculates the Coherence Index by using an nearest neighbors approach.
#' @param dat  data.frame.
#' @param cl  charater - name of column containing the classes
#' @param xcord numeric - column with x-coordinates
#' @param ycord numeric - column with y-coordinates
#' @param nk numeric - amount of nearest neighbors to use. Maximum is 19.
#' @return returns the coherence index for each data point including local variation.
#' @note no notes
#' @author Andreas Schönberg
#' @export coherenceIndex2
#' @aliases coherenceIndex2
#' @examples
#' # load librarys
#' require(LinguGeo)
#' require(spatstat)
#' require(stringr)
#'
#' # load data
#' csv <- read.csv(system.file("extdata","hunde_utm.csv",package = "LinguGeo"))
#' # take a look
#' head(csv)
#'
#' # reclassify and trim all rows with no class (0)
#' csv <- phenmn_class2(data=csv,colname = "hunde",
#'                      pat_exp = c("nd|nt","ng|n.g","nn|n$")
#'                      ,cl_to = c("nd"   ,"ng"    ,"nn")
#'                      ,trim=T)
#'
#' # calculate Coherence Index for "hunde" with "type"
#' coh2 <- coherenceIndex2(csv,csv$class,2,3,nk=5)
#'
#' # compare to older version without local variation
#' coh <- coherenceIndex(csv,csv$class,2,3,nk=5)


coherenceIndex2 <- function(dat, cl,xcord=NULL,ycord=NULL,nk=NULL) {

  # check input
  if(nk>19){
    stop("nk > 19 is not supported ... yet")
  }
  # requires to change variable in pattern from n1$ to exact n1


  # create window based on max extend of points in dataset
  box_vec <- spatstat.geom::owin(xrange = c(min(dat[[xcord]]),
                                            max(dat[[xcord]])),
                                 yrange = c(min(dat[[ycord]]),
                                            max(dat[[ycord]])))
  # point pattern
  vec_ppp <- spatstat.geom::ppp(dat[[xcord]],
                                dat[[ycord]],
                                box_vec)

  # get neighbors
  neigh <- spatstat.geom::nnwhich(vec_ppp, k = 1:nk)
  neigh <-as.data.frame(neigh)
  head(neigh)

  # cbind NN class to dataframe (i controls for nk selected)
  for (i in 1:nk) {
    if(i==1){
      #print(i)
      neigh_df2 <- cbind(
        as.data.frame(vec_ppp),
        cl)
      # handel names
      colnames(neigh_df2) <- c("xcord","ycord","data")


      # bind first NN (for i=1)
      neigh_df2 <- cbind(neigh_df2,
                         cl[neigh[[i]]])
      # set name
      names(neigh_df2)[names(neigh_df2) == 'cl[neigh[[i]]]'] <- paste0("NN",i)
    } else {
      # bind further NN
      neigh_df2 <-cbind(neigh_df2,cl[neigh[[i]]])
      # set name
      names(neigh_df2)[names(neigh_df2) == 'cl[neigh[[i]]]'] <- paste0("NN",i)

      # loop control
      #print(i)
      head(neigh_df2)
    }


  }

  # split entries with 1 or more language items

  # add index
  neigh_df2$index <- 1:nrow(neigh_df2)
  # split rows which have more than 1 langugae item
  df_i1 <-neigh_df2[which(str_count(neigh_df2$data,",")==0),]
  df_i2 <-neigh_df2[which(str_count(neigh_df2$data,",")>0),]


  # NN check loop for df_i1 (1 item in data)
  for(nn in 1:nk){

    # open new vector
    newcol <- vector()
    # iteration over all rows
    for(r in 1:nrow(df_i1)){

      # condition NN == data
      if(df_i1$data[r] == df_i1[r,colnames(df_i1)==paste0("NN",nn)]){
        newcol[r] <- 1

        # condition any items in NN is == data and amount of items in NN > 1
      } else if(any(unlist(str_split(df_i1[r,colnames(df_i1)==paste0("NN",nn)],","))==df_i1$data[r] &
                    length(unlist(str_split(df_i1[r,colnames(df_i1)==paste0("NN",nn)],",")))!=1)==TRUE){
        newcol[r] <- 1/length(unlist(str_split(df_i1[r,colnames(df_i1)==paste0("NN",nn)],",")))
        # condition NN is != data
      } else{
        newcol[r] <- 0
      }

    } # end iteration r

    # cbind new columns to data
    if(nn==1){
      df_i1_2 <- cbind(df_i1,newcol)
      #set name for new col
      names(df_i1_2)[names(df_i1_2) == 'newcol'] <- paste0("n",nn)
    } else {
      df_i1_2 <- cbind(df_i1_2,newcol)
      #set name for new col
      names(df_i1_2)[names(df_i1_2) == 'newcol'] <- paste0("n",nn)
    }

  } # end loop over nk for 1 item

  # NN check loop for df_i2 (2 item in data)
  if(length(df_i2)!=0){
    for(nn in 1:nk){

    # open new vector
    newcol <- vector()
    # iteration over all rows
    for(r in 1:nrow(df_i2)){

      # set variables
      items_NN <- unlist(str_split(df_i2[r,colnames(df_i1)==paste0("NN",nn)],","))
      items_DT <- unlist(str_split(df_i2$data[r],","))

      # condition NN == data (all items in NN are in Data)
      if(all(items_NN%in%items_DT & length(items_NN)==length(items_DT))==TRUE){
        newcol[r] <- 1

        # condition any items in NN is == data and amount of items in NN > 1
      } else if(any(items_NN%in%items_DT)){
        newcol[r] <- length(items_NN)/length(items_DT)
        # condition NN is != data
      } else {
        newcol[r] <- 0
      }

    } # end iteration r

    # cbind new columns to data
    if(nn==1){
      df_i2_2 <- cbind(df_i2,newcol)
      #set name for new col
      names(df_i2_2)[names(df_i2_2) == 'newcol'] <- paste0("n",nn)
    } else {
      df_i2_2 <- cbind(df_i2_2,newcol)
      #set name for new col
      names(df_i2_2)[names(df_i2_2) == 'newcol'] <- paste0("n",nn)
    }

  } # end loop over nk for 1 item

    # rbind both dataframes
    neigh_df3 <- rbind(df_i1_2,df_i2_2)
       # end if local variation detected in data
       } else {
         neigh_df3 <- df_i1_2
       }



  # inverse cbind sum of NN values
  for (k in 1:nk) {

    # handle for 1st NN (no Sum needed)
    if(k==1){
      newcol <- neigh_df3[,grep("n1$",colnames(neigh_df3))]
      neigh_df4 <- cbind(neigh_df3,newcol)
      #set name for new col
      names(neigh_df4)[names(neigh_df4) == 'newcol'] <- paste0("nSum",k)

      # handle all NN =! 1
    } else {
      newcol <- rowSums(neigh_df3[,grep("n1$",colnames(neigh_df3)):grep(paste0("n",k),colnames(neigh_df3))])
      neigh_df4 <- cbind(neigh_df4,newcol)
      #set name for new col
      names(neigh_df4)[names(neigh_df4) == 'newcol'] <- paste0("nSum",k)
    }
  }# end cbind


  # normalize
  neigh_df4$nrm <- neigh_df4[,ncol(neigh_df4)]/nk

  # global coherence index - sum of nSum / max (obersavtions*nk)
  glob <-sum(neigh_df4[grep(paste0("nSum",nk),colnames(neigh_df4))]) /
    (nk*nrow(neigh_df4))

  # corrected global coherence index - glob in realtion to n class in type
  glob_corr <- (glob - 1/length(table(cl))) / (1 - 1/length(table(cl)))

  uvar <-unique(unlist(str_split(cl,",")))
  if(any(uvar==0)){
    uvar <- length(uvar[which(uvar!=0)])
  } else {
    uvar <- length(uvar)
  }

  cat(paste0("Global coh uncorrected: ",round(glob,4)," @ ",uvar," variants"),sep = "\n")
  cat(paste0("Global coh: ",round(glob_corr,4)," @ ",uvar," variants"),sep = "\n")

  # return #####################################################################
  return(neigh_df4)

}
