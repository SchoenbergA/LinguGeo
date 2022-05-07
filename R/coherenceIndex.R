#' coherenceIndex
#' @description Calculates the Coherence Index by using an nearest neighbors approach.
#' @param dat  data.frame.
#' @param cl  charater - name of column containing the classes
#' @param xcord numeric - column with x-coordinates
#' @param ycord numeric - column with y-coordinates
#' @param nk numeric - amount of nearest neighbors to use. Maximum is 19.
#' @param reverse boolean - develop. If TRUE saves to df in order, else in reverse.
#' @return return
#' @note For
#' @author Andreas Schönberg
#' @export coherenceIndex
#' @aliases coherenceIndex
#' @examples
#' # load librarys
#' require(spatstat)
#' # load data
#' csv <- read.csv(system.file("extdata","class_hunde_utm.csv",package = "LinguGeo"))
#' # take a look
#' head(csv)
#' # calculate Coherence Index for "hunde" with "type"
#' CohInd <- coherenceIndex(csv,csv$class,2,3,nk=5,reverse = F)
#' head(CohInd)


coherenceIndex <- function(dat, cl,xcord=NULL,ycord=NULL,nk=NULL,reverse,develop=F) {

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

  # convert to character
  #neigh_df2[, 3:ncol(neigh_df2)] <- lapply(neigh_df2[, 3:ncol(neigh_df2)], as.character)
  # fun control

  # if NN j is identical to central point 1 else 0
  for (j in 1:nk) {

    newcol <- as.numeric(ifelse(neigh_df2$data == neigh_df2[colnames(neigh_df2)==paste0("NN",j)],1,0))
    if(j==1){
      neigh_df3 <- cbind(neigh_df2,newcol)
      #set name for new col
      names(neigh_df3)[names(neigh_df3) == 'newcol'] <- paste0("n",j)
    } else {
      neigh_df3 <- cbind(neigh_df3,newcol)
      #set name for new col
      names(neigh_df3)[names(neigh_df3) == 'newcol'] <- paste0("n",j)
    }
  }



  if(reverse==F){
    # inverse cbind
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
    }
  } # end if reverse==FALSE

  if(reverse==T){
    # reverse cbind
    for (k in nk:1) {


      if(k==nk){
        newcol <- rowSums(neigh_df3[,grep("n1$",colnames(neigh_df3)):grep(paste0("n",k),colnames(neigh_df3))])
        neigh_df4 <- cbind(neigh_df3,newcol)
        #set name for new col
        names(neigh_df4)[names(neigh_df4) == 'newcol'] <- paste0("nSum",k)
      } else if(k==1){
        newcol <- neigh_df3[,grep("n1$",colnames(neigh_df3))]
        neigh_df4 <- cbind(neigh_df4,newcol)
        #set name for new col
        names(neigh_df4)[names(neigh_df4) == 'newcol'] <- paste0("nSum",k)
      }else {
        newcol <- rowSums(neigh_df3[,grep("n1$",colnames(neigh_df3)):grep(paste0("n",k),colnames(neigh_df3))])
        neigh_df4 <- cbind(neigh_df4,newcol)
        #set name for new col
        names(neigh_df4)[names(neigh_df4) == 'newcol'] <- paste0("nSum",k)
      }
    }
  }

  # product output neigh_df4

  # normalize
  neigh_df4$nrm <- neigh_df4[,ncol(neigh_df4)]/nk

  # global coherence index - sum of nSum / max (obersavtions*nk)
  glob <-sum(neigh_df4[grep(paste0("nSum",nk),colnames(neigh_df4))]) /
    (nk*nrow(neigh_df4))

  # corrected global coherence index - glob in realtion to n class in type
  glob_corr <- (glob - 1/length(table(cl))) / (1 - 1/length(table(cl)))
  nvar <- length(table(cl))
  cat(paste0("Global coh uncorrected: ",round(glob,4)," @ ",nvar," variants"),sep = "\n")
  cat(paste0("Global coh: ",round(glob_corr,4)," @ ",nvar," variants"),sep = "\n")

  # return #####################################################################
  return(neigh_df4)

}


