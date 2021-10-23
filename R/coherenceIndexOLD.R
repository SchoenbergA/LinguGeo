#' coh_re - Function for plotting of PAM correction.
#' @description visualizes the correction of PAM values by correction value.
#' @param df data.frame - with column "CTR" for control values, "PAM" for pam values
#' and optional "PAM_corr" for corrected PAM values.
#' @param al numeric - a vector of numeric values on the x axes to draw lines. Default=0
#' @param yl numeric - ylim adjustment. Default=c(0,2)
#' @param sortby character - column name to reorder the inoput datafram 'df'. IF set the argument 'al' will not be used.
#' @param abs_dif boolean - IF TRUE plots the differences as absolute values (no negative values). Default=TRUE.
#' @param titel character - desired maintitel for the plot . Default="PAMcorrection".
#' @return returns a plot containing the original PAM (orange) and Control (blue) values along with the difference (grey).
#' If corrected PAM values are available those will be plotted in red along with the differnece between the corrected PAM and COntrol values.
#' Additionally prints the total Difference along with mean and standart divation for PAM or if given PAM corrected and Control
#' @note For visualization purposes the difference between the corrected PAM and Control is used as absolute values (negative values are plotted as positive).
#' Further the mean and sd is calculated for the absolute values.
#' @author Andreas Schönberg
#' @export coh_re
#' @aliases coh_re
#' @examples
#' # load data
#'

coh_re <- function(x, y,long=NULL,lat=NULL,nk,reverse) {

  # check input
  if(nk>9){
    stop("nk > 9 is not supported ... yet")
  }
  # requires to change variable in pattern from n0 to n to support nk >9


  # create window based on max extend of points in dataset
  box_vec <- owin(xrange = c(min(x[[long]]),
                             max(x[[long]])),
                  yrange = c(min(x[[lat]]),
                             max(x[[lat]])))
  # plot result
  plot(box_vec)

  # point pattern
  # ! waring dublicated points comes from problem in dataframe
  vec_ppp <- ppp(x[[long]],
                 x[[lat]],
                 box_vec)

  # get neighbors
  neigh <- nnwhich(vec_ppp, k = 1:nk)
  neigh <-as.data.frame(neigh)
  head(neigh)

  # cbind NN class to dataframe (i controls for nk selected)
  for (i in 1:nk) {
    if(i==1){
      #print(i)
      neigh_df2 <- cbind(
        as.data.frame(vec_ppp),
        y)
      # handel names
      colnames(neigh_df2) <- c("x","y","data")


      # bind first NN (for i=1)
      neigh_df2 <- cbind(neigh_df2,
                         y[neigh[[i]]])
      # set name
      names(neigh_df2)[names(neigh_df2) == 'y[neigh[[i]]]'] <- paste0("n",i)
    } else {
      # bind further NN
      neigh_df2 <-cbind(neigh_df2,y[neigh[[i]]])
      # set name
      names(neigh_df2)[names(neigh_df2) == 'y[neigh[[i]]]'] <- paste0("n",i)

      # loop control
      print(i)
    }


  }

  # convert to charcter
  neigh_df2[, 3:ncol(neigh_df2)] <- lapply(neigh_df2[, 3:ncol(neigh_df2)], as.character)
  # fun control

  # if NN is identical to central point 1 else 0
  for (j in 1:nk) {

    newcol <- as.numeric(ifelse(neigh_df2$data == neigh_df2[colnames(neigh_df2)==paste0("n",j)],1,0))
    if(j==1){
      neigh_df3 <- cbind(neigh_df2,newcol)
      #set name for new col
      names(neigh_df3)[names(neigh_df3) == 'newcol'] <- paste0("n0",j)
    } else {
      neigh_df3 <- cbind(neigh_df3,newcol)
      #set name for new col
      names(neigh_df3)[names(neigh_df3) == 'newcol'] <- paste0("n0",j)
    }
  }



  ###


  #######

  if(reverse==F){
    # inverse cbind
    for (k in 1:nk) {

      # handle for 1st NN (no Sum needed)
      if(k==1){
        newcol <- neigh_df3[,grep("n01$",colnames(neigh_df3))]
        neigh_df4 <- cbind(neigh_df3,newcol)
        #set name for new col
        names(neigh_df4)[names(neigh_df4) == 'newcol'] <- paste0("nSum",k)

        # handle all NN =! 1
      } else {
        newcol <- rowSums(neigh_df3[,grep("n01$",colnames(neigh_df3)):grep(paste0("n0",k),colnames(neigh_df3))])
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
        newcol <- rowSums(neigh_df3[,grep("n01$",colnames(neigh_df3)):grep(paste0("n0",k),colnames(neigh_df3))])
        neigh_df4 <- cbind(neigh_df3,newcol)
        #set name for new col
        names(neigh_df4)[names(neigh_df4) == 'newcol'] <- paste0("nSum",k)
      } else if(k==1){
        newcol <- neigh_df3[,grep("n01$",colnames(neigh_df3))]
        neigh_df4 <- cbind(neigh_df4,newcol)
        #set name for new col
        names(neigh_df4)[names(neigh_df4) == 'newcol'] <- paste0("nSum",k)
      }else {
        newcol <- rowSums(neigh_df3[,grep("n01$",colnames(neigh_df3)):grep(paste0("n0",k),colnames(neigh_df3))])
        neigh_df4 <- cbind(neigh_df4,newcol)
        #set name for new col
        names(neigh_df4)[names(neigh_df4) == 'newcol'] <- paste0("nSum",k)
      }
    }
  }

  # calculate global and global corrected overall coherence
  head(neigh_df4)

  glob <-sum(neigh_df4[grep(paste0("nSum",nk),colnames(neigh_df4))]) /
    (nk*nrow(neigh_df4))
  glob_corr <- (glob - 1/length(table(y))) / (1 - 1/length(table(y)))

  cat(paste0("global coh: ",round(glob,2)),sep = "/n")

  print(glob)
  print(glob_corr)
  return(neigh_df4)

}


