#' phenmn_class
#' @description Phenomenon Classification. Semi-automatic classification of strings by expressions.
#' @param data  data.frame or SpatialPointData.frame.
#' @param colname  charater - name of column containing the strings
#' @param pat_exp character - expressions which will be used for pattern matching
#' @param cl_to character - classnames. Must be of same lenght as 'pat_exp'

#' @return returns the dataframe with an additional column containing the classes.

#' @author Andreas Schönberg
#' @export phenmn_class
#' @aliases phenmn_class
#' @examples
#' # load data
#' csv <- read.csv(system.file("extdata","hunde.csv",package = "LinguGeo"))
#' # take a look
#' head(csv)
#' # reclassifie
#' new_class <- phenmn_class(data=csv,colname = "hunde",
#'                           pat_exp = c("nd|nt","ng|n.g","nn|n$")
#'                           ,cl_to = c("nd"   ,"ng"    ,"nn"))


phenmn_class <- function(data,colname,pat_exp,cl_to){

  # check input

  # pattern and desired classes
  if(length(pat_exp)!=length(cl_to)){
    stop("Defined pattern and desired classes must be of same length!")
  }
  #data <- as.data.frame(data)
  if(class(data)!="data.frame"){
    stop("Input must be of type 'data.frame'")
  }
  # save ncol for original data
  ncol_data <- ncol(data)
  # get column for input
  cn <-which(colnames(data)==colname)

  # pattern matching
  for (i in 1:length(pat_exp)) {

    data[,ncol(data)+1] <- ifelse(grepl(pat_exp[i],data[,cn]),cl_to[i],0)
  }# end loop


  # add dummy column and dummy value
  data$class <-2019

  # get column positions
  pos_min <-ncol_data+1
  pos_max <-pos_min+length(pat_exp)-1
  pos_class<- pos_max+1


  # get the class row by row
  for(j in 1:nrow(data)){

    # all are 0 <- get class 0
    if(any(data[j,pos_min:pos_max]!=0)==F){
      data[j,pos_class] <- "2_no match"}

    # more than 1 is !=0
    else if(length(which(data[j,pos_min:pos_max]!=0))>1){
      data[j,pos_class] <- "1_double"
    } else {

      # normal case: exact 1 is !=0
      data[j,pos_class] <- data[j,which(data[j,pos_min:pos_max]!=0)+ncol_data]
    }
  }# end loop


  # print results
  print(table(data$class))

  # return
  return(data)

}# end of function
