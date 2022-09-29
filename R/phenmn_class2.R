#' phenmn_class
#' @description semi-automatic classification of language items by regular expression
#' @param data data.frame - with at least x y coordinates and one data column
#' @param colname character - name of column containing the data to be classified
#' @param pat_exp character - expressions which will be used for pattern matching
#' @param cl_to character - desired names for the classes. Must be of same length as 'pat_exp'
#' @param set_NA boolean - if TRUE will convert all class '0' to NA (see notes)

#' @return Returns the data.frame with an additional column containing the classes.
#' @note The data may match with multiple patterns which could lead to more than 1 class for a single item.
#' If single items contain multiple classes the function will provide a warning.
#' All entries which does not match any of the given pattern will be assigned to class "0". If 'set_NA' == T all class 0 entries will be converted to NA.


#' @author Andreas Schönberg
#' @export phenmn_class
#' @aliases phenmn_class
#' @examples
#' # load librarys

phenmn_class <- function(data,colname,pat_exp,cl_to,set_NA=F){

  # check input

  # pattern and desired classes
  if(length(pat_exp)!=length(cl_to)){
    stop("Defined pattern and desired classes must be of same length!")
  }
  #data <- as.data.frame(data)
  if(class(data)!="data.frame"){
    stop("Input must be of type 'data.frame'")
  }
  if(colname%in%colnames(data)==F){
    stop("Selected colname not in data")
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
    if(all(data[j,pos_min:pos_max]==0)){
      data[j,pos_class] <- 0

    } else {
      # paste all items != 0
      data[j,pos_class] <-paste0(data[j,pos_min:pos_max][which(data[j,pos_min:pos_max]!=0)],collapse = ", ")
    }

  }# end loop

  # add column with amount of variation
  # add by loop
  local_variation <- vector()
  n_items <- vector()

  for(k in 1:nrow(data)){
    local_variation[k]<-length(which(data[k,pos_min:pos_max]!=0))
    n_items[k] <- length(unlist(str_split(data[k,cn],", ")))
    length(unlist(str_split(data[k,cn],", ")))
  }

  data <- cbind(data, local_variation,n_items)

  n_corrupted <-length(which(data$n_items==1 & data$local_variation>data$n_items))
  if(n_corrupted>0){
    warning(paste0(n_corrupted," data items have exact one item but match multiple expressions"))
  }
    # delete unneeded columns (outcomment to get full data)
    data <- subset(data,select = -c(pos_min:pos_max))

  # print result
  print(table(data$class))


  if(set_NA==T){
    cat("Setting class '0' to NA",sep="\n")
    data$class[which(data$class==0)] <- NA
  }
  # return
  return(data)

}# end of function
