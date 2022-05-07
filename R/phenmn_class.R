#' phenmn_class
#' @description Phenomenon Classification. Semi-automatic classification of strings by expressions.
#' @param data  data.frame or SpatialPointData.frame.
#' @param colname  charater - name of column containing the strings
#' @param pat_exp character - expressions which will be used for pattern matching
#' @param cl_to character - classnames. Must be of same lenght as 'pat_exp'
#' @param user_class boolean - if True the user can choose to classifie lines with multiple matches by hand.
#' @param trim boolean - if TRUE will trim rows which have none applied class (0).
#' @param develop boolean - if TRUE will NOT delete columns which are used to identify multiple matches. This is used to check the function.

#' @return returns the dataframe with an additional column containing the classes.

#' @author Andreas Schönberg
#' @export phenmn_class
#' @aliases phenmn_class
#' @examples
#' # load data
#' csv <- read.csv(system.file("extdata","hunde_utm.csv",package = "LinguGeo"))
#' # take a look
#' head(csv)
#' # reclassifie
#' new_class <- phenmn_class(data=csv,colname = "hunde",
#'                           pat_exp = c("nd|nt","ng|n.g","nn|n$")
#'                           ,cl_to = c("nd"   ,"ng"    ,"nn"))
#'
#' # using user classification for multiple matches (user_class=TRUE)
#' new_class <- phenmn_class(data=csv,colname = "hunde",
#'                           pat_exp = c("nd|nt","ng|n.g","nn|n$")
#'                           ,cl_to = c("nd"   ,"ng"    ,"nn"),
#'                           user_class=TRUE)
#'
#' # trim all rows with no class (0)
#'  new_class <- phenmn_class(data=csv,colname = "hunde",
#'                          pat_exp = c("nd|nt","ng|n.g","nn|n$")
#'                          ,cl_to = c("nd"   ,"ng"    ,"nn"),
#'                          user_class=TRUE,trim=T)


phenmn_class <- function(data,colname,pat_exp,cl_to,user_class=F,trim=F,develop=T){

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
      data[j,pos_class] <- 0}

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
  if(user_class==T){
  cat(paste0("There are ", length(which(data$class=="1_double")), " multiple matches.",sep="\n"))
  cat("Do you want to classifie by hand ?",sep="\n")
  var1 <-readline("1 for yes 2 to skip")
  if(var1==1){
    # start user classification
    cat("Choose one of the classes below or '0' to skip",sep="\n")
    cat(cl_to)
    cat(" ",sep="\n")
    cat("Choose 'abort' to abort the classification",sep="\n")
    cat("Input which is not a class or 0 or abort, will abort the classification",sep="\n")
    cat(" ",sep = "\n")
    # get pos of multiple matches
    pos_double <-which(data$class=="1_double")
    for(j in 1:length(which(data$class=="1_double"))){

      cat(data[pos_double[j],cn],sep="\n")
      var <- readline("Which class to apply? '0' to skip <- ")
      if(var=="abort"){
        stop("Aborted by user")
      }
      if(var=="0"){
        data$class[pos_double[j]] <- 0
      }


      if(var%in%c(cl_to,0)){
        data$class[pos_double[j]] <- var
      } else {
        stop("wrong in put: Input must be one of the classes or 0 or abort")
      }



    }# end j loop
    cat("User Classification finished",sep="\n")
    # no user classification
  } else {
    cat("User Classification skipped",sep="\n")
  }
  }# end user class

  if(develop==F){
  # delete unneeded columns
  data <- subset(data,select = -c(pos_min:pos_max))
  }
  # trim output
  if(trim ==T){
    cat(paste0("Recycling ",length(which(data$class=="1_double")), " entries with class = '1_double' to class '0'"),sep="\n")
    data$class[data$class=="1_double"] <- 0
    cat(paste0("Trimming ",nrow(data)-nrow(subset(data,data$class!=0)), " entries with class = '0'"),sep="\n")
    data <- subset(data,data$class!=0)
    print(table(data$class))
    }

  # return
  return(data)

}# end of function
