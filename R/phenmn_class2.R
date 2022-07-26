#' phenmn_class2
#' @description Phenomenon Classification. Semi-automatic classification of strings by expressions.
#' @param data  data.frame or SpatialPointData.frame.
#' @param colname  charater - name of column containing the strings
#' @param pat_exp character - expressions which will be used for pattern matching
#' @param cl_to character - classnames. Must be of same lenght as 'pat_exp'
#' @param trim boolean - if TRUE will trim rows which have none applied class (0).
#' @param develop boolean - if TRUE will NOT delete columns which are used to identify multiple matches. This is used to check the function.

#' @return returns the dataframe with an additional column containing the classes.
#' @note The data may match with multiple patterns which could lead to more than 1 class for a single items.
#' If single items contain multiple classes the function will provide a warning.
#'
#' TEXT describing other cases! coming soon

#' @author Andreas Schönberg
#' @export phenmn_class2
#' @aliases phenmn_class2
#' @examples
#' # load librarys
#' require(LinguGeo)
#' require(stringr)
#'
#' # load data
#' csv <- read.csv(system.file("extdata","hunde_utm.csv",package = "LinguGeo"))
#' # take a look
#' head(csv)
#'
#' # reclassify "hunde" using the old version and trim all rows with no class (0)
#' new_class <- phenmn_class(data=csv,colname = "hunde",
#'                           pat_exp = c("nd|nt","ng|n.g","nn|n$")
#'                           ,cl_to = c("nd"   ,"ng"    ,"nn"))
#'
#'
#' # compare to new version
#' new_class <- phenmn_class2(data=csv,colname = "hunde",
#'                            pat_exp = c("nd|nt","ng|n.g","nn|n$")
#'                            ,cl_to = c("nd"   ,"ng"    ,"nn")
#'                            ,trim=T)
#'
#' # test for n items > 2
#' # get test data
#' testdata <- csv[1:5,]
#' testdata$hunde[1] <- "Hunde, Hunn, Hung"
#' testdata$hunde[2] <- "Hude, Hunng, Hunnd, Hunde"
#'
#' test <- phenmn_class2(data=testdata,colname = "hunde",pat_exp = c("nd|nt","ng|n.g","nn|n$"),
#'                       cl_to = c("nd"   ,"ng"    ,"nn"))

phenmn_class2 <- function(data,colname,pat_exp,cl_to,trim=F,develop=T){

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

  if(develop==F){
    # delete unneeded columns
    data <- subset(data,select = -c(pos_min:pos_max))
  }

  # print result
  print(table(data$class))
  # trim output
  if(trim ==T){
    cat(paste0("Trimming ",nrow(data)-nrow(subset(data,data$class!=0)), " entries with class = '0'"),sep="\n")
    data <- subset(data,data$class!=0)
    print(table(data$class))
  }


  # return
  return(data)

}# end of function
