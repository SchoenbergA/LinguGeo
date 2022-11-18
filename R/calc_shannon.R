#' calc_shannon
#' @description calculates the local measure of Shannon entropy
#' @param df  data.frame - with at least the coherence values and x y coordinates
#' @param colname_coh character - column name with the coherence values. Default="coh" (provided by 'LinguGeo::coherence'
#' @return returns the data.frame and adds a column with the Shannon entropy values
#' @author Andreas Schönberg
#' @export calc_shannon
#' @aliases calc_shannon
#' @examples
#' # load data
#' hunde_utm <- read.csv(system.file("extdata","Hunde_classified_utm.csv",package = "LinguGeo"),row.names = 1)
#'
#' head(hunde_utm)
#'
#' # calculate coherence
#' coh <- coherence(dat = hunde_utm,cl = "new",xcord = 1,ycord = 2,nk = 5,na_value = NULL,develop = F)
#'
#' # calculate shannon
#' shn <- calc_shannon(coh)


calc_shannon <- function(df,colname_coh="coh"){

  # check input
  if(any(colnames(df)==colname_coh)==F){
    stop("Input colname does not occure in dataframe")
  }
  # get column for coh values
  coh_col <- which(colnames(df)==colname_coh)

  # function to calculate shaonnen per row
  shannon_entropy <- function(ROW) {
    # shannon with binary log (base = 2)
    entr <- -(df[ROW,coh_col] * log(df[ROW,coh_col], base = 2) +
                (1-df[ROW,coh_col]) * log(1-df[ROW,coh_col], base = 2))

    return(entr) # unit = bit
  }


  # calculate shannon for each row
  shn <- vector()
  for (i in 1:nrow(df)) {
    shn[i] <- shannon_entropy(i)
  }

  # set NaN to 0
  cat(paste0("recycling ",length(which(is.na(shn))), " NaN to 0"),sep="\n")
  shn[which(is.na(shn))]<- 0

  # add shannon to df
  df$shn <- shn

  # return
  return(df)

} # end of function

