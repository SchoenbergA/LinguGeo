#' calc_shannon
#' @description Calculates the shannon entropy.
#' @param df  data.frame.
#' @param colname_coh character - name of column with the coherence values. Default="nrm" (provided by 'LinguGeo::CoherenceIndex'.

#' @return returns the data.frame and adds a column with shannon entropy values.

#' @author Andreas Schönberg
#' @export calc_shannon
#' @aliases calc_shannon
#' @examples
#' # load librarys


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

