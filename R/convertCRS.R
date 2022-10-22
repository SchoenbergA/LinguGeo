#' convertCRS
#' @description convert the CRS for the dataframe
#' @param df  data.frame - with at least x and y coordinates
#' @param pos_x numeric - column position with X-coordinates
#' @param pos_y numeric - column position with Y-coordinates
#' @param src_proj character - CRS string for the source CRS
#' @param trg_proj character - CRS string for the traget CRS
#' @param colnames_trg character - desired column names for the converted coordinates.
#' IF NULL the default names will be "trg_x", "trg_y". Default=NULL.
#' @return returns the data.frame with additional columns for the desired x y coordinates.
#' @author Andreas Schönberg
#' @export convertCRS
#' @aliases convertCRS
#' @examples
#' # load librarys


convertCRS <- function(df,pos_x,pos_y,src_proj,trg_proj,colnames_trg=NULL){

  # check for NA
  if(any(is.na(df[,pos_x]))){
    stop("NA detected in X coordinates")
  }
  if(any(is.na(df[,pos_y]))){
    stop("NA detected in Y coordinates")
  }
  # check for numeric coordinates
  if(class(df[,pos_x])!="numeric"){
    df[,pos_x] <- as.numeric(df[,pos_x])
    cat("Converting X coordinates to numeric",sep="\n")
  } else {
    cat("X coordinates are numeric",sep="\n")
  }
  if(class(df[,pos_y])!="numeric"){
    df[,pos_y] <- as.numeric(df[,pos_y])
    cat("Converting Y coordinates to numeric",sep="\n")
  } else {
    cat("Y coordinates are numeric",sep="\n")
  }

  # get spatial object
  df_spt <- sp::SpatialPointsDataFrame(df[,pos_x:pos_y],df)
  # set projection
  sp::proj4string(df_spt) <- src_proj

  # reproject
  df_trg <- sp::spTransform(df_spt,trg_proj)

  # add geometry to df
  geo <- raster::geom(df_trg)

  # write UTM geometry
  df$trg_x <- geo[,2]
  df$trg_y <- geo[,3]

  #
  if(is.null(colnames_trg)==F){
    # write UTM geometry
    colnames(df)[which(colnames(df)=="trg_x")] <-colnames_trg[1]
    colnames(df)[which(colnames(df)=="trg_y")] <-colnames_trg[2]
  }

  # return
  return(df)
} # end of function
