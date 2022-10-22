# LinguGeo 0.0.0.9017
Development version

* Bugfixes
  convertCRS - as.numeric for y coordinates now uses y position instead of x position
  coherence - add check for non numeric xy coordinates

# LinguGeo 0.0.0.9016
Clean up and release preparations
Development version

* clean correct and update descriptions
* add an check dependecies. add :: and imports
* add new function "convertCRS"
* spelling corrections with Alfred Lameli
* rename some functins BUT not rename .R files

# LinguGeo 0.0.0.9015
Clean up and release preparations
Development version
Test version

* several tests with different input data format (column order, n columns and CRS), different order of coordinates (xy,yx).

* Delete all old versions of functions

* update 'multiplacesNA'
  col_ls now does not need to include class col. If NULL no additional columns will be merge. If set     but includes the class column, it will be recycled

  Output colname change from 'n' to 'n_places'

* Update 'phenmn_class2
  Delete option to trim class 0 due to new NA workflow. Trimming class 0 would result in missing values   for places. The set_NA argmunet will set class 0 to NA to keep all places in the dataframe.

  Delete develop mode to keep unneeded columns

* bugfix for 'coherenceIndex3'
  Now the check for NA uses the column position which == cl. Older version used hardcoded column 4       which would be correct if 'phenmn_class2' is used with develop=F. But this bug would lead to an error   if develop=T or using another column order.

  Remove unneeded 'index' column.
  Rbind of "NA Places" now uses variable x,y columns and retunrs NA for each other column. Older versions used hardcoded columns.
  
  Add warning if na_value is set to 0.
  
  Add error for wrong input 'NA_value'
  
  Bugfix if no local variation is detected. Older version did not work due to an issue. To check if LV occures old version asked for "length of vector" but require to ask for "nrow".

* bugfix for 'plot_agt'
  fixed wrong variable name

* minor fixes
  calc_channon change variable default
  add check for length of xy coordinates to 'aggregate_coh' receive warning if unequal

* new function 'plot_agt2'
Development function to plot NA values. The problem is that the n scale by alpha seems not to work properly if only n=1 occures

# LinguGeo 0.0.0.9014
Major Update - Add NA handling to several functions
Development version

* add new function "mergeMultiPlacesX". Advanced version of mergeMultiPlaces which is able to merge selected columns instead of only predefined "class" column. Useable to merge huge dataframes which have dubilcated coordinates but has no kind of sorting except for "class".

* update 'coherencIndex3' 
now set a given value to NA and excludes NA values for coh calculation. The NA values are later binded again. BUT: due to the structur of the function an index is not possible to reorder all places to its original input state. Solution could be to sort by unique xy coordinates becasue there should not be any dublicate left (if mergeMultiPlaces is used)

* update "mergePlaces" function (X2): now excludes places with NA values. BUT: if any dubilcated place has an NA it wouold not be deleted leading to different n places compared to the function without using NA exclud. Further this could cause problems for later aggregation and plotting because some places may have coh values as well as NA leading to NA symbol directly over the COH value and or problems when aggregating values for a specific place.

* update 'phenmn_class2
Add 'set_NA' argument which will convert all class 0 entires to NA. Now the return of 'set_NA' can be used in multiplacesNA

* new function 'multiplacesNA'
add NA handling to MergeMultiPlaces function. Now the returning classes (and mixed classes) does not include NAs. NA only occure if all entires are NA. The retunr can be used in coherencIndex3

* new function 'plot_coh2'
add NA plotting and remove "invert" and "degree" argumnets.

* new function 'aggregate_coh'

* new function 'plot_agt'

# LinguGeo 0.0.0.9013
Development version

* add "development" mode for "coherenceindex2". If FALSE deletes all unneded columns leading to a better overview.
* changed output columnname for coherence value from 'nrm' to 'coh'

* update "plot_coh" now using viridis color palette.

# LinguGeo 0.0.0.9012
Update

* new function "mergeMultiplaces3". Improvement of mergeMultiplaces2 uses a combination of x and y coordinates to detect duplicates.

# LinguGeo 0.0.0.9011
Develop version
*Development Update*

* add new function "mergeMultiPlaces2" which merges multple entiries from equal places. Works atm only for classified data (class and data will be merged) and is hardcoded
* fix a major bug in "coherenceIndex2" where a variable was set to empty due to "<-vector()"
* fixed workflow for local variation by adding a ", " collapse instead of "," for phenomclass2

# LinguGeo 0.0.0.9010
Develop version
*Major Update*

* add new development versions of 'phenmn_class2' and 'coherence Index2'
  - add feature to use local variation in coherence index
  - remove "user classification" for multiple entires
  - rework example
  
# LinguGeo 0.0.0.9009
Develop version

*add new function 'calc_shannon'. Calculates the shannon entropy (with binary log base=2). Returns the input dataframe with an additional column with the shannon entropy values.

* add ne function 'plot_shannon'. Specialized wrapper for ggplot2 to display the shannon entropy.

# LinguGeo 0.0.0.9008
Develop version

* update 'plot_coh'.
* update examples: add 'require()' LinguGeo and dependecies.

# LinguGeo 0.0.0.9007
Develop version / structural change

* remove dependencies to rgdal (was former used for creating spatial objects and loading shp example data) 
* remove usgae and support of shapefile format in th package
* rework examples and new example data (all based on dataframes stored in csv)

* update 'coherenceIndex'
  - remove coh correction formula
  - remove develop mode argument (not need due to no more corrections)
  - remove output as spatial points (former was used for plotting with mapview)
  
* add new function 'plot_coh' a specialized wrapper for ggplot2 to easy plot coh

# LinguGeo 0.0.0.9006
Develop version

* add output of global corrected value

# LinguGeo 0.0.0.9005
Develop version

* add advanced trimming for 'phenmn_class'. Now will recycle multiply entires eveen if not classified by hand (in older version the output could contain "1_double" class if multilpy entries were not cleaned by hand)
* add CRS argument for 'CoherenceIndex'. Now if given will set the projection for the output              SpatrialPointsDataFrame.

# LinguGeo 0.0.0.9004
Develop version

* add user input for multiple matches for 'phenmn_class'
* add trimmimng and deleting of unneeded columns for 'phenmn_class'
* update example

# LinguGeo 0.0.0.9003
Develop version

* add correction due to amount of variations for local COH (fun CoherenceIndex)
* add checkup for 'phenmn_class' to only use data.frame
* update example

# LinguGeo 0.0.0.9002
Develop version

* add 'phenmn_class' function

# LinguGeo 0.0.0.9001
Develop version

* add 'CoherenceIndex' function along old original version to test for errors.
* add example data

# LinguGeo 0.0.0.9000
Initial version

* create package via devtools::create()
add git repo via Rstudio Build
add NEWS via usethis::use_news_md()
* set description
* add main package script
