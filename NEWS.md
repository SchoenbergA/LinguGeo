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
