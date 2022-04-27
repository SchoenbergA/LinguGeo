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
