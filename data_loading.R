###############################################################################
###############################################################################
###############################################################################

## I am loading data ----------------------------------------------------------

setwd(
    paste(
        mother_working_directory,
        "inputs",
        sep = "/"
    )
)

my_data <- read.csv(
    
    file = "_data_.csv",
    header = TRUE,
    sep = ",",
    colClasses = "character"
    
)

setwd(
    mother_working_directory
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





