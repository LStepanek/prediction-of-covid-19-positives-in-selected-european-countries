###############################################################################
###############################################################################
###############################################################################

## I am setting a working directory -------------------------------------------

while(
    ! "__main__.R" %in% dir()
){
    setwd(choose.dir())
}

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################

## I am running the scripts one by one ----------------------------------------

for(
    my_script in c(
        
        "global_values",           # global values and constants, if any
        "helper_functions",        # definition of helper functions
        "initialization",          # initialization of packages and files
        "data_loading",            # all data loading
        "data_processing",         # data processing
        "analysis",                # searching for root mean square errors
                                   # depending on time period lengths
                                   # for various algorithms and countries
        "plots",                   # plotting the root mean square errors
                                   # depending on time period lengths
                                   # for various algorithms and countries
        "other_output_saving"      # outputting some other outcomes, mainly
                                   # systematic captions for the manuscript
                                   # and some number summaries
        
    )
){
    
    ## I am setting the mother working directory as a working directory -------
    
    setwd(
        mother_working_directory
    )
    
    
    ## initial log messages ---------------------------------------------------
    
    flush.console()
    
    cat(
        paste(
            "The script '",
            my_script,
            ".R' is running.",
            sep = ""
        )
    )
    cat("\n")
    
    
    ## the script "my_script" is executed -------------------------------------
    
    source(
        
        paste(
            my_script,
            ".R",
            sep = ""
        ),
        echo = TRUE,
        encoding = "UTF-8",
        max.deparse.length = Inf
        
    )
    
    
    ## ending log messages ----------------------------------------------------
    
    flush.console()
    
    cat(
        paste(
            "The script '",
            my_script,
            ".R' has been successfully executed.",
            sep = ""
        )
    )
    cat("\n")
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





