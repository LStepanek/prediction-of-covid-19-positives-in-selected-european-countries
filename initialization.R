###############################################################################
###############################################################################
###############################################################################

## I am installing and initializung all necessary packages --------------------

invisible(
    
    lapply(
        
        c(
            
            "nnet",
            "glmnet",
            "e1071",
            "randomForest",
            "neuralnet",
            "xtable"
            
        ),
        
        function(my_package){
            
            if(
                ! my_package %in% rownames(installed.packages())
            ){
                
                install.packages(
                    
                    my_package,
                    dependencies = TRUE,
                    repos = "http://cran.us.r-project.org"
                    
                )
                
            }
            
            library(
                
                my_package,
                character.only = TRUE
                
            )
            
        }
        
    )
    
)


## ----------------------------------------------------------------------------

###############################################################################

## a path to Rtools if needed -------------------------------------------------

Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip")


## ----------------------------------------------------------------------------

###############################################################################

## I am creating subfolders "inputs" and "outputs" if those haven't been
## initialized yet ------------------------------------------------------------

setwd(
    mother_working_directory
)

for(
    my_subdirectory in c(
        "inputs",
        "outputs"
    )
){
    
    if(
        ! file.exists(my_subdirectory)
    ){
        
        dir.create(
            file.path(
                
                mother_working_directory,
                my_subdirectory
                
            )
        )
        
    }
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





