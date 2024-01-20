###############################################################################
###############################################################################
###############################################################################

## I am plotting the table captions for each country and algorithm ------------

setwd(
    paste(
        mother_working_directory,
        "outputs",
        sep = "/"
    )
)

if(
    file.exists(
        paste(
            "table_captions",
            "_k_=_",
            k,
            ".txt",
            sep = ""
        )
    )
){
    invisible(
        file.remove(
            paste(
                "table_captions",
                "_k_=_",
                k,
                ".txt",
                sep = ""
            )
        )
    )
}

for(
    my_algorithm in c(
        
        "regression",
        "lasso",
        "ridge",
        "svm",
        "forests"
        
    )
){
    
    for(
        my_country in my_countries
    ){
        
        sink(
            file = paste(
                "table_captions",
                "_k_=_",
                k,
                ".txt",
                sep = ""
            ),
            append = TRUE
        )
        
        cat(
            paste(
                "\\caption{%",
                "\n",
                "Root mean square errors (RMSE) ",
                "for each iteration of the 10-fold cross-validation ",
                "and each time period length \\textit{t}, ",
                "calculated for ",
                switch(
                    my_algorithm,
                    "regression" = {"multivariate regression"},
                    "lasso" = {"least absolute shrinkage and selection operator"},
                    "ridge" = {"ridge regression"},
                    "svm" = {"support vector machines"},
                    "forests" = {"random forests"}
                ),
                " algorithm and ",
                my_country,
                " as a country.",
                "\n",
                paste(
                    "\\label{",
                    my_algorithm,
                    "_",
                    my_country,
                    "_",
                    "table",
                    "}%",
                    sep = ""
                ),
                "\n",
                "}",
                sep = ""
            )
        )
        
        cat("\n")
        cat("\n")
        
        sink()
        
    }
    
}

setwd(
    mother_working_directory
)


## I am calculating average RMSE for each algorithm ---------------------------

for(
    my_algorithm in c(
        
        "regression",
        "lasso",
        "ridge",
        "svm",
        "forests"
        
    )
){
    
    my_rmse <- NULL
    
    for(
        my_country in my_countries
    ){
        
        my_final_data <- my_root_mean_square_errors[[
            paste(
                my_algorithm,
                "_",
                my_country,
                sep = ""
            )
        ]]
        
        my_rmse <- c(
            my_rmse,
            min(
                unlist(lapply(my_final_data, "mean"))
            )
        )
        
    }
    
    cat(
        my_algorithm
    )
    cat("\n")
    
    print(summary(my_rmse))
    
    cat("\n")
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





