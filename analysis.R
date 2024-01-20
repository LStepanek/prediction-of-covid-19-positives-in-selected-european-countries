###############################################################################
###############################################################################
###############################################################################

## I am performing the main analysis -- for various conutries and
## time period lenghts of days back in the past we utilize the data from,
## I predict the next-day number of COVID-19 positives,
## using machine-learning approaches and selected predictor variables

my_root_mean_square_errors <- list()

for(
    my_country in my_countries
){
    
    for(
        my_time_period in my_time_periods
    ){
        
        #### I am subsetting the dataset of my interest only ------------------
        
        temp_data <- my_data[
            my_data[, "location"] == my_country
            ,
        ]
        
        
        #### now, I am transforming data of my interest and preparing formulas
        #### for the predictive models ----------------------------------------
        
        my_final_starting_date <- max(
            my_starting_date,
            getMyLongestTimeInterval(
                temp_data,
                date_variable = "date"
            )[["first_day"]]
        )
        
        my_final_ending_date <- min(
            my_ending_date,
            getMyLongestTimeInterval(
                temp_data,
                date_variable = "date"
            )[["last_day"]]
        )
        
        # for(
            # my_variable in c(
                # my_independent_variables,
                # my_dependent_variable
            # )
        # ){
            
            # temp_data[my_variable] <- replaceMissingValues(
                # my_data = temp_data,
                # my_variable = my_variable
            # )
            
        # }
        
        my_transformed_data <- getMyTransformedData(
            
            my_data = temp_data,
            my_independent_variables = my_independent_variables,
            my_dependent_variable = my_dependent_variable,
            date_variable = "date",
            starting_date = my_final_starting_date,
            ending_date = my_final_ending_date,
            time_period = my_time_period
            
        )
        
        my_transformed_data <- my_transformed_data[
            complete.cases(my_transformed_data)
            ,
        ]
        
        my_transformed_independent_variables <-
            getMyTransformedIndependentVariables(
                
                my_independent_variables = my_independent_variables,
                time_period = my_time_period
                
            )
        
        my_transformed_dependent_variable <-
            getMyTransformedDependentVariable(
                
                my_dependent_variable = my_dependent_variable
                
            )
        
        my_formula <-
            getMyFormula(
                
                my_independent_variables = my_independent_variables,
                my_dependent_variable = my_dependent_variable,
                time_period = my_time_period
                
            )
        
        
        #### I am checking whether the transformed data contain at least one
        #### observation ------------------------------------------------------
        
        if(
            dim(my_transformed_data)[1] == 0
        ){
            
            break
            
        }
        
        
        #### now, I am running the k-fold cross-validation --------------------
        
        for(i in 1:k){
            
            #### I am splitting the dataset into a train set, containing
            #### (k - 1)/k of all observations, and a test set, containing
            #### 1/k of data --------------------------------------------------
            
            test_set_indices <- c(
                (
                    (i - 1) * floor(dim(my_transformed_data)[1] / k) + 1
                ):(
                    i * floor(dim(my_transformed_data)[1] / k)
                )
            )
            
            train_set <- my_transformed_data[-test_set_indices, ]
            test_set <- my_transformed_data[test_set_indices, ]
            
            
            ## multivariate regression ----------------------------------------
            
            my_model <- NULL
            my_model <- lm(
                
                formula = my_formula,
                data = train_set,
                
            )
            
            my_predictions <- NULL
            my_predictions <- predict(
                object = my_model,
                newdata = test_set[
                    ,
                    my_transformed_independent_variables
                ]
            )
            
            my_root_mean_square_errors[[
                paste("regression", my_country, sep = "_")
            ]][[
                paste(
                    "t = ",
                    my_time_period,
                    sep = ""
                )
            ]] <- c(
                
                my_root_mean_square_errors[[
                    paste("regression", my_country, sep = "_")
                ]][[
                    paste(
                        "t = ",
                        my_time_period,
                        sep = ""
                    )
                ]],
                getRootMeanSquareError(
                    observed_values = test_set[
                        ,
                        my_transformed_dependent_variable
                    ],
                    predicted_values = my_predictions
                )
                
            )
            
            
            ## LASSO ----------------------------------------------------------
            
            my_model <- NULL
            my_model <- glmnet::cv.glmnet(
                
                x = as.matrix(
                    train_set[
                        ,
                        my_transformed_independent_variables
                    ]
                ),
                y = train_set[
                    ,
                    my_transformed_dependent_variable
                ],
                type.measure = "mse",
                alpha = 1,   # 1 for lasso, 0 for ridge regression
                family = "gaussian",
                nfolds = k
                
            )
            
            my_predictions <- NULL
            my_predictions <- predict(
                object = my_model,
                newx = as.matrix(
                    test_set[
                        ,
                        my_transformed_independent_variables
                    ]
                ),
                s = my_model[["lambda.min"]]
            )[, 1]
            
            my_root_mean_square_errors[[
                paste("lasso", my_country, sep = "_")
            ]][[
                paste(
                    "t = ",
                    my_time_period,
                    sep = ""
                )
            ]] <- c(
                
                my_root_mean_square_errors[[
                    paste("lasso", my_country, sep = "_")
                ]][[
                    paste(
                        "t = ",
                        my_time_period,
                        sep = ""
                    )
                ]],
                getRootMeanSquareError(
                    observed_values = test_set[
                        ,
                        my_transformed_dependent_variable
                    ],
                    predicted_values = my_predictions
                )
                
            )
            
            
            ## ridge regression -----------------------------------------------
            
            my_model <- NULL
            my_model <- glmnet::cv.glmnet(
                
                x = as.matrix(
                    train_set[
                        ,
                        my_transformed_independent_variables
                    ]
                ),
                y = train_set[
                    ,
                    my_transformed_dependent_variable
                ],
                type.measure = "mse",
                alpha = 0,   # 1 for lasso, 0 for ridge regression
                family = "gaussian",
                nfolds = k
                
            )
            
            my_predictions <- NULL
            my_predictions <- predict(
                object = my_model,
                newx = as.matrix(
                    test_set[
                        ,
                        my_transformed_independent_variables
                    ]
                ),
                s = my_model[["lambda.min"]]
            )[, 1]
            
            my_root_mean_square_errors[[
                paste("ridge", my_country, sep = "_")
            ]][[
                paste(
                    "t = ",
                    my_time_period,
                    sep = ""
                )
            ]] <- c(
                
                my_root_mean_square_errors[[
                    paste("ridge", my_country, sep = "_")
                ]][[
                    paste(
                        "t = ",
                        my_time_period,
                        sep = ""
                    )
                ]],
                getRootMeanSquareError(
                    observed_values = test_set[
                        ,
                        my_transformed_dependent_variable
                    ],
                    predicted_values = my_predictions
                )
                
            )
            
            
            ## support vector machines ----------------------------------------
            
            my_model <- NULL
            my_model <- e1071::svm(
                
                x = train_set[
                    ,
                    my_transformed_independent_variables
                ],
                y = train_set[
                    ,
                    my_transformed_dependent_variable
                ],
                type = "eps-regression",
                scale = TRUE,
                epsilon = 0.1
                
            )
            
            my_predictions <-NULL
            my_predictions <- predict(
                object = my_model,
                newdata = test_set[
                    ,
                    my_transformed_independent_variables
                ]
            )
            
            my_root_mean_square_errors[[
                paste("svm", my_country, sep = "_")
            ]][[
                paste(
                    "t = ",
                    my_time_period,
                    sep = ""
                )
            ]] <- c(
                
                my_root_mean_square_errors[[
                    paste("svm", my_country, sep = "_")
                ]][[
                    paste(
                        "t = ",
                        my_time_period,
                        sep = ""
                    )
                ]],
                getRootMeanSquareError(
                    observed_values = test_set[
                        ,
                        my_transformed_dependent_variable
                    ][
                        which(
                            names(my_predictions) %in% rownames(test_set)
                        )
                    ],
                    predicted_values = my_predictions
                )
                
            )
            
            
            ## random forests -------------------------------------------------
            
            my_model <- NULL
            my_model <- randomForest::randomForest(
                
                x = train_set[
                    ,
                    my_transformed_independent_variables
                ],
                y = train_set[
                    ,
                    my_transformed_dependent_variable
                ],
                importance = TRUE,
                ntree = 500
                
            )
            
            my_predictions <-NULL
            my_predictions <- predict(
                object = my_model,
                newdata = test_set[
                    ,
                    my_transformed_independent_variables
                ]
            )
            
            my_root_mean_square_errors[[
                paste("forests", my_country, sep = "_")
            ]][[
                paste(
                    "t = ",
                    my_time_period,
                    sep = ""
                )
            ]] <- c(
                
                my_root_mean_square_errors[[
                    paste("forests", my_country, sep = "_")
                ]][[
                    paste(
                        "t = ",
                        my_time_period,
                        sep = ""
                    )
                ]],
                getRootMeanSquareError(
                    observed_values = test_set[
                        ,
                        my_transformed_dependent_variable
                    ],
                    predicted_values = my_predictions
                )
                
            )
            
            
            # ## neural networks ------------------------------------------------
            
            # # for(   # I am rescaling all numerical varaibles to range of <0, 1>
                # # temp_temp_data_name in c(
                    # # "train_set",
                    # # "test_set"
                # # )
            # # ){
                
                # # temp_temp_data <- get(temp_temp_data_name)
                
                # # for(
                    # # my_variable in colnames(temp_temp_data)
                # # ){
                    
                    # # if(
                        # # class(temp_temp_data[, my_variable]) == "numeric"
                    # # ){
                        
                        # # temp_temp_data[, my_variable] <- (
                            # # temp_temp_data[, my_variable] - min(
                                # # temp_temp_data[, my_variable],
                                # # na.rm = TRUE
                            # # )
                        # # ) / (
                            # # max(
                                # # temp_temp_data[, my_variable],
                                # # na.rm = TRUE
                            # # ) - min(
                                # # temp_temp_data[, my_variable],
                                # # na.rm = TRUE
                            # # )
                        # # )
                        
                    # # }
                    
                # # }
                
            # # }
            
            # # my_model <- NULL
            # # my_model <- neuralnet::neuralnet(
                
                # # formula = my_formula,
                # # data = train_set,
                # # hidden = 10,
                # # ### c(
                # # ###    dim(train_set)[1]
                # # ### ),
                # # linear.output = TRUE,
                # # algorithm = "rprop+"
                
            # # )
            
            # # my_predictions <-NULL
            # # my_predictions <- neuralnet::compute(
                # # x = my_model,
                # # covariate = test_set,
            # # )[["net.result"]][, 1]
            
            # # my_root_mean_square_errors[[
                # # paste("nnets", my_country, sep = "_")
            # # ]][[
                # # paste(
                    # # "t = ",
                    # # my_time_period,
                    # # sep = ""
                # # )
            # # ]] <- c(
                
                # # my_root_mean_square_errors[[
                    # # paste("nnets", my_country, sep = "_")
                # # ]][[
                    # # paste(
                        # # "t = ",
                        # # my_time_period,
                        # # sep = ""
                    # # )
                # # ]],
                # # getRootMeanSquareError(
                    # # observed_values = test_set[
                        # # ,
                        # # my_transformed_dependent_variable
                    # # ],
                    # # predicted_values = my_predictions
                # # )
                
            # # )
            
            
            ## log messages ---------------------------------------------------
            
            # flush.console()
            # cat(
                # paste(
                    # "time period = ",
                    # my_time_period,
                    # " | ",
                    # i,
                    # "-th iteration out of k = ",
                    # k,
                    # sep = ""
                # )
            # )
            # cat("\n")
            
        }
        
        
        ## log messages -------------------------------------------------------
        
        flush.console()
        cat(
            paste(
                "country = ",
                my_country,
                " | ",
                "time period = ",
                my_time_period,
                " | ",
                "analysis completed",
                sep = ""
            )
        )
        cat("\n")
        
    }
    
    
    #### ----------------------------------------------------------------------
    
}


#### I am saving the final list as an output ----------------------------------

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
            "my_root_mean_square_errors_list",
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
                "my_root_mean_square_errors_list",
                "_k_=_",
                k,
                ".txt",
                sep = ""
            )
        )
    )
}

sink(
    file = paste(
        "my_root_mean_square_errors_list",
        "_k_=_",
        k,
        ".txt",
        sep = ""
    ),
    append = TRUE
)

my_list <- dput(
    my_root_mean_square_errors
)

#print(my_list)

sink()

setwd(
    mother_working_directory
)


#### I am saving the final list as LaTeX-friendly tables ----------------------

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
            "my_root_mean_square_errors_tables",
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
                "my_root_mean_square_errors_tables",
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
        
        my_final_data <- my_root_mean_square_errors[[
            paste(
                my_algorithm,
                "_",
                my_country,
                sep = ""
            )
        ]]
        
        names(my_final_data) <- gsub(
            "^t = ",
            "",
            names(my_final_data)
        )
        
        sink(
            file = paste(
                "my_root_mean_square_errors_tables",
                "_k_=_",
                k,
                ".txt",
                sep = ""
            ),
            append = TRUE
        )
        
        cat(
            paste(
                "% ",
                my_algorithm,
                ", ",
                my_country,
                sep = ""
            )
        )
        cat("\n")
        
        print(
            xtable(
                do.call("rbind", my_final_data),
                digits = 1
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


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





