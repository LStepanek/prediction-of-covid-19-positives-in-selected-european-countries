###############################################################################
###############################################################################
###############################################################################

## I am defining helper functions ---------------------------------------------

getMyTransformedData <- function(
    
    my_data,
    my_independent_variables,
    my_dependent_variable,
    date_variable = "date",
    starting_date,
    ending_date,
    time_period = 1
    
){
    
    # '''
    # Given the dataset "my_data" containing both "my_independent_variables"
    # and "my_dependent_variable", the function tramsforms the data as
    # follows:
    #    
    # -- starting the last available date, it takes all lines corresponding
    #    the dates between (inclusively)
    #    
    #     T = <last available date - time_period, last available date - 1>
    #    
    #    and using all independent variables, it creates new one for each
    #    combination of d from T and each independent variable X by adding
    #    a suffix to make the new label of "X_t_-_d";
    # -- thus, the number of new variables created the described way is
    #    {# of independent variables} * {lenght of T};
    # -- number of lines in the new dataset is equal to the number of all
    #    possible dates and is lower than or equal to
    #    
    #     {# of all possible dates} <= {# length of dataset} - time_period ;
    #    
    # -- final dimensions of the dataset is
    #    
    #     {# of all possible dates}
    #       X
    #     {{# of independent variables} * {lenght of T} + 1},
    #    
    #    where the last variable is the dependent one.
    #    
    # We assume the observations in "my_data" dataset are sorted in an
    # increasing order according to their date.
    # '''
    
    temp_data <- my_data[
        my_data[, date_variable] >= starting_date &
        my_data[, date_variable] <= ending_date
        ,
    ]
    
    if(
        dim(temp_data)[1] < time_period + 1
    ){
        stop(
            "Number of observations is less than (time_period + 1)!"
        )
    }
    
    my_table <- NULL
    
    for(
        my_last_date in as.character(
            seq(
                from = temp_data[
                    time_period + 1,
                    date_variable
                ],
                to = temp_data[
                    dim(temp_data)[1],
                    date_variable
                ],
                by = "day"
            )
        )
    ){
        
        my_last_date <- as.Date(my_last_date)
        
        my_line <- NULL
        
        my_dates <- seq(
            from = my_last_date - time_period,
            to = my_last_date - 1,
            by = "day"
        )
        
        for(
            my_independent_variable in my_independent_variables
        ){
            
            for(
                my_date in as.character(my_dates)
            ){
                
                my_date <- as.Date(my_date)
                
                my_line <- c(
                    
                    my_line,
                    temp_data[
                        temp_data[, date_variable] == my_date,
                        my_independent_variable
                    ]
                    
                )
                
                names(my_line)[
                    length(my_line)
                ] <- paste(
                    my_independent_variable,
                    "_",
                    "t_minus_",
                    as.numeric(my_last_date - my_date),
                    sep = ""
                )
                
            }
            
        }
        
        my_line <- c(
            
            my_line,
            temp_data[
                temp_data[, date_variable] == my_last_date,
                my_dependent_variable
            ]
            
        )
        
        names(my_line)[
            length(my_line)
        ] <- paste(
            my_dependent_variable,
            "_",
            "t_minus_",
            as.numeric(my_last_date - my_last_date),
            sep = ""
        )
        
        my_table <- rbind(
            
            my_table,
            my_line
            
        )
        
    }
    
    my_table <- data.frame(
        
        my_table,
        check.names = FALSE
        
    )
    
    rownames(my_table) <- as.character(1:dim(my_table)[1])
    
    return(
        my_table
    )
    
}


getMyFormula <- function(
    
    my_independent_variables,
    my_dependent_variable,
    time_period = 1
    
){
    
    # '''
    # The function returns a formula for machine-learning models.
    # based on "my_independent_variables", "my_dependent_variable",
    # "time_period" and the final dataset output by function
    # getMyTransformedData().
    # '''
    
    my_formula_right_hand_side <- ""
    
    for(
        my_independent_variable in my_independent_variables
    ){
        
        for(
            my_lag in c(
                time_period:1
            )
        ){
            
            my_formula_right_hand_side <- paste(
                my_formula_right_hand_side,
                paste(
                    my_independent_variable,
                    "_",
                    "t_minus_",
                    my_lag,
                    sep = ""
                ),
                sep = " + "
            )
            
        }
        
    }
    
    my_formula_right_hand_side <- gsub(
        "^ \\+ ",
        "",
        my_formula_right_hand_side
    )
    
    return(
        paste(
            paste(
                my_dependent_variable,
                "_",
                "t_minus_0",
                sep = ""
            ),
            " ~ ",
            my_formula_right_hand_side,
            sep = ""
        )
    )
    
}


getMyTransformedIndependentVariables <- function(
    
    my_independent_variables,
    time_period = 1
    
){
    
    # '''
    # The function returns transformed "my_independent_variables" as they
    # are newly denoted using function getMyTransformedData().
    # '''
    
    my_output <- NULL
    
    for(
        my_independent_variable in my_independent_variables
    ){
        
        for(
            my_lag in c(
                time_period:1
            )
        ){
            
            my_output <- c(
                my_output,
                paste(
                    my_independent_variable,
                    "_",
                    "t_minus_",
                    my_lag,
                    sep = ""
                )
            )
            
        }
        
    }
    
    return(
        my_output
    )
    
}


getMyTransformedDependentVariable <- function(
    
    my_dependent_variable
    
){
    
    # '''
    # The function returns transformed "my_dependent_variable" as it
    # is newly denoted using function getMyTransformedData().
    # '''
    
    return(
        paste(
            my_dependent_variable,
            "_",
            "t_minus_0",
            sep = ""
        )
    )
    
}

getRootMeanSquareError <- function(
    
    observed_values,
    predicted_values
    
){
    
    # '''
    # Returns a value of the root mean square error (RMSE),
    # given both the vectors "observed_values" and "predicted_values"
    # are of the same length.
    # '''
    
    return(
        sqrt(
            mean(
                (
                    predicted_values - observed_values
                ) ^ 2
            )
        )
    )
    
}


getMeanAbsolutePercentageError <- function(
    
    observed_values,
    predicted_values
    
){
    
    # '''
    # Returns a value of the mean absolute percentage error (MAPE),
    # given both the vectors "observed_values" and "predicted_values"
    # are of the same length.
    # '''
    
    return(
        mean(
            abs(
                (predicted_values - observed_values) / observed_values
            )
        ) * 100
    )
    
}


getMyLongestTimeInterval <- function(
    
    my_data,
    date_variable = "date"
    
){
    
    # '''
    # Using data "my_data" and date variable "date_variable",
    # it searches and outputs for the longest period of all consecutive
    # days in the data.
    # '''
    
    my_potential_days <- seq(
        min(my_data[, date_variable]),
        max(my_data[, date_variable]),
        by = "day"
    )
    
    my_first_day <- my_potential_days[1]
    
    while(
        ! (
            my_potential_days[
                which(
                    my_potential_days == my_first_day
                )
            ] %in% my_data[, date_variable] &
            my_potential_days[
                which(
                    my_potential_days == my_first_day
                ) + 1
            ] %in% my_data[, date_variable]
        )
    ){
        
        my_first_day <- my_potential_days[
            which(
                my_potential_days == my_first_day
            ) + 1
        ]
        
    }
    
    my_last_day <- my_first_day
    
    while(
       my_potential_days[
            which(
                my_potential_days == my_last_day
            ) + 1
        ] %in% my_data[, date_variable]
    ){
        
        my_last_day <- my_potential_days[
            which(
                my_potential_days == my_last_day
            ) + 1
        ]
        
    }
    
    return(
        list(
            "first_day" = my_first_day,
            "last_day" = my_last_day
        )
    )
    
}

replaceMissingValues <- function(
    
    my_data,
    my_variable
    
){
    
    # '''
    # Particularly for varaibles that are weekly smoothed by rolling average,
    # it copies the value from each week's end to positiions of missing values
    # that belong to the previous week.
    # '''
    
    my_output <- NULL
    my_last_value <- NA
    
    for(
        i in length(my_data[, my_variable]):1
    ){
        
        if(
            ! is.na(my_data[i, my_variable])
        ){
            my_last_value <- my_data[i, my_variable]
        }
        
        my_output <- c(
            
            my_output,
            my_last_value
            
        )
        
    }
    
    return(
        my_output
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





