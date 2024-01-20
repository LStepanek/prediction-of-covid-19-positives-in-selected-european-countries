###############################################################################
###############################################################################
###############################################################################

## I am plotting the root mean square error for each time period length
## and each country and algorithm ---------------------------------------------

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
        
        setwd(
            paste(
                mother_working_directory,
                "outputs",
                sep = "/"
            )
        )
        
        # png(
            # file = paste(
                # my_algorithm,
                # "_",
                # my_country,
                # "_rmse_for_various_t",
                # "_k_=_",
                # k,
                # ".png",
                # sep = ""
            # ),
            # width = 7,
            # height = 2.5,
            # units = "in",
            # res = 600
        # )
        
        cairo_ps(
            file = paste(
                my_algorithm,
                "_",
                my_country,
                "_rmse_for_various_t",
                "_k_=_",
                k,
                ".eps",
                sep = ""
            ),
            width = 9.0,
            height = 2.25,
            pointsize = 12
        )
        
        par(mar = c(4.0, 4.1, 1.2, 0.1))
        
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
        
        boxplot(
            my_final_data,
            xlab = expression(
                paste(
                    "time period lenght [days], ",
                    italic(t),
                    sep = ""
                )
            ),
            ylab = "RMSE",
            xaxt = "n",
            #cex.axis = 0.9,
            outline = FALSE
        )
        
        # axis(
            # side = 1,
            # at = seq(
                # from = 1,
                # to = 1 + 1.0 * (length(my_final_data) - 1),
                # by = 1.0
            # ),
            # labels = names(my_final_data)
        # )
        
        text(
            x = seq(
                from = 1,
                to = 1 + 1.0 * (length(my_final_data) - 1),
                by = 1.0
            ),
            y = par("usr")[3] - 0.2 * (par("usr")[4] - par("usr")[3]),
            labels = names(my_final_data),
            srt = 45,
            xpd = TRUE
        )
        
        legend(
            x = "topleft",
            title = paste("  ", my_country, sep = ""),
            # switch(
                # my_algorithm,
                # "regression" = {"  multivariate regression"},
                # "lasso" = {"  LASSO"},
                # "ridge" = {"  ridge regression"},
                # "svm" = {"  support vector machines"},
                # "forests" = {"  random forests"}
            # ),
            legend = "",
            bty = "n"
        )
        
        points(
            x = seq(
                from = 1,
                to = 1 + 1.0 * (length(my_final_data) - 1),
                by = 1.0
            ),
            y = unlist(
                lapply(
                    my_final_data,
                    "mean"
                )
            ),
            type = "b",
            col = "red",
            pch = 15
        )
        
        dev.off()
        
        setwd(
            mother_working_directory
        )
        
    }
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





