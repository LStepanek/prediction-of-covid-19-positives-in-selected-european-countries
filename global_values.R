###############################################################################
###############################################################################
###############################################################################

## global values and constants ------------------------------------------------

k <- 10 #10   # number of k-fold cross validation repeating

my_countries <- c(
    # "Albania",
    # "Andorra",
    # "Austria",
    # "Belarus",
    # "Belgium",
    # "Bosnia and Herzegovina",
    # "Bulgaria",
    # "Croatia",
    # "Cyprus",
    "Czechia",
    # "Denmark",
    # "England",
    # "Estonia",
    # "Faeroe Islands",
    # "Finland",
    "France",
    "Germany",
    # "Gibraltar",
    # "Greece",
    # "Guernsey",
    # "Hungary",
    # "Iceland",
    # "Ireland",
    # "Isle of Man",
    "Italy",
    # "Jersey",
    # "Kosovo",
    # "Latvia",
    # "Liechtenstein",
    # "Lithuania",
    # "Luxembourg",
    # "Malta",
    # "Moldova",
    # "Monaco",
    # "Montenegro",
    "Netherlands",
    # "North Macedonia",
    # "Northern Ireland",
    # "Norway",
    # "Poland",
    # "Portugal",
    # "Romania",
    # "Russia",
    # "San Marino",
    # "Scotland",
    # "Serbia",
    # "Slovakia",
    # "Slovenia",
    "Spain"#,
    # "Sweden",
    # "Switzerland",
    # "Ukraine",
    # "United Kingdom",
    # "Vatican",
    # "Wales"
)

my_dependent_variable <- "new_cases_smoothed"

my_independent_variables <- c(
    "new_deaths_smoothed",
    "reproduction_rate",
    "weekly_icu_admissions",
    "weekly_hosp_admissions",
    #"people_vaccinated",
    #"people_fully_vaccinated",
    "new_vaccinations_smoothed",
    "new_people_vaccinated_smoothed"
)

my_starting_date <- as.Date("2021-01-01") #as.Date("2020-01-01")
my_ending_date <- as.Date("2022-12-31") #as.Date("2023-12-31") #as.Date("2023-09-30")

my_time_periods <- c(
    1,
    3,
    5,
    7,
    10,
    15,
    20,
    30,
    40,
    50,
    60,
    80,
    100,
    120#,
    #140,
    #180
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





