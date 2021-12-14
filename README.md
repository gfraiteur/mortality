## Analysis of mortality based on the Human Mortality Database

This repo provides an analysis of the excess mortality in 2020 in most European countries. It contains the source code of both the scripts and the
article, but not the data itself. There are two articles in this repo:

* [Methodology and detailed per-country graphs](https://gfraiteur.github.io/mortality/detailed.html) (see also the [source code](detailed.Rmd)).
* [Comparison of countries and correlations](https://gfraiteur.github.io/mortality/comparison.html) (see also the [source code](comparison.Rmd)).


The scripts behind the article are:
* [country.R](country.R) analyzes one country,
* [all_countries.R](all_countries.R) aggregates all countries (this is typically the entry point).


### Running the scripts

If you are interested in the scripts:

* Create an account with the website https://www.mortality.org/. Create a file named _passwords.R_ with the following content:
        ```
        hmd_username = "your@email.com"
        hmd_password = "******"
        ```

* The script that downloads the data and processes them is [generic.R](generic.R). You need to set the `current_country_hmd_code` variable to a valid
HMD country code before running this script.

* The top-level script is [all_countries.R](all_countries.R)

## Analysis of mortality with or without COVID-19 vaccination based UK Office for National Statistics

This analysis is available (here)[https://gfraiteur.github.io/mortality/mortality_vaccinated_uk.html].


## Reporting bugs
 
This article and the scripts are open sources and the data sources are public. If you found errors, please
[submit an issue](https://github.com/gfraiteur/mortality/issues) or, better, a pull request.
 
 
