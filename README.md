This repo provides an analysis of the excess mortality in 2020 in most European countries. It contains the source code of both the scripts and the
article, but not the data itself.

* To read the article, go to the [rendered article](https://gfraiteur.github.io/mortality/detailed.html).
* To edit the article, open [all_countries.Rmd](detailed.Rmd).


## Running the scripts

If you are interested in the scripts:

* Create an account with the website https://www.mortality.org/. Create a file named _passwords.R_ with the following content:
        ```
        hmd_username = "your@email.com"
        hmd_password = "******"
        ```

* The script that downloads the data and processes them is [generic.R](generic.R). You need to set the `current_country_hmd_code` variable to a valid
HMD country code before running this script.

* The top-level script is [all_countries.R](all_countries.R)


## Reporting bugs
 
This article and the scripts are open sources and the data sources are public. If you found errors, please
[submit an issue](https://github.com/gfraiteur/mortality/issues) or, better, a pull request.
 
 
