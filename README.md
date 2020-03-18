# SEIR_COVID19

The code for creating the R Shiny application https://alhill.shinyapps.io/COVID19seir/ is in the directory COVID19seir. This code is an SEIR model for COVID-19 infection, including different clinical trajectories of infection, interventions to reduce transmission, and comparisons to healthcare capacity. 

The code that produces the interface and functionality of the Shiny App is in files
* **server.R**
* **ui.R**

Files used in the explanatory sections of the app are
* **SEIR.Rmd**
* **www/Parameters.nb.html**

All the functions that actually run the model and process the parameters are in the **code/functions.R** file

If you want to run the code to produce the same outputs as Shiny but without dealing with the app structure, you can use the R scripts
* **runSpread.R**
* **runInterventions.R**
* **runCapacity.R**

When trying out new model structures or plots, it is much easier to work with scripts instead of directly with the app. Once troubleshooting is done, it can be integrated with the app

There is also a Python notebook simulating the same model (.ipynb)

You can use this page to report issues you find with the app

Developers: No pull requests will be accepted from this repository, which is only for sharing the stable code used in the app. If you are contributing to this project please use the developmental repository https://github.com/alsnhll/SEIR_COVID19_Dev

Caution: This model is still a work in progress and will be updated over the next few days. 
