# Machine learning prediction of EVD survival in pediatric patients

This repository contains the code to construct, validate, and update the EVD Prognosis in Children (EPiC) model for prognosis prediction in pediatric patients of Ebola Virus Disease (EVD).

The code is organized in two folders:

* r-notebooks: It contains all the R notebooks that were used to clean-up the data, generate the prognostic model from the EDP training dataset, validate the model on the DRC dataset, and update the model with the addition of laboratory biomarkers.

* shiny-app: It contains the source code of the Shiny app that is provides an easy-to-use risk calculator that prints the output of the prognostic model given the values of its input variables. It is available online at https://kelseymbutler.shinyapps.io/epic-calculator/