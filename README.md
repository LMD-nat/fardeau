# Fardeau

Fardeau is the project name for my MSc project, entitled *L’influence du fardeau des proches aidants sur l’utilisation des services d’urgence par les   personnes aînées en transition de soins : une étude observationnelle à méthodes mixtes*.  

## Directory

The relevant files are organized as follows. 

`analyses` contains code for the regression models and sensitivity analyses. 

`clean` contains the code for both the categorization of factor variables and the cleaning of the data as extracted from the medical database. 

`simulations` contains the code for a power analysis to get an idea of how many predictors could be accomodated in a given model based on the effect size, error, and sample size. 

`src` contains any assets or files relevant to the project, like PDFs or images. No data will be uploaded here. 

```bash
- analyses -
           |_ Poisson Model 30 Day Revisits.R
           |_ Seven Day Revisit Models.Rmd
           |_ Thirty Day Admission Models.Rmd
           |_ Thirty Day Revisit Models.Rmd
           |_ Three Day Revisit Models.Rmd
           |_ sensitivity-analyses.R

- clean -
        |_ AddFactors.R
        |_ ExtractMedGPS.Rmd

- simulations -
              |_ PowerAnalysis.R

- src -
      |_ JSCISSCA_2024.pdf

```

## Manuscripts and Posters

[Poster](https://github.com/LMD-nat/fardeau/blob/main/src/JSCISSCA_2024.pdf) presented at the 5e Journée scientifique du Centre de recherche du CISSS de Chaudière-Appalaches. 
