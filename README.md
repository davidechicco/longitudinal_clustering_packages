# longitudinal trajectiories and mixed models R packages

## Requirements
To run the scripts of this repository, the user needs to have installed:
* R (version >= 4.1.3)
* R libraries: `easypackages`, `plyr`, `annotate`, `GEOquery`, `geneExpressionFromGEO`, `randomForest`,  `PRROC`, `e1071`, `class`, `gmodels`, `formula.tools`, `dplyr`, `pastecs`, `ROSE`

## Exectution
The first step is the execution of the script that downloads the dataset from GEO, applies our pan-cancer signature, and produces the dataset ready for the binary classification.
For example, if the user wants to download the [GSE25066 Hatzis2009 dataset](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=gse25066) and prepare it for the binary classification, she/he needs to run:


`Rscript preparation_dataset_breast_cancer_Hatzis2009_our_signature.r`

This script execution will generate an output file whose name will be similar to: `Hatzis2009_GSE25066_breast_cancer_our_pancancer_signature_probesets_dataset_1786.csv`

Once the previous script finished correctly, the user will have to edit the first lines of the Random Forests script by inserting the name of the generated data file of the Hatzis2009 dataset. Then, she/will will have to execute that script:

`Rscript random_forest_classification.r Hatzis2009_GSE25066_breast_cancer_our_pancancer_signature_probesets_dataset_1786.csv`

## Article
More information about this study can be found in the following article:

> Davide Chicco, Abbas Alameer,  Sara Rahmati, and Giuseppe Jurman, "[Towards a potential pan-cancer prognostic signature for gene expression based on probesets and ensemble machine learning](https://doi.org/10.1186/s13040-022-00312-y)", _BioData Mining_ 15, 28 (2022).

## Contacts
The software code was developed by [Davide Chicco](https://www.DavideChicco.it). Questions should be
addressed to davidechicco(AT)davidechicco.it
