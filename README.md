# Improving Archaeological Metadata Reporting in Human Paleogenomic Studies

This repository contains data and R scripts to generate results and figures in the paper.

## Requirements

This project was performed on R version 4.4.0 and requires the following packages:

- ```tidyverse```
#### Reproducible workflow
- ```here```
#### Statistical analysis
- ```FactoMineR```
- ```betareg```
- ```statmod```
- ```sandwich```
- ```lmtest```
- ```car```
- ```rstatix```
- ```FSA```
#### Data visualisation
- ```factoextra```
- ```GGally```
- ```ggh4x```
- ```MetBrewer```
- ```RColorBrewer```
- ```patchwork```

You can install them using:
```r
install.packages(c("tidyverse", "here",
  "FactoMineR", "betareg", "statmod", "sandwich", "lmtest", "car", "rstatix", "FSA",
  "factoextra", "GGally", "ggh4x", "MetBrewer", "RColorBrewer", "patchwork"))
```

You should also create a ```figures/``` directory under the project directory to store the figure output.

## File descriptions
- ```generate_overview.R```: Generate the overview figures
- ```variance_analysis.R```: Analyse variances across metadata and publications
- ```correlation_analysis.R```: Analyse correlations between differerent pairs of variables
- ```pca.R```: Principal component analyses (PCA) on metadata completeness and explanatory variables
- ```regression_analysis```: Regression analysis on the relationships between metadata completeness and explanatory variables
  
The following will be read in the scripts above (i.e. no need to run)

- ```data_wrangling.R```: Load and wrangle data
- ```functions.R```: Some functions written for other scripts
- ```plot_style.R``` Contains additional objects for data visualization
