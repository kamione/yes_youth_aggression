# Background Information

This repo contains analysis scripts for _*Data-driven derived irritability scores predict aggression mediated by psychopathology in a youth community cohort*_

# Analysis Scripts

The analysis scripts are formated in a stepwise approach. Please run the script one-by-one.

```{plain text}
01_datacleaning.R
```

Step 1: Clean the dataset and save the preprocessed dataset for further analysis

```{plain text}
02_visualization.R
```

Step 2: Visualize basic demographics and basic information of the variables

```{plain text}
03_aggressionprediction.R
```

Step 3: Predict aggression scores using LASSO for feature selection

```{plain text}
04_networkanaysis.R
```

Step 4: Network analysis for understand the interactions among outcome and features

```{plain text}
05_directedacyclicgraph.R
```

Step 5: Check for the putative casuality between outcome and features

```{plain text}
06_mediationanalysis.R
```

Step 6: Sum the items found in step 5 and examine the mediation effect of psychopathology
