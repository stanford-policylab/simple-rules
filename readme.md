# Simple rules

Code for replicating public portion of simple rules paper (under review).

All raw data are included in the repo for convenience.

## Main results

UCI results in the paper can be reproduced with four sequential `make` commands:

```bash
make preprocess  # Proprocess and clean raw data
make build       # Train glm, lasso, rf, and srr models for all k, M
make eval        # Evaluate models on test data
make plot        # Generate plots
```

## Appendix case study

Case study in the appendix can be reproduced by running the `src/case_study.R` script.

## Exploration notebook

An exploration notebook `src/explore.Rmd` is provided for convenient exploration.
This requires that the data are preprocessed, so run

```bash
make preprocess
```

before exploring the `Rmd` file.
