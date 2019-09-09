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

Case study in the appendix can be reproduced with

```bash
make case
```

which runs the script `src/case_study.R` and its dependencies.

## Exploration notebook

An exploration notebook `src/explore.Rmd` is provided for convenient exploration.
This requires that the data are preprocessed.

The preprocessing command `make preprocess`` is included as a shell script in the
first executable cell of the markdown file.
However, if your system setup does not support running `sh` commands (which might be the case
for Windows machines), manually run

```bash
make preprocess
```

and remove the corresponding cell of the `Rmd` file before compiling.
