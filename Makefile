# Version string to keep track of different runs
VERSION ?= v3

# Set to anything other than TRUE to refit all models from scratch
USE_SAVED_MODELS ?= TRUE

ifeq ($(USE_SAVED_MODELS), TRUE)
	RFLAGS=-v $(VERSION) -c
else
	RFLAGS=-v $(VERSION) -f
endif

# List of 21 datasets
names=\
	adult \
	annealing \
	audiology-std \
	bank \
	bankruptcy \
	car \
	chess-krvk \
	chess-krvkp \
	congress-voting \
	contrac \
	credit-approval \
	ctg \
	cylinder-bands \
	dermatology \
	german_credit \
	heart-cleveland \
	ilpd \
	mammo \
	mushroom \
	wine \
	wine_qual

raw=$(addsuffix .csv.gz, $(addprefix data/raw/, $(names)))
data=$(addsuffix /data.rds, $(addprefix data/, $(names)))
pred=$(addsuffix /pred.rds, $(addprefix data/, $(names)))
perf=$(addsuffix /perf.rds, $(addprefix data/, $(names)))


# Helpful shortcuts
print-%: ; @echo $* = $($*)

.PHONY: preprocess
preprocess: $(data)

.PHONY: build
build: $(pred)

.PHONY: eval
eval: $(perf)

.PHONY: plot
plot: fig/Fig1_color_auc_no_legend.pdf

data/%/data.rds: $(raw) src/utils.R src/preprocess.R
	cd src && ./preprocess.R $(RFLAGS)

data/%/pred.rds: data/%/data.rds src/build.R src/build_deps.R
	cd src && ./build.R $(RFLAGS)

data/%/perf.rds: data/%/pred.rds src/evaluate.R
	cd src && ./evaluate.R $(RFLAGS)

fig/%: data/predictions.$(VERSION).rds
	cd src && ./plots.R $(RFLAGS)
