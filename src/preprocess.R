#!/usr/bin/env Rscript
source("utils.R")

# Helper functions --------------------------------------------------------
read_data <- function(dataname, ...) {
  # Read raw data for specified dataname

  # Args:
  #   dataname: name of UCI data to load
  #   col_names: column names
  #   ...: other arguments passed to read_csv
  filename <- paste0(dataname, ".csv.gz")
  rawfile_path <- file.path(DATA_DIR, "raw", filename)

  if (!file.exists(rawfile_path))
      stop(sprintf(
        "No raw data for %s. Please download\n", dataname
      ))

  cat(sprintf("Start [%s]\n", dataname))

  read_csv(rawfile_path, ...)
}

annotate <- function(d, nfolds = 10) {
  # Create folds and assign unique id to each example
  set.seed(666)  # set random seed for reproducibility

  cat(sprintf("\t%.2f%% base rate\n", mean(d$label)*100))

  d <- d %>%
    mutate(example_id = 1:n())
           # fold_idx__ = sample(rep_len(1:nfolds, n())))


  # Create folds and run sanity checks
  cnlist <- colnames(d)
  foldInd <- caret::createFolds(d$label, k = nfolds)
  fold_id <- "fold_idx__"

  d[fold_id] <- NaN

  for (i in 1:nfolds) {
      testInd <- foldInd[[i]]
      trainInd <- -testInd
      d[testInd, fold_id] <- i
      for (cn in cnlist) {
          if (cn != "label" && is.factor(d[[cn]]) &&
              (min(table(d[trainInd, cn])) == 0)){
              # Make sure there will be more than 2 levels remaining,
              # otherwise remove
              count <- table(d[trainInd, cn])
              cut <- min(count[count > 0]) + 1  # add 1 for good measure ...
              nBins <- length(count[count >= cut])
              if (nBins > 0) {
                  # Bin all empty levels in the training set with the first
                  # nonzero level as OTHER
                  bin <- group_by_(d[trainInd,], cn) %>%
                      summarize(count = n()) %>% filter(count >= cut)
                  bin <- bin[[cn]]
                  oldLvl <- levels(d[[cn]])
                  newLvl <- ifelse(oldLvl %in% bin, oldLvl, "OTHER")
                  levels(d[[cn]]) <- newLvl
                  warning(
                      sprintf('\t%s \t binned at \t%d \tfor test fold %d\n',
                              cn, cut, i))
              } else {  # remove column
                  d <- select(d, -get(cn))
                  warning(sprintf('\t%s \t removed; only one bin left\n', cn))
              }
          }
      }  # END: bin data for empty levels
  }  # END: generate fold column and run sanity checks

  d
}

write_data <- function (d, dataname) {
  # Write data frame d as rds, given the dataname
  filepath <- getCleanDataPath(dataname)
  filedir <- dirname(filepath)
  if (!dir.exists(filedir)) dir.create(filedir)
  cat(sprintf("\twriting: %s\n", filepath))
  write_rds(d, filepath)
  cat(sprintf("End [%s]\n", dataname))
}

load_old <- function(name) {
  load(sprintf("../data_mnt/%s/data.Rdata", name))
  clean_data
}

# DATASET: adult ----------------------------------------------------------
dataname <- "adult"
target <- ">50K"
col_names <- c("age", "workclass", "fnlwgt", "education", "education_no",
               "marital_status", "occupation", "relationship", "race", "sex",
               "capital_gain", "capital_loss", "hours_per_week",
               "native_country", "label")

read_data(dataname, col_names = col_names, na = "?", col_type = cols_only(
    age = col_integer(),
    workclass = col_factor(levels = NULL),
    education_no = col_factor(levels = NULL, ordered = TRUE),
    marital_status = col_factor(levels = NULL),
    occupation = col_factor(levels = NULL),
    relationship = col_factor(levels = NULL),
    race = col_factor(levels = NULL),
    sex = col_factor(levels = NULL),
    capital_gain = col_integer(),
    capital_loss = col_integer(),
    hours_per_week = col_integer(),
    native_country = col_factor(levels = NULL),
    label = col_character()
  )) %>%
  na.omit() %>%
  mutate(label = (label == target)) %>%
  annotate() %>%
  write_data(dataname)

# DATASET: annealing ------------------------------------------------------
# from dataset description:
#    The "-" values are actually "not_applicable" values rather than
#    "missing_values" (and so can be treated as legal discrete values rather than
#    as showing the absence of a discrete value).
dataname <- "annealing"
target <- 3
col_names <- c("family", "product", "steel", "carbon", "hardness",
               "temper_rolling", "condition", "formability", "strength", "non",
               "surface_finish", "surface_quality", "enamelability", "bc", "bf",
               "bt", "bw", "bl", "m", "chrom", "phos", "cbond", "marvi",
               "exptl", "ferro", "corr", "blue", "lustre", "jurofm", "s", "p",
               "shape", "thick", "width", "len", "oil", "bore", "packing",
               "label")

read_data(dataname, col_names = col_names, col_type = cols_only(
    family = col_factor(levels = NULL),
    steel = col_factor(levels = NULL),
    carbon = col_integer(),
    hardness = col_integer(),
    temper_rolling = col_factor(levels = NULL),
    condition = col_factor(levels = NULL),
    formability = col_factor(levels = c("?", "1", "2", "3", "4", "5"),
                             ordered=TRUE),
    strength = col_integer(),
    non = col_factor(levels = NULL),
    surface_finish = col_factor(levels = NULL),
    surface_quality = col_factor(levels = NULL),
    enamelability = col_factor(levels = c("?", "1", "2", "3", "4", "5"),
                               ordered=TRUE),
    bf = col_factor(levels = NULL),
    bt = col_factor(levels = NULL),
    bw = col_factor(levels = NULL),
    bl = col_factor(levels = NULL),
    chrom = col_factor(levels = NULL),
    phos = col_factor(levels = NULL),
    cbond = col_factor(levels = NULL),
    ferro = col_factor(levels = NULL),
    blue = col_factor(levels = NULL),
    lustre = col_factor(levels = NULL),
    shape = col_factor(levels = NULL),
    thick = col_double(),
    width = col_double(),
    len = col_integer(),
    oil = col_factor(levels = NULL),
    bore = col_integer(),
    packing = col_factor(levels = NULL),
    label = col_character()
  )) %>%
  mutate(label = (label == target)) %>%
  annotate() %>%
  write_data(dataname)

# DATASET: audiology-std --------------------------------------------------
# Using pre-processed data due to bad formatting on UCI servers
dataname <- "audiology-std"

read_data(dataname, col_names = TRUE, col_type = cols_only(
    label = col_logical(),
    age_gt_60 = col_factor(levels = NULL),
    air = col_factor(
      levels = c("mild", "moderate", "severe", "normal", "profound", "OTHER"),
      ordered = TRUE
    ),
    airBoneGap = col_factor(levels = NULL),
    ar_c = col_factor(levels = c("normal", "elevated", "absent"),
                      ordered = TRUE),
    ar_u = col_factor(levels = c("normal", "absent", "elevated"),
                      ordered = TRUE),
    boneAbnormal = col_factor(levels = NULL),
    history_dizziness = col_factor(levels = NULL),
    history_fluctuating = col_factor(levels = NULL),
    history_nausea = col_factor(levels = NULL),
    history_noise = col_factor(levels = NULL),
    history_recruitment = col_factor(levels = NULL),
    history_ringing = col_factor(levels = NULL),
    history_roaring = col_factor(levels = NULL),
    history_vomiting = col_factor(levels = NULL),
    late_wave_poor = col_factor(levels = NULL),
    m_m_gt_2k = col_factor(levels = NULL),
    m_m_sn = col_factor(levels = NULL),
    m_m_sn_gt_1k = col_factor(levels = NULL),
    m_m_sn_gt_2k = col_factor(levels = NULL),
    m_sn_gt_1k = col_factor(levels = NULL),
    m_sn_gt_2k = col_factor(levels = NULL),
    m_sn_gt_3k = col_factor(levels = NULL),
    m_sn_gt_4k = col_factor(levels = NULL),
    m_sn_lt_1k = col_factor(levels = NULL),
    middle_wave_poor = col_factor(levels = NULL),
    mod_sn_gt_2k = col_factor(levels = NULL),
    mod_sn_gt_3k = col_factor(levels = NULL),
    mod_sn_gt_4k = col_factor(levels = NULL),
    mod_sn_gt_500 = col_factor(levels = NULL),
    notch_4k = col_factor(levels = NULL),
    notch_at_4k = col_factor(levels = NULL),
    o_ar_c = col_factor(levels = c("normal", "elevated", "absent"),
                        ordered = TRUE),
    o_ar_u = col_factor(levels = c("normal", "absent", "elevated"),
                        ordered = TRUE),
    s_sn_gt_1k = col_factor(levels = NULL),
    s_sn_gt_2k = col_factor(levels = NULL),
    s_sn_gt_4k = col_factor(levels = NULL),
    speech = col_factor(levels = NULL),
    static_normal = col_factor(levels = NULL),
    tymp = col_factor(levels = NULL),
    wave_V_delayed = col_factor(levels = NULL),
    waveform_ItoV_prolonged = col_factor(levels = NULL)
  )) %>%
  annotate() %>%
  write_data(dataname)

# DATASET: bank -----------------------------------------------------------
# missing data labeled "unknown" is kept, since it seems possibly informative
dataname <- "bank"
target <- "yes"
col_names <- c("age", "job", "marital", "education", "default", "housing",
               "loan", "contact", "month", "day_of_week", "duration",
               "campaign", "pdays", "previous", "poutcome", "emp.var.rate",
               "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed",
               "label")

read_data(dataname, col_names = col_names, col_type = cols_only(
    age = col_integer(),
    job = col_factor(levels = NULL),
    marital = col_factor(levels = NULL),
    education = col_factor(levels = NULL),
    default = col_factor(levels = NULL),
    housing = col_factor(levels = NULL),
    loan = col_factor(levels = NULL),
    contact = col_factor(levels = NULL),
    month = col_factor(levels = NULL),
    day_of_week = col_factor(levels = NULL),
    campaign = col_integer(),
    pdays = col_integer(),
    previous = col_integer(),
    poutcome = col_factor(levels = NULL),
    emp.var.rate = col_double(),
    cons.price.idx = col_double(),
    cons.conf.idx = col_double(),
    euribor3m = col_double(),
    nr.employed = col_double(),
    label = col_character()
  )) %>%
  mutate(label = (label == target)) %>%
  annotate() %>%
  write_data(dataname)

# DATASET: bankruptcy -----------------------------------------------------
dataname <- "bankruptcy"
target <- "B"
col_names <- c("industrial_risk", "management_risk", "financial_flex",
               "credibility", "competitiveness", "operating_risk", "label")

read_data(dataname, col_names = col_names, col_type = cols_only(
    industrial_risk = col_factor(levels = NULL),
    management_risk = col_factor(levels = NULL),
    financial_flex = col_factor(levels = NULL),
    credibility = col_factor(levels = NULL),
    competitiveness = col_factor(levels = NULL),
    operating_risk = col_factor(levels = NULL),
    label = col_character()
  )) %>%
  mutate(label = (label == target)) %>%
  annotate() %>%
  write_data(dataname)

# DATASET: car ------------------------------------------------------------
dataname <- "car"
target <- "unacc"
col_names <- c("buying", "maint", "doors", "persons", "lug_boot", "safety",
               "label")

read_data(dataname, col_names = col_names, col_type = cols_only(
    buying = col_factor(levels = NULL),
    maint = col_factor(levels = NULL),
    doors = col_factor(levels = NULL),
    persons = col_factor(levels = NULL),
    lug_boot = col_factor(levels = NULL),
    safety = col_factor(levels = NULL),
    label = col_character()
  )) %>%
  mutate(label = (label == target)) %>%
  annotate() %>%
  write_data(dataname)

# DATASET: chess-krvk -----------------------------------------------------
dataname <- "chess-krvk"
target <- "draw"
col_names <- c("White_King_file", "White_King_rank", "White_Rook_file",
               "White_Rook_rank", "Black_King_file", "Black_King_rank", "label")

read_data(dataname, col_names = col_names, col_type = cols_only(
    White_King_file = col_factor(levels = NULL),
    White_King_rank = col_factor(levels = NULL),
    White_Rook_file = col_factor(levels = NULL),
    White_Rook_rank = col_factor(levels = NULL),
    Black_King_file = col_factor(levels = NULL),
    Black_King_rank = col_factor(levels = NULL),
    label = col_character()
  )) %>%
  mutate(label = (label == target)) %>%
  annotate() %>%
  write_data(dataname)

# DATASET: chess-krvkp ----------------------------------------------------
dataname <- "chess-krvkp"
target <- "won"
# set the feature names manually (see data_desc.txt file for details)
col_names <- c( "bkblk", "bknwy", "bkon8", "bkona", "bkspr", "bkxbq", "bkxcr",
               "bkxwp", "blxwp", "bxqsq", "cntxt", "dsopp", "dwipd", "hdchk",
               "katri", "mulch", "qxmsq", "r2ar8", "reskd", "reskr", "rimmx",
               "rkxwp", "rxmsq", "simpl", "skach", "skewr", "skrxp", "spcop",
               "stlmt", "thrsk", "wkcti", "wkna8", "wknck", "wkovl", "wkpos",
               "wtoeg", "label")

cls_pos <- c("won")

read_data(dataname, col_names = col_names, col_type = cols_only(
    bkblk = col_factor(levels = NULL),
    bknwy = col_factor(levels = NULL),
    bkon8 = col_factor(levels = NULL),
    bkona = col_factor(levels = NULL),
    bkspr = col_factor(levels = NULL),
    bkxbq = col_factor(levels = NULL),
    bkxcr = col_factor(levels = NULL),
    bkxwp = col_factor(levels = NULL),
    blxwp = col_factor(levels = NULL),
    bxqsq = col_factor(levels = NULL),
    cntxt = col_factor(levels = NULL),
    dsopp = col_factor(levels = NULL),
    dwipd = col_factor(levels = NULL),
    hdchk = col_factor(levels = NULL),
    katri = col_factor(levels = NULL),
    mulch = col_factor(levels = NULL),
    qxmsq = col_factor(levels = NULL),
    r2ar8 = col_factor(levels = NULL),
    reskd = col_factor(levels = NULL),
    reskr = col_factor(levels = NULL),
    rimmx = col_factor(levels = NULL),
    rkxwp = col_factor(levels = NULL),
    rxmsq = col_factor(levels = NULL),
    simpl = col_factor(levels = NULL),
    skach = col_factor(levels = NULL),
    skewr = col_factor(levels = NULL),
    skrxp = col_factor(levels = NULL),
    stlmt = col_factor(levels = NULL),
    thrsk = col_factor(levels = NULL),
    wkcti = col_factor(levels = NULL),
    wkna8 = col_factor(levels = NULL),
    wknck = col_factor(levels = NULL),
    wkovl = col_factor(levels = NULL),
    wkpos = col_factor(levels = NULL),
    wtoeg = col_factor(levels = NULL),
    label = col_character()
  )) %>%
  mutate(label = (label == target)) %>%
  annotate() %>%
  write_data(dataname)

# DATASET: congress-voting ------------------------------------------------
dataname <- "congress-voting"
target <- "democrat"
col_names <- c("label", "handicapped_infants", "water_project_cost_sharing",
               "adoption_of_the_budget_resolution", "physician_fee_freeze",
               "el_salvador_aid", "religious_groups_in_schools",
               "anti_satellite_test_ban", "aid_to_nicaraguan_contras",
               "mx_missile", "immigration", "synfuels_corporation_cutback",
               "education_spending", "superfund_right_to_sue", "crime",
               "duty_free_exports", "export_administration_act_south_africa")

read_data(dataname, na = "?", col_names = col_names, col_type = cols_only(
  label = col_character(),
  handicapped_infants = col_factor(levels = NULL),
  water_project_cost_sharing = col_factor(levels = NULL),
  adoption_of_the_budget_resolution = col_factor(levels = NULL),
  physician_fee_freeze = col_factor(levels = NULL),
  el_salvador_aid = col_factor(levels = NULL),
  religious_groups_in_schools = col_factor(levels = NULL),
  anti_satellite_test_ban = col_factor(levels = NULL),
  aid_to_nicaraguan_contras = col_factor(levels = NULL),
  mx_missile = col_factor(levels = NULL),
  immigration = col_factor(levels = NULL),
  synfuels_corporation_cutback = col_factor(levels = NULL),
  education_spending = col_factor(levels = NULL),
  superfund_right_to_sue = col_factor(levels = NULL),
  crime = col_factor(levels = NULL),
  duty_free_exports = col_factor(levels = NULL),
  export_administration_act_south_africa = col_factor(levels = NULL)
  )) %>%
  mutate(label = (label == target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)

# DATASET: contrac --------------------------------------------------------
dataname <- "contrac"
target <- 1
col_names <- c("age", "wifes_education", "husbands_education", "nchildren",
               "religion", "working", "husbands_occupation", "living_std",
               "media_exposure", "label")

read_data(dataname, col_names = col_names, col_type = cols_only(
    age = col_integer(),
    wifes_education = col_factor(levels = 1:4, ordered = TRUE),
    husbands_education = col_factor(levels = 1:4, ordered = TRUE),
    nchildren = col_integer(),
    religion = col_factor(levels = NULL),
    working = col_factor(levels = NULL),
    husbands_occupation = col_factor(levels = NULL),
    living_std = col_factor(levels = 1:4, ordered = TRUE),
    media_exposure = col_factor(levels = NULL),
    label = col_integer()
  )) %>%
  mutate(label = (label == target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)

# DATASET: credit-approval ------------------------------------------------
dataname <- "credit-approval"
target <- "+"
col_names <- c(paste0("A", seq(1,15)), "label")

read_data(dataname, na = "?", col_names = col_names, col_type = cols_only(
  A1 = col_factor(levels = NULL),
  A2 = col_double(),
  A3 = col_double(),
  A4 = col_factor(levels = NULL),
  A5 = col_factor(levels = NULL),
  A6 = col_factor(levels = NULL),
  A7 = col_factor(levels = NULL),
  A8 = col_double(),
  A9 = col_factor(levels = NULL),
  A10 = col_factor(levels = NULL),
  A11 = col_integer(),
  A12 = col_factor(levels = NULL),
  A13 = col_factor(levels = NULL),
  A14 = col_integer(),
  A15 = col_integer(),
  label = col_character()
  )) %>%
  mutate(label = (label == target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)

# DATASET: ctg ------------------------------------------------------------
dataname <- "ctg"
target <- 1
col_names <- c("FileName", "Date", "b", "e", "LBE", "LB", "AC", "FM", "UC",
               "ASTV", "mSTV", "ALTV", "mLTV", "DL", "DS", "DP", "DR", "Width",
               "Min", "Max", "Nmax", "Nzeros", "Mode", "Mean", "Median",
               "Variance", "Tendency", "A", "B", "C", "D", "SH", "AD", "DE",
               "LD", "FS", "SUSP", "CLASS", "label")

read_data(dataname, na = "?", col_names = col_names, col_type = cols_only(
    LBE = col_integer(),
    LB = col_integer(),
    AC = col_integer(),
    FM = col_integer(),
    UC = col_integer(),
    ASTV = col_integer(),
    mSTV = col_double(),
    ALTV = col_integer(),
    mLTV = col_double(),
    DL = col_integer(),
    DS = col_integer(),
    DP = col_integer(),
    DR = col_integer(),
    Width = col_integer(),
    Min = col_integer(),
    Max = col_integer(),
    Nmax = col_integer(),
    Nzeros = col_integer(),
    Mode = col_integer(),
    Mean = col_integer(),
    Median = col_integer(),
    Variance = col_integer(),
    Tendency = col_integer(),
    A = col_integer(),
    B = col_integer(),
    C = col_integer(),
    D = col_integer(),
    SH = col_integer(),
    AD = col_integer(),
    DE = col_integer(),
    LD = col_integer(),
    FS = col_integer(),
    SUSP = col_integer(),
    label = col_integer()
  )) %>%
  mutate(label = (label == target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)

# DATASET: cylinder-bands -------------------------------------------------
dataname <- "cylinder-bands"
target <- "noband"
col_names <- c("timestamp", "cylinder_number", "customer", "job_number",
               "grain_screened", "ink_color", "proof_on_ctd_ink", "blade_mfg",
               "cylinder_division", "paper_type", "ink_type", "direct_steam",
               "solvent_type", "type_on_cylinder", "press_type", "press",
               "unit_number", "cylinder_size", "paper_mill_location",
               "plating_tank", "proof_cut", "viscosity", "caliper",
               "ink_temperature", "humifity", "roughness", "blade_pressure",
               "varnish_pct", "press_speed", "ink_pct", "solvent_pct",
               "ESA_Voltage", "ESA_Amperage", "wax", "hardener",
               "roller_durometer", "current_density", "anode_space_ratio",
               "chrome_content", "label")

read_data(dataname, na = "?", col_names = col_names, col_type = cols_only(
  grain_screened = col_factor(levels = NULL),
  proof_on_ctd_ink = col_factor(levels = NULL),
  paper_type = col_factor(levels = NULL),
  ink_type = col_factor(levels = NULL),
  solvent_type = col_factor(levels = NULL),
  type_on_cylinder = col_factor(levels = NULL),
  press_type = col_factor(levels = NULL),
  press = col_factor(levels = NULL),
  unit_number = col_factor(levels = NULL),
  cylinder_size = col_factor(levels = NULL),
  paper_mill_location = col_factor(levels = NULL),
  plating_tank = col_factor(levels = NULL),
  proof_cut = col_double(),
  viscosity = col_integer(),
  caliper = col_double(),
  ink_temperature = col_double(),
  humifity = col_integer(),
  roughness = col_double(),
  blade_pressure = col_integer(),
  varnish_pct = col_double(),
  press_speed = col_integer(),
  ink_pct = col_double(),
  solvent_pct = col_double(),
  ESA_Voltage = col_double(),
  ESA_Amperage = col_double(),
  wax = col_double(),
  hardener = col_double(),
  roller_durometer = col_double(),
  current_density = col_integer(),
  anode_space_ratio = col_double(),
  chrome_content = col_integer(),
  label = col_character()
  )) %>%
  mutate(label = (label == target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)

# DATASET: dermatology ----------------------------------------------------
dataname <- "dermatology"
target <- 1
col_names <- c("erythema", "scaling", "definite_borders", "itching",
               "koebner_phenomenon", "polygonal_papules", "follicular_papules",
               "oral_mucosal_involvement", "knee_and_elbow_involvement",
               "scalp_involvement", "family_history", "melanin_incontinence",
               "eosinophils_in_the_infiltrate", "PNL_infiltrate",
               "fibrosis_of_the_papillary_dermis", "exocytosis", "acanthosis",
               "hyperkeratosis", "parakeratosis", "clubbing_of_the_rete_ridges",
               "elongation_of_the_rete_ridges",
               "thinning_of_the_suprapapillary_epidermis", "spongiform_pustule",
               "munro_microabcess", "focal_hypergranulosis",
               "disappearance_of_the_granular_layer",
               "vacuolisation_and_damage_of_basal_layer", "spongiosis",
               "saw_tooth_appearance_of_retes", "follicular_horn_plug",
               "perifollicular_parakeratosis",
               "inflammatory_monoluclear_inflitrate", "band_like_infiltrate",
               "age", "label")

read_data(dataname, na = "?", col_names = col_names, col_type = cols_only(
    erythema = col_integer(),
    scaling = col_integer(),
    definite_borders = col_integer(),
    itching = col_integer(),
    koebner_phenomenon = col_integer(),
    polygonal_papules = col_integer(),
    follicular_papules = col_integer(),
    oral_mucosal_involvement = col_integer(),
    knee_and_elbow_involvement = col_integer(),
    scalp_involvement = col_integer(),
    family_history = col_integer(),
    melanin_incontinence = col_integer(),
    eosinophils_in_the_infiltrate = col_integer(),
    PNL_infiltrate = col_integer(),
    fibrosis_of_the_papillary_dermis = col_integer(),
    exocytosis = col_integer(),
    acanthosis = col_integer(),
    hyperkeratosis = col_integer(),
    parakeratosis = col_integer(),
    clubbing_of_the_rete_ridges = col_integer(),
    elongation_of_the_rete_ridges = col_integer(),
    thinning_of_the_suprapapillary_epidermis = col_integer(),
    spongiform_pustule = col_integer(),
    munro_microabcess = col_integer(),
    focal_hypergranulosis = col_integer(),
    disappearance_of_the_granular_layer = col_integer(),
    vacuolisation_and_damage_of_basal_layer = col_integer(),
    spongiosis = col_integer(),
    saw_tooth_appearance_of_retes = col_integer(),
    follicular_horn_plug = col_integer(),
    perifollicular_parakeratosis = col_integer(),
    inflammatory_monoluclear_inflitrate = col_integer(),
    band_like_infiltrate = col_integer(),
    age = col_integer(),
    label = col_integer()
  )) %>%
  mutate(label = (label == target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)

# DATASET: german_credit --------------------------------------------------
dataname <- "german_credit"
target <- 1
col_names <- c("Status_of_checking_account", "Duration_in_months",
               "Credit_history", "Purpose", "Credit_amount", "Savings_account",
               "Employment_since", "Installment_rate", "status_and_sex",
               "Other_debtors", "residence_since", "Property", "Age",
               "Other_installment", "Housing", "existing_credits", "Job",
               "Number_of_dependents", "Have_Telephone", "foreign_worker",
               "label")

read_data(dataname, col_names = col_names, col_type = cols_only(
    Status_of_checking_account = col_factor(levels = NULL),
    Duration_in_months = col_integer(),
    Credit_history = col_factor(levels = NULL),
    Purpose = col_factor(levels = NULL),
    Credit_amount = col_integer(),
    Savings_account = col_factor(levels = NULL),
    Employment_since = col_factor(levels = NULL),
    Installment_rate = col_integer(),
    status_and_sex = col_factor(levels = NULL),
    Other_debtors = col_factor(levels = NULL),
    residence_since = col_integer(),
    Property = col_factor(levels = NULL),
    Age = col_integer(),
    Other_installment = col_factor(levels = NULL),
    Housing = col_factor(levels = NULL),
    existing_credits = col_integer(),
    Job = col_factor(levels = NULL),
    Number_of_dependents = col_integer(),
    Have_Telephone = col_factor(levels = NULL),
    foreign_worker = col_factor(levels = NULL),
    label = col_integer()
  )) %>%
  mutate(label = (label == target)) %>%
  annotate() %>%
  write_data(dataname)

# DATASET: heart-cleveland ------------------------------------------------
dataname <- "heart-cleveland"
target <- seq(1,5)
col_names <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
               "thalach", "exang", "oldpeak", "slope", "ca", "thal", "label")

read_data(dataname, na = "-9", col_names = col_names, col_type = cols_only(
    age = col_double(),
    sex = col_factor(levels = NULL),
    cp = col_factor(levels = NULL),
    trestbps = col_double(),
    chol = col_double(),
    fbs = col_factor(levels = NULL),
    restecg = col_factor(levels = NULL),
    thalach = col_double(),
    exang = col_factor(levels = NULL),
    oldpeak = col_double(),
    slope = col_factor(levels = NULL),
    ca = col_double(),
    thal = col_factor(levels = NULL),
    label = col_integer()
  )) %>%
  mutate(label = (label %in% target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)

# DATASET: ilpd -----------------------------------------------------------
dataname <- "ilpd"
col_names <- c("age", "gender", "tb", "db", "alkphos", "sgpt", "sgot", "tp",
               "alb", "ag", "label")
target <- 1

read_data(dataname, col_names = col_names, col_type = cols_only(
    age = col_integer(),
    gender = col_factor(levels = NULL),
    tb = col_double(),
    db = col_double(),
    alkphos = col_integer(),
    sgpt = col_integer(),
    sgot = col_integer(),
    tp = col_double(),
    alb = col_double(),
    ag = col_double(),
    label = col_integer()
  )) %>%
  mutate(label = (label == target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)

# DATASET: mammo ----------------------------------------------------------
dataname <- "mammo"
col_names <- c("BIRADS", "age", "shape", "margin", "density", "label")
target <- 1

read_data(dataname, na = "?", col_names = col_names, col_type = cols_only(
    BIRADS = col_factor(levels = NULL, ordered = TRUE),
    age = col_number(),
    shape = col_factor(levels = NULL),
    margin = col_factor(levels = NULL),
    density = col_factor(levels = 1:4, ordered = TRUE),
    label = col_integer()
  )) %>%
  mutate(label = (label == target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)

# DATASET: mushroom -------------------------------------------------------
dataname <- "mushroom"
target <- "p"
col_names <- c("label", "cap_shape", "cap_surface", "cap_color", "bruises",
               "odor", "gill_attachment", "gill_spacing", "gill_size",
               "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above",
               "stalk_surface_below", "stalk_color_above", "stalk_color_below",
               "veil_type", "veil_color", "ring_number", "ring_type",
               "spore_print_color", "population", "habitat")

read_data(dataname, na = "?", col_names = col_names, col_type = cols_only(
    label = col_character(),
    cap_shape = col_factor(levels = NULL),
    cap_surface = col_factor(levels = NULL),
    cap_color = col_factor(levels = NULL),
    bruises = col_factor(levels = NULL),
    odor = col_factor(levels = NULL),
    gill_attachment = col_factor(levels = NULL),
    gill_spacing = col_factor(levels = NULL),
    gill_size = col_factor(levels = NULL),
    gill_color = col_factor(levels = NULL),
    stalk_shape = col_factor(levels = NULL),
    stalk_root = col_factor(levels = NULL),
    stalk_surface_above = col_factor(levels = NULL),
    stalk_surface_below = col_factor(levels = NULL),
    stalk_color_above = col_factor(levels = NULL),
    stalk_color_below = col_factor(levels = NULL),
    ring_number = col_factor(levels = NULL),
    ring_type = col_factor(levels = NULL),
    spore_print_color = col_factor(levels = NULL),
    population = col_factor(levels = NULL),
    habitat = col_factor(levels = NULL)
  )) %>%
  mutate(label = (label == target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)

# DATASET: wine -----------------------------------------------------------
dataname <- "wine"
target <- 2
col_names <- c("label", "Alcohol", "Malic_acid", "Ash", "Alcalinity_ash",
               "Magnesium", "Total_phenols", "Flavanoids",
               "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity",
               "Hue", "OD280_OD315", "Proline")

read_data(dataname, col_names = col_names, col_type = cols_only(
    label = col_integer(),
    Alcohol = col_double(),
    Malic_acid = col_double(),
    Ash = col_double(),
    Alcalinity_ash = col_double(),
    Magnesium = col_integer(),
    Total_phenols = col_double(),
    Flavanoids = col_double(),
    Nonflavanoid_phenols = col_double(),
    Proanthocyanins = col_double(),
    Color_intensity = col_double(),
    Hue = col_double(),
    OD280_OD315 = col_double(),
    Proline = col_integer()
  )) %>%
  mutate(label = (label == target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)

# DATASET: wine_qual ------------------------------------------------------
dataname <- "wine_qual"
target <- seq(6, 9)
col_names <- c("fixed.acidity", "volatile.acidity", "citric.acid",
               "residual.sugar", "chlorides", "free.sulfur.dioxide",
               "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol",
               "label", "color")

read_data(dataname, col_names = col_names, col_type = cols_only(
    fixed.acidity = col_double(),
    volatile.acidity = col_double(),
    citric.acid = col_double(),
    residual.sugar = col_double(),
    chlorides = col_double(),
    free.sulfur.dioxide = col_double(),
    total.sulfur.dioxide = col_integer(),
    density = col_double(),
    pH = col_double(),
    sulphates = col_double(),
    alcohol = col_double(),
    label = col_integer(),
    color = col_factor(levels = NULL)
  )) %>%
  mutate(label = (label %in% target)) %>%
  na.omit() %>%
  annotate() %>%
  write_data(dataname)
