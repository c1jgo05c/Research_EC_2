# ============================================================
# FULL PIPELINE (UPDATED) — UTF-8 safe outputs + robust renaming
# + Calibration via survey::calibrate()
# + NORMALIZED weights for brms (critical)
# + brms weights inside formula (older brms compatible)
# + Windows-stable sampling (cores=1, refresh)
#
# Files (all in \Research_EC):
#   EC_registered_voters_english.xlsx   (sheet: EC_Count_English)
#   PHC_national_age_sex_18plus.xlsx    (sheet: national_age_sex_18plus)
#   response_data_1.xlsx                (sheet 1)
#   survey_coding_sheet_english.xlsx    (sheets: Variable_Mapping, Category_Codes, MultiSelect_Options, District_Raw_List)
#
# Outputs: F:\Research_EC\outputs\  (XLSX/PNG/RDS/TXT UTF-8)
# ============================================================

# =========================
# CHUNK 0: UTF-8 locale (Windows)
# =========================
try(Sys.setlocale("LC_ALL", "English_United States.utf8"), silent = TRUE)
try(Sys.setlocale("LC_ALL", "en_US.UTF-8"), silent = TRUE)

# =========================
# CHUNK 1: Setup + packages + paths
# =========================
base_dir <- "D:/Research_EC_2"
out_dir  <- file.path(base_dir, "outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

f_ec   <- file.path(base_dir, "EC_registered_voters_english.xlsx")
f_phc  <- file.path(base_dir, "PHC_national_age_sex_18plus.xlsx")
f_svy  <- file.path(base_dir, "response_data_1.xlsx")
f_code <- file.path(base_dir, "survey_coding_sheet_english.xlsx")

pkgs <- c(
  "readxl","writexl","dplyr","stringr","tidyr","forcats","janitor",
  "ggplot2","scales",
  "survey",
  "brms","tidybayes","posterior"
)
to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(to_install)) install.packages(to_install)

library(readxl); library(writexl)
library(dplyr); library(stringr); library(tidyr); library(forcats); library(janitor)
library(ggplot2); library(scales)
library(survey)
library(brms); library(tidybayes); library(posterior)

options(mc.cores = parallel::detectCores())

# =========================
# CHUNK 2: Helpers
# =========================
clean_text <- function(x){
  x <- as.character(x)
  x <- str_squish(x)
  x <- str_replace_all(x, "☐\\s*", "")
  x
}

save_plot <- function(p, filename, w=8, h=5){
  ggsave(file.path(out_dir, filename), p, width = w, height = h, dpi = 150)
}

write_utf8_txt <- function(lines, path){
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con, useBytes = TRUE)
}

safe_write_xlsx <- function(x, path){
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  # If file exists, attempt delete (fails if locked by Excel)
  if (file.exists(path)) {
    try(unlink(path, force = TRUE), silent = TRUE)
  }
  
  # Write to a unique temp name first
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  tmp_path <- sub("\\.xlsx$", paste0("_", ts, ".xlsx"), path)
  
  writexl::write_xlsx(x, tmp_path)
  
  # Try to copy to final path (overwrite). If locked, keep temp.
  ok <- FALSE
  try({
    file.copy(tmp_path, path, overwrite = TRUE)
    ok <- TRUE
  }, silent = TRUE)
  
  if (ok) {
    try(unlink(tmp_path, force = TRUE), silent = TRUE)
    return(invisible(path))
  } else {
    message("Target file is likely locked. Saved instead to: ", tmp_path)
    return(invisible(tmp_path))
  }
}

# Add to CHUNK 2, after save_plot()/write_utf8_txt()/safe_write_xlsx()
assert_calibrated <- function(df, targets_vec, tol = 1e-3) {
  stopifnot("sex_age" %in% names(df), "w_cal" %in% names(df))
  des <- svydesign(ids = ~1, data = df, weights = ~w_cal)
  tab <- svytable(~sex_age, des)
  d   <- as.numeric(tab[names(targets_vec)]) - as.numeric(targets_vec)
  if (max(abs(d)) > tol) {
    stop("CALIBRATION GUARD FAILED: weighted cell totals do not match PHC targets ",
         "(max |diff| = ", round(max(abs(d)), 2), "). The `survey` object reaching ",
         "the model is NOT carrying corrected weights. Re-run the corrected CHUNK 8 first.")
  }
  invisible(TRUE)
}

# =========================
# CHUNK 3: Load coding sheet tabs (XLSX safe)
# =========================
map <- read_excel(f_code, sheet = "Variable_Mapping") %>%
  mutate(
    short_var = clean_text(short_var),
    original_header_bn = clean_text(original_header_bn)
  )

codes <- read_excel(f_code, sheet = "Category_Codes") %>%
  mutate(short_var = clean_text(short_var),
         label_bn = clean_text(label_bn))

msel <- read_excel(f_code, sheet = "MultiSelect_Options") %>%
  mutate(multi_block = clean_text(multi_block),
         option_bn = clean_text(option_bn),
         recommended_dummy_var = clean_text(recommended_dummy_var))

dist_map <- read_excel(f_code, sheet = "District_Raw_List") %>%
  mutate(district_raw = clean_text(district_raw),
         district_std = clean_text(district_std))

stopifnot(all(c("short_var","original_header_bn") %in% names(map)))
stopifnot(all(c("district_raw","district_std") %in% names(dist_map)))

write_xlsx(list(
  Variable_Mapping = map,
  Category_Codes = codes,
  MultiSelect_Options = msel,
  District_Raw_List = dist_map
), file.path(out_dir, "00_codebook_snapshot.xlsx"))

# =========================
# CHUNK 4: Load survey + ROBUST rename of core vars (no exact-match dependency)
# =========================
survey_raw <- read_excel(f_svy, sheet = 1)

orig_cols <- names(survey_raw)
clean_cols <- clean_text(orig_cols)
names(survey_raw) <- clean_cols

write_xlsx(list(headers = data.frame(colname = names(survey_raw))),
           file.path(out_dir, "DEBUG_survey_columns.xlsx"))

pick_col <- function(cols, patterns, label){
  hits <- cols[Reduce(`|`, lapply(patterns, function(p) str_detect(cols, p)))]
  if (length(hits) == 0) {
    stop("Could not find column for: ", label,
         "\nTried patterns: ", paste(patterns, collapse=" | "),
         "\nCheck outputs/DEBUG_survey_columns.xlsx for exact headers.")
  }
  if (length(hits) > 1) {
    message("Multiple matches for ", label, ":\n", paste(hits, collapse="\n"),
            "\nUsing first: ", hits[1])
  }
  hits[1]
}

col_age <- pick_col(names(survey_raw),
                    c("আপনার বয়স", "আপনার বয়স", "বয়স কোন গ্রুপ", "বয়স কোন গ্রুপ", "বয়স", "বয়স"),
                    "age group")
col_sex <- pick_col(names(survey_raw),
                    c("আপনার লিঙ্গ", "লিঙ্গ"),
                    "sex")
col_edu <- pick_col(names(survey_raw),
                    c("শিক্ষাগত যোগ্যতা", "শিক্ষা"),
                    "education")
col_dis <- pick_col(names(survey_raw),
                    c("জেলার নাম", "আপনার জেলার নাম", "জেলা"),
                    "district name")
col_tur <- pick_col(names(survey_raw),
                    c("ভোট দেওয়ার সম্ভাবনা", "ভোট দেওয়ার সম্ভাবনা", "ভোট.*সম্ভাবনা", "নির্বাচন\\+গণভোটে"),
                    "turnout intention")

survey <- survey_raw %>%
  rename(
    age_grp = all_of(col_age),
    sex = all_of(col_sex),
    edu = all_of(col_edu),
    district_raw = all_of(col_dis),
    turnout_intent = all_of(col_tur)
  ) %>%
  mutate(across(where(is.character), clean_text))

req <- c("age_grp","sex","edu","district_raw","turnout_intent")
stopifnot(all(req %in% names(survey)))

write_xlsx(list(survey_core = survey),
           file.path(out_dir, "01_survey_renamed_corevars.xlsx"))

# =========================
# CHUNK 5: District mapping + standardize labels
# =========================
survey <- survey %>%
  mutate(
    district_raw = clean_text(district_raw),
    sex = case_when(
      sex %in% c("Male","male","পুরুষ") ~ "পুরুষ",
      sex %in% c("Female","female","নারী") ~ "নারী",
      TRUE ~ clean_text(sex)
    ),
    age_grp = clean_text(age_grp),
    edu = clean_text(edu),
    turnout_intent = clean_text(turnout_intent)
  ) %>%
  left_join(dist_map, by = "district_raw")

unmatched_d <- survey %>% filter(is.na(district_std)) %>% distinct(district_raw)
write_xlsx(list(unmatched_districts = unmatched_d),
           file.path(out_dir, "02_unmatched_districts_in_survey.xlsx"))

survey <- survey %>% filter(!is.na(district_std))

survey <- survey %>%
  mutate(
    age_grp = str_replace_all(age_grp, "\\s+", ""),
    age_grp = recode(age_grp,
                     "১৮–২৪"="১৮–২৪","18–24"="১৮–২৪","18-24"="১৮–২৪",
                     "২৫–৩৪"="২৫–৩৪","25–34"="২৫–৩৪","25-34"="২৫–৩৪",
                     "৩৫–৪৪"="৩৫–৪৪","35–44"="৩৫–৪৪","35-44"="৩৫–৪৪",
                     "৪৫–৫৯"="৪৫–৫৯","45–59"="৪৫–৫৯","45-59"="৪৫–৫৯",
                     "৬০বাতদূর্ধ্ব"="৬০+","60+"="৬০+","৬০+"="৬০+"
    ),
    age_grp = factor(age_grp, levels = c("১৮–২৪","২৫–৩৪","৩৫–৪৪","৪৫–৫৯","৬০+")),
    sex = factor(sex, levels = c("পুরুষ","নারী")),
    district_std = factor(district_std),
    edu = factor(edu)
  ) %>%
  filter(!is.na(age_grp), !is.na(sex))

write_xlsx(list(survey_ready = survey),
           file.path(out_dir, "03_survey_ready_corevars.xlsx"))

# =========================
# CHUNK 6: Outcomes
# =========================
survey <- survey %>%
  mutate(
    y_strict = ifelse(turnout_intent == "অবশ্যই ভোট দেব", 1L, 0L),
    y_likely = ifelse(turnout_intent %in% c("অবশ্যই ভোট দেব","সম্ভবত ভোট দেব"), 1L, 0L)
  ) %>%
  filter(!is.na(y_strict))

write_xlsx(list(survey_outcomes = survey),
           file.path(out_dir, "04_survey_with_outcomes.xlsx"))

# =========================
# CHUNK 7: Load PHC targets + align
# =========================
phc <- read_excel(f_phc, sheet = "national_age_sex_18plus") %>% janitor::clean_names()
stopifnot(all(c("age_group","sex","share_adult_mf") %in% names(phc)))

phc_targets <- phc %>%
  transmute(
    sex = case_when(
      tolower(sex) %in% c("male","m") ~ "পুরুষ",
      tolower(sex) %in% c("female","f") ~ "নারী",
      TRUE ~ clean_text(sex)
    ),
    age_grp = recode(clean_text(age_group),
                     "18–24"="১৮–২৪","25–34"="২৫–৩৪","35–44"="৩৫–৪৪","45–59"="৪৫–৫৯","60+"="৬০+",
                     "18-24"="১৮–২৪","25-34"="২৫–৩৪","35-44"="৩৫–৪৪","45-59"="৪৫–৫৯"
    ),
    N_target = as.numeric(share_adult_mf) * 1e6
  ) %>%
  filter(sex %in% c("পুরুষ","নারী")) %>%
  filter(age_grp %in% levels(survey$age_grp)) %>%
  mutate(
    sex = factor(sex, levels = levels(survey$sex)),
    age_grp = factor(age_grp, levels = levels(survey$age_grp))
  )

write_xlsx(list(phc_targets = phc_targets),
           file.path(out_dir, "05_phc_targets_age_sex.xlsx"))

# ============================================================
# CHUNK 8 (CORRECTED) - Post-stratification weights to PHC age x sex (18-59)
#
# WHY THIS REPLACES THE OLD CHUNK 8
# ---------------------------------
# The old block passed `population = phc_targets_vec` to survey::calibrate().
# The names of that vector were built in PHC row order (sex interleaved within
# age) and did NOT match the column names of the calibration model matrix
# (~ sex_age - 1), which are ordered by the Unicode-sorted factor levels.
# With names unmatched, survey::calibrate() fell back to POSITIONAL matching,
# and the two orderings differ. The result: each weighted cell total was set
# equal to the WRONG cell's target. The eight weighted totals came out as an
# exact permutation of the eight targets (two cells correct, six wrong;
# per-cell weighted/target ratios 0.80, 0.86, 0.98, 1.00, 1.00, 1.07, 1.17, 1.19;
# max |weighted - target| ~= 24,831 on cells of ~100,000).
# The code computed a `diff` column but never asserted it was ~0, so the
# misalignment was silent and propagated into w_cal -> w_cal_norm -> the brms
# likelihood weights -> every model-derived estimate.
#
# Calibration on a fully-interacted (saturated) age x sex margin is
# mathematically identical to direct post-stratification: the unique exact
# solution sets each cell's weighted total equal to its target. VARIANT A below
# computes that solution directly and is the recommended fix. VARIANT B keeps
# survey::calibrate() in the methods narrative but repairs the name/order match.
# Use ONE of them. Both add a stopifnot() guard that converts a silent failure
# into a hard stop.
# ============================================================

# Build the joint cell key (unchanged from the original pipeline)
survey <- survey %>%
  mutate(sex_age = factor(paste(sex, age_grp, sep = ":")))

# PHC target totals for the cells present in the data, named by sex_age
phc_targets_vec <- setNames(
  phc_targets$N_target,
  paste(phc_targets$sex, phc_targets$age_grp, sep = ":")
)
lvl <- levels(droplevels(survey$sex_age))
phc_targets_vec <- phc_targets_vec[names(phc_targets_vec) %in% lvl]

# Keep only rows whose cell has a target (drops 60+ if still present)
survey <- survey %>% filter(sex_age %in% names(phc_targets_vec))
survey$sex_age <- droplevels(survey$sex_age)


# ------------------------------------------------------------
# VARIANT A (RECOMMENDED): direct post-stratification weights
#   w_i = N_target(cell_i) / n(cell_i)
# Exact for a saturated margin, fully transparent, no solver.
# ------------------------------------------------------------
cell_n   <- survey %>% count(sex_age, name = "n_cell")
cell_tgt <- tibble(sex_age  = names(phc_targets_vec),
                   N_target = as.numeric(phc_targets_vec))

survey <- survey %>%
  left_join(cell_n,   by = "sex_age") %>%
  left_join(cell_tgt, by = "sex_age") %>%
  mutate(w_cal = N_target / n_cell)

# LOAD-BEARING GUARD: weighted cell totals must equal targets.
chk <- survey %>%
  group_by(sex_age) %>%
  summarise(wt = sum(w_cal), .groups = "drop") %>%
  left_join(cell_tgt, by = "sex_age")
stopifnot(all(abs(chk$wt - chk$N_target) < 1e-6))


# ------------------------------------------------------------
# VARIANT B (minimal edit, keeps survey::calibrate in the narrative)
# Uncomment to use instead of Variant A. Requires des0 as in the original.
# ------------------------------------------------------------
# des0 <- svydesign(ids = ~1, data = survey, weights = ~1)
# pop  <- phc_targets_vec[levels(survey$sex_age)]   # order to factor levels
# names(pop) <- paste0("sex_age", names(pop))       # match ~sex_age-1 columns
# cal  <- calibrate(des0, ~ sex_age - 1, population = pop, calfun = "raking")
# survey$w_cal <- as.numeric(weights(cal))
# # verify BEFORE any trimming:
# stopifnot(
#   max(abs(
#     svytable(~sex_age, svydesign(~1, survey, weights = ~w_cal))[levels(survey$sex_age)] - pop
#   )) < 1e-3
# )


# ------------------------------------------------------------
# Trimming + normalization (unchanged logic, applied AFTER the guard passes)
# ------------------------------------------------------------
cap_raw <- as.numeric(quantile(survey$w_cal, 0.995, na.rm = TRUE))
survey$w_cal <- pmin(survey$w_cal, cap_raw)

# Normalize for brms/Stan: mean = 1 (prevents the "stuck sampling" pathology)
survey$w_cal_norm <- survey$w_cal / mean(survey$w_cal, na.rm = TRUE)
cap_norm <- as.numeric(quantile(survey$w_cal_norm, 0.995, na.rm = TRUE))
survey$w_cal_norm <- pmin(survey$w_cal_norm, cap_norm)

# Diagnostics + calibration check (now expected to show diff ~ 0 in every cell)
desW_raw  <- svydesign(ids = ~1, data = survey, weights = ~w_cal)
tab       <- svytable(~sex_age, desW_raw)
cal_check <- tibble(
  sex_age        = names(tab),
  weighted_total = as.numeric(tab),
  target_total   = as.numeric(phc_targets_vec[names(tab)]),
  diff           = weighted_total - target_total
)
stopifnot(max(abs(cal_check$diff)) < 1e-3)   # second guard, post-trim sanity

w_diag <- tibble(
  n = nrow(survey),
  min_raw = min(survey$w_cal),   mean_raw = mean(survey$w_cal),   max_raw = max(survey$w_cal),
  min_norm = min(survey$w_cal_norm), mean_norm = mean(survey$w_cal_norm), max_norm = max(survey$w_cal_norm)
)

write_xlsx(list(weight_diagnostics = w_diag, survey_with_weights = survey),
           file.path(out_dir, "06_weights_and_weighted_survey.xlsx"))
write_xlsx(list(calibration_check = cal_check),
           file.path(out_dir, "06b_calibration_check.xlsx"))

# ============================================================
# AFTER THIS BLOCK: re-fit fit_core and the five sensitivity scenarios,
# re-derive national/district/kappa/validation, and regenerate every
# model-derived output workbook BEFORE any figure work. The only scenario
# that does not change is S2_strict_unweighted (weights = 1).
# ============================================================

# =========================
# CHUNK 9: Graphs + descriptive turnout tables
# =========================
p_w <- ggplot(survey, aes(x = w_cal_norm)) +
  geom_histogram(bins = 40) +
  labs(title = "Normalized Weight Distribution (for brms)", x = "w_cal_norm", y = "Count")
save_plot(p_w, "plot_weights_norm_hist.png", 8, 5)

# Calibration check (compare weighted joint totals vs PHC targets)
desW_raw <- svydesign(ids=~1, data=survey, weights=~w_cal)
tab <- svytable(~sex_age, desW_raw)

cal_check <- tibble(
  sex_age = names(tab),
  weighted_total = as.numeric(tab),
  target_total = as.numeric(phc_targets_vec[names(tab)]),
  diff = weighted_total - target_total
)

write_xlsx(list(calibration_check = cal_check),
           file.path(out_dir, "06b_calibration_check.xlsx"))

age_turn <- survey %>%
  group_by(age_grp) %>%
  summarise(turnout = weighted.mean(y_strict, w_cal_norm), .groups="drop")

sex_turn <- survey %>%
  group_by(sex) %>%
  summarise(turnout = weighted.mean(y_strict, w_cal_norm), .groups="drop")

p_age <- ggplot(age_turn, aes(x = age_grp, y = turnout, group = 1)) +
  geom_line() + geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Strict Turnout (Weighted) by Age Group", x = "Age group", y = "Turnout")
save_plot(p_age, "plot_turnout_by_age_line.png", 8, 5)

p_sex <- ggplot(sex_turn, aes(x = sex, y = turnout)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Strict Turnout (Weighted) by Sex", x = "Sex", y = "Turnout")
save_plot(p_sex, "plot_turnout_by_sex_bar.png", 6, 5)

write_xlsx(list(
  turnout_by_age = age_turn,
  turnout_by_sex = sex_turn
), file.path(out_dir, "07_descriptive_turnout_tables.xlsx"))

# =========================
# CHUNK 10: Load EC counts + district×sex poststrat frame
# =========================
ec <- read_excel(f_ec, sheet = "EC_Count_English") %>% janitor::clean_names()

need_ec <- c("district_std","district_bn","total_voters","male_voters","female_voters","hijra_voters")
stopifnot(all(need_ec %in% names(ec)))

post_ec <- ec %>%
  transmute(
    district_std = clean_text(district_std),
    total = as.numeric(total_voters),
    male = as.numeric(male_voters),
    female = as.numeric(female_voters)
  ) %>%
  pivot_longer(cols = c(male, female), names_to = "sex", values_to = "N_ec") %>%
  mutate(sex = recode(sex, male = "পুরুষ", female = "নারী"))

write_xlsx(list(ec_poststrat = post_ec),
           file.path(out_dir, "08_ec_poststrat_district_sex.xlsx"))

# =========================
# CHUNK 11: brms MRP regression (older brms compatible) — uses NORMALIZED weights
# =========================
priors <- c(
  set_prior("normal(0, 1.5)", class = "b"),
  set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
  set_prior("exponential(1)", class = "sd")
)

# GUARD: refuse to fit unless the survey carries corrected calibration weights
assert_calibrated(survey, phc_targets_vec)

# QUICK sanity fit (should show iteration progress)
fit_quick <- brm(
  bf(y_strict | weights(w_cal_norm) ~ age_grp + sex + edu + (1 | district_std)),
  data = survey,
  family = bernoulli("logit"),
  prior = priors,
  chains = 1, iter = 400, warmup = 200,
  seed = 20260209,
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  cores = 1,
  refresh = 10
)
saveRDS(fit_quick, file.path(out_dir, "model_quick.rds"))
write_utf8_txt(capture.output(summary(fit_quick)), file.path(out_dir, "09a_model_quick_summary.txt"))

# FULL fit (Windows-stable)
fit_core <- brm(
  bf(y_strict | weights(w_cal_norm) ~ age_grp + sex + edu + (1 | district_std)),
  data = survey,
  family = bernoulli("logit"),
  prior = priors,
  chains = 4, iter = 2000, warmup = 1000,
  seed = 20260209,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  cores = 1,
  refresh = 50
)

saveRDS(fit_core, file.path(out_dir, "model_core.rds"))
write_utf8_txt(capture.output(summary(fit_core)), file.path(out_dir, "09_model_core_summary.txt"))

# =========================
# CHUNK 12 (FIXED for older brms): Poststratification using model factor levels
# =========================

# 1) Factor levels actually used by the fitted model (source of truth)
age_levels_model  <- levels(fit_core$data$age_grp)
sex_levels_model  <- levels(fit_core$data$sex)
dist_levels_model <- levels(fit_core$data$district_std)
edu_levels_model  <- levels(fit_core$data$edu)

# 2) Build national age shares restricted to model age levels
age_shares <- phc_targets %>%
  mutate(age_grp_chr = as.character(age_grp)) %>%
  filter(age_grp_chr %in% age_levels_model) %>%
  group_by(sex) %>%
  mutate(p_age = N_target / sum(N_target)) %>%
  ungroup() %>%
  transmute(
    sex,
    age_grp = factor(age_grp_chr, levels = age_levels_model),
    p_age
  )

# 3) Expand EC district×sex to district×sex×age (many-to-many intended)
# If your dplyr doesn't support relationship=, remove that argument.
post3 <- post_ec %>%
  left_join(age_shares, by = "sex") %>%
  mutate(
    district_std = factor(district_std, levels = dist_levels_model),
    sex = factor(sex, levels = sex_levels_model),
    edu = factor(edu_levels_model[1], levels = edu_levels_model)
  ) %>%
  filter(!is.na(district_std), !is.na(sex), !is.na(age_grp), !is.na(p_age))

# 4) Predict expected probability for each poststrat cell
pred <- tidybayes::add_epred_draws(fit_core, newdata = post3, re_formula = NULL)

# 5) Collapse over age using PHC shares -> district×sex per draw
dist_sex_draws <- pred %>%
  group_by(district_std, sex, .draw) %>%
  summarise(p_hat = sum(.epred * p_age), .groups = "drop")

# 6) Combine with EC counts -> district overall per draw
dist_draws <- dist_sex_draws %>%
  left_join(post_ec, by = c("district_std", "sex")) %>%
  group_by(district_std, .draw) %>%
  summarise(
    turnout_hat = sum(p_hat * N_ec, na.rm = TRUE) / sum(N_ec, na.rm = TRUE),
    .groups = "drop"
  )

# 7) District summaries (mean + 95% interval)
dist_summary <- dist_draws %>%
  group_by(district_std) %>%
  summarise(
    mean = mean(turnout_hat),
    lo   = quantile(turnout_hat, 0.025),
    hi   = quantile(turnout_hat, 0.975),
    .groups = "drop"
  ) %>%
  arrange(desc(mean))

# 8) National aggregation (EC district totals)
district_totals <- ec %>%
  transmute(
    district_std = clean_text(district_std),
    total = as.numeric(total_voters)
  )

national_draws <- dist_draws %>%
  left_join(district_totals, by = "district_std") %>%
  group_by(.draw) %>%
  summarise(
    national_turnout = sum(turnout_hat * total, na.rm = TRUE) / sum(total, na.rm = TRUE),
    .groups = "drop"
  )

national_summary <- national_draws %>%
  summarise(
    mean = mean(national_turnout),
    lo   = quantile(national_turnout, 0.025),
    hi   = quantile(national_turnout, 0.975)
  )

# Save estimates (use safe_write_xlsx to avoid Excel lock issues)
safe_write_xlsx(list(
  district_estimates = dist_summary,
  national_estimate  = national_summary
), file.path(out_dir, "10_turnout_estimates.xlsx"))

# Plots
topN <- 30
plot_df <- dist_summary %>%
  slice_max(mean, n = topN) %>%
  mutate(district_std = factor(district_std, levels = rev(district_std)))

p_dist <- ggplot(plot_df, aes(x = district_std, y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = paste0("Estimated Strict Turnout by District (Top ", topN, ")"),
    x = "District", y = "Turnout (mean with 95% interval)"
  )
save_plot(p_dist, "plot_district_turnout_top30.png", 10, 10)

p_nat <- ggplot(national_summary, aes(x = "National", y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "National Strict Turnout Estimate", x = "", y = "Turnout")
save_plot(p_nat, "plot_national_turnout.png", 6, 5)


# =========================
# CHUNK 13: Final results pack (safe write)
# =========================
safe_write_xlsx(list(
  phc_targets          = phc_targets,
  weight_diagnostics   = w_diag,
  calibration_check    = cal_check,
  ec_poststrat         = post_ec,
  turnout_by_age       = age_turn,
  turnout_by_sex       = sex_turn,
  district_estimates   = dist_summary,
  national_estimate    = national_summary
), file.path(out_dir, "RESULTS_pack.xlsx"))


# ============================================================
# CHUNK 14+: SENSITIVITY CHECKS (Outcome + Weight + Model spec)
#   Produces:
#     - SENSITIVITY_national.xlsx  (national summary table)
#     - SENSITIVITY_district_top30.xlsx (top 30 districts per scenario)
#     - plots: national comparison bar, district scatter vs baseline, etc.
#   Assumes you already ran up to:
#     - CHUNK 10 (post_ec, ec loaded)
#     - CHUNK 11 (priors defined; fit_core exists)
#     - phc_targets exists
#     - survey contains y_strict, y_likely, w_cal_norm (and w_cal)
# ============================================================

# =========================
# CHUNK 14: Build outcomes + weight variants for sensitivity
# =========================
# Ensure outcomes exist (safe even if already created)
survey <- survey %>%
  mutate(
    y_strict = ifelse(turnout_intent == "অবশ্যই ভোট দেব", 1L, 0L),
    y_likely = ifelse(turnout_intent %in% c("অবশ্যই ভোট দেব","সম্ভবত ভোট দেব"), 1L, 0L)
  )

# Ensure normalized calibrated weights exist
if (!"w_cal_norm" %in% names(survey)) {
  stop("w_cal_norm not found. Ensure CHUNK 8 created normalized weights.")
}

survey <- survey %>%
  mutate(
    w_none    = 1,
    w_cal_use = w_cal_norm,
    w_cal_99  = pmin(w_cal_norm, as.numeric(quantile(w_cal_norm, 0.99,  na.rm = TRUE))),
    w_cal_995 = pmin(w_cal_norm, as.numeric(quantile(w_cal_norm, 0.995, na.rm = TRUE)))
  )

safe_write_xlsx(list(
  weight_summary = tibble(
    var = c("w_none","w_cal_use","w_cal_99","w_cal_995"),
    mean = c(mean(survey$w_none), mean(survey$w_cal_use), mean(survey$w_cal_99), mean(survey$w_cal_995)),
    min = c(min(survey$w_none), min(survey$w_cal_use), min(survey$w_cal_99), min(survey$w_cal_995)),
    max = c(max(survey$w_none), max(survey$w_cal_use), max(survey$w_cal_99), max(survey$w_cal_995))
  )
), file.path(out_dir, "SENSITIVITY_00_weight_variants.xlsx"))


# =========================
# CHUNK 15: Helper — fit + poststrat + return national + district summaries
#   (Older brms compatible: uses fit$data factor levels)
# =========================
run_mrp_scenario <- function(
    scenario_id,
    y_var = "y_strict",
    w_var = "w_cal_use",
    model_spec = c("baseline","no_edu","age_sex_interaction"),
    iter = 1200, warmup = 600, chains = 4
){
  model_spec <- match.arg(model_spec)
  
  # Build RHS based on model spec
  rhs <- switch(model_spec,
                baseline = "age_grp + sex + edu + (1 | district_std)",
                no_edu   = "age_grp + sex + (1 | district_std)",
                age_sex_interaction = "age_grp * sex + edu + (1 | district_std)"
  )
  
  # brms weighted formula (weights inside formula for older versions)
  fml <- as.formula(paste0(y_var, " | weights(", w_var, ") ~ ", rhs))
  
  # GUARD: never fit a scenario on stale / un-calibrated weights
  assert_calibrated(survey, phc_targets_vec)
  
  fit <- brm(
    bf(fml),
    data = survey,
    family = bernoulli("logit"),
    prior = priors,
    chains = chains, iter = iter, warmup = warmup,
    seed = 20260209,
    control = list(adapt_delta = 0.95, max_treedepth = 12),
    cores = 1,
    refresh = 50
  )
  
  saveRDS(fit, file.path(out_dir, paste0("SENS_model_", scenario_id, ".rds")))
  write_utf8_txt(capture.output(summary(fit)),
                 file.path(out_dir, paste0("SENS_model_", scenario_id, "_summary.txt")))
  
  # --- Factor levels allowed by this fit
  age_levels_model  <- levels(fit$data$age_grp)
  sex_levels_model  <- levels(fit$data$sex)
  dist_levels_model <- levels(fit$data$district_std)
  
  # edu may be absent in no_edu spec; handle safely
  edu_levels_model <- if ("edu" %in% names(fit$data)) levels(fit$data$edu) else NULL
  
  # --- Age shares (PHC) restricted to model age levels
  age_shares <- phc_targets %>%
    mutate(age_grp_chr = as.character(age_grp)) %>%
    filter(age_grp_chr %in% age_levels_model) %>%
    group_by(sex) %>%
    mutate(p_age = N_target / sum(N_target)) %>%
    ungroup() %>%
    transmute(
      sex,
      age_grp = factor(age_grp_chr, levels = age_levels_model),
      p_age
    )
  
  # --- Construct poststrat frame district×sex×age (+ edu placeholder if needed)
  post3 <- post_ec %>%
    left_join(age_shares, by = "sex") %>%
    mutate(
      district_std = factor(district_std, levels = dist_levels_model),
      sex = factor(sex, levels = sex_levels_model)
    ) %>%
    filter(!is.na(district_std), !is.na(sex), !is.na(age_grp), !is.na(p_age))
  
  if (model_spec %in% c("baseline","age_sex_interaction")) {
    # brms needs edu in newdata if it was in the model
    post3$edu <- factor(edu_levels_model[1], levels = edu_levels_model)
  }
  
  # --- Predict
  pred <- tidybayes::add_epred_draws(fit, newdata = post3, re_formula = NULL)
  
  # --- Collapse over age within district×sex per draw
  dist_sex_draws <- pred %>%
    group_by(district_std, sex, .draw) %>%
    summarise(p_hat = sum(.epred * p_age), .groups = "drop")
  
  # --- District overall using EC sex counts
  dist_draws <- dist_sex_draws %>%
    left_join(post_ec, by = c("district_std","sex")) %>%
    group_by(district_std, .draw) %>%
    summarise(
      turnout_hat = sum(p_hat * N_ec, na.rm = TRUE) / sum(N_ec, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- District summary
  dist_summary <- dist_draws %>%
    group_by(district_std) %>%
    summarise(
      mean = mean(turnout_hat),
      lo   = quantile(turnout_hat, 0.025),
      hi   = quantile(turnout_hat, 0.975),
      .groups = "drop"
    ) %>%
    mutate(scenario = scenario_id)
  
  # --- National draw aggregation
  district_totals <- ec %>%
    transmute(district_std = clean_text(district_std),
              total = as.numeric(total_voters))
  
  national_draws <- dist_draws %>%
    left_join(district_totals, by = "district_std") %>%
    group_by(.draw) %>%
    summarise(
      national_turnout = sum(turnout_hat * total, na.rm = TRUE) / sum(total, na.rm = TRUE),
      .groups = "drop"
    )
  
  national_summary <- national_draws %>%
    summarise(
      mean = mean(national_turnout),
      lo   = quantile(national_turnout, 0.025),
      hi   = quantile(national_turnout, 0.975)
    ) %>%
    mutate(
      scenario = scenario_id,
      outcome = y_var,
      weights = w_var,
      model_spec = model_spec
    )
  
  list(
    fit = fit,
    dist_summary = dist_summary,
    national_summary = national_summary
  )
}


# =========================
# CHUNK 16: Define sensitivity scenarios (keep small but defensible)
#   You can add more scenarios later.
# =========================
scenarios <- tribble(
  ~scenario,                 ~y_var,      ~w_var,        ~model_spec,
  "BASE_strict_cal",         "y_strict",  "w_cal_use",   "baseline",
  "S1_likely_cal",           "y_likely",  "w_cal_use",   "baseline",
  "S2_strict_unweighted",    "y_strict",  "w_none",      "baseline",
  "S3_strict_trim99",        "y_strict",  "w_cal_99",    "baseline",
  "S4_strict_noedu",         "y_strict",  "w_cal_use",   "no_edu"
)

# For speed, start with iter=1200; increase to 2000 for final publication runs
iter_sens   <- 1200
warmup_sens <- 600
chains_sens <- 4


# =========================
# CHUNK 17: Run scenarios + collect outputs  (connection-safe version)
# =========================

# --- 17.1: Run all scenarios, isolating failures so one bad fit doesn't
#           kill the batch and waste the successful ones ---
sens_out <- lapply(seq_len(nrow(scenarios)), function(i){
  sc <- scenarios[i, ]
  message("Running scenario: ", sc$scenario)
  tryCatch(
    run_mrp_scenario(
      scenario_id = sc$scenario,
      y_var       = sc$y_var,
      w_var       = sc$w_var,
      model_spec  = sc$model_spec,
      iter        = iter_sens,
      warmup      = warmup_sens,
      chains      = chains_sens
    ),
    error = function(e){
      message("FAILED: ", sc$scenario, " -> ", conditionMessage(e))
      NULL
    }
  )
})
names(sens_out) <- scenarios$scenario

# --- 17.2: Report which scenarios succeeded / failed ---
ok_flags <- !vapply(sens_out, is.null, logical(1))
message("Scenarios succeeded: ", sum(ok_flags), " / ", length(ok_flags))
if (any(!ok_flags)) {
  message("Scenarios FAILED: ", paste(names(sens_out)[!ok_flags], collapse = ", "))
}

# --- 17.3: Re-run ONLY the failed scenarios (if any), after a fresh
#           connection reset. Repeat this block until all succeed. ---
failed_ids <- names(sens_out)[!ok_flags]
if (length(failed_ids) > 0) {
  while (sink.number() > 0) sink()
  while (sink.number(type = "message") > 0) sink(type = "message")
  closeAllConnections()
  
  for (id in failed_ids) {
    sc <- scenarios[scenarios$scenario == id, ]
    message("Retrying scenario: ", id)
    res <- tryCatch(
      run_mrp_scenario(
        scenario_id = sc$scenario,
        y_var       = sc$y_var,
        w_var       = sc$w_var,
        model_spec  = sc$model_spec,
        iter        = iter_sens,
        warmup      = warmup_sens,
        chains      = chains_sens
      ),
      error = function(e){
        message("STILL FAILED: ", id, " -> ", conditionMessage(e)); NULL
      }
    )
    if (!is.null(res)) sens_out[[id]] <- res
  }
  ok_flags <- !vapply(sens_out, is.null, logical(1))
}

# --- 17.4: Do not proceed to binding unless every scenario is present ---
stopifnot(all(ok_flags))

# --- 17.5: Collect outputs (drops any NULLs defensively) ---
sens_out_ok  <- sens_out[ok_flags]
sens_national <- bind_rows(lapply(sens_out_ok, `[[`, "national_summary"))
sens_district <- bind_rows(lapply(sens_out_ok, `[[`, "dist_summary"))

# Save tables
safe_write_xlsx(list(
  scenarios = scenarios,
  national  = sens_national,
  district  = sens_district
), file.path(out_dir, "SENSITIVITY_01_tables.xlsx"))


# =========================
# CHUNK 18: Graphs — National comparison (bar with 95% intervals)
# =========================
p_nat_cmp <- ggplot(sens_national, aes(x = scenario, y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  labs(
    title = "Sensitivity: National Turnout Estimates (mean + 95% interval)",
    x = "Scenario", y = "Turnout"
  )

save_plot(p_nat_cmp, "SENS_plot_national_comparison.png", 10, 6)


# =========================
# CHUNK 19: Graphs — District comparison vs baseline (scatter)
#   Shows whether district ranking changes a lot.
# =========================
baseline_id <- "BASE_strict_cal"
base_dist <- sens_district %>%
  filter(scenario == baseline_id) %>%
  select(district_std, base_mean = mean)

dist_vs_base <- sens_district %>%
  left_join(base_dist, by = "district_std") %>%
  filter(scenario != baseline_id)

p_dist_scatter <- ggplot(dist_vs_base, aes(x = base_mean, y = mean)) +
  geom_point() +
  facet_wrap(~scenario) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Sensitivity: District Turnout vs Baseline",
    x = "Baseline district turnout", y = "Scenario district turnout"
  )

save_plot(p_dist_scatter, "SENS_plot_district_scatter_vs_baseline.png", 12, 8)


# =========================
# CHUNK 20: Tables — Top 30 districts per scenario
# =========================
topN <- 30
top30_by_scenario <- sens_district %>%
  group_by(scenario) %>%
  arrange(desc(mean)) %>%
  slice_head(n = topN) %>%
  ungroup()

safe_write_xlsx(list(top30 = top30_by_scenario),
                file.path(out_dir, "SENSITIVITY_02_district_top30.xlsx"))

# Plot top 20 for each scenario (readable)
top20 <- sens_district %>%
  group_by(scenario) %>%
  slice_max(mean, n = 20) %>%
  ungroup() %>%
  group_by(scenario) %>%
  mutate(district_std = fct_reorder(district_std, mean)) %>%
  ungroup()

p_top20 <- ggplot(top20, aes(x = district_std, y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  coord_flip() +
  facet_wrap(~scenario, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Sensitivity: Top 20 District Turnout (per scenario)",
    x = "District", y = "Turnout (mean + 95% interval)"
  )

save_plot(p_top20, "SENS_plot_top20_by_scenario.png", 14, 10)


# =========================
# CHUNK 21: Summary text block (UTF-8) for report paste-in
# =========================
nat_text <- sens_national %>%
  mutate(
    mean_pct = sprintf("%.1f%%", 100*mean),
    lo_pct   = sprintf("%.1f%%", 100*lo),
    hi_pct   = sprintf("%.1f%%", 100*hi),
    line = paste0(scenario, ": ", mean_pct, " (95%: ", lo_pct, "–", hi_pct, "); ",
                  "outcome=", outcome, ", weights=", weights, ", spec=", model_spec)
  ) %>%
  arrange(match(scenario, scenarios$scenario)) %>%
  pull(line)

write_utf8_txt(
  c(
    "Sensitivity checks summary (national turnout):",
    nat_text,
    "",
    "Interpretation guidance:",
    "- BASE_strict_cal is the headline estimate (strict definition, calibrated weights).",
    "- Compare S1 to assess outcome definition sensitivity (strict vs likely).",
    "- Compare S2 to assess reliance on calibration.",
    "- Compare S3 to assess robustness to weight trimming.",
    "- Compare S4 to assess role of education covariate in the regression."
  ),
  file.path(out_dir, "SENSITIVITY_03_summary_text.txt")
)

message("SENSITIVITY DONE. Outputs saved in: ", out_dir)