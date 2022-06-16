source(here::here("R", "utilities.R"))
library(pwr)
library(stargazer)
library(margins)
width <- 4.5
height <- 3

# Data import and setup ========================================================
dl <- list(
  movers = read_fst(here("data", "movers_anonymized.fst")),
  stayers = read_fst(here("data", "stayers_anonymized.fst"))
)

glm_list <- lm_list <- list()
vl <- list(
  main = c("nudged", "info_poll", "dist"),
  bc = c(
    # gen2016 is controlled inside the function
    "move_times", "poll_dist_2018", "pav", "age", "gender", "race", "party",
    "hhinc", "foreign_born", "szDistrictName_1B"
  )
)

# Main regression ==============================================================
# Policy model: subset to June 2018 movers
dat_large <- dat <- dl$movers %>%
  filter(
    coa_movdat_2 %in% as.Date(c("2018-06-01", "2018-05-01", "2018-07-01")) & 
      !is.na(info_poll)
  ) %>%
  mutate(nudged = as.factor(nudged)) %>%
  mutate(
    white = ifelse(race == "white", 1, 0),
    hispanic = ifelse(race == "hispanic", 1, 0),
    asian = ifelse(race == "asian", 1, 0),
    male = ifelse(gender == "male", 1, 0),
    dem = ifelse(party == "Dem", 1, 0),
    rep = ifelse(party == "Rep", 1, 0)
  ) %>%
  mutate(
    party4 = case_when(
      szPartyName == "Republican" ~ 2,
      szPartyName == "Democratic" ~ 3,
      szPartyName == "No Party Preference" ~ 4,
      TRUE ~ 1
    ),
    party4 = factor(
      party4,
      levels = seq(4),
      labels = c("Third-Party", "NPP", "Rep", "Dem")
    )
  ) %>%
  filter(!is.na(nudged))

dat <- dat %>%
  filter(coa_movdat_2 %in% as.Date("2018-06-01") & !is.na(info_poll))

lm_list[["pol_all"]] <- full_reg_set(df = dat, vl)

# Subgroups: by absentee voter =================================================
lm_list[["pol_pav_y"]] <-
  full_reg_set(df = dat %>% filter(pav == 1), vl, exc = "pav")
lm_list[["pol_pav_n"]] <-
  full_reg_set(df = dat %>% filter(pav == 0), vl, exc = "pav")

# Subgroups: by race ===========================================================
lm_list[["pol_race_white"]] <-
  full_reg_set(df = dat %>% filter(race == "white"), vl, exc = "race")
lm_list[["pol_race_hisp"]] <-
  full_reg_set(df = dat %>% filter(race == "hispanic"), vl, exc = "race")
lm_list[["pol_race_asian"]] <-
  full_reg_set(df = dat %>% filter(race == "asian"), vl, exc = "race")
lm_list[["pol_race_other"]] <- full_reg_set(
  df = dat %>% filter(race == "black" | race == "others"), vl, exc = "race"
)

# Subgroups: by party ==========================================================
lm_list[["pol_party_dem"]] <-
  full_reg_set(df = dat %>% filter(party == "Dem"), vl, exc = "party")
lm_list[["pol_party_rep"]] <-
  full_reg_set(df = dat %>% filter(party == "Rep"), vl, exc = "party")
lm_list[["pol_party_npp"]] <- full_reg_set(
  df = dat %>% filter(party == "None/Third-Party"), vl, exc = "party"
)

lm_list[["pol_party_npponly"]] <- full_reg_set(
  df = dat %>% filter(party4 == "NPP"), vl, exc = c("party4", "party")
)
lm_list[["pol_party_notrep"]] <- full_reg_set(
  df = dat %>% filter(party != "Rep"), vl, exc = c("party")
)

# Subgroups: by info structure =================================================
lm_list[["info_lvl1"]] <- full_reg_set(
  df = dat %>%
    filter(info_poll == "Same Address" | info_poll == "Same Precinct"),
  vl, main = c("nudged", "dist"), exc = "info_poll"
)
lm_list[["info_lvl2"]] <- full_reg_set(
  df = dat %>%
    filter(
      info_poll != "Same Address" & info_poll != "Same Precinct" &
        info_poll != "Diff. Cong."
    ),
  vl, main = c("nudged", "dist"), exc = "info_poll"
)

lm_list[["info_lvl3"]] <- full_reg_set(
  df = dat %>%
    filter(info_poll == "Diff. Cong."),
  vl, main = c("nudged", "dist"), exc = "info_poll"
)

# Main figure export ===========================================================
temp <- fig_prep(lm_list$pol_all)
pdf(here("fig/policy_main.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(fig_main(temp))))
dev.off()

ppi <- 300
png(
  here("fig/policy_main.png"),
  width = width * ppi, height = height * ppi, res = ppi
)
print(plot_nolegend(pdf_default(fig_main(temp))))
dev.off()

# Subgroup figure export =======================================================
temp <- fig_prep(lm_list$pol_pav_y)
pdf(here("fig/policy_pav_y.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(
  fig_main(temp, min = -.15, max = .25, by = 0.05)
)))
dev.off()

temp <- fig_prep(lm_list$pol_pav_n)
pdf(here("fig/policy_pav_n.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(
  fig_main(temp, min = -.15, max = .25, by = 0.05, annotate = FALSE)
)))
dev.off()

temp <- fig_prep(lm_list$pol_race_white)
pdf(here("fig/policy_race_white.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(
  fig_main(temp, min = -.15, max = .25, by = 0.05)
)))
dev.off()

temp <- fig_prep(lm_list$pol_race_hisp)
pdf(here("fig/policy_race_hisp.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(
  fig_main(temp, min = -.15, max = .25, by = 0.05, annotate = FALSE)
)))
dev.off()

temp <- fig_prep(lm_list$pol_race_asian)
pdf(here("fig/policy_race_asian.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(
  fig_main(temp, min = -.15, max = .25, by = 0.05, annotate = FALSE)
)))
dev.off()

temp <- fig_prep(lm_list$pol_race_other)
pdf(here("fig/policy_race_other.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(
  fig_main(temp, min = -.15, max = .25, by = 0.05, annotate = FALSE)
)))
dev.off()

temp <- fig_prep(lm_list$pol_party_dem)
pdf(here("fig/policy_party_dem.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(fig_main(temp))))
dev.off()

temp <- fig_prep(lm_list$pol_party_rep)
pdf(here("fig/policy_party_rep.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(fig_main(temp, annotate = FALSE))))
dev.off()

temp <- fig_prep(lm_list$pol_party_npp)
pdf(here("fig/policy_party_npp.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(fig_main(temp, annotate = FALSE))))
dev.off()

temp <- fig_prep(lm_list$info_lvl1)
pdf(here("fig/policy_info_lvl1.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(
  fig_main(temp, min = -.15, max = .25, by = 0.05)
)))
dev.off()

temp <- fig_prep(lm_list$info_lvl2)
pdf(here("fig/policy_info_lvl2.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(
  fig_main(temp, min = -.15, max = .25, by = 0.05, annotate = FALSE)
)))
dev.off()

temp <- fig_prep(lm_list$info_lvl3)
pdf(here("fig/policy_info_lvl3.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(
  fig_main(temp, min = -.15, max = .25, by = 0.05, annotate = FALSE)
)))
dev.off()

## Power (good enough!)
pwr.f2.test(
  u = length(lm_list$pol_race_asian$main$robust$coefficients),
  f2 = summary(lm_list$pol_race_asian$main$robust)$r.squared / (
    1 - summary(lm_list$pol_race_asian$main$robust)$r.squared),
  sig.level = .001, power = .9
)

pwr.f2.test(
  u = length(lm_list$info_lvl1$main$robust$coefficients),
  f2 = summary(lm_list$info_lvl1$main$robust)$r.squared / (
    1 - summary(lm_list$info_lvl1$main$robust)$r.squared),
  sig.level = .001, power = .9
)

# Coeff exports ================================================================
lm_list %>%
  map_dbl(~ .x$main$robust$coefficients[["nudgedTreated"]])
print_ci("pol_all")
print_ci("pol_party_dem")
print_ci("pol_party_rep")
print_ci("pol_party_npp")
print_ci("pol_party_npponly")

## https://declaredesign.org/r/estimatr/articles/regression-tables.html
stargazer_custom(lm_list$pol_all, se_type = "stata")
stargazer_custom(lm_list$pol_party_dem, fname = "dem-full.tex")
stargazer_custom(lm_list$pol_party_rep, fname = "rep-full.tex")
stargazer_custom(lm_list$pol_party_npp, fname = "npp-full.tex")

# RD assumption: Kolmogorov Smirnoff balance tests =============================
dat <- dat %>%
  ## gender categorical
  mutate(male = case_when(gender == "male" ~ 1, TRUE ~ 0)) %>%
  ## szDistrictName_1B categorical
  mutate(
    cong39 = case_when(grepl("39", szDistrictName_1B) ~ 1, TRUE ~ 0),
    cong45 = case_when(grepl("45", szDistrictName_1B) ~ 1, TRUE ~ 0),
    cong46 = case_when(grepl("46", szDistrictName_1B) ~ 1, TRUE ~ 0),
    cong47 = case_when(grepl("47", szDistrictName_1B) ~ 1, TRUE ~ 0),
    cong48 = case_when(grepl("48", szDistrictName_1B) ~ 1, TRUE ~ 0),
    cong49 = case_when(grepl("49", szDistrictName_1B) ~ 1, TRUE ~ 0)
  )

ks_full_list <- c(
  "age", "male", "white", "hispanic", "asian",
  "hhinc", "hhincB", "foreign_born",
  "gen2016", "pri2016", "gen2014", "pri2014", "gen2012", "pri2012",
  "dem", "rep", "cong39", "cong45", "cong46", "cong47", "cong48", "cong49",
  "pav", "move_times", "poll_dist_2018"
)

ks_result <- ks_full_list %>% map(~ ks_full(dat, .x))
ks_result_subgroups <- setdiff(ks_full_list, c("dem", "rep")) %>%
  map(~ ks_full(dat, .x, sub = "party"))

dat %>%
  filter(nudged == "Treated") %>%
  prop(., "pav")
dat %>%
  filter(nudged == "Not Treated") %>%
  prop(., "pav")

dat %>%
  filter(nudged == "Treated") %$% poll_dist_2018 %>%
  summary()
dat %>%
  filter(nudged == "Not Treated") %$% poll_dist_2018 %>%
  summary()

dat %>%
  filter(nudged == "Treated") %>%
  .$hhinc %>%
  {
    mean(. * 1000)
  }
dat %>%
  filter(nudged == "Not Treated") %>%
  .$hhinc %>%
  {
    mean(. * 1000)
  }

## Showing that PAV is associated with higher turnout
dl$stayers %>%
  filter(pav == 0) %>%
  prop(., "gen2018")
dl$stayers %>%
  filter(pav == 1) %>%
  prop(., "gen2018")
dl$stayers %>%
  filter(pav == 0) %>%
  prop(., "pri2018")
dl$stayers %>%
  filter(pav == 1) %>%
  prop(., "pri2018")

# Stayers' hhinc + poll_dist_2018 and turnout
lm_robust(
  gen2018 ~
    hhinc + poll_dist_2018 + pav + age + gender + race +
    party + foreign_born + szDistrictName_1,
  data = dl$stayers
) %>%
  broom::tidy()

## App F mention
lm_robust(gen2018 ~ hhinc, dl$stayers, se_type = "stata") %>% broom::tidy()
lm_robust(gen2018 ~ poll_dist_2018, dl$stayers, se_type = "stata") %>%
  broom::tidy()

# Logistic regressions =========================================================
glm_list[["pol_all"]] <- full_reg_set(df = dat, vl, model = "logit")
exp(glm_list$pol_all$main$robust)

out <- glm_list$pol_all$main$raw
temp <- margins(out, data = dat, vcov = vcovHC(out, type = "HC1"))
temp
marginal_effects(out, data = dat, vcov = vcovHC(out, type = "HC1"))

stargazer_custom(glm_list$pol_all, fname = "main-glm-full.tex", model = "logit")

# Robustness: precinct-level clustering ========================================
lm_list[["pol_precinct_cluster"]] <-
  full_reg_set(df = dat, vl, cluster = as.name("sPrecinctIDB"))

# Reviewer-requested descriptives ==============================================
dat %>% group_by(nudged) %>% summarise(mean(gen2018, na.rm = TRUE))
dat %>% group_by(nudged, party) %>% summarise(mean(gen2018, na.rm = TRUE))
dl$stayers %>% .$gen2018 %>% mean()
dl$stayers %>% group_by(party) %>% summarise(mean(gen2018, na.rm = TRUE))

dl$stayers %>% prop(., "gen2018")
dat %>%
  filter(nudged == "Treated") %>%
  prop(., "gen2018")
dat %>%
  filter(nudged == "Not Treated") %>%
  prop(., "gen2018")

compare_list <- dl %>%
  map(
    ~ .x %>%
      mutate(male = ifelse(gender == "male", 1, 0)) %>%
      mutate(white = ifelse(race == "white", 1, 0)) %>%
      mutate(hispanic = ifelse(race == "hispanic", 1, 0)) %>%
      mutate(asian = ifelse(race == "asian", 1, 0)) %>%
      mutate(dem = ifelse(party == "Dem", 1, 0)) %>%
      mutate(rep = ifelse(party == "Rep", 1, 0))
  )

demo_compare_list <- c(
  "age", "male", "white", "hispanic", "asian", "hhinc", "dem", "rep",
  "pri2014", "gen2014", "pri2016", "gen2016", "pri2018", "gen2018"
) 

xtab <- compare_list %>%
  map_dfr(
    function(x) {
      demo_compare_list %>%
        map_dfr(
          ~ tibble(
            var = .x,
            mean = mean(x[[.x]], na.rm = TRUE),
            sd = sd(x[[.x]], na.rm = TRUE)
          )
        )
    },
    .id = "type"
  )

## Comparison of movers and stayers
xtab <- xtab %>%
  pivot_wider(
    id_cols = "var", names_from = "type", values_from = c("mean", "sd")
  ) %>%
  rowwise() %>%
  mutate(
    var = case_when(
      grepl("gen", var) ~ gsub("gen", "Gen. ", var),
      grepl("pri", var) ~ gsub("pri", "Pri. ", var),
      var == "hhinc" ~ "Imputed Income",
      TRUE ~ simple_cap(var)
    )
  ) %>%
  ungroup() %>%
  select(
    Variable = var,
    `Mean (Movers)` = mean_movers,
    `Std. Dev. (Movers)` = sd_movers,
    `Mean (Stayers)` = mean_stayers,
    `Std. Dev. (Stayers)` = sd_stayers
  )

print(
  xtable(xtab), file = here("tab", "mover_stayer_comparison.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE
)

demo_compare_list %>%
  map(~ t.test(compare_list$stayers[[.x]], compare_list$movers[[.x]]))

## Reviewer-requested larger bandwidths
dat_large <- dat_large %>%
  filter(!(coa_movdat_2 %in% as.Date("2018-05-01") & nudged == "Not Treated"))
dat_large %>% group_by(nudged) %>% summarise(mean(gen2018, na.rm = TRUE))

table(dat_large$coa_movdat_2, dat_large$nudged)
lm_large_list <- list(pol_all = full_reg_set(df = dat_large, vl))
temp <- fig_prep(lm_large_list$pol_all)

pdf(here("fig/policy_main_large.pdf"), width = width, height = height)
print(plot_nolegend(pdf_default(fig_main(temp))))
dev.off()

ks_full_list %>% map(~ ks_full(dat, .x, export = FALSE)) ## similar
