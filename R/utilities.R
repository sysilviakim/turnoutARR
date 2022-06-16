# CRAN libraries ===============================================================
library(plyr)
library(tidyverse)
library(lubridate)
library(rlang)
library(assertthat)
library(fst)
library(here)
library(broom)
library(xtable)
library(estimatr)
library(sandwich)
library(lmtest)

# Custom packages ==============================================================
## https://github.com/sysilviakim/Kmisc
# library(remotes)
# install_github("sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch"))
library(Kmisc) 

# Custom functions =============================================================
lm_print_custom <- function(lm, ctrls, choice) {
  if (choice == "raw") {
    lm <- lm$raw
  } else {
    lm <- lm$robust
  }
  
  sum <- summary(lm)
  sum$coefficients <- sum$coefficients[
    setdiff(
      seq(nrow(sum$coefficients)),
      ctrls %>%
        map(~ grep(.x, names(lm$coefficients)) %>% unlist()) %>%
        unlist()
    ),
  ]
  print(sum)
}

data_filter <- function(data, y) {
  date_tbl <- data.frame(
    y = c(
      "gen2018", "pri2018", "gen2016", "pri2016",
      "gen2014", "pri2014", "gen2012", "pri2012"
    ),
    date = as.Date(c(
      "2018-11-06", "2018-06-05", "2016-11-08", "2016-06-07",
      "2014-11-04", "2014-06-03", "2012-11-06", "2012-06-05"
    ))
  )
  data %>%
    filter(
      dtOrigRegDate <= date_tbl$date[which(date_tbl$y == y)] &
        dtBirthDate <= (date_tbl$date[which(date_tbl$y == y)] - years(18))
    )
}

lm_custom <- function(data,
                      model,
                      y = "gen2018",
                      main = NULL,
                      ctrls = NULL,
                      gam_ctrls = NULL,
                      cluster = NULL) {
  frml <- as.formula(paste0(
    y, " ~ ", c(main, ctrls) %>% paste(collapse = " + ")
  ))
  if (model == "gam") {
    frml <- as.formula(
      paste0(
        y, " ~ ",
        setdiff(c(main, ctrls), gam_ctrls) %>% paste(collapse = " + "),
        " + s(", gam_ctrls %>% paste(collapse = ") + s("), ")"
      )
    )
  }
  date_tbl <- data.frame(
    y = c(
      "gen2018", "pri2018", "gen2016", "pri2016",
      "gen2014", "pri2014", "gen2012", "pri2012"
    ),
    date = as.Date(c(
      "2018-11-06", "2018-06-05", "2016-11-08", "2016-06-07",
      "2014-11-04", "2014-06-03", "2012-11-06", "2012-06-05"
    ))
  )
  data <- data %>%
    filter(
      dtOrigRegDate < date_tbl$date[which(date_tbl$y == y)] &
        dtBirthDate <= (date_tbl$date[which(date_tbl$y == y)] - years(18))
    )
  if (model == "lpm") {
    if (is.null(cluster)) {
      out <- list(
        raw = do.call(
          "lm", list(
            as.formula(frml), data = as.name("data")
          )
        ),
        robust = do.call(
          "lm_robust", list(
            as.formula(frml), data = as.name("data"), se_type = "stata"
          )
        )
      )
    } else {
      out <- list(
        raw = do.call(
          "lm", list(
            as.formula(frml), data = as.name("data")
          )
        ),
        robust = do.call(
          "lm_robust", list(
            as.formula(frml), data = as.name("data"), se_type = "stata",
            clusters = cluster
          )
        )
      )
    }
  } else if (model == "logit") {
    if (is.null(cluster)) {
      out <- do.call(
        "glm", list(
          as.formula(frml), data = as.name("data"), family = "binomial"
        )
      )
      out <- list(
        raw = out,
        robust = coeftest(out, vcov = vcovHC(out, type = "HC1"))[, "Std. Error"]
      )
    } else {
      out <- do.call(
        "glm", list(
          as.formula(frml), data = as.name("data"), family = "binomial",
          clusters = cluster
        )
      )
      out <- list(
        raw = out,
        robust = coeftest(out, vcov = vcovHC(out, type = "HC1"))[, "Std. Error"]
      )
    }
  } else if (model == "gam") {
    out <- do.call(
      "gam", list(
        as.formula(frml),
        data = as.name("data"), family = "binomial",
        method = "REML"
      )
    )
  }
  if (model == "lpm") {
    lm_print_custom(out, ctrls = ctrls, choice = "robust")
    assert_that(length(out$raw$coefficients) == out$raw$rank)
  } else if (model == "logit") {
    assert_that(length(out$raw$coefficients) == out$raw$rank)
  }
  return(out)
}

main_model <- function(data, main, basic.control, model = "lpm",
                       gctr = NULL, ctr = NULL, ...) {
  lm_custom(
    data = data, model = model, main = main,
    gam_ctrls = gctr, ctrls = c("gen2016", basic.control, ctr), ...
  )
}

pc_model <- function(data, main, basic.control, model = "lpm",
                     gctr = NULL, ctr = NULL, ...) {
  # c("gen2016", "pri2016", "gen2014", "pri2014", "gen2012", "pri2012") %>%
  c("gen2016", "pri2016") %>%
    set_names(., .) %>%
    map(
      function(x) lm_custom(
        data = data, y = x, model = model, main = main,
        gam_ctrls = gctr, ctrls = c(basic.control, ctr), ...
      )
    )
}

subset2016_model <- function(data, main, basic.control, model = "lpm",
                             gctr = NULL, ctr = NULL) {
  list(
    voted = lm_custom(
      data = data %>%
        filter(
          gen2016 == 1 &
            dtOrigRegDate <= as.Date("2016-11-08") &
            dtBirthDate <= as.Date("1998-11-08")
        ),
      model = model, main = main,
      gam_ctrls = gctr, ctrls = c(basic.control, ctr)
    ),
    not = lm_custom(
      data = data %>%
        filter(
          gen2016 == 0 &
            dtOrigRegDate <= as.Date("2016-11-08") &
            dtBirthDate <= as.Date("1998-11-08")
        ),
      model = model, main = main,
      gam_ctrls = gctr, ctrls = c(basic.control, ctr)
    )
  )
}

fig_prep <- function(x, choice = "robust") {
  x$pc$main <- x$main
  # out <- x$pc[c(7, seq(6))] %>%
  out <- x$pc[c(3, seq(2))] %>%
    map(~ tidy(.x[[choice]], conf.int = TRUE)[2, ]) %>%
    bind_rows() %>%
    rowid_matrix_to_df(colname = "election") %>%
    mutate(
      election = factor(
        election,
        levels = as.character(seq(3)),
        labels = c(
          "2018\nGen.", "2016\nGen.", "2016\nPri." # , 
          # "2014\nGen.", "2014\nPri.", "2012\nGen.", "2012\nPri." 
        )
      )
    ) %>%
    mutate(main = ifelse(election == "2018\nGen.", "Y", "N"))
  return(out)
}

fig_main <- function(x, min = -.12, max = .13, by = 0.04, annotate = TRUE) {
  p <- ggplot(x, aes(election, estimate, color = main)) +
    geom_point() +
    geom_pointrange(size = 1.2, aes(ymin = conf.low, ymax = conf.high)) +
    xlab("Main and Placebo Regressions") +
    ylab("Treatment Effect (95% CI)") +
    scale_colour_manual(values = c("Y" = "red", "N" = "gray10")) + 
    geom_hline(yintercept = 0) + 
    scale_y_continuous(
      limits = c(min, max), breaks = seq(min, max, by = by),
      labels = scales::percent
      # scales::number_format(accuracy = 0.01)
    ) +
    annotate(
      "rect",
      xmin = 1.7, xmax = 3.3, ymin = min, ymax = max,
      fill = "lightgray", alpha = 0.4
    )
  
  if (annotate) {
    p <- p +
      annotate(
        "text",
        x = 2.5, y = .1, label = "Placebo Tests", family = "CM Roman"
      ) # + 
      # geom_text(
      #   data = x, 
      #   aes(
      #     x = election, y = min + abs(min) / 3, family = "CM Roman",
      #     label = paste0(
      #       formatC(estimate, digits = 2, format = "f"), 
      #       ",\n 95% CI \n [", 
      #       formatC(conf.low, digits = 2, format = "f"), ", ",
      #       formatC(conf.high, digits = 2, format = "f"), "]"
      #     )
      #   )
      # )
  }
  return(p)
}

full_reg_set <- function(df, vl, exc = NULL, ctr = "hhincB", main = NULL,
                         model = "lpm", ...) {
  if (is.null(main)) main <- vl$main
  list(
    main = main_model(
      df,
      main = main, ctr = ctr, basic.control = setdiff(vl$bc, exc),
      model = model, ...
    ),
    pc = pc_model(
      df,
      main = main, ctr = ctr, basic.control = setdiff(vl$bc, exc),
      model = model, ...
    )
    # s16 = subset2016_model
  )
}

ks_full <- function(dat, x, sub = NULL, export = TRUE) {
  dat <- dat %>% mutate(pav = as.numeric(pav) - 1)
  treatment <- dat %>% filter(nudged == "Treated")
  control <- dat %>% filter(nudged == "Not Treated")
  
  # Summary stats out to tab folder
  if (!is.null(sub)) {
    for (var in names(table(dat[[sub]]))) {
      t2 <- treatment %>% filter(!!as.name(sub) == var) %>% .[[x]]
      c2 <- control %>% filter(!!as.name(sub) == var) %>% .[[x]]
      
      out <- ks.test(t2, c2)
      print(out)
      
      if (length(str_match_all(tolower(var), "(.*?)/")[[1]]) == 0) {
        var <- tolower(var)
      } else {
        var <- str_match_all(tolower(var), "(.*?)/")[[1]][1, 2]
      }
      if (export) {
        write(
          formatC(mean(t2), digits = 2, format = "f"), 
          file = here("tab", paste0(x, "_", sub, "_", var, "_treated_mean.tex"))
        )
        write(
          formatC(mean(c2), digits = 2, format = "f"),
          file = here("tab", paste0(x, "_", sub, "_", var, "_control_mean.tex"))
        )
        
        write(
          formatC(sd(t2), digits = 2, format = "f"),
          file = here("tab", paste0(x, "_", sub, "_", var, "_treated_sd.tex"))
        )
        write(
          formatC(sd(c2), digits = 2, format = "f"),
          file = here("tab", paste0(x, "_", sub, "_", var, "_control_sd.tex"))
        )
      }
      
      if (export) {
        write(
          formatC(out$statistic, digits = 4, format = "f"),
          file = here("tab", paste0(x, "_", sub, "_", var,  "_ks_statistic.tex"))
        )
        write(
          formatC(out$p.value, digits = 4, format = "f"),
          file = here("tab", paste0(x, "_", sub, "_", var, "_ks_pvalue.tex"))
        )
      }
    }
  } else {
    treatment <- treatment %>% .[[x]]
    control <- control %>% .[[x]]
    
    out <- ks.test(treatment, control)
    print(out)
    if (export) {
      write(
        formatC(mean(treatment), digits = 2, format = "f"), 
        file = here("tab", paste0(x, "_treated_mean.tex"))
      )
      write(
        formatC(mean(control), digits = 2, format = "f"),
        file = here("tab", paste0(x, "_control_mean.tex"))
      )
      
      write(
        formatC(sd(treatment), digits = 2, format = "f"),
        file = here("tab", paste0(x, "_treated_sd.tex"))
      )
      write(
        formatC(sd(control), digits = 2, format = "f"),
        file = here("tab", paste0(x, "_control_sd.tex"))
      )
      
      write(
        formatC(out$statistic, digits = 4, format = "f"),
        file = here("tab", paste0(x, "_ks_statistic.tex"))
      )
      write(
        formatC(out$p.value, digits = 4, format = "f"),
        file = here("tab", paste0(x, "_ks_pvalue.tex"))
      )
    }
    
    # Export test statistics and p-value
    return(out)
  }
}

stargazer_custom <- function(x, fname = "main-full.tex", model = "lpm", ...) {
  if (model == "lpm") {
    stargazer(
      x$main$raw,
      x$pc$gen2016$raw,
      x$pc$pri2016$raw,
      # x$pc$gen2014,
      # x$pc$pri2014,
      # x$pc$gen2012,
      # x$pc$pri2012,
      star.cutoffs = c(.05, 0.01, .001),
      star.char = c("*", "**", "***"),
      float = FALSE,
      column.labels = c(
        "Gen. 2018", "Gen. 2016", "Pri. 2016" # , 
        # "Gen. 2014", "Pri. 2014", "Gen. 2012", "Pri. 2012"
      ),
      # dep.var.caption = "",
      dep.var.labels.include = FALSE,
      model.numbers = FALSE,
      df = FALSE,
      covariate.labels = c(
        "ARR treatment",
        "Same precincts", "Same subdistricts", "Same cong. dist.",
        "Diff. cong. dist.", "Distance moved", "Gen. 2016 turnout",
        "Times moved", "Distance to poll", "Permanent absentee",
        "Age", "Female", "Male", "Black", "Hispanic", "Others", "White",
        "Third-party/no party", "Democrat", "Imputed income (old home)",
        "Born abroad",
        "39th cong. dist.", "45th cong. dist.", "46th cong. dist.",
        "47th cong. dist.", "48th cong. dist.", "49th cong. dist.",
        "Imputed income (new home)", "Intercept"
      ),
      omit = "Constant",
      omit.stat = "ser",
      no.space = TRUE,
      out = here("tab", fname),
      se = starprep(
        x$main$raw,
        x$pc$gen2016$raw,
        x$pc$pri2016$raw,
        # x$pc$gen2014,
        # x$pc$pri2014,
        # x$pc$gen2012,
        # x$pc$pri2012,
        ...
      )
    )
  } else if (model == "logit") {
    stargazer(
      x$main$raw,
      x$pc$gen2016$raw,
      x$pc$pri2016$raw,
      # x$pc$gen2014,
      # x$pc$pri2014,
      # x$pc$gen2012,
      # x$pc$pri2012,
      star.cutoffs = c(.05, 0.01, .001),
      star.char = c("*", "**", "***"),
      float = FALSE,
      column.labels = c(
        "Gen. 2018", "Gen. 2016", "Pri. 2016" # , 
        # "Gen. 2014", "Pri. 2014", "Gen. 2012", "Pri. 2012"
      ),
      # dep.var.caption = "",
      dep.var.labels.include = FALSE,
      model.numbers = FALSE,
      df = FALSE,
      covariate.labels = c(
        "ARR treatment",
        "Same precincts", "Same subdistricts", "Same cong. dist.",
        "Diff. cong. dist.", "Distance moved", "Gen. 2016 turnout",
        "Times moved", "Distance to poll", "Permanent absentee",
        "Age", "Female", "Male", "Black", "Hispanic", "Others", "White",
        "Third-party/no party", "Democrat", "Imputed income (old home)", 
        "Born abroad",
        "39th cong. dist.", "45th cong. dist.", "46th cong. dist.",
        "47th cong. dist.", "48th cong. dist.", "49th cong. dist.",
        "Imputed income (new home)", "Intercept"
      ),
      omit = "Constant",
      omit.stat = "ser",
      no.space = TRUE,
      out = here("tab", fname),
      se = list(
        x$main$robust,
        x$pc$gen2016$robust,
        x$pc$pri2016$robust
      )
    )
  }
}

print_ci <- function(choice, type = "main") {
  c("conf.low", "conf.high") %>%
    map_chr(
      ~ formatC(
        lm_list[[choice]][[type]][["robust"]][[.x]][["nudgedTreated"]], 
        format = "f", digits = 4
      )
    )
}

# Other settings ===============================================================
options(digits = 4, scipen = 999)
