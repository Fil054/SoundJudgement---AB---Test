# ============================================================

# Dataset: ABtestdata

# Variables:

# pairwise : pair ID 1–766

# happyBright : 1 = Happy & Bright music, 0 = Dark & Ominous

# hit : 1 = believes children will be hit, 0 = not

# effectiveRailwayVideo : 1–10 rating of railway safety video

# ============================================================

# —- PACKAGES ———————————————–

    required <- c("tidyverse", "ggplot2", "patchwork", "scales", "readxl")
    to_install <- required[!required %in% rownames(installed.packages())]
    if (length(to_install)) install.packages(to_install)
    lapply(required, library, character.only = TRUE)

    ## ── Attaching core tidyverse packages ─────────────────────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ───────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
    ## 
    ## Caricamento pacchetto: 'scales'
    ## 
    ## 
    ## Il seguente oggetto è mascherato da 'package:purrr':
    ## 
    ##     discard
    ## 
    ## 
    ## Il seguente oggetto è mascherato da 'package:readr':
    ## 
    ##     col_factor

    ## [[1]]
    ##  [1] "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"    
    ##  [8] "tibble"    "ggplot2"   "tidyverse" "readxl"    "stats"     "graphics"  "grDevices"
    ## [15] "utils"     "datasets"  "methods"   "base"     
    ## 
    ## [[2]]
    ##  [1] "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"    
    ##  [8] "tibble"    "ggplot2"   "tidyverse" "readxl"    "stats"     "graphics"  "grDevices"
    ## [15] "utils"     "datasets"  "methods"   "base"     
    ## 
    ## [[3]]
    ##  [1] "patchwork" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"    
    ##  [8] "tidyr"     "tibble"    "ggplot2"   "tidyverse" "readxl"    "stats"     "graphics" 
    ## [15] "grDevices" "utils"     "datasets"  "methods"   "base"     
    ## 
    ## [[4]]
    ##  [1] "scales"    "patchwork" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"    
    ##  [8] "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse" "readxl"    "stats"    
    ## [15] "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
    ## 
    ## [[5]]
    ##  [1] "scales"    "patchwork" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"    
    ##  [8] "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse" "readxl"    "stats"    
    ## [15] "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"

# ============================================================

# SECTION 1: DESCRIPTIVE ANALYSIS & ERROR DETECTION

# ============================================================

    library(readxl)
    ABtestdata <- read_excel("C:/Users/User1/OneDrive - Alma Mater Studiorum Università di Bologna/Business Analytics/Business statistics/Bayesian project/ABtestdata.xlsx")

    cat("=== 1. DATASET OVERVIEW ===\n")

    ## === 1. DATASET OVERVIEW ===

    glimpse(ABtestdata)

    ## Rows: 1,532
    ## Columns: 4
    ## $ pairwise              <dbl> 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 1…
    ## $ happyBright           <dbl> 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, …
    ## $ hit                   <dbl> 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, …
    ## $ effectiveRailwayVideo <dbl> 2, 10, 10, 10, 8, 9, 6, 8, 7, 7, 9, 8, 8, 6, 7, 7, 10, 1, 7, 6,…

    cat("\n=== 1.1 SUMMARY STATISTICS (RAW) ===\n")

    ## 
    ## === 1.1 SUMMARY STATISTICS (RAW) ===

    print(summary(ABtestdata))

    ##     pairwise      happyBright       hit          effectiveRailwayVideo
    ##  Min.   :  1.0   Min.   :0.0   Min.   :-1.0000   Min.   :-10.000      
    ##  1st Qu.:192.0   1st Qu.:0.0   1st Qu.: 0.0000   1st Qu.:  5.000      
    ##  Median :383.5   Median :0.5   Median : 0.0000   Median :  7.000      
    ##  Mean   :383.5   Mean   :0.5   Mean   : 0.3668   Mean   :  6.726      
    ##  3rd Qu.:575.0   3rd Qu.:1.0   3rd Qu.: 1.0000   3rd Qu.:  8.000      
    ##  Max.   :766.0   Max.   :1.0   Max.   : 1.0000   Max.   : 10.000

# —- Identify errors —————————————-

# Expected ranges: hit in {0,1}, effectiveRailwayVideo in {1,…,10}

    error_rows <- ABtestdata %>%
      mutate(row_id = row_number()) %>%
      filter(!hit %in% c(0, 1) |
             effectiveRailwayVideo < 1 |
             effectiveRailwayVideo > 10)

    cat("\n=== 1.2 ERROR ROWS DETECTED ===\n")

    ## 
    ## === 1.2 ERROR ROWS DETECTED ===

    print(error_rows)

    ## # A tibble: 2 × 5
    ##   pairwise happyBright   hit effectiveRailwayVideo row_id
    ##      <dbl>       <dbl> <dbl>                 <dbl>  <int>
    ## 1      761           1    -1                     9   1522
    ## 2      762           1     0                   -10   1524

# Expected output:

# row 1522: pairwise=761, happyBright=1, hit=-1, effectiveRailwayVideo=9

# row 1524: pairwise=762, happyBright=1, hit=0, effectiveRailwayVideo=-10

# ERROR 1 — hit = -1: impossible value; true value unknown → REMOVE

# ERROR 2 — effectiveRailwayVideo = -10: likely accidental minus sign;

# but since we cannot be certain it was 10, we → REMOVE

# Both rows represent &lt; 0.14% of the data so removal has negligible impact.

    ABclean <- ABtestdata %>%
      filter(hit %in% c(0, 1),
             effectiveRailwayVideo >= 1,
             effectiveRailwayVideo <= 10)

    cat(sprintf("\nOriginal: %d rows  |  After cleaning: %d rows  |  Removed: %d\n",
                nrow(ABtestdata), nrow(ABclean), nrow(ABtestdata) - nrow(ABclean)))

    ## 
    ## Original: 1532 rows  |  After cleaning: 1530 rows  |  Removed: 2

# —- Descriptive stats by group —————————–

    ABclean <- ABclean %>%
      mutate(Music = ifelse(happyBright == 1, "Happy & Bright", "Dark & Ominous"))

    desc_stats <- ABclean %>%
      group_by(Music) %>%
      summarise(
        n               = n(),
        n_hit           = sum(hit),
        prop_hit        = mean(hit),
        mean_railway    = mean(effectiveRailwayVideo),
        sd_railway      = sd(effectiveRailwayVideo),
        median_railway  = median(effectiveRailwayVideo),
        .groups = "drop"
      )

    cat("\n=== 1.3 DESCRIPTIVE STATISTICS BY GROUP ===\n")

    ## 
    ## === 1.3 DESCRIPTIVE STATISTICS BY GROUP ===

    print(desc_stats)

    ## # A tibble: 2 × 7
    ##   Music              n n_hit prop_hit mean_railway sd_railway median_railway
    ##   <chr>          <int> <dbl>    <dbl>        <dbl>      <dbl>          <dbl>
    ## 1 Dark & Ominous   766   299    0.390         6.80       2.36              7
    ## 2 Happy & Bright   764   264    0.346         6.67       2.33              7

# ── Binary variables: frequency tables ──────────────────────

    cat("=== happyBright ===\n")

    ## === happyBright ===

    table(ABclean$happyBright) %>% 
      prop.table() %>% 
      cbind(count = table(ABclean$happyBright), proportion = .) %>% 
      print()

    ##   count proportion
    ## 0   766  0.5006536
    ## 1   764  0.4993464

    cat("\n=== hit ===\n")

    ## 
    ## === hit ===

    table(ABclean$hit) %>% 
      prop.table() %>% 
      cbind(count = table(ABclean$hit), proportion = .) %>% 
      print()

    ##   count proportion
    ## 0   967  0.6320261
    ## 1   563  0.3679739

# ── Continuous variable: proper numeric summary ──────────────

    cat("\n=== effectiveRailwayVideo ===\n")

    ## 
    ## === effectiveRailwayVideo ===

    ABclean %>% 
      summarise(
        n       = n(),
        mean    = mean(effectiveRailwayVideo),
        sd      = sd(effectiveRailwayVideo),
        min     = min(effectiveRailwayVideo),
        Q1      = quantile(effectiveRailwayVideo, 0.25),
        median  = median(effectiveRailwayVideo),
        Q3      = quantile(effectiveRailwayVideo, 0.75),
        max     = max(effectiveRailwayVideo)
      ) %>% print()

    ## # A tibble: 1 × 8
    ##       n  mean    sd   min    Q1 median    Q3   max
    ##   <int> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1  1530  6.74  2.35     1     5      7     8    10

# —- VISUALISATIONS —————————————–

# – PLOT 1: Flagging the two error rows ———————-

    p_errors <- ABtestdata %>%
      mutate(
        row_id    = row_number(),
        has_error = !hit %in% c(0, 1) |
                    effectiveRailwayVideo < 1 |
                    effectiveRailwayVideo > 10
      ) %>%
      ggplot(aes(x = row_id, y = effectiveRailwayVideo, colour = has_error)) +
      geom_point(size = 0.5, alpha = 0.4) +
      geom_point(data = . %>% filter(has_error),
                 size = 5, shape = 8, colour = "red") +
      annotate("text",
               x = c(1522, 1524),
               y = c(-10, -10),
               label = c("hit = -1\n(pair 761)", "rating = -10\n(pair 762)"),
               colour = "red", size = 3.2, vjust = 1.2, hjust = 0.5) +
      scale_colour_manual(values = c("FALSE" = "#95a5a6", "TRUE" = "red"),
                          labels = c("Valid", "Error"),
                          guide  = guide_legend(override.aes = list(size = 3))) +
      labs(title    = "Data Validation: Railway Video Ratings",
           subtitle = "Red stars mark the two erroneous observations",
           x = "Row index", y = "effectiveRailwayVideo",
           colour = "Status") +
      theme_minimal(base_size = 13)

    print(p_errors)

![](SoundJudgement_files/figure-markdown_strict/unnamed-chunk-8-1.png)

# – PLOT 2: Proportion of ‘hit’ by music group —————

    p_hit_bar <- ABclean %>%
      count(Music, hit) %>%
      group_by(Music) %>%
      mutate(prop = n / sum(n),
             label_pct = scales::percent(prop, accuracy = 0.1),
             Hit = ifelse(hit == 1, "Will be hit", "Won't be hit")) %>%
      ggplot(aes(x = Music, y = prop, fill = Hit)) +
      geom_col(width = 0.55, colour = "white", linewidth = 0.4) +
      geom_text(aes(label = label_pct),
                position = position_stack(vjust = 0.5),
                size = 4.5, colour = "white", fontface = "bold") +
      scale_fill_manual(values = c("Will be hit"  = "#e74c3c",
                                   "Won't be hit" = "#2ecc71")) +
      scale_y_continuous(labels = scales::percent) +
      labs(title    = "Q1: Proportion believing children will be hit",
           subtitle = "By music condition (cleaned data)",
           x = NULL, y = "Proportion", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")

    print(p_hit_bar)

![](SoundJudgement_files/figure-markdown_strict/unnamed-chunk-9-1.png)

# – PLOT 3: Railway rating distributions ———————

    p_rail_hist <- ABclean %>%
      ggplot(aes(x = effectiveRailwayVideo, fill = Music)) +
      geom_histogram(binwidth = 1, position = "dodge",
                     colour = "white", alpha = 0.85) +
      geom_vline(data = desc_stats,
                 aes(xintercept = mean_railway, colour = Music),
                 linetype = "dashed", linewidth = 1.1) +
      scale_fill_manual(values  = c("Happy & Bright" = "#f39c12",
                                    "Dark & Ominous" = "#2c3e50")) +
      scale_colour_manual(values = c("Happy & Bright" = "#f39c12",
                                     "Dark & Ominous" = "#2c3e50")) +
      scale_x_continuous(breaks = 1:10) +
      labs(title    = "Q2: Railway Video Effectiveness Ratings",
           subtitle = "Dashed lines = group means",
           x = "Rating (1–10)", y = "Count",
           fill = "Music", colour = "Music") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")

    p_rail_violin <- ABclean %>%
      ggplot(aes(x = Music, y = effectiveRailwayVideo, fill = Music)) +
      geom_violin(alpha = 0.45, width = 0.8, colour = NA) +
      geom_boxplot(width = 0.22, alpha = 0.9,
                   outlier.shape = 21, outlier.size = 1.5) +
      scale_fill_manual(values = c("Happy & Bright" = "#f39c12",
                                   "Dark & Ominous" = "#2c3e50")) +
      scale_y_continuous(breaks = 1:10) +
      labs(title = "Q2: Rating Distribution (violin + boxplot)",
           x = NULL, y = "Rating (1–10)", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")

    print(p_rail_hist + p_rail_violin)

![](SoundJudgement_files/figure-markdown_strict/unnamed-chunk-10-1.png)

# ============================================================

# SECTION 2: Q1 — DOES MUSIC AFFECT P(CHILDREN WILL BE HIT)?

# Binary outcome → Beta-Binomial Bayesian + proportion test

# ============================================================

    HB <- ABclean %>% filter(happyBright == 1) %>% pull(hit)
    DO <- ABclean %>% filter(happyBright == 0) %>% pull(hit)

    n_HB <- length(HB); k_HB <- sum(HB)
    n_DO <- length(DO); k_DO <- sum(DO)

    cat(sprintf("\n=== 2. Q1 — HIT PROPORTIONS ===\n"))

    ## 
    ## === 2. Q1 — HIT PROPORTIONS ===

    cat(sprintf("Happy & Bright : %d/%d  = %.4f\n", k_HB, n_HB, k_HB / n_HB))

    ## Happy & Bright : 264/764  = 0.3455

    cat(sprintf("Dark & Ominous : %d/%d  = %.4f\n", k_DO, n_DO, k_DO / n_DO))

    ## Dark & Ominous : 299/766  = 0.3903

# —- 2.1 Frequentist: two-proportion z-test —————–

    freq_q1 <- prop.test(c(k_HB, k_DO), c(n_HB, n_DO),
                         alternative = "two.sided", correct = FALSE)
    cat("\n--- Frequentist Two-Proportion Test ---\n")

    ## 
    ## --- Frequentist Two-Proportion Test ---

    print(freq_q1)

    ## 
    ##  2-sample test for equality of proportions without continuity correction
    ## 
    ## data:  c(k_HB, k_DO) out of c(n_HB, n_DO)
    ## X-squared = 3.2994, df = 1, p-value = 0.06931
    ## alternative hypothesis: two.sided
    ## 95 percent confidence interval:
    ##  -0.09306504  0.00348567
    ## sample estimates:
    ##    prop 1    prop 2 
    ## 0.3455497 0.3903394

# —- 2.2 Bayesian: Beta-Binomial —————————-

# Non-informative prior: Beta(1, 1) = Uniform\[0,1\] for both groups

    prior_a <- 1; prior_b <- 1

    # Posterior update: Beta(alpha + successes, beta + failures)
    a_HB_post <- prior_a + k_HB;   b_HB_post <- prior_b + (n_HB - k_HB)
    a_DO_post <- prior_a + k_DO;   b_DO_post <- prior_b + (n_DO - k_DO)

    cat(sprintf("\n--- Bayesian Beta-Binomial (full data) ---\n"))

    ## 
    ## --- Bayesian Beta-Binomial (full data) ---

    cat(sprintf("Posterior HB : Beta(%d, %d)  =>  mean = %.4f\n",
                a_HB_post, b_HB_post, a_HB_post / (a_HB_post + b_HB_post)))

    ## Posterior HB : Beta(265, 501)  =>  mean = 0.3460

    cat(sprintf("Posterior DO : Beta(%d, %d)  =>  mean = %.4f\n",
                a_DO_post, b_DO_post, a_DO_post / (a_DO_post + b_DO_post)))

    ## Posterior DO : Beta(300, 468)  =>  mean = 0.3906

# Monte Carlo estimate of P(p\_HB &gt; p\_DO)

    set.seed(42)
    N_MC <- 300000
    p_HB_mc <- rbeta(N_MC, a_HB_post, b_HB_post)
    p_DO_mc  <- rbeta(N_MC, a_DO_post, b_DO_post)
    prob_HB_wins_q1 <- mean(p_HB_mc > p_DO_mc)

    cat(sprintf("P(p_HB > p_DO | data)   = %.4f\n", prob_HB_wins_q1))

    ## P(p_HB > p_DO | data)   = 0.0343

    cat(sprintf("95%% Credible Interval HB : [%.4f, %.4f]\n",
                qbeta(0.025, a_HB_post, b_HB_post), qbeta(0.975, a_HB_post, b_HB_post)))

    ## 95% Credible Interval HB : [0.3127, 0.3800]

    cat(sprintf("95%% Credible Interval DO : [%.4f, %.4f]\n",
                qbeta(0.025, a_DO_post, b_DO_post), qbeta(0.975, a_DO_post, b_DO_post)))

    ## 95% Credible Interval DO : [0.3564, 0.4254]

# —- VIZ: Posterior densities Q1 —————————-

    theta_grid <- seq(0.1, 0.7, length.out = 2000)

    post_df_q1 <- data.frame(
      theta = rep(theta_grid, 3),
      density = c(
        dbeta(theta_grid, prior_a,   prior_b),
        dbeta(theta_grid, a_HB_post, b_HB_post),
        dbeta(theta_grid, a_DO_post, b_DO_post)
      ),
      group = rep(c("Prior  Beta(1, 1)",
                    "Posterior: Happy & Bright",
                    "Posterior: Dark & Ominous"), each = 2000)
    )

    p_posterior_q1 <- ggplot(post_df_q1,
                             aes(x = theta, y = density,
                                 colour = group, linetype = group)) +
      geom_line(linewidth = 1.2) +
      scale_colour_manual(values = c("Prior  Beta(1, 1)"          = "#95a5a6",
                                     "Posterior: Happy & Bright"  = "#f39c12",
                                     "Posterior: Dark & Ominous"  = "#2c3e50")) +
      scale_linetype_manual(values = c("Prior  Beta(1, 1)"         = "dashed",
                                       "Posterior: Happy & Bright" = "solid",
                                       "Posterior: Dark & Ominous" = "solid")) +
      annotate("label",
               x = 0.62, y = max(dbeta(theta_grid, a_HB_post, b_HB_post)) * 0.75,
               label = sprintf("P(p_HB > p_DO) = %.3f", prob_HB_wins_q1),
               size = 4.5, colour = "#c0392b", fill = "white", label.size = 0.4,
               fontface = "bold") +
      labs(title    = "Q1: Posterior distributions — P(children will be hit)",
           subtitle = "Beta-Binomial model with non-informative prior Beta(1, 1)",
           x = "Probability of 'hit'", y = "Posterior density",
           colour = "Distribution", linetype = "Distribution") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top",
            legend.text = element_text(size = 10))

    ## Warning in annotate("label", x = 0.62, y = max(dbeta(theta_grid, a_HB_post, : Ignoring unknown
    ## parameters: `label.size`

    print(p_posterior_q1)

![](SoundJudgement_files/figure-markdown_strict/unnamed-chunk-15-1.png)

# —- VIZ: Posterior of difference (p\_HB - p\_DO) —————————–

    diff_q1_mc <- p_HB_mc - p_DO_mc

    p_diff_q1 <- ggplot(data.frame(diff = diff_q1_mc[1:50000]), aes(x = diff)) +
      geom_histogram(aes(y = after_stat(density)), bins = 80,
                     fill = "#3498db", colour = "white", alpha = 0.8) +
      geom_vline(xintercept = 0,
                 colour = "red", linetype = "dashed", linewidth = 1.3) +
      geom_vline(xintercept = mean(diff_q1_mc),
                 colour = "#2c3e50", linewidth = 1.1) +
      annotate("label",
               x = mean(diff_q1_mc) + 0.01,
               y = max(density(diff_q1_mc[1:50000])$y) * 0.7,
               label = sprintf("Mean = %.4f\n95%% CI: [%.4f, %.4f]",
                               mean(diff_q1_mc),
                               quantile(diff_q1_mc, 0.025),
                               quantile(diff_q1_mc, 0.975)),
               hjust = 0, size = 3.8, fill = "white", label.size = 0.3) +
      labs(title    = "Q1: Posterior of (p_HB − p_DO)",
           subtitle = "Area right of 0 = P(p_HB > p_DO | data)",
           x = "Difference in hit probability (HB − DO)", y = "Density") +
      theme_minimal(base_size = 13)

    ## Warning in annotate("label", x = mean(diff_q1_mc) + 0.01, y =
    ## max(density(diff_q1_mc[1:50000])$y) * : Ignoring unknown parameters: `label.size`

    print(p_diff_q1)

![](SoundJudgement_files/figure-markdown_strict/unnamed-chunk-16-1.png)

# ============================================================

# SECTION 3: Q2 — DOES MUSIC AFFECT RAILWAY VIDEO RATINGS?

# Continuous outcome → Normal-Normal Bayesian + Welch t-test

# ============================================================

    rail_HB <- ABclean %>% filter(happyBright == 1) %>% pull(effectiveRailwayVideo)
    rail_DO <- ABclean %>% filter(happyBright == 0) %>% pull(effectiveRailwayVideo)

    xbar_HB <- mean(rail_HB); s_HB <- sd(rail_HB); n2_HB <- length(rail_HB)
    xbar_DO <- mean(rail_DO); s_DO <- sd(rail_DO); n2_DO <- length(rail_DO)

    cat(sprintf("\n=== 3. Q2 — RAILWAY VIDEO RATINGS ===\n"))

    ## 
    ## === 3. Q2 — RAILWAY VIDEO RATINGS ===

    cat(sprintf("Happy & Bright : mean = %.4f, sd = %.4f, n = %d\n", xbar_HB, s_HB, n2_HB))

    ## Happy & Bright : mean = 6.6728, sd = 2.3345, n = 764

    cat(sprintf("Dark & Ominous : mean = %.4f, sd = %.4f, n = %d\n", xbar_DO, s_DO, n2_DO))

    ## Dark & Ominous : mean = 6.7977, sd = 2.3625, n = 766

# —- 3.1 Frequentist: Welch t-test ————————–

    freq_q2 <- t.test(rail_HB, rail_DO,
                      alternative = "two.sided", var.equal = FALSE)
    cat("\n--- Frequentist Welch t-test ---\n")

    ## 
    ## --- Frequentist Welch t-test ---

    print(freq_q2)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  rail_HB and rail_DO
    ## t = -1.0399, df = 1527.9, p-value = 0.2985
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3604162  0.1106656
    ## sample estimates:
    ## mean of x mean of y 
    ##  6.672775  6.797650

# —- 3.2 Bayesian: Normal-Normal (known variance) ———–

# Prior: μ ~ N(μ\_0, σ\_0²)

# Choice: weakly informative — centre of scale, wide spread

    mu0         <- 5.5   # midpoint of 1–10 scale
    sigma0_sq   <- 4     # prior sd = 2, diffuse over the scale

# ‘Known’ σ²: use pooled within-group variance as a plug-in estimate

    sigma_sq <- ((n2_HB - 1) * s_HB^2 + (n2_DO - 1) * s_DO^2) / (n2_HB + n2_DO - 2)
    cat(sprintf("\nPooled SD used as known σ: %.4f\n", sqrt(sigma_sq)))

    ## 
    ## Pooled SD used as known σ: 2.3485

# Conjugate Normal-Normal posterior:

# σ\_n² = 1 / (1/σ\_0² + n/σ²)

# μ\_n = σ\_n² × (μ\_0/σ\_0² + n·x̄/σ²)

    normal_normal_post <- function(n, xbar, mu0, sigma0_sq, sigma_sq) {
      var_n <- 1 / (1 / sigma0_sq + n / sigma_sq)
      mu_n  <- var_n * (mu0 / sigma0_sq + n * xbar / sigma_sq)
      list(mu = mu_n, var = var_n, sd = sqrt(var_n))
    }

    post_HB_q2 <- normal_normal_post(n2_HB, xbar_HB, mu0, sigma0_sq, sigma_sq)
    post_DO_q2 <- normal_normal_post(n2_DO, xbar_DO, mu0, sigma0_sq, sigma_sq)

    cat("\n--- Bayesian Normal-Normal (full data) ---\n")

    ## 
    ## --- Bayesian Normal-Normal (full data) ---

    cat(sprintf("Posterior HB : N(%.4f, %.6f)  =>  95%% CI: [%.4f, %.4f]\n",
                post_HB_q2$mu, post_HB_q2$var,
                post_HB_q2$mu - 1.96 * post_HB_q2$sd,
                post_HB_q2$mu + 1.96 * post_HB_q2$sd))

    ## Posterior HB : N(6.6707, 0.007206)  =>  95% CI: [6.5043, 6.8370]

    cat(sprintf("Posterior DO : N(%.4f, %.6f)  =>  95%% CI: [%.4f, %.4f]\n",
                post_DO_q2$mu, post_DO_q2$var,
                post_DO_q2$mu - 1.96 * post_DO_q2$sd,
                post_DO_q2$mu + 1.96 * post_DO_q2$sd))

    ## Posterior DO : N(6.7953, 0.007188)  =>  95% CI: [6.6292, 6.9615]

# P(μ\_HB &gt; μ\_DO) via Monte Carlo

    mu_HB_mc <- rnorm(N_MC, post_HB_q2$mu, post_HB_q2$sd)
    mu_DO_mc  <- rnorm(N_MC, post_DO_q2$mu, post_DO_q2$sd)
    prob_HB_wins_q2 <- mean(mu_HB_mc > mu_DO_mc)
    diff_q2_mc <- mu_HB_mc - mu_DO_mc

    cat(sprintf("P(μ_HB > μ_DO | data)   = %.4f\n", prob_HB_wins_q2))

    ## P(μ_HB > μ_DO | data)   = 0.1495

    cat(sprintf("Posterior mean diff     = %.4f\n", mean(diff_q2_mc)))

    ## Posterior mean diff     = -0.1246

    cat(sprintf("95%% Credible Interval diff: [%.4f, %.4f]\n",
                quantile(diff_q2_mc, 0.025), quantile(diff_q2_mc, 0.975)))

    ## 95% Credible Interval diff: [-0.3599, 0.1098]

# —- VIZ: Posterior densities Q2 —————————-

    mu_grid <- seq(4.5, 8.0, length.out = 2000)

    post_df_q2 <- data.frame(
      mu = rep(mu_grid, 3),
      density = c(
        dnorm(mu_grid, mu0, sqrt(sigma0_sq)),
        dnorm(mu_grid, post_HB_q2$mu, post_HB_q2$sd),
        dnorm(mu_grid, post_DO_q2$mu, post_DO_q2$sd)
      ),
      group = rep(c("Prior  N(5.5, 4)",
                    "Posterior: Happy & Bright",
                    "Posterior: Dark & Ominous"), each = 2000)
    )

    p_posterior_q2 <- ggplot(post_df_q2,
                             aes(x = mu, y = density,
                                 colour = group, linetype = group)) +
      geom_line(linewidth = 1.2) +
      scale_colour_manual(values = c("Prior  N(5.5, 4)"           = "#95a5a6",
                                     "Posterior: Happy & Bright"  = "#f39c12",
                                     "Posterior: Dark & Ominous"  = "#2c3e50")) +
      scale_linetype_manual(values = c("Prior  N(5.5, 4)"          = "dashed",
                                       "Posterior: Happy & Bright" = "solid",
                                       "Posterior: Dark & Ominous" = "solid")) +
      annotate("label",
               x = max(mu_grid) * 0.94,
               y = max(dnorm(mu_grid, post_HB_q2$mu, post_HB_q2$sd)) * 0.7,
               label = sprintf("P(μ_HB > μ_DO) = %.3f", prob_HB_wins_q2),
               size = 4.5, colour = "#c0392b", fill = "white", label.size = 0.4,
               fontface = "bold", hjust = 1) +
      labs(title    = "Q2: Posterior distributions — Mean railway video rating",
           subtitle = "Normal-Normal conjugate model with weakly informative prior N(5.5, 4)",
           x = "Mean effectiveness rating μ", y = "Posterior density",
           colour = "Distribution", linetype = "Distribution") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top",
            legend.text = element_text(size = 10))

    ## Warning in annotate("label", x = max(mu_grid) * 0.94, y = max(dnorm(mu_grid, : Ignoring
    ## unknown parameters: `label.size`

    print(p_posterior_q2)

![](SoundJudgement_files/figure-markdown_strict/unnamed-chunk-25-1.png)

# —- VIZ: Posterior of difference (μ\_HB - μ\_DO) ————

    p_diff_q2 <- ggplot(data.frame(diff = diff_q2_mc[1:50000]), aes(x = diff)) +
      geom_histogram(aes(y = after_stat(density)), bins = 80,
                     fill = "#9b59b6", colour = "white", alpha = 0.8) +
      geom_vline(xintercept = 0,
                 colour = "red", linetype = "dashed", linewidth = 1.3) +
      geom_vline(xintercept = mean(diff_q2_mc),
                 colour = "#2c3e50", linewidth = 1.1) +
      annotate("label",
               x = mean(diff_q2_mc) + 0.01,
               y = max(density(diff_q2_mc[1:50000])$y) * 0.7,
               label = sprintf("Mean = %.4f\n95%% CI: [%.4f, %.4f]",
                               mean(diff_q2_mc),
                               quantile(diff_q2_mc, 0.025),
                               quantile(diff_q2_mc, 0.975)),
               hjust = 0, size = 3.8, fill = "white", label.size = 0.3) +
      labs(title    = "Q2: Posterior of (μ_HB − μ_DO)",
           subtitle = "Area right of 0 = P(μ_HB > μ_DO | data)",
           x = "Difference in mean rating (HB − DO)", y = "Density") +
      theme_minimal(base_size = 13)

    ## Warning in annotate("label", x = mean(diff_q2_mc) + 0.01, y =
    ## max(density(diff_q2_mc[1:50000])$y) * : Ignoring unknown parameters: `label.size`

    print(p_diff_q2)

![](SoundJudgement_files/figure-markdown_strict/unnamed-chunk-26-1.png)

# ============================================================

# SECTION 4: Q3 — SEQUENTIAL BAYESIAN ANALYSIS

# “Was running the A/B test for two years excessive?”

# 

# IDEA: Process the 766 pairs one at a time, in chronological

# order. After each pair, update the Bayesian posterior and

# record the probability that the ‘Happy & Bright’ arm is

# the winner. If this probability reaches a decisive threshold

# (e.g. 0.95 or 0.05) early, the experiment could have been

# stopped sooner.

# ============================================================

    cat("\n=== 4. SEQUENTIAL BAYESIAN ANALYSIS (Running pair by pair) ===\n")

    ## 
    ## === 4. SEQUENTIAL BAYESIAN ANALYSIS (Running pair by pair) ===

    data_seq <- ABclean %>% arrange(pairwise)
    n_pairs  <- max(data_seq$pairwise)

# Containers for sequential results

    seq_res <- data.frame(
      pair              = seq_len(n_pairs),
      prob_HB_wins_hit  = NA_real_,   # Q1: P(p_HB > p_DO)
      pmean_HB_hit      = NA_real_,   # Q1: posterior mean HB
      pmean_DO_hit      = NA_real_,   # Q1: posterior mean DO
      prob_HB_wins_rail = NA_real_,   # Q2: P(μ_HB > μ_DO)
      pmean_HB_rail     = NA_real_,   # Q2: posterior mean HB
      pmean_DO_rail     = NA_real_    # Q2: posterior mean DO
    )

# — Initialise priors —

# Q1 — Beta-Binomial

    a_HB <- 1; b_HB <- 1
    a_DO <- 1; b_DO <- 1

# Q2 — Normal-Normal (pooled σ² used throughout)

    n_HB2 <- 0; sum_HB2 <- 0
    n_DO2 <- 0; sum_DO2 <- 0

    set.seed(42)
    N_seq <- 15000  # MC draws per step (trade speed for precision)

    cat("Running sequential loop over", n_pairs, "pairs...\n")

    ## Running sequential loop over 766 pairs...

    for (k in seq_len(n_pairs)) {

      pair_k <- data_seq %>% filter(pairwise == k)

      # ---- Q1 update (Beta) ----
      obs_HB_hit <- pair_k %>% filter(happyBright == 1) %>% pull(hit)
      obs_DO_hit <- pair_k %>% filter(happyBright == 0) %>% pull(hit)

      if (length(obs_HB_hit) > 0) {
        a_HB <- a_HB + sum(obs_HB_hit)
        b_HB <- b_HB + sum(1L - obs_HB_hit)
      }
      if (length(obs_DO_hit) > 0) {
        a_DO <- a_DO + sum(obs_DO_hit)
        b_DO <- b_DO + sum(1L - obs_DO_hit)
      }

      p1 <- rbeta(N_seq, a_HB, b_HB)
      p2 <- rbeta(N_seq, a_DO, b_DO)
      seq_res$prob_HB_wins_hit[k] <- mean(p1 > p2)
      seq_res$pmean_HB_hit[k]     <- a_HB / (a_HB + b_HB)
      seq_res$pmean_DO_hit[k]     <- a_DO / (a_DO + b_DO)
      
      # ---- Q2 update (Normal) ----
      
    obs_HB_rail <- pair_k %>% filter(happyBright == 1) %>% pull(effectiveRailwayVideo)
      obs_DO_rail <- pair_k %>% filter(happyBright == 0) %>% pull(effectiveRailwayVideo)

      if (length(obs_HB_rail) > 0) {
        n_HB2   <- n_HB2 + length(obs_HB_rail)
        sum_HB2 <- sum_HB2 + sum(obs_HB_rail)
      }
      if (length(obs_DO_rail) > 0) {
        n_DO2   <- n_DO2 + length(obs_DO_rail)
        sum_DO2 <- sum_DO2 + sum(obs_DO_rail)
      }

      if (n_HB2 > 0 && n_DO2 > 0) {
        post_HB_s <- normal_normal_post(n_HB2, sum_HB2 / n_HB2, mu0, sigma0_sq, sigma_sq)
        post_DO_s <- normal_normal_post(n_DO2, sum_DO2 / n_DO2, mu0, sigma0_sq, sigma_sq)

        m1 <- rnorm(N_seq, post_HB_s$mu, post_HB_s$sd)
        m2 <- rnorm(N_seq, post_DO_s$mu, post_DO_s$sd)
        seq_res$prob_HB_wins_rail[k] <- mean(m1 > m2)
        seq_res$pmean_HB_rail[k]     <- post_HB_s$mu
        seq_res$pmean_DO_rail[k]     <- post_DO_s$mu
      }
    }

    cat("Sequential loop complete.\n")

    ## Sequential loop complete.

# —- When did the probability first cross decisive thresholds? —-

    THRESHOLD_HIGH <- 0.95
    THRESHOLD_LOW  <- 0.05

    first_cross_q1 <- seq_res %>%
      filter(prob_HB_wins_hit >= THRESHOLD_HIGH | prob_HB_wins_hit <= THRESHOLD_LOW) %>%
      slice(1)

    first_cross_q2 <- seq_res %>%
      filter(!is.na(prob_HB_wins_rail) &
             (prob_HB_wins_rail >= THRESHOLD_HIGH | prob_HB_wins_rail <= THRESHOLD_LOW)) %>%
      slice(1)

    cat(sprintf("\nQ1 — first pair where P crosses 0.95 or 0.05: pair %s (of 766)\n",
                ifelse(nrow(first_cross_q1) > 0, first_cross_q1$pair, "never")))

    ## 
    ## Q1 — first pair where P crosses 0.95 or 0.05: pair 10 (of 766)

    cat(sprintf("Q2 — first pair where P crosses 0.95 or 0.05: pair %s (of 766)\n",
                ifelse(nrow(first_cross_q2) > 0, first_cross_q2$pair, "never")))

    ## Q2 — first pair where P crosses 0.95 or 0.05: pair 16 (of 766)

# —- VIZ: Sequential P(HB wins) — Q1 ———————–

    p_seq_q1 <- ggplot(seq_res, aes(x = pair, y = prob_HB_wins_hit)) +
      geom_line(colour = "#e74c3c", linewidth = 0.7) +
      geom_hline(yintercept = THRESHOLD_HIGH,
                 linetype = "dashed", colour = "#27ae60", linewidth = 0.9) +
      geom_hline(yintercept = THRESHOLD_LOW,
                 linetype = "dashed", colour = "#27ae60", linewidth = 0.9) +
      geom_hline(yintercept = 0.5,
                 linetype = "dotted", colour = "#7f8c8d", linewidth = 0.8) +
      {if (nrow(first_cross_q1) > 0)
        geom_vline(xintercept = first_cross_q1$pair,
                   linetype = "solid", colour = "#8e44ad", linewidth = 1)} +
      annotate("text", x = 20, y = THRESHOLD_HIGH + 0.02,
               label = "0.95 — decisive evidence for HB",
               hjust = 0, size = 3.2, colour = "#27ae60") +
      annotate("text", x = 20, y = THRESHOLD_LOW - 0.02,
               label = "0.05 — decisive evidence for DO",
               hjust = 0, size = 3.2, colour = "#27ae60") +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(title    = "Q1 Sequential Analysis: P(p_HB > p_DO | data so far)",
           subtitle = paste("Purple line = first pair crossing a decisive threshold;",
                            "Test ran to pair 766"),
           x = "Pairwise allocations accumulated", y = "P(Happy & Bright wins | hit)") +
      theme_minimal(base_size = 13)

    print(p_seq_q1)

![](SoundJudgement_files/figure-markdown_strict/unnamed-chunk-33-1.png)

# —- VIZ: Sequential P(HB wins) — Q2 ———————–

    p_seq_q2 <- ggplot(seq_res %>% filter(!is.na(prob_HB_wins_rail)),
                       aes(x = pair, y = prob_HB_wins_rail)) +
      geom_line(colour = "#3498db", linewidth = 0.7) +
      geom_hline(yintercept = THRESHOLD_HIGH,
                 linetype = "dashed", colour = "#27ae60", linewidth = 0.9) +
      geom_hline(yintercept = THRESHOLD_LOW,
                 linetype = "dashed", colour = "#27ae60", linewidth = 0.9) +
      geom_hline(yintercept = 0.5,
                 linetype = "dotted", colour = "#7f8c8d", linewidth = 0.8) +
      {if (nrow(first_cross_q2) > 0)
        geom_vline(xintercept = first_cross_q2$pair,
                   linetype = "solid", colour = "#8e44ad", linewidth = 1)} +
      annotate("text", x = 20, y = THRESHOLD_HIGH + 0.02,
               label = "0.95 — decisive evidence for HB",
               hjust = 0, size = 3.2, colour = "#27ae60") +
      annotate("text", x = 20, y = THRESHOLD_LOW - 0.02,
               label = "0.05 — decisive evidence for DO",
               hjust = 0, size = 3.2, colour = "#27ae60") +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(title    = "Q2 Sequential Analysis: P(μ_HB > μ_DO | data so far)",
           subtitle = paste("Purple line = first pair crossing a decisive threshold;",
                            "Test ran to pair 766"),
           x = "Pairwise allocations accumulated",
           y = "P(Happy & Bright wins | railway rating)") +
      theme_minimal(base_size = 13)

    print(p_seq_q2)

![](SoundJudgement_files/figure-markdown_strict/unnamed-chunk-34-1.png)

# —- VIZ: Evolving posterior means (Q2) ———————

    p_means_seq <- seq_res %>%
      filter(!is.na(pmean_HB_rail)) %>%
      pivot_longer(cols = c(pmean_HB_rail, pmean_DO_rail),
                   names_to  = "group",
                   values_to = "post_mean") %>%
      mutate(group = ifelse(group == "pmean_HB_rail", "Happy & Bright", "Dark & Ominous")) %>%
      ggplot(aes(x = pair, y = post_mean, colour = group)) +
      geom_line(linewidth = 0.8) +
      scale_colour_manual(values = c("Happy & Bright" = "#f39c12",
                                     "Dark & Ominous" = "#2c3e50")) +
      labs(title    = "Q2 Sequential: Evolving posterior mean (railway rating)",
           subtitle = "How the estimated group means converge as data accumulates",
           x = "Pairwise allocations accumulated",
           y = "Posterior mean effectiveness rating",
           colour = "Music group") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")

    print(p_means_seq)

![](SoundJudgement_files/figure-markdown_strict/unnamed-chunk-35-1.png)

# ============================================================

# FINAL SUMMARY TABLE

# ============================================================

    cat("\n\n========================================================\n")

    ## 
    ## 
    ## ========================================================

    cat("  FINAL SUMMARY\n")

    ##   FINAL SUMMARY

    cat("========================================================\n\n")

    ## ========================================================

    cat("[Q1] Does music affect P(children will be hit by car)?\n")

    ## [Q1] Does music affect P(children will be hit by car)?

    cat(sprintf("  Frequentist p-value           : %.5f\n", freq_q1$p.value))

    ##   Frequentist p-value           : 0.06931

    cat(sprintf("  Interpretation                : %s\n",
                ifelse(freq_q1$p.value < 0.05,
                       "Statistically significant at 5% level",
                       "Not statistically significant at 5% level")))

    ##   Interpretation                : Not statistically significant at 5% level

    cat(sprintf("  P(p_HB > p_DO | data)         : %.4f\n", prob_HB_wins_q1))

    ##   P(p_HB > p_DO | data)         : 0.0343

    cat(sprintf("  Posterior means HB / DO       : %.4f / %.4f\n",
                a_HB_post / (a_HB_post + b_HB_post),
                a_DO_post / (a_DO_post + b_DO_post)))

    ##   Posterior means HB / DO       : 0.3460 / 0.3906

    cat("\n[Q2] Does music affect railway video effectiveness rating?\n")

    ## 
    ## [Q2] Does music affect railway video effectiveness rating?

    cat(sprintf("  Frequentist p-value           : %.5f\n", freq_q2$p.value))

    ##   Frequentist p-value           : 0.29854

    cat(sprintf("  Interpretation                : %s\n",
                ifelse(freq_q2$p.value < 0.05,
                       "Statistically significant at 5% level",
                       "Not statistically significant at 5% level")))

    ##   Interpretation                : Not statistically significant at 5% level

    cat(sprintf("  P(μ_HB > μ_DO | data)         : %.4f\n", prob_HB_wins_q2))

    ##   P(μ_HB > μ_DO | data)         : 0.1495

    cat(sprintf("  Posterior means HB / DO       : %.4f / %.4f\n",
                post_HB_q2$mu, post_DO_q2$mu))

    ##   Posterior means HB / DO       : 6.6707 / 6.7953

    cat("\n[Q3] Was two years excessive?\n")

    ## 
    ## [Q3] Was two years excessive?

    cat(sprintf("  Q1 first decisive pair        : %s / 766\n",
                ifelse(nrow(first_cross_q1) > 0, first_cross_q1$pair, "never crossed")))

    ##   Q1 first decisive pair        : 10 / 766

    cat(sprintf("  Q2 first decisive pair        : %s / 766\n",
                ifelse(nrow(first_cross_q2) > 0, first_cross_q2$pair, "never crossed")))

    ##   Q2 first decisive pair        : 16 / 766

    cat("  Conclusion: see sequential plots — if a threshold was crossed\n")

    ##   Conclusion: see sequential plots — if a threshold was crossed

    cat("  well before pair 766, the trial could have been stopped earlier.\n")

    ##   well before pair 766, the trial could have been stopped earlier.

    cat("\n========================================================\n")

    ## 
    ## ========================================================
