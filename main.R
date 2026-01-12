## main.R — VaR & Expected Shortfall (historical + Gaussian) with backtesting
## Author: <your name>
## Notes:
## - VaR/ES are returned BOTH as return-quantities (usually negative) and as positive loss magnitudes.

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyquant)
  library(lubridate)
  library(conflicted)
  library(memoise)
})

conflict_prefer("filter", "dplyr")
conflict_prefer("lag",    "dplyr")
conflict_prefer("first",  "dplyr")
conflict_prefer("last",   "dplyr")

# Data: download prices + log-returns

# Cache tq_get calls to reduce repeated downloads (peer selection can be heavy)
.tq_get_cached <- memoise(function(ticker, from, to) {
  tidyquant::tq_get(ticker, from = from, to = to, get = "stock.prices")
})

get_stock_returns <- function(ticker, years_back = 3) {
  end_date   <- Sys.Date()
  start_date <- end_date - lubridate::years(years_back)
  
  prices <- suppressWarnings(
    try(.tq_get_cached(ticker, start_date, end_date), silent = TRUE)
  )
  
  if (inherits(prices, "try-error") || nrow(prices) == 0) {
    stop("Could not download data for ticker '", ticker, "'.")
  }
  
  prices %>%
    arrange(date) %>%
    mutate(ret = log(adjusted / dplyr::lag(adjusted))) %>%
    filter(!is.na(ret))
}

# Risk metrics: historical + Gaussian (parametric)

compute_var_es_hist <- function(returns, conf_level = 0.99) {
  if (!is.numeric(returns) || length(returns) < 250) {
    stop("Not enough return observations to compute VaR/ES.")
  }
  
  alpha <- 1 - conf_level
  VaR_ret <- as.numeric(stats::quantile(returns, probs = alpha, na.rm = TRUE))
  ES_ret  <- mean(returns[returns <= VaR_ret], na.rm = TRUE)
  
  list(
    model     = "historical",
    conf_level = conf_level,
    alpha      = alpha,
    VaR_ret    = VaR_ret,
    ES_ret     = ES_ret,
    VaR_loss   = -VaR_ret,
    ES_loss    = -ES_ret
  )
}

compute_var_es_gaussian <- function(returns, conf_level = 0.99) {
  if (!is.numeric(returns) || length(returns) < 250) {
    stop("Not enough return observations to compute VaR/ES.")
  }
  
  alpha <- 1 - conf_level
  mu <- mean(returns, na.rm = TRUE)
  s  <- stats::sd(returns, na.rm = TRUE)
  
  z <- stats::qnorm(alpha)
  VaR_ret <- mu + s * z
  # ES for left tail under Normal
  ES_ret  <- mu - s * stats::dnorm(z) / alpha
  
  list(
    model      = "gaussian",
    conf_level = conf_level,
    alpha      = alpha,
    VaR_ret    = VaR_ret,
    ES_ret     = ES_ret,
    VaR_loss   = -VaR_ret,
    ES_loss    = -ES_ret
  )
}

# Backtesting: hits + Kupiec + Christoffersen

# hits_t = 1 if return_t <= VaR_ret_t
make_hits <- function(returns, var_series_ret) {
  if (length(returns) != length(var_series_ret)) stop("Length mismatch in hits computation.")
  as.integer(returns <= var_series_ret)
}

kupiec_uc_test <- function(n_exceed, n_obs, alpha) {
  if (n_obs <= 0 || is.na(n_exceed)) return(list(LR = NA_real_, p_value = NA_real_))
  pi_hat <- n_exceed / n_obs
  if (pi_hat <= 0 || pi_hat >= 1) return(list(LR = NA_real_, p_value = NA_real_))
  
  # log-likelihoods to avoid underflow
  ll0 <- (n_obs - n_exceed) * log(1 - alpha) + n_exceed * log(alpha)
  ll1 <- (n_obs - n_exceed) * log(1 - pi_hat) + n_exceed * log(pi_hat)
  
  LR <- -2 * (ll0 - ll1)
  pval <- 1 - stats::pchisq(LR, df = 1)
  list(LR = LR, p_value = pval)
}

christoffersen_ind_test <- function(hits) {
  # Independence test (LRind), based on 2-state Markov transitions
  hits <- as.integer(hits)
  if (length(hits) < 3) return(list(LR = NA_real_, p_value = NA_real_))
  
  h0 <- hits[-length(hits)]
  h1 <- hits[-1]
  
  n00 <- sum(h0 == 0 & h1 == 0)
  n01 <- sum(h0 == 0 & h1 == 1)
  n10 <- sum(h0 == 1 & h1 == 0)
  n11 <- sum(h0 == 1 & h1 == 1)
  
  # transition probabilities
  p01 <- if ((n00 + n01) > 0) n01 / (n00 + n01) else NA_real_
  p11 <- if ((n10 + n11) > 0) n11 / (n10 + n11) else NA_real_
  p   <- (n01 + n11) / (n00 + n01 + n10 + n11)
  
  if (is.na(p01) || is.na(p11) || p <= 0 || p >= 1) {
    return(list(LR = NA_real_, p_value = NA_real_))
  }
  if (p01 <= 0 || p01 >= 1 || p11 <= 0 || p11 >= 1) {
    return(list(LR = NA_real_, p_value = NA_real_))
  }
  
  ll_indep <- (n00 + n10) * log(1 - p) + (n01 + n11) * log(p)
  ll_markov <- n00 * log(1 - p01) + n01 * log(p01) + n10 * log(1 - p11) + n11 * log(p11)
  
  LR <- -2 * (ll_indep - ll_markov)
  pval <- 1 - stats::pchisq(LR, df = 1)
  list(LR = LR, p_value = pval)
}

# Rolling VaR backtest (out-of-sample)
rolling_backtest <- function(returns, conf_level = 0.99, window = 250, model = c("historical", "gaussian")) {
  model <- match.arg(model)
  n <- length(returns)
  if (n <= window + 5) stop("Not enough data for rolling backtest.")
  
  alpha <- 1 - conf_level
  VaR_ret <- rep(NA_real_, n)
  
  for (t in (window + 1):n) {
    past <- returns[(t - window):(t - 1)]
    if (model == "historical") {
      VaR_ret[t] <- as.numeric(stats::quantile(past, probs = alpha, na.rm = TRUE))
    } else {
      mu <- mean(past, na.rm = TRUE)
      s  <- stats::sd(past, na.rm = TRUE)
      VaR_ret[t] <- mu + s * stats::qnorm(alpha)
    }
  }
  
  hits <- make_hits(returns, VaR_ret)
  # evaluate only OOS part
  oos_hits <- hits[(window + 1):n]
  n_obs <- length(oos_hits)
  n_exc <- sum(oos_hits == 1, na.rm = TRUE)
  
  uc  <- kupiec_uc_test(n_exc, n_obs, alpha)
  ind <- christoffersen_ind_test(oos_hits)
  
  list(
    model = model,
    conf_level = conf_level,
    window = window,
    alpha = alpha,
    VaR_ret = VaR_ret,
    hits = hits,
    n_obs = n_obs,
    n_exceed = n_exc,
    expected_exceed = n_obs * alpha,
    kupiec_LR = uc$LR,
    kupiec_pval = uc$p_value,
    christoffersen_LR = ind$LR,
    christoffersen_pval = ind$p_value
  )
}

# Peer selection: same sector, then most correlated (no arbitrary slice unless asked)

find_peers_sp500 <- function(main_ticker, main_data, years_back, n_peers = 3, max_candidates = Inf) {
  spx_raw <- tidyquant::tq_index("SP500")
  
  sector_col <- grep("sector", names(spx_raw), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(sector_col)) stop("No sector column found in tq_index('SP500').")
  
  spx <- tibble(
    ticker = spx_raw$symbol,
    sector = spx_raw[[sector_col]]
  ) %>%
    filter(!is.na(ticker), ticker != "", ticker != "-")
  
  main_row <- spx %>% filter(ticker == main_ticker)
  if (nrow(main_row) == 0) stop("Ticker ", main_ticker, " not found in S&P 500 index table.")
  
  main_sector <- main_row$sector[1]
  
  candidates <- spx %>%
    filter(sector == main_sector, ticker != main_ticker)
  
  if (is.finite(max_candidates) && nrow(candidates) > max_candidates) {
    # practical limit: reduce API calls (explained)
    candidates <- candidates %>% slice_head(n = max_candidates)
  }
  
  message("Computing correlations to select peers in sector… (this can take a bit)")
  
  corr_tbl <- purrr::map_dfr(candidates$ticker, function(tk) {
    dat <- try(get_stock_returns(tk, years_back = years_back), silent = TRUE)
    if (inherits(dat, "try-error")) return(tibble(ticker = tk, corr = NA_real_))
    
    merged <- inner_join(
      main_data %>% select(date, ret_main = ret),
      dat       %>% select(date, ret_peer = ret),
      by = "date"
    )
    if (nrow(merged) < 100) return(tibble(ticker = tk, corr = NA_real_))
    
    tibble(
      ticker = tk,
      corr = stats::cor(merged$ret_main, merged$ret_peer, use = "complete.obs")
    )
  }) %>%
    filter(!is.na(corr)) %>%
    arrange(desc(corr))
  
  if (nrow(corr_tbl) == 0) {
    peers <- candidates %>% slice_sample(n = min(n_peers, nrow(candidates))) %>% pull(ticker)
  } else {
    peers <- corr_tbl %>% slice_head(n = min(n_peers, nrow(corr_tbl))) %>% pull(ticker)
  }
  
  list(sector = main_sector, peers = peers)
}

# Console debrief

print_debrief <- function(summary_tbl, conf_level, main_ticker, invest_amount) {
  alpha <- 1 - conf_level
  
  cat("\nRisk comparison summary (", round(conf_level * 100), "% VaR / ES)\n", sep = "")
  cat("--------------------------------------------------------\n")
  print(summary_tbl)
  cat("--------------------------------------------------------\n\n")
  
  ordered_es <- summary_tbl %>% arrange(ES_ret) # more negative ES_ret = worse
  worst <- ordered_es %>% slice(1)
  best  <- ordered_es %>% slice(n())
  
  cat("Tail risk (ES):\n")
  cat("  worst:", worst$ticker, " | ES_ret=", sprintf("%.4f", worst$ES_ret),
      " | ES_loss=", sprintf("%.4f", worst$ES_loss), "\n", sep = "")
  cat("  best :", best$ticker,  " | ES_ret=", sprintf("%.4f", best$ES_ret),
      " | ES_loss=", sprintf("%.4f", best$ES_loss), "\n\n", sep = "")
  
  main_row <- summary_tbl %>% filter(ticker == main_ticker)
  if (nrow(main_row) == 1) {
    cat("Notional:", format(invest_amount, big.mark = ","), "USD\n")
    cat("  VaR_loss:", format(round(main_row$VaR_loss * invest_amount, 0), big.mark = ","), "USD\n")
    cat("  ES_loss :", format(round(main_row$ES_loss  * invest_amount, 0), big.mark = ","), "USD\n\n")
  }
  
  cat("Backtest note:\n")
  cat("  Kupiec = coverage rate, Christoffersen = clustering/independence.\n\n")
}

# Main CLI

run_cli <- function() {
  cat("VaR & Expected Shortfall — Stock Comparison Tool\n")
  
  ticker_input <- toupper(trimws(readline("Enter a US stock ticker (ex: AAPL, MSFT, NVDA, SPY..): ")))
  if (ticker_input == "") stop("No ticker provided.")
  
  lookback_input <- readline("Lookback horizon in YEARS (ex: 1, 3, 5) [default = 3]: ")
  lookback_years <- suppressWarnings(as.numeric(lookback_input))
  if (is.na(lookback_years) || lookback_years <= 0) lookback_years <- 3
  
  conf_input <- readline("VaR confidence level (ex: 0.95, 0.99) [default = 0.99]: ")
  conf_level <- suppressWarnings(as.numeric(conf_input))
  if (is.na(conf_level) || conf_level <= 0 || conf_level >= 1) conf_level <- 0.99
  
  notional_input <- readline("Notional in USD for P&L illustration [default = 10000000]: ")
  invest_amount  <- suppressWarnings(as.numeric(notional_input))
  if (is.na(invest_amount) || invest_amount <= 0) invest_amount <- 1e7
  
  cat("\nDownloading data and computing returns...\n")
  
  main_data <- get_stock_returns(ticker_input, years_back = lookback_years)
  
  # Models on full sample (descriptive)
  hist <- compute_var_es_hist(main_data$ret, conf_level)
  gaus <- compute_var_es_gaussian(main_data$ret, conf_level)
  
  # Peers
  peer_info <- find_peers_sp500(
    main_ticker = ticker_input,
    main_data   = main_data,
    years_back  = lookback_years,
    n_peers     = 3,
    max_candidates = 80 # limit API calls (practical)
  )
  peer_tickers <- peer_info$peers
  sector_name  <- peer_info$sector
  
  cat("Sector:", sector_name, "\n")
  cat("Peers :", paste(peer_tickers, collapse = ", "), "\n\n")
  
  # Summaries
  rows <- list(
    tibble(
      ticker = ticker_input,
      model  = "historical",
      n_obs  = nrow(main_data),
      mean_ret = mean(main_data$ret, na.rm = TRUE),
      sd_ret   = sd(main_data$ret, na.rm = TRUE),
      VaR_ret = hist$VaR_ret, ES_ret = hist$ES_ret,
      VaR_loss = hist$VaR_loss, ES_loss = hist$ES_loss
    ),
    tibble(
      ticker = ticker_input,
      model  = "gaussian",
      n_obs  = nrow(main_data),
      mean_ret = mean(main_data$ret, na.rm = TRUE),
      sd_ret   = sd(main_data$ret, na.rm = TRUE),
      VaR_ret = gaus$VaR_ret, ES_ret = gaus$ES_ret,
      VaR_loss = gaus$VaR_loss, ES_loss = gaus$ES_loss
    )
  )
  
  for (tk in peer_tickers) {
    dat <- get_stock_returns(tk, years_back = lookback_years)
    h <- compute_var_es_hist(dat$ret, conf_level)
    
    rows[[length(rows) + 1]] <- tibble(
      ticker = tk,
      model  = "historical",
      n_obs  = nrow(dat),
      mean_ret = mean(dat$ret, na.rm = TRUE),
      sd_ret   = sd(dat$ret, na.rm = TRUE),
      VaR_ret = h$VaR_ret, ES_ret = h$ES_ret,
      VaR_loss = h$VaR_loss, ES_loss = h$ES_loss
    )
  }
  
  summary_tbl <- bind_rows(rows) %>%
    mutate(across(c(mean_ret, sd_ret, VaR_ret, ES_ret, VaR_loss, ES_loss), ~round(.x, 5)))
  
  print_debrief(summary_tbl, conf_level, ticker_input, invest_amount)
  
  # Rolling backtest on main ticker (OOS)
  cat("Running rolling backtest on main ticker (window=250)…\n")
  bt_hist <- rolling_backtest(main_data$ret, conf_level, window = 250, model = "historical")
  bt_gaus <- rolling_backtest(main_data$ret, conf_level, window = 250, model = "gaussian")
  
  bt_tbl <- tibble(
    model = c(bt_hist$model, bt_gaus$model),
    n_obs = c(bt_hist$n_obs, bt_gaus$n_obs),
    n_exceed = c(bt_hist$n_exceed, bt_gaus$n_exceed),
    expected = c(bt_hist$expected_exceed, bt_gaus$expected_exceed),
    kupiec_p = c(bt_hist$kupiec_pval, bt_gaus$kupiec_pval),
    christoffersen_p = c(bt_hist$christoffersen_pval, bt_gaus$christoffersen_pval)
  )
  
  cat("\nBacktest (main ticker):\n")
  print(bt_tbl)
  
  # Plots
  plots_dir <- "figures"
  if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
  
  # VaR line plot (historical, in-sample) for main ticker
  var_png <- file.path(plots_dir, paste0("var_hist_", ticker_input, "_", round(conf_level * 100), "pct.png"))
  
  p_var <- ggplot(main_data, aes(x = date, y = ret)) +
    geom_line(color = "grey30") +
    geom_hline(yintercept = hist$VaR_ret, color = "red", linetype = "dashed") +
    labs(
      title = paste0("Daily log-returns + ", round(conf_level * 100), "% VaR (historical) — ", ticker_input),
      x = "Date", y = "Log-return"
    ) +
    theme_minimal()
  
  ggsave(var_png, plot = p_var, width = 8, height = 4.5, dpi = 300)
  cat("Saved:", var_png, "\n")
  
  # Cumulative performance (log-returns => exp(cumsum)-1 is the correct version)
  main_series <- main_data %>% select(date, ret) %>% mutate(ticker = ticker_input)
  
  peer_series <- purrr::map_dfr(peer_tickers, function(tk) {
    get_stock_returns(tk, years_back = lookback_years) %>%
      select(date, ret) %>%
      mutate(ticker = tk)
  })
  
  all_series <- bind_rows(main_series, peer_series) %>%
    group_by(ticker) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(cum_ret = exp(cumsum(ret)) - 1) %>%
    ungroup()
  
  cumperf_png <- file.path(plots_dir, paste0("cumperf_", ticker_input, "_vs_peers.png"))
  
  p_cum <- ggplot(all_series, aes(x = date, y = cum_ret, colour = ticker)) +
    geom_line() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste0("Cumulative performance — ", ticker_input, " vs peers"),
      x = "Date", y = "Cumulative return", colour = "Ticker"
    ) +
    theme_minimal()
  
  ggsave(cumperf_png, plot = p_cum, width = 8, height = 4.5, dpi = 300)
  cat("Saved:", cumperf_png, "\n")
}

if (sys.nframe() == 0) {
  run_cli()
}