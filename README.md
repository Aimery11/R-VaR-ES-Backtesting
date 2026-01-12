# R-VaR-ES-Backtesting

Recruiter-friendly R project that computes **Value at Risk (VaR)** and **Expected Shortfall (ES)** on US equities, compares a ticker to sector peers, and runs **rolling backtests** (Kupiec + Christoffersen).  
Outputs are saved as PNG charts in `figures/`.



## What this project does

Given a US ticker (e.g., `AAPL`):

1. Downloads daily adjusted prices from Yahoo Finance (via `tidyquant`)
2. Computes daily **log-returns**
3. Computes **VaR/ES** under:
   - **Historical** (empirical quantile + tail average)
   - **Gaussian** (parametric normal)
4. Finds **S&P 500 peers** in the same sector, then selects the most correlated peers
5. Runs **rolling out-of-sample VaR backtests** with window = 250 trading days:
   - **Kupiec unconditional coverage test**
   - **Christoffersen independence test**
6. Saves two figures:
   - `figures/var_hist_<TICKER>_<CONF>pct.png`
   - `figures/cumperf_<TICKER>_vs_peers.png`



## Quick start (recommended for recruiters)

Example: ticker = `AAPL`, lookback = 3 years, confidence = 0.99, notional = 10,000,000.

From the project root:

```r
source("run.R")
	

**What `run.R` does**
- Ensures required packages are available (installs missing ones in interactive mode)
- If `renv.lock` is present: restores the environment via `renv::restore(prompt = FALSE)`
- Runs `main.R` and launches the CLI (`run_cli()`)
	
Output

Saved to figures/:
	•	figures/var_hist_<TICKER>_<CONF>pct.png (daily log-returns + historical VaR line)
	•	figures/cumperf_<TICKER>_vs_peers.png (cumulative performance vs peers)

Notes / assumptions
	•	Data source: Yahoo Finance via tidyquant::tq_get()
	•	Rolling window: 250 trading days
	•	For speed, peer search limits candidate tickers (practical API call cap)
	
## Example outputs

![VaR plot](figures/var_hist_AAPL_99pct.png)
![Cumulative performance](figures/cumperf_AAPL_vs_peers.png)


## Reproducibility (renv)

If you want strict reproducibility, recreate the environment with:

```r
install.packages("renv")
renv::restore()


