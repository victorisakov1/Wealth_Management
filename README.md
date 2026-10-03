# Wealth Management

> **University coursework.** These projects were built for wealth and investment management
> courses at Hult International Business School. They show what I learned at the time, not
> production code, and they are not financial advice.

Investment and portfolio analysis projects in R and Python.

## Projects

| Folder | What it does | Tools |
|---|---|---|
| `AI for Investments/` | Detects market regimes with a hidden Markov model, then optimizes portfolio weights from each regime's returns and risk | Python, hmmlearn, SciPy |
| `Fisher_Investments_Portfolio_Management/` | Analyzes the top 10 holdings of a Fisher Investments portfolio: volatility, Sharpe ratio and tracking error against the NASDAQ | R, quantmod |
| `Ultra_High_Net_Worth_Individual_Wealth_Management/` | Recommends a portfolio for an ultra high net worth client, with risk analysis and optimization. The PDF is the written report | R, quantmod, quadprog |

## Getting started

**Python notebook:** install Python 3.10 or newer, then:

```bash
pip install -r requirements.txt
jupyter notebook
```

The notebook downloads its price data from GitHub, so it needs an internet connection.

**R scripts:** open the script in RStudio and install the packages it loads with
`install.packages()`. The scripts download prices from Yahoo Finance through quantmod, so results
change as new data comes in.

## License

Licensed under the [Apache License 2.0](LICENSE).
