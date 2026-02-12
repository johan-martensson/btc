# btc

[![GitHub](https://img.shields.io/github/license/johan-martensson/btc)](https://github.com/johan-martensson/btc)

Bitcoin price tracker written in Haskell. Includes a CLI tool and a browser-based GUI dashboard.

**Repository:** [github.com/johan-martensson/btc](https://github.com/johan-martensson/btc)

Uses the [CoinGecko](https://www.coingecko.com/) free API with automatic fallback to [Binance](https://www.binance.com/) public API. No API keys required.

## Building

Requires GHC with the `time` library (included in standard GHC installations).

```bash
make
```

Or individually:

```bash
ghc -O2 -o btcprice btcprice.hs
ghc -O2 -o btcgui btcgui.hs
```

## Usage

### CLI (`btcprice`)

```bash
# Current price in USD, EUR, GBP
./btcprice

# Daily price stats for a date range
./btcprice 2025-10-01 2026-01-01
```

Example output:

```
=== Bitcoin Daily Prices: 2025-12-01 to 2026-01-15 ===

Date        Price (USD)     Change
----------------------------------------
2025-12-01  $90,359.87
2025-12-02  $86,281.50      -4.51%
2025-12-03  $91,320.08      +5.84%
...
----------------------------------------

=== Summary ===
  High:    $96,933.53
  Low:     $85,450.33
  Average: $89,832.48
  Change:  +7.27%
```

### GUI (`btcgui`)

```bash
# Default: last 90 days
./btcgui

# Custom date range
./btcgui 2025-10-01 2026-01-01
```

Generates an interactive HTML dashboard and opens it in your browser. Features:

- Current price (USD, EUR, GBP)
- Preset range buttons: **1D**, **1W**, **1M**, **1Y**, **All**
- Interactive date range picker — change the period and click **Fetch** to reload data live from the browser
- Line chart with hover tooltips (Chart.js)
- Period statistics (high, low, average, change %)
- Scrollable daily price table with color-coded changes
- Dark theme

## Data Sources

Both tools try [CoinGecko](https://www.coingecko.com/en/api) first. If it fails (rate limit, timeout, or error), they automatically fall back to [Binance](https://developers.binance.com/docs/binance-spot-api-docs/rest-api) public endpoints. This applies to:

- Current price fetching (CLI and GUI startup)
- Historical data fetching (CLI and GUI startup)
- Live date range updates in the GUI (client-side JavaScript)

CoinGecko's free tier is limited to the last 365 days. The Binance fallback has no such limit — long ranges are fetched automatically via pagination (1000-day chunks).
