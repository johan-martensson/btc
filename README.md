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

Date        Price (USD)     Volume (USD)          Change
------------------------------------------------------------
2025-12-01  $90,359.87      $37,064,742,380
2025-12-02  $86,281.50      $21,163,124,465       -4.51%
2025-12-03  $91,320.08      $50,705,395,176       +5.84%
...
------------------------------------------------------------

=== Summary ===
  High:       $96,933.53
  Low:        $85,450.33
  Average:    $89,832.48
  Change:     +7.27%
  Total Vol:  $1,443,270,103,254
  Avg Vol:    $45,102,190,727
```

### GUI (`btcgui`)

```bash
# Default: last 90 days
./btcgui

# Custom date range
./btcgui 2025-10-01 2026-01-01
```

Generates an interactive HTML dashboard and opens it in your browser. Features:

- Current price (USD, EUR, GBP) and 24h trading volume
- Preset range buttons: **1D**, **1W**, **1M**, **3M**, **1Y**, **All**
- Interactive date range picker — change the period and click **Fetch** to reload data live from the browser
- Line chart with hover tooltips (Chart.js) and **volume bars** on secondary Y-axis
- **Bitcoin Power Law** overlay — support, fair value, and resistance corridor lines with 20% forward projection
- **Log Scale** toggle (Y-axis logarithmic)
- **Log-Log** chart mode — days since genesis on X-axis (log) vs. price (log), where the power law appears as straight lines; tooltips show actual date, fair value, support, and resistance prices
- Period statistics (high, low, average, change %, avg daily volume, total volume)
- Scrollable daily price table with volume and color-coded changes
- Dark theme

## Data Sources

Both tools try [CoinGecko](https://www.coingecko.com/en/api) first. If it fails (rate limit, timeout, or error), they automatically fall back to [Binance](https://developers.binance.com/docs/binance-spot-api-docs/rest-api) public endpoints. This applies to:

- Current price and 24h volume fetching (CLI and GUI startup)
- Historical price and daily volume fetching (CLI and GUI startup)
- Live date range updates in the GUI (client-side JavaScript)

CoinGecko's free tier is limited to the last 365 days. The Binance fallback has no such limit — long ranges are fetched automatically via pagination (1000-day chunks).
