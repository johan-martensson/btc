# btc

Bitcoin price tracker written in Haskell. Includes a CLI tool and a browser-based GUI dashboard.

Uses the [CoinGecko](https://www.coingecko.com/) free API (no API key required, limited to last 365 days of history).

## Building

Requires GHC with the `time` library (included in standard GHC installations).

```bash
ghc -o btcprice btcprice.hs
ghc -o btcgui btcgui.hs
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

Generates an interactive HTML dashboard and opens it in your browser. Includes:

- Current price (USD, EUR, GBP)
- Interactive price chart (Chart.js)
- Period statistics (high, low, average, change)
- Daily price table with color-coded changes
