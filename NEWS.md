# climateYear (development version)

- new output `climateYearsUsed`, a table with one row per simulation year and climate variable giving the climate year used, whether it was historical or projected, the source file and layer it was read from, and the per-year file written (`yearFile`), so the climate used in each simulation year can be traced later. Every year's file is kept.
- `sampleYear()` no longer returns a year before the data when `samplingRange` has a single value: `sample(2003, 1)` draws from `1:2003`; it now indexes the range with `sample.int()`.

# climateYear 0.0.1 (07 January 2026)

- initial module version
