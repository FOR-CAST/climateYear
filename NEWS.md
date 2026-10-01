# climateYear (development version)

- the `getClimate` event now removes the previous year's `year<YYYY>_<runName>_<pid>.tif` before writing the new one, so one file is kept per run instead of one per simulated year (about 72 MB each). `currentClimateRasters` is unchanged and still has its own file.
- `sampleYear()` no longer returns a year before the data when `samplingRange` has a single value: `sample(2003, 1)` draws from `1:2003`; it now indexes the range with `sample.int()`.

# climateYear 0.0.1 (07 January 2026)

- initial module version
