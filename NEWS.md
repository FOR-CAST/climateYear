# climateYear (development version)

- the `getClimate` event now removes the previous year's `year<YYYY>_<runName>_<pid>.tif` before writing the new one, so one file is kept per run instead of one per simulated year (about 72 MB each). `currentClimateRasters` is unchanged and still has its own file.

# climateYear 0.0.1 (07 January 2026)

- initial module version
