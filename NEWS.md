# climateYear (development version)

- each simulation year's climate now has its own small `.vrt` (about 1 KB) that points at the band of the source stack, instead of a full `.tif` copy (about 72 MB). The files are written to `file.path(outputPath(sim), "climate")` as `climate_simYear<sim year>_climYear<climate year>.vrt` (one per simulation year, even if a climate year is reused) instead of next to the source stacks. `currentClimateRasters` is read from it, with the same values, names, extent and crs. Sources that are not single files on disk are still copied to a `.tif`. The module now lists `sf` and `terra` in `reqdPkgs`.
- new output `climateYearsUsed`, a table with one row per simulation year and climate variable giving the climate year used, whether it was historical or projected, the source file and its md5 (computed once per file per run), the layer read, and the per-year file (`yearFile`), also written to `climateYearsUsed.csv` in `outputPath(sim)` (rewritten every year, so a killed run keeps its finished years), so the climate used in each simulation year can be traced and a changed or moved source stack detected.
- `sampleYear()` no longer returns a year before the data when `samplingRange` has a single value: `sample(2003, 1)` draws from `1:2003`; it now indexes the range with `sample.int()`.

# climateYear 0.0.1 (07 January 2026)

- initial module version
