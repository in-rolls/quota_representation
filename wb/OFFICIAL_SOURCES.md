# Official West Bengal reservation sources

The pinned official-source exports add reservation descriptions, but no observed later GP-head winner sex.
No persistence model is estimated. Reservation assignments and elected GP-member seats have different units.

The module checks 5 parsed exports and 68 original source files by SHA-256 before writing outputs.
The availability ledger records district, year, stage, tier, physical rows and observed axes. Mention counts are not GP counts.
Positive-list nonlisting and a blank opposite reservation axis remain unknown; neither becomes UR or woman_reserved=0.
No candidate or winner sex is inferred from names. Alipurduar 2018 results concern GP members, not Pradhans.

## Nadia 2013

The complete synopsis yields 187 head/deputy pairs, with 186 pairs having both women-reservation statuses observed.
Pairs use the source document's GP serial key, not a fuzzy name join. Both original names and codes remain in the pair export.
Repeated GP names remain distinct. Head serial 123 has a source-blank code and stays missing.
Explicit UR and full category codes in this complete synopsis support its negative axes; that rule is not applied to positive lists.
The source audit was performed by one assistant, was non-blinded, and is not human double entry.
The category and women-axis cross-tabs describe contemporaneous office allocation, not subsequent political persistence.

## South 24 Parganas 2018

Draft/final positive sets match only on tier, block, GP name and category after case/whitespace normalization.
No punctuation removal, fuzzy matching or cross-year linking is used. A draft-only/final-only mention is not a verified change to UR.
Shared positive mentions: 425; draft-only: 0; final-only: 0.
The deputy source's printed totals disagree with its visually enumerated named cells. This comparison preserves named evidence and does not force it to totals.

## Run and verify

From the wb directory, with the source checkout supplied explicitly:

```r
source('scripts/03_official_sources.R')
wb_official_sources(Sys.getenv('WB_LOCAL_ELECTIONS'), '.')
```

Run `WB_LOCAL_ELECTIONS=/path/to/local_elections Rscript tests/test_official_sources.R` for checksum, join and absence-policy checks.
Inputs are pinned in `official_source_manifest.csv`; outputs are `data/official_*.csv` and `tabs/official_*.csv`.
The module performs no downloads and no updates to observed source values. Updating a checksum requires an explicit source review.
