# Crosswalk among NFI, CANFI, and jurisdiction-specific species codes

A normalized lookup table linking package species entries to the code
systems used across Canadian jurisdictions and inventories.

## Usage

``` r
species_code_lookup
```

## Format

A tibble with the following columns:

- code_system:

  Code system name: `"canfi"` or `"jurisdiction"`.

- jurisdiction:

  Jurisdiction identifier for provincial/territorial codes; `NA` for
  CANFI codes.

- code:

  The code value in the specified code system.

- NFI_code:

  Canonical NFI species code for the linked entry.
