# Species dictionary for Canadian tree code systems

A canonical species reference table derived from the raw
cross-jurisdiction species coding source used by the package.

## Usage

``` r
species_dictionary
```

## Format

A tibble with one row per canonical NFI species code and the following
columns:

- NFI_code:

  Canonical NFI species code.

- CommonNameEnglish:

  English common name, when available.

- CommonNameFrench:

  French common name, when available.

- ScientificName:

  Scientific name, when available.

- Genus:

  Four-letter NFI genus code.

- Species:

  Three-letter NFI species code or `"SPP"`.

- Var:

  Optional three-letter variety or subspecies code.
