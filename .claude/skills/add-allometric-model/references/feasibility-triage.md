# Feasibility triage

The first substantive step, before full extraction. A **fast skim** to answer one
question: *can this model be faithfully implemented from this document?* It is a
go/no-go decision, deliberately cheaper and shallower than Stage 1 extraction — the
goal is to avoid investing in extraction (and the user's review) only to discover
the parameters live in a companion report or the tables are unreadable.

## How to skim

- Extract prose with `pdftotext -layout` / `pdftools::pdf_text()` to read the
  methods, model form, and scope quickly.
- Locate the parameter table(s); render the relevant page(s) to an image to confirm
  the coefficients are actually there and legible (see `extraction-rules.md`). Do
  **not** transcribe every value yet — just confirm presence, completeness, legibility.
- Note the citation from the rendered text (filenames can be wrong).

## Checklist

| Check | What "pass" looks like |
|-------|------------------------|
| Model form | The equation(s) are given explicitly; both directions if invertible |
| Parameters present | Coefficient table(s) appear *in this document*, not "see [other report]" |
| Parameter coverage | Rows cover the species / regions the model claims to support |
| Units | Units stated for every variable (diameter, height, age, volume) |
| Age basis / base age | Stated (SI models): total vs breast-height age; base age |
| Validation | A worked example / reference table (fidelity), OR a viable same-family plausibility comparison exists |
| Legibility | Text-based, or scanned but tables readable when rendered |

### Additional checks for DBH–height (`hd_`) models

| Check | What "pass" looks like |
|-------|------------------------|
| Direction | Predicts **height from DBH**. A regression of DBH on height (e.g. Alberta T/606–T/608) is **NO-GO** for `hd_` — never invert it. |
| Height convention | Total height vs height above breast height stated; breast-height offset (1.3 vs 1.37 m) identifiable in the model form |
| Units | DBH (cm vs mm) and height (m) units stated |
| Extra predictors defined | Every predictor beyond DBH (`BA`, `TPH`, `HDOM`, `QMD`, `BAL`, `CCF`, `si`, region/class codes, climate) has a computation rule: tree-size threshold, all-species vs species-specific, top-height definition, plot size where relevant |
| Fit type | Fixed-effects (OLS/NLS) vs mixed model is clear; for mixed models, which coefficients the source recommends for prediction without local data (fixed fit vs mixed fixed effects with u = 0) |
| Applicability ranges | Data ranges per species (DBH, height, stand variables) located, or noted as absent |

If a predictor's definition cannot be pinned down, the verdict is at best
**GO WITH GAPS** — do not guess a definition or substitute a proxy.

## Verdict

Emit exactly one, each item with a one-line justification:

- **GO** — all checks pass; everything needed is present and legible.
  → **Name the function** (`prefix_<author><year>`, confirming the citation from the
  rendered text) and proceed to Stage 1 extraction.
- **GO WITH GAPS** — implementable, but something is missing or weak. Examples:
  parameters split across a companion report; no fidelity benchmark (Tier-2 only);
  a subset of claimed species not tabulated. → List the gaps explicitly and
  **consult the user** on how to resolve (source the companion doc? accept a
  plausibility-only validation and flag it? narrow the species scope?) before
  proceeding.
- **NO-GO** — cannot be faithfully implemented from this document. Examples: the
  model form is incomplete; coefficients are not published here at all; tables are
  illegible and no clean copy is available. → Explain why and **stop**; ask the user
  for a better source or a different scope.

## Why this gate matters (validated)

- A paper may describe *national curves* while the full coefficient tables live in a
  separate technical report — extraction would stall or produce partial parameters.
- Scanned PDFs can have tables that OCR badly; triage catches "not faithfully
  transcribable" before extraction, not after.
- The validation tier (fidelity vs plausibility) is partly decided here: if no
  reference values exist, that is a GO-WITH-GAPS conversation up front, not a
  surprise at test-writing time.
