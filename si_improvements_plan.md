# SI registry, discovery, and wrapper improvement plan

**Current age-category policy:** Auger-Ward and Prégent models use `total_age`
for input and reference ages. Their ages remain years since planting, without
a nursery-age adjustment. This supersedes the separate `plantation_age`
category described in the historical planning notes below.

## Goal

Make site-index model selection scientifically explicit and maintainable, then
provide discovery helpers and an optional `si()` dispatcher analogous to `hd()`.
Preserve all existing model equations, coefficients, numerical behaviour, engine
signatures, and engine return types.

This document is a handoff for separate agent sessions, not authorization to
implement every phase at once. Run phases sequentially in the same worktree, or
merge each phase before starting its dependants. Do not have separate agents edit
the registry concurrently.

## Working rules for every agent

- Read `AGENTS.md`, `.codex_instructions`, this plan, and relevant local
  instructions before acting.
- Inspect the current worktree and preserve unrelated user changes.
- Work only on the requested phase. Do not launch other agents or workflows.
- Inspect actual code and source evidence; the observations below are starting
  points, not substitutes for verification.
- Do not add models, change equations, alter parameter data, or introduce
  dependencies as part of this work.
- Preserve existing registry IDs and columns unless an explicitly reviewed
  scientific correction requires a change. Prefer additive metadata.
- Do not hand-edit `man/*.Rd` or `R/sysdata.rda`. Regenerate documentation using
  the repository's configured roxygen2 version when needed.
- Do not build the website or edit generated `docs/` output by default.
- Use focused tests and existing package tooling. Do not create tooling merely
  for this project. Documentation-only planning requires no tests.
- Do not commit unless the user requests it.
- At completion, update only this document's phase status and handoff section
  with verified decisions, affected files, checks, and unresolved questions.
  Keep the design readable; do not turn it into a command transcript.
- Stop and request a decision if evidence is ambiguous or an implementation
  choice would change scientific interpretation or public behaviour.

## Baseline and evidence

At planning time, `si_model_registry()` contains 51 variants representing 45
engines. Reconfirm these counts before implementation.

| Family | Variants | Behaviour |
|---|---:|---|
| Conventional height-age models | 47 | Age + height -> SI; age + SI -> height |
| Growth-intercept models | 2 | Age + growth intercept -> SI only |
| Ecological-site curves | 2 | Ecological keys + age -> height and fixed-curve IQS |

Important existing files:

- `R/si_model_registry.R`: registry, species coverage, existing fixed arguments.
- `R/hd_model_registry.R`: row constructors, predictor metadata, discovery
  functions, and registry consistency checks to use as prior art.
- `R/hd.R`, `R/vol.R`: existing dispatcher contracts and output shapes.
- `tests/testthat/test-si_model_registry.R`: current registry expectations.
- `tests/testthat/test-hd_model_registry.R`: validation/discovery test patterns.
- `vignettes/articles/Site-Index-Models.qmd`: a current registry consumer.
- `R/si_*.R`, associated tests, and source publications/specifications:
  scientific definitions and engine contracts.

Known issues to investigate:

- Prégent 2010/2016 accept plantation age but are registered with breast-height
  age. Auger-Ward 2021 also uses plantation age rather than generic total age.
  See registry `age_basis` and the engines' age documentation.
- Laflèche 2013 uses corrected age above 1 m, not breast-height age. It does
  not invert measured height to estimate SI or consume an arbitrary SI value.
- `nigh2000_gi` is registered as supporting height prediction, but its engine
  accepts `age` and `gi` and returns SI only.
- Scott-Voorhis registry age labels need reconciliation with the implementation:
  `convert_to_total_age = TRUE` adds estimated years to breast height to the
  supplied age. Do not infer input age basis from a variant ID.
- Huang 2009 accepts total input age even when SI is indexed to breast-height
  age. Input age basis and SI reference-age basis need separate metadata.
- Several Sharma engines return SI above breast height even when the height
  argument and height predictions use total height.
- Prégent and Auger-Ward defaults use SI25; registry plot settings may use
  SI50. Plotting choices must not become undocumented selection defaults.
- Huang 2026 climate returns inherent HI, not necessarily realized height at
  reference age, and requires four correctly aggregated climate inputs.
- Nigh 2009 exposes base, operational, and zonal variants but has only one
  current registry entry.
- Ontario black spruce has multiple candidates with rank 89. Rank alone does
  not encode stand origin, composition, or empirical superiority.
- Existing subregion metadata does not fully represent ecological keys,
  conditional inputs, coast/interior applicability, or discrete GI age domains.

## Scientific selection contract

Keep these concepts distinct:

1. Input age clock: breast-height, total, since planting, or corrected above 1 m.
2. SI reference age and its age clock.
3. SI vertical datum: total height versus height above breast height.
4. Height sampling definition and population: top/dominant/codominant height,
   sampling rules, natural/plantation origin, and stand composition where known.
5. Productivity quantity: conventional SI, climate inherent HI, or ecological IQS.
6. Operation: infer SI from measured height, predict height from supplied SI,
   infer SI from GI, or evaluate an ecological curve.

Unknown metadata is not evidence of applicability. Record uncertainty explicitly;
never infer source support from missing restrictions.

Compatibility must be checked before ranking. Do not silently convert age
clocks, substitute stand origins, change SI scales, or switch between conventional
SI and inherent HI. Do not assume one model's SI is interchangeable with
another's, even if both share a reference age and units.

## Phases and dependencies

### Phase 0: Audit and agree the metadata contract

**Status:** Audit complete; schema and verified-only Phase 1 scope approved.
**Depends on:** Nothing.

Read-only research and design; do not edit R code.

Deliverables:

- Inventory every registered variant against engine signatures, documentation,
  existing tests, and available publication evidence.
- Record verified discrepancies with file/line or publication locators.
- Propose a minimal metadata schema covering the selection contract, native
  defaults, operation capabilities, predictor requirements, and uncertainty.
- Distinguish publication evidence, current engine behaviour, and registry
  plotting overrides. Flag contradictions rather than choosing silently.
- Identify registry consumers affected by new age-basis values or fields.
- Separate corrections that can preserve IDs from cases needing reviewed new
  variants or clarified legacy IDs.
- Add the inventory, schema proposal, and unresolved decisions to this document.

**Acceptance:** Every current variant is accounted for; unresolved scientific
questions are explicit.

**Approval gate:** Stop for user approval of the schema, interpretation of
disputed age definitions, and any proposed ID/variant changes before Phase 1.

### Phase 1: Correct verified metadata

**Status:** Complete — verified-only corrections, affected consumers and focused regressions implemented.
**Depends on:** Approved Phase 0.

Implement only reviewed metadata corrections and the minimum additive fields
needed to express them. Keep structural refactoring and ranking changes out.

Deliverables:

- Correct age clocks and prediction capabilities supported by evidence.
- Represent SI definitions and stand applicability using the approved contract.
- Preserve plotting-specific fixed arguments unless their change is explicitly
  approved; label their purpose rather than treating them as engine defaults.
- Update dependent consumer logic where new values would otherwise produce
  misleading labels or behaviour, including article age-axis labels if needed.
- Add focused regression tests for corrected scientific metadata and update
  documentation/NEWS as appropriate.

**Acceptance:** Corrections have evidence and regression tests; engines and
parameter data remain untouched; affected consumers handle new metadata.

### Phase 2: Refactor registry construction

**Status:** Complete — per-variant definitions implemented; complete registry and species-expanded outputs verified identical.
**Depends on:** Phase 1.

Replace parallel column vectors with one self-contained row definition per
registered variant, following the HD registry's general pattern.

Deliverables:

- Small SI-specific row constructor and model-definition helper.
- Preserve Phase 1 IDs, ordering, column types, list-column contents, ranks,
  fixed arguments, and species coverage.
- Preserve the public `si_model_registry()` interface and internal species
  expansion behaviour.
- Compare complete before/after registry outputs, not just row counts.

**Acceptance:** Registry construction changes, but registry meaning and values
do not. No additional variants, ranking policy, or engine changes in this phase.

### Phase 3: Add predictor metadata and consistency checks

**Status:** Complete — operation-aware contracts, conditional constraints and consistency checks implemented with focused regressions.
**Depends on:** Phase 2 and approved Phase 0 schema.

Create one authoritative representation of SI predictor requirements. Reuse HD
patterns where appropriate, but do not assume HD's single-operation requirements
can express SI's alternative and conditional inputs.

Deliverables:

- Predictor definitions with units, scientific meaning, evidence locators, and
  required/optional status by operation or explicit registered variant.
- Express height-versus-SI alternatives, GI-only requirements, ecological keys,
  and climate requirements without falsely requiring every input in every mode.
- Represent discrete age domains and region-dependent requirements where needed.
- Add only Phase 0-approved variant entries; preserve legacy IDs and document
  any compatibility mapping.
- Consistency checks for duplicate IDs/predictors, invalid engine arguments,
  missing required engine inputs, fixed-argument conflicts, incompatible
  capabilities, and required scientific metadata.
- Focused invariant tests plus representative conditional-requirement tests.

**Acceptance:** Every variant has an auditable operation/input contract; checks
detect incomplete or contradictory definitions. No prediction dispatcher yet.

### Phase 4: Add discovery helpers

**Status:** Complete — discovery/info helpers, scientific near-misses, focused tests and generated API documentation implemented.
**Depends on:** Phase 3.

Implement `find_si_models()` and `get_si_model_info()` without automatically
executing or selecting a model.

Deliverables:

- Discovery by species, jurisdiction, requested operation, and available inputs.
- Scientific compatibility filters using the approved metadata; distinguish
  unavailable information from a known mismatch.
- Keep near-misses visible with missing inputs and incompatibility reasons.
- Clearly distinguish name-level input availability from validated row-level
  eligibility. Merely having a predictor column does not prove its values or
  scientific definition are suitable.
- Model information exposing native SI definitions, stand applicability,
  predictor definitions, evidence, and limitations.
- Include all model families in discovery, labelled by operation/productivity
  quantity; do not imply all are conventional dispatcher candidates.
- Tests, roxygen documentation, examples, and NEWS.

**Acceptance:** Users can see applicable models, exclusions, missing information,
and scientific definitions without reading engine internals.

**Approval gate:** Review discovery output and the proposed wrapper API,
ranking/tie-break policy, output SI scale, and fallback rules before Phase 5.

### Phase 5: Implement the initial conventional SI wrapper

**Status:** Complete — approved native-scale conventional dispatcher, provenance, diagnostics and direct-engine equivalence tests implemented.
**Depends on:** Phase 4 and the wrapper approval recorded in the handoff.

Initial scope: conventional non-climate height-age/SI models only. Leave
growth-intercept models, ecological-site curves, and climate inherent HI
discoverable but outside ordinary automatic selection.

Decisions to approve before coding:

- Explicit age-basis and requested SI-definition inputs; whether native scales
  are preserved or a documented wrapper-level scale is offered.
- `si()` input naming and both prediction directions.
- Explicit model-ID selection for reproducibility and trajectory consistency.
- Deterministic preference/tie-break rules; ranks are pragmatic preferences,
  not evidence that a model is statistically best.
- Policy for unknown stand origin, missing geographic detail, missing values,
  out-of-domain predictions, and extrapolation.
- Output provenance fields and failure reporting.
- Whether selection is observation-wise or can be held fixed for a
  stand/trajectory; avoid accidental model switching within projections.

Expected implementation:

- Compatibility filtering before ranking.
- Vectorized/recycled inputs and tidyverse-safe return shapes.
- `pick_best` and `keep_model_id` controls consistent with existing wrappers.
- One tibble row per observation for best-model mode; per-observation candidate
  tibbles for comparison mode, with SI-definition provenance.
- Explicit candidate failures/fallback reasons. Do not silently catch arbitrary
  errors and report a successful alternative with no explanation.
- Do not silently fall back across incompatible scientific definitions.
- Focused tests for selection, ties, explicit model IDs, direction, recycling,
  missing data, scientific incompatibility, and failures.
- Numerical equivalence to direct engine calls; retain existing benchmark tests.

**Acceptance:** Selection is explainable and reproducible; wrapper predictions
match direct calls on the documented scale; existing engines remain unchanged.

### Phase 6: Consider specialized dispatchers or extensions

**Status:** Deferred. **Depends on:** Phase 5 and a separate user decision.

Evaluate GI, ecological-site productivity, and climate-aware operations
independently. Decide whether each fits `si()` or warrants a separate entry point.
Do not treat them as ordinary fallback models merely because they return `si`.

## Verification strategy

- Phase 0: evidence review and complete inventory; no code validation needed.
- Phases 1-3: targeted SI registry tests and directly affected consumer/model
  tests; structural equivalence checks for Phase 2.
- Phase 4: registry and discovery tests, including near-misses and incompatible
  definitions.
- Phase 5: dispatcher tests and numerical comparisons against direct engines.
- For documentation changes, regenerate through the configured toolchain and
  inspect generated diffs for unintended churn.
- Inspect existing CI/package tooling and run required relevant checks. Broaden
  testing when cross-package changes or failures justify it; perform full
  package checks before releasing the completed feature.
- Record validation results in the handoff; report blockers plainly.

## Handoff log

### Planning

- Plan created; no implementation phases started.
- Metadata and ranking observations above were identified from current code.
  Publication-level reconciliation remains Phase 0 work.
- No engine changes, parameter changes, new models, or automatic agent execution
  are authorized by creation of this document.

### Phase 0 — 2026-11-10: inventory and proposed contract

**Outcome:** All 51 registered variants / 45 distinct engines accounted for.
The inventory below is an audit, not approval of any scientific correction.
Only this plan was edited. No R code, coefficients, tests, generated
documentation, website output, or model IDs were changed.

#### Evidence scope and interpretation

- Read registry construction and species expansion, inspected all 45 public
  signatures and their documented operation contracts, reviewed existing
  per-engine tests and relevant implementation branches, and examined local
  publications and extraction specifications. Every engine has an existing
  test file; the 46 `test-si_*.R` files include the registry tests.
- Primary PDFs are available under the **git-ignored** `sources/site_index/`,
  with Nigh 2004 under `tmp/si_papers/` and Huang 2026 under `sources/`.
  An ordinary ignore-aware file search does not inventory these publications.
  Text extraction and selected rendered pages were used; no outside service
  received source documents. Session-only extracted text/images are not new
  package artifacts.
- Evidence labels below: **E** = current engine documentation/signature/code;
  **T** = existing test assertions (not a new test result);
  **P** = primary publication passage examined; **S** = existing extraction
  specification, secondary evidence where the original detail was not
  independently reconciled in this audit. A reference in an R comment or a
  formula copied into a test is not independent publication validation.
- `R/si_model_registry.R:13-65,118-223,278-330,384-436,468-572`
  establishes the inventory, fixed arguments, age labels and capability flags;
  `:901-1093` supplies ranks and species sources. Species expansion
  (`:1106-1170`) prefers manual coverage over parameter-derived coverage.
  Expanded coverage was inspected for every row, not inferred from whether
  `species` happens to be an engine argument.
- More precise family accounting than the planning baseline is **46
  non-climate conventional height-age variants, 1 climate inherent-HI variant,
  2 GI variants, and 2 ecological-curve variants**. The baseline's 47
  height-age variants included climate HI; it must not imply SI-scale
  interchangeability.

#### Complete variant inventory

Reading key:

- `B`, `T`, `P`, `C` = breast-height, total/from-seed, since-planting, and
  corrected-above-1-m age clocks. `50B` means a 50-year BH reference age,
  **not** necessarily a BH input age. `?` marks an unresolved source
  interpretation, not a permissive match.
- `H/S` = infer SI from age + measured height, or predict height from age +
  supplied SI (exactly one of height/SI). `G` = age + GI -> SI only.
  `ECO` = age + ecological key -> both height and fixed-curve IQS.
- All heights/SI are metres; `total` and `above BH` describe **vertical
  datums**, not height sampling. `sp` means the engine requires `species`.
  Single-species engines without that argument still require species
  compatibility during discovery.
- For a row's engine stem `x`, **E** locators mean `R/si_x.R:<lines>` and
  **T** locators mean `tests/testthat/test-si_x.R:<lines>`. Normally the stem
  is the model ID; exceptions are explicitly named. For `sharma2015`, the
  test filename is `test-si_sharmaetal2015.R`.
- Population summaries are verified restrictions or evidence gaps, not
  complete ecological eligibility rules. Unstated origin/composition/sampling
  remains unknown. Existing province lists remain legacy registry coverage;
  they are not independently validated geographic transfer domains.

| # | Registered ID (engine stem if different) | Input clock -> native SI reference / datum | Operation, required extras and native options | Population / evidence and discrepancy |
|---|---|---|---|---|
| 1 | `buckman2006` | T -> 50T / total | H/S; no sp | Red pine; Lake States; source uses mean dominant/codominant height, age from seed. E:19-48; T:1-35,75; P: Buckman 2006, printed pp.15,18-19 (PDF pp.31,34-35). Registry ON is a Canadian use scope, not source calibration geography. |
| 2 | `carmean1989` | T -> 50T / total, documented for implemented subset | H/S; sp; 11 registered species | Eastern-US compilation; do not infer one sampling/origin rule across its component sources. E:7-35; T:1-35; P: Carmean 1989, introduction (PDF pp.6-7), distinguishes the usual total-age convention from exceptions. Component-source eligibility remains incompletely specified. |
| 3 | `carmean1996` | B -> 50B / total | H/S; sp; 7 species | Northwest Ontario; separate fitted inverse equations for some species, not universal round-trip identities. E:12-39; T:68,225; P: TR-105 appendices III-IV (PDF pp.133-135); E:17-22 explicitly records this distinction. Source compilation requires species-level provenance rather than a blanket exact-inverse claim. |
| 4 | `carmeanhahn1981` | T -> 50T / total | H/S; sp; fir/white spruce | Lake States conversion from BH to total-age curves. E:12-30; T:1-35; P: Carmean and Hahn 1981, first page, abstract/methods. Registry ON is not the original study area. |
| 5 | `cieszewskibella1991` | B -> 50B / total | H/S; sp; 4 species | Alberta dominant/codominant stem analyses; variable-age source specialized to 50B in engine. E:10-26; T:69,203; P: Forest Management Note 51, methods (PDF p.3), curves (PDF p.5). No age-label discrepancy established. |
| 6 | `kerbowling1991` | B -> 50B / total | H/S; sp; 4 softwoods | NB dominant/codominant trees; source allows some short budworm suppression history, so do not assert all training trees were suppression-free. E:23-28; T:1-40; P: Ker and Bowling 1991, pp.728-729 (rendered PDF pp.1-2). |
| 7 | `lundgrendolid1970_exponential_monomolecular` (`lundgrendolid1970`) | Engine says B -> 50B / total; P supports species-dependent clocks | H/S; sp; `variant="exponential_monomolecular"` fixed | 11 Lake States species. E:21-25,97-110; T:1-40; P: NC-36 Table 1, pp.3-4 (PDF pp.6-7). Only balsam fir/white spruce explicitly use BH ages. White-spruce form is not recommended by authors; see D8. |
| 8 | `lundgrendolid1970_monomolecular` (`lundgrendolid1970`) | Same species-clock conflict as #7; reference 50 on source-specific clock | H/S; sp; `variant="monomolecular"` fixed | Same 11 species. E:21-25,97-110; T:1-40; P: NC-36 pp.4-5, Table 3. Preserve alternative form; do not convert ages or silently remove white spruce. D8. |
| 9 | `nigh2000` | B -> 50B / total | H/S; no sp | Interior BC western redcedar; four largest suitable cedar trees per 0.04-ha plot, averaged by BHA. E:10-30,56; T:1-35; P: RR18 methods (PDF pp.10-12). BC-interior applicability is not an engine input. |
| 10 | `nigh2000_gi` | B -> 50B / total | G; `gi` cm/year; integer ages 1:50 | Same interior cedar population; age-specific GI, not arbitrary height increment. E:6-16,30-59; T:1-32,34-64; P: RR18 eq.1 (PDF p.12), age-specific fitting (PDF p.16), Table 4. Registry incorrectly advertises height prediction and lacks finite/discrete age metadata. D3. |
| 11 | `nighcourtin1998_si25` (`nighcourtin1998`) | B -> 25B / total | H/S; `si50=FALSE` fixed and native | Coastal BC red alder; dominant height, three suitable dominant trees per 0.04 ha, pure stands. E:9-31; T:1-32; P: Nigh and Courtin 1998, methods (PDF p.3), eqs.5-6. |
| 12 | `nighcourtin1998_si50` (`nighcourtin1998`) | B -> converted 50B scale / total | H/S; `si50=TRUE` fixed; engine native is FALSE | Same population; engine's affine SI25/SI50 conversion is not proof that H(50) equals supplied SI50 exactly. E:27-31; T:33-65,236; P: paper directly establishes SI25; conversion's independent provenance not established here. Preserve ID and explicitly label converted scale. |
| 13 | `huang1994` | B -> 50B / total | H/S; **sp required**; `natural_subregion="All"` | 8 Alberta species; optional natural-region grouping. E:11-29,75-81; T:39-80; P: T/291 p.7, eq.4 definitions (existing PDF-p.15 image). Registry `requires_species=FALSE` is wrong. D4. |
| 14 | `augerward2021` | **P** -> 25P / total | H/S; sp; `base_age=25`, configurable | QC jack pine/black spruce plantations; 100 tallest planted trees/ha. E:11-23,33-43; T:36-85; P: corrected SSS-06, PDF pp.4,8,20-21, equations p.24. Registry T label is too broad; no registry SI50 override exists. D1. |
| 15 | `pregent2010` | **P** -> 25P / total | H/S; no sp; native `base_age=25`; registry fixes 50 for plots | QC white spruce plantations, 100 tallest trees/ha; published curves to 60 years. E:11-25,35-42; T:33-85; P: Memoire 160 section 1.2 (PDF pp.25-26), eq.4. Registry B label and unlabeled plotting override need correction. D1,D5. |
| 16 | `pregent2016` | **P** -> 25P / total | H/S; no sp; native `base_age=25`; registry fixes 50 for plots | QC Norway spruce plantations, 100 tallest trees/ha; published tables to 70 years. E:11-25,35-42; T:33-84; P: Memoire 176 section 1.2 (PDF pp.26-27), eq.4. D1,D5. |
| 17 | `sharma2021` | B -> 50B / **above BH** | H/S; sp; `base_age=50`, `total_height=TRUE` | ON natural-origin mixed stands; measured/predicted height defaults to total, SI never gains 1.3 m. E:10-24,35-52,137,217-222; T:1-32,60-100; P: mixed PJ/SB paper, methods PDF p.2 and SI definition p.4. D6. |
| 18 | `sharmaparton2018a` | B -> 50B engine default / **above BH**; source uses 25B | H/S; no sp; `base_age=50`, `total_height=TRUE` | ON even-aged monospecific white-spruce plantations; average of three sampled dominant/codominant trees/site. E:9-24,37-52,120,178-183; T:1-30,56-95; P: white-spruce paper PDF pp.2-4. Source confirms the above-BH scale that engine docs call an assumption. D6. |
| 19 | `sharmaparton2018b` | B -> 50B engine default / **above BH**; source uses 25B | H/S; no sp; `base_age=50`, `total_height=TRUE` | ON red-pine plantations; largest suitable planted tree sampled per plot, three plots/site. E:9-24,37-52,119,177-182; T:1-30,56-95; P: red-pine paper PDF pp.2-3. D6. |
| 20 | `sharmaparton2019` | B -> 50B engine default / **above BH**; source illustrates 25B | H/S; no sp; `base_age=50`, `total_height=TRUE` | ON white-pine plantations; sampled dominant/codominant height trajectories. E:9-24,37-52,127,185-190; T:1-30,56-95; P: white-pine paper PDF pp.2-4; source/engine base-age distinction E:20-24. D6. |
| 21 | `sharma2015` | B -> 50B engine default / **above BH**; source uses 25B | H/S; sp; `base_age=50`, `total_height=TRUE` | ON jack pine/black spruce plantations; mean of three dominant/codominant trees/plot. E:9-25,36-53,125,205-210; T:1-32,57-95; P: Sharma et al.2015 methods and Reference Age for SI (PDF pp.3-4). D6. |
| 22 | `parresolvissage1998` | B documented -> 50B engine default / total; source clock **?** | H/S; no sp; `base_age=50`; age and base_age >=10 | Eastern white pine; fitted to Frothingham second-growth dominant/codominant yield-table means for southern-US survey use, not Canadian calibration. E:9-23,35-42; T:27; P: RP-SRS-10 pp.1-3 (PDF pp.2-4). Inspected passage does not explicitly establish a BH input clock. D9. |
| 23 | `payandeh1974` | B documented; source clock **?** -> **50/80/100 by species** / total | H/S; sp; 9 species; no configurable base_age | E:6-10 conflicts with `@param si` at :23; separate fitted height/SI equations :107-154. T:1-35; P: Payandeh 1974 p.194; parameter CSV :1-10. Do not assign universal SI50 or assume exact inversion. D7,D9. |
| 24 | `sharmareid2018` | B -> 50B / **above BH per engine documentation** | H/S; sp; `base_age=50`; **no** total-height option/offset | ON natural-origin, even-aged fully stocked undisturbed stands. E:10-20,32-40,94-135; T:1-75; P: Forest Science 64:33-40, pp.34-35 (PDF pp.2-3) establishes population/BHA/SI50, but inspected text does not explicitly confirm above-BH height. Keep datum disputed rather than silently normalize. D6. |
| 25 | `sharma2022` | B -> 50B / **above BH supported by source** | H/S; sp; `base_age=50`; **no** total-height option/offset | ON natural-origin mixed black spruce/aspen; source says all analyses use height from breast height. E:10-20,32-40,102-147; T:1-74; P: Forests 13:1314, methods PDF p.3 and SI analysis p.5. Engine `height` docs omit datum. D6. |
| 26 | `lafleche2013_potential` (`lafleche2013`) | **C** -> 50C IQS / total | ECO; sp + ecological_region + ecological_type; conditional ecological_subregion; `curve_set="potential"` fixed | Southern QC ecological-site potential curves, not free SI trajectories. E:14-35,38-59,107-155; T:1-35,108,259; P: indices-qualite section 4.6.3, PDF p.31; corrected-age figures pp.24-25. One IQSstation/PICE.GLA LOGIST3 key is intentionally unsupported. D2,D10. |
| 27 | `lafleche2013_observed` (`lafleche2013`) | **C as implemented/documented** -> 50C IQS / total; source observed-clock interpretation **?** | ECO; same keys; `curve_set="observed"` fixed (native potential) | Separate observed-growth fits, not inverse SI estimation. E/T as #26. Source section 4.6.3 specifically describes IQSstation; do not silently assume its corrected-age interpretation establishes the observed-curve clock too. D2,D10. |
| 28 | `scottvoorhis1986_bh_age` (`scottvoorhis1986`) | B documented; source equation consumes T directly -> **50T source SI**, not 50B | H/S; sp; `convert_to_total_age=FALSE` fixed/native | 19 northeastern species; raw supplied age goes directly into total-age equation. E:18-46,114-133; T:29-61; P: Scott and Voorhis p.145 eq.4, p.146 algorithm (PDF pp.2-3). Legacy ID and doc conflict require a reviewed interpretation. D11. |
| 29 | `scottvoorhis1986_total_age` (`scottvoorhis1986`) | **B supplied**, then estimated YTBH added -> **50T source SI** | H/S; sp; `convert_to_total_age=TRUE` fixed | Same population; ID names internal conversion, not the input clock. E:38-46,114-133; T:63-102; P: same eq.4/algorithm. Do not relabel input T, add YTBH twice, or rename ID silently. D11. |
| 30 | `thrower1994` | B -> 50B / total | H/S; sp; 11 registered species | Interior BC model collection; species-specific source forms/sampling, not one universal curve. E:22-26,55; T:1-40; P: SIBEC field guide SSES015, species equation definitions, PDF pp.1-2. Keep publication lineage per species. |
| 31 | `hugarcia2009` | B -> 50B / total | H/S; sp (PICE.GLA/PICE.ENG) | SBS interior spruce; suitable largest-DBH tree in 0.01 ha, not interchangeable with conventional largest-100/ha averaging. E:12-17,33-39; T:87-120; P: Hu and Garcia 2009 methods, PDF pp.2,4. BHA 0.5 at BH convention. D12. |
| 32 | `nigh1997` | B -> 50B / total | H/S; no sp | Coastal BC Sitka spruce; site/top height, suitable largest-diameter trees; detailed eligibility remains source-specific. E:12-21,34-38; T:63-98; P: Nigh 1997 p.363 definitions, p.366 eq.8; S: Nigh1997_model-spec :23-52. Coastal restriction absent from registry subregion metadata. D12. |
| 33 | `nigh2002` | B -> 50B / total | H/S; no sp; `bec_zone=NULL` native base, supplied zone selects extended fit | BC aspen; closed-canopy even-aged plots; 6 extended-model zones. E:10-25,35-42,65; T:44,63-90; P: Nigh et al.2002 methods/model, PDF p.2; S: Nigh2002_model-spec :19-32,45-66. Both coefficient modes currently share one ID. D13. |
| 34 | `cieszewski1993` | B -> 50B engine specialization / total | H/S; sp; 11 species | Saskatchewan preliminary variable-age model; screened stem analyses. E:17-32,39-44; T:77,189-225; P: report printed p.3 (PDF p.4); S: Cieszewski1993_model-spec :29-39,104-136. SAS and source coefficient variants must remain distinguished. |
| 35 | `goudie1984` | B -> 50B / total | H/S; sp; pine/white spruce | Engine follows later SAS modifications and dry-site pine fit, not an unmodified 1984 equation. E:10-44,46-52; T:120-165; P: Goudie report introduction (PDF p.2) includes BC and AB; S: Goudie1984_model-spec :23-36,53-90. Do not expand BC registry coverage to AB without review. D12. |
| 36 | `nigh2004` | **T** -> conventional SI predictor, reference clock not explicit in engine; S suggests standard BC SI, verify lineage | H/S; sp; `bec_zone=NULL` province-wide; 7 zone alternatives | Managed interior BC juvenile pine/spruce. E:12-31,40-50; T:1-30,62-100; P: RR25 methods/model PDF pp.10-11, eqs.3,5-6; S: Nigh2004_model-spec :23-33. Source juvenile range about T<=15 pine / <=20 spruce; registry max20 is not universal support. D14. |
| 37 | `nigh2009` | B -> 50B / total | H/S; no sp; native `variant=1`, `bec_zone=NULL`; variant3 requires zone | Interior BC paper birch; ICH/IDF/SBS calibration. E:10-31,40-49,139-160; T:44,73-110,139; P: paper pp.254,259 (PDF pp.3-4,8); S: Nigh2009_model-spec :19-33,57-68. One ID currently hides base/operational/zonal fits. D13. |
| 38 | `carmean2006` | B -> 50B / total | H/S; sp; black spruce/aspen | NW ON natural, fully stocked even-aged dominant/codominant stands; fits <=100B. E:11-19,45-51; T:104-180; P: paper abstract/methods PDF pp.1-3; S: Carmean2006_model-spec :20-34. Black-spruce/aspen evidence strength differs (SAS check only aspen). |
| 39 | `carmean2001` | B -> 50B / total | H/S; no sp | Northern ON jack pine; fully stocked even-aged undisturbed stands; recommended fit <=100B. E:10-18,42-46; T:75-130; P: paper abstract PDF p.1; S: Carmean2001_model-spec :22-35. No age-label discrepancy established. |
| 40 | `goelz1992` | B -> 50B engine specialization / total | H/S; no sp; height/SI >1.3 | North-central ON jack pine. E:9-14,46-50; T:41-65; P: Goelz and Burk 1992 p.776 abstract; S: Goelz1992_model-spec :20-35,49-66. Source base-age invariance does not make engine reference age configurable. |
| 41 | `nigh2017` | B -> 50B / total, approximate SI-to-parameter conversion | H/S; no sp | BC lodgepole pine; SI conversion intended for 5-30 m, not unlimited SI. E:11-30,41-46; T:48-70; P: RR31 introduction and discussion p.18; S: Nigh2017_model-spec :17-32,58-81. Do not require exact base-age equality/round trip. D15. |
| 42 | `batho2014` | B -> 50B / total | H/S; **sp required** despite single species | SBS lodgepole pine, natural + planted data; suitable largest tree/0.01 ha; BHA0.5 origin. E:11-16,33-38,67,180-196; T:55-84; P: Batho and Garcia 2014 methods PDF p.2; S: Batho2014_model-spec :17-31. Registry requires_species FALSE is wrong; SBS restriction missing. D4,D12. |
| 43 | `nigh1998` | B -> 50B / total | H/S; no sp | Interior BC western hemlock, not a coast-wide hemlock default. E:1-16,32-36; T:19-50; P: Nigh 1998 pp.588-589 (PDF pp.1-2); S: Nigh1998_model-spec :22-42,64-75. Interior applicability is missing from registry subregion metadata. D12. |
| 44 | `nigh1998_gi` | B -> 50B / total | G; gi cm/year; ages **{5,10,20,30,40,50} only** | Same interior hemlock; no interpolation between coefficient ages. E:8-24,58-72; T:14-28,36-70; P/S: Nigh1998_model-spec :77-91,147-156, Table 3. Registry max50 alone is insufficient. D3. |
| 45 | `alemdag1991` | B -> 50B / total | H/S; no sp; separate fitted SI/height equations | National natural unmanaged even-aged white spruce, dominant/codominant tree height; no calibration data from BC or NS. E:3-18,44-48; T:49-85; P: Alemdag 1991 p.1466; S: Alemdag1991_model-spec :21-39. ALL scope is not evidence that every province was sampled. D12,D15. |
| 46 | `brisco2002` | B -> nominal 50B / total, **not exactly conditioned** | H/S; no sp | SE BC western larch; natural fire-origin even-aged stands; source BHA45-134, not a hard engine minimum. E:10-22,34-38,147-151; T:45-70; P: Brisco et al.2002 p.66 (PDF p.1); S: Brisco2002_model-spec :19-35,102-110. Docs' exact H(50)=SI claim contradicts fitted form/tests. D15. |
| 47 | `chenklinka2000` | B -> 50B / total | H/S; sp; fir/Engelmann spruce/pine | BC ESSF, not all BC forest types. E:1-20,29-35; T:118-159; S: ChenKlinka1998_model-spec :7-34,47-71. Original paper/source identity is 2000 although internal parameter/spec names say 1998. Primary source was not located under an unambiguous matching PDF name; retain secondary evidence status. D12. |
| 48 | `huang2009_bh_age` (`huang2009`) | **T input** -> 50B / total | H/S; sp; `index_age="breast_height"` fixed/native | AB natural/post-harvest stands; mean height of 100 largest-DBH trees/ha. E:10-23,38-46; T:111-145; P: T/216 PDF p.6 definitions, section4 and Appendix1; S: Huang2009_model-spec :26-41,90-111. Existing input-age label is already correct. D16. |
| 49 | `huang2009_total_age` (`huang2009`) | **T input** -> 50T / total | H/S; sp; `index_age="total"` fixed | Same population; engine conversion makes the SI reference clock distinct from the input clock. E/T/P as #48. Preserve both IDs/scales; do not infer input age from suffix. D16. |
| 50 | `huang2026` | T -> 50T realized HI / total | H/S; sp; native `variant="non_climate"`; climate inputs forbidden | 8 AB species; dominant/codominant **tree** height, not standardized largest-DBH stand mean. E:3-29,60-79,149-158; T:6-45; P: T/2026-RIBS01 sections2.1,3.1, Table3.4; S: Huang2026_model-spec :24-46. Conventional-height-index family; no climate substitution. D16. |
| 51 | `huang2026_climate` (`huang2026`) | T -> inherent HI, nominal 50T reference / total | H/S for **inherent HI**; sp + MAP + MWMT + MAP_normal + MWMT_normal; `variant="climate"` fixed | Same 8 species; H(50)=C*HI. E:31-58,68-79,149-158; T: climate cases in test-si_huang2026.R:177-390; P: report sections2.4,3.1,4, Table3.1; S: Huang2026_climate_model-spec :29-104. Not conventional SI fallback. D16. |

#### Discrepancies, evidence strength, and disposition

| ID | Verified discrepancy / evidence | Proposed disposition (not implemented) |
|---|---|---|
| D1 | Registry age labels at `R/si_model_registry.R:292-294` contradict plantation-age E/P evidence for Auger-Ward and both Pregent engines. | Preserve IDs; use `plantation_age` for input and reference clocks. Do not equate years since planting with years since germination or prescribe a conversion from nursery age. |
| D2 | Lafleche registry labels at `:305-306` say BHA; E says corrected age above 1 m (`R/si_lafleche2013.R:14-24`). Source figures (indices-qualite PDF pp.24-25) distinguish total, age above 1 m, **and corrected** age. Section4.6.3 (PDF p.31) evaluates IQSstation at corrected age50. | Preserve curve-set IDs and equations. Use an explicit corrected clock, not merely "age since reaching 1 m." Potential interpretation is supported; observed-family clock needs separate reconciliation. Retain legacy capability booleans but document that `supports_predict_height` does not mean age+arbitrary-SI prediction. |
| D3 | `nigh2000_gi` height capability TRUE (`R/si_model_registry.R:519-530`) contradicts `function(age, gi)` and single-column output (`R/si_nigh2000_gi.R:30,109`). Its age_domain_max is NA, while ages must be integer1:50 (:48-59). Hemlock GI only accepts six ages (`R/si_nigh1998_gi.R:58-72`). | Correct cedar height capability, add operation-specific discrete domains; keep both GI IDs and SI50-total-height definitions. Do not derive height curves from GI or interpolate ages. |
| D4 | `requires_species` is FALSE for **Huang1994 and Batho2014**, though signatures require it (`R/si_huang1994.R:75-81`, `R/si_batho2014.R:67`); registry `:397,426`. Exhaustive signature comparison found no other such mismatch. All existing fixed-argument names are valid formals. | Correct flags without changing engine signatures. Required-input checks must include single-species engines with a required `species` formal, and discovery must still match species for engines lacking that formal. |
| D5 | Pregent fixed_args contain base_age50 (`R/si_model_registry.R:187-188`), while native defaults are25; existing registry tests explicitly preserve50 (`tests/testthat/test-si_model_registry.R:60-91`). Auger-Ward has no such override. | Preserve raw fixed_args/plot output; tag those two entries as plot-only. Distinguish publication reference25, native default25, and plotting reference50. Do not impose plot SI50 on selection. |
| D6 | Five engines (Sharma2015/2021, Parton2018a/b/2019) convert public height by1.3 m but **not SI**. SharmaReid2018 passes both scales through and documents above-BH height; inspected source describes D/CD height but does not conclusively resolve that datum. Sharma2022's source explicitly uses height from BH, while engine docs omit the datum and code has no offset. | Record height and SI datums independently and condition height datum on `total_height` where present. Record Sharma2015/Parton2018a/b/2019 source SI25 separately from engine SI50. Keep Reid datum disputed; propose above-BH for Sharma2022 with cited source support. No automatic 1.3-m conversion, engine change, or universal "total height" assumption. |
| D7 | Payandeh source p.194 and `data-raw/Payandeh_1974_parameters.csv:1-10` establish SI50 for ABIE.BAL/PICE.GLA/PICE.RUB/PINU.MON, SI80 for PINU.CON, SI100 for PINU.PON/PSEU.MEN/PICE.SIT/TSUG.HET. `R/si_payandeh1974.R:6-10` agrees; :23 misleadingly says SI50. `index_age` is not used to rescale runtime inputs (:107-154). | Preserve ID; metadata definition must resolve reference age **by species**, from authoritative source/parameter index_age, not a global50. Flag doc conflict for later reviewed documentation work. Separate fitted forward/inverse equations are not an inverse guarantee. |
| D8 | Lundgren-Dolid Table1 p.3 and prose p.4 identify BH age only for balsam fir/white spruce, whereas both registry rows and engine docs say BHA universally. Remaining species use unfootnoted source ages (proposed total-age reading). Both white-spruce forms are explicitly not recommended (p.4; CSV recommended=FALSE); preprocessing drops `recommended`/notes (`data-raw/preprocess_data.R:869-874`). | Preserve both form IDs. Represent species-conditioned age definitions and source recommendation independently from numerical support. Approve the remaining-species total-age interpretation before publishing it; do not remove coefficients, alter code, or silently hide supported species. |
| D9 | Payandeh p.194 calls A "stand age" and uses several antecedent curve sources; inspected text does not establish a uniform BHA clock. Parresol RP-SRS-10 uses second-growth yield-table ages without an explicit BH-clock statement in the inspected passages. Engines document BHA. | Keep current engine age contracts visible but mark publication-age basis unresolved; resolve antecedent sources before claiming scientific compatibility. Do not infer a clock from H(0)=0, title, region, or existing registry value. |
| D10 | All `subregion_required` values are FALSE (`R/si_model_registry.R:688`), but Lafleche requires ecological_region/type in addition to species, and subregion can discriminate keys. A matching species is insufficient; the potential table retains one unsupported LOGIST3 row (E:31-35; T:108-145). | Keep ecological operation separate. Require complete matching ecological keys, express conditional subregion requirements and unsupported key/form exclusions. Do not flip the single legacy subregion flag to TRUE for every observation. |
| D11 | Scott-Voorhis source p.145 defines SI at **50 total years** and A as total age; p.146 algorithm accepts BHA and adds estimated YTBH, or skips conversion when total age is known. Engine docs label both age and SI as BH-based (`R/si_scottvoorhis1986.R:18-46`), while code adds YTBH only when TRUE (:114-133). Existing tests deliberately protect both arithmetic paths (:29-102). | Proposed source-consistent interpretation: FALSE/direct-equation variant consumes total age; TRUE/conversion variant consumes BHA; both use source SI50T. This conflicts with legacy labels/docs. Approve interpretation and wording before changing metadata; preserve the two IDs and both arithmetic paths, with explicit legacy-ID notes. No re-normalization at50. |
| D12 | Geography is often applicability, not a predictor: Nigh1997 coast; Nigh1998/GI interior; Batho SBS; Chen-Klinka ESSF; Hu-Garcia SBS. Some already have BC-interior labels, others "none". Hu-Garcia/Batho top-height sampling is not simply average largest100/ha. Goudie source includes AB, registry BC only; Alemdag ALL includes provinces absent from fitting data. | Add geographic/sampling applicability and calibration-domain notes separately from actual engine region arguments. Preserve current jurisdiction lists pending approval of transfer claims. Unknown stand origin/composition is not "all." |
| D13 | Nigh2009 native variant1, optional variant2, and zone-required variant3 share one registry row; `bec_zone` is ignored (warns) in variants1/2 (`R/si_nigh2009.R:139-161`). Nigh2002 and Nigh2004 switch coefficient sets when zone is supplied, without distinct IDs. | For Nigh2009 propose retaining `nigh2009` explicitly as variant1 plus new operational/zonal IDs, only if approved. For Nigh2002/2004 initially express conditional parameter-set selection within existing IDs; obtain explicit approval before adding any further IDs or making zonal fits automatic. |
| D14 | Age maxima mix mathematical limits, tables, fitted sample ranges, and plot truncation. Nigh2004 max20 hides roughly15 pine/20 spruce (S:23-33); Brisco max134 omits lower fitting age45; Parresol hard minimum10 absent; shifted BH forms can need age>0.5; Huang2026 age>180 warns rather than rejects. | Add typed, species/operation-conditioned constraints and keep `age_domain_max` as legacy plotting summary. Never make a plot cap an eligibility proof or conflate warning/extrapolation with mathematical failure. |
| D15 | Brisco docs assert H(50)=SI exactly (`R/si_brisco2002.R:21-22`), but published unconstrained fit and tests allow0.35 m (`tests/testthat/test-si_brisco2002.R:45-70`); a read-only call SI20 -> H50=20.08866. Nigh2017 uses an approximate conversion (tests :48-70). Carmean1996, Payandeh and Alemdag have separately fitted directions. | Do not enforce universal base-age/round-trip identities or "repair" equations. Store nominal definition separately from conditioning/inversion limitations; retain source-specific tolerances and evidence strength. |
| D16 | Huang2009 uses T input for both SI clocks (E:10-23), already correctly registered. Huang2026 climate returns inherent HI with H50=C*HI; all four climate inputs are mandatory (E:31-58,149-158). Non-climate coefficients differ, even at C=1. | Add separate input/reference clocks and productivity quantity. Climate windows: previous20 years for aspen/poplar/white spruce/fir,10 for black spruce, all previous years since initiation for pine/Douglas-fir/jack pine; exclude current year and pre-initiation years. Normals are location-specific1961-1990. Caller aggregation cannot be verified from four numbers. No defaults or cross-family fallback. |
| D17 | ON black spruce candidates Carmean1996, Sharma2021, SharmaReid2018 and Sharma2022 share rank89 (`R/si_model_registry.R:901-953`) but differ in sampling, datum, composition, and stand origin. Several eastern-US fits have Canadian jurisdiction lists without Canadian calibration. | Preserve ranks/order; distinguish legacy preference from evidence of superiority. Compatibility/uncertainty precedes rank. Tie-breaks, transfer policy and selection under unknown origin remain Phase4/5 approval topics. |

#### Proposed minimal metadata schema (for approval)

Use the existing public registry as the **variant identity table**, retaining
its 22 current columns, IDs, order, ranks, list-column types, and raw fixed_args.
Add SI-specific structured metadata rather than copying the HD registry's
single-operation `required` boolean. No implementation or helper name below
is mandatory; the data contract is the decision.

| Component | Proposed fields / types | Meaning and invariant |
|---|---|---|
| Existing `age_basis` | character enum: existing `total_age`, `breast_height_age`; add `plantation_age`, `corrected_age_above_1m`, `species_dependent`, `unresolved` | Always **input** clock. Never derive SI reference clock from it. `species_dependent` requires resolved child definitions; `unresolved` is not an automatic match. |
| Scientific definitions (list-column/child table, one row per model + explicit condition) | `model_id`, `definition_id`, optional `species`; `input_age_basis`, `age_definition`; `reference_age` numeric, `reference_age_basis`; `productivity_quantity`; `si_datum`, `height_datum`; `height_definition`; `reference_age_role`; `limitations` | Child input_age_basis resolves a species-dependent summary. Quantity enum `site_index`, `height_index`, `inherent_height_index`, `ecological_iqs`. Datum enum `total_height`, `above_breast_height`, `unresolved`, with literal measurement-height/age-convention details in definitions. reference_age_role distinguishes fixed model scale, configurable engine default, converted index, and nominal parameter reference. No universal1.3-m offset or exact H(reference)=SI assumption. |
| Applicability (list-column/child rows) | `model_id`, optional `definition_id`/`species`; `stand_origin`, `stand_structure`, `composition_definition`, `geographic_definition`, `height_sampling_definition`, `calibration_domain` | Origin values `natural`, `plantation`, `natural_and_plantation`, `unresolved`; unknown is not unrestricted. Composition and sampling retain precise source definitions in text until a justified machine-filter vocabulary is agreed; "mixed" alone cannot encode species percentages. Applicability geography does not imply a corresponding formal argument. |
| Operation/input table (authoritative child rows) | `model_id`, `operation`, `arg`, `role`, `units`, `definition`, optional `condition_id` | Operations: `infer_si_from_height`, `predict_height_from_si`, `infer_si_from_gi`, `evaluate_ecological_curve`. Role: `required`, `optional`, `fixed`, `forbidden`; conditions refer only to named engine selectors/species/keys, not free-form executable expressions. Each H/S operation independently requires age and exactly its appropriate height/SI input; ECO requires neither height nor SI. Climate HI uses the H/S operation names **with its distinct quantity**. |
| Constraint/conditional-rule table | `model_id`, `condition_id`, `operation`, optional `species`, `arg`, `kind`, `values` (typed list), `definition`, `engine_action` | Kinds distinguish mathematical domain, discrete support, fitted/calibration range, published recommendation, conditional key requirement, and unsupported key/form. engine_action `error`, `warning`, `not_enforced`, or `not_applicable`; record actual behavior separately from proposed discovery policy. Climate aggregation rules are semantic prerequisites, not numerical bounds. Do not create a general-purpose rule language. |
| Existing fixed_args + roles | additive `fixed_arg_roles` named character list, same names as fixed_args; values `variant`, `plot_only` | Existing selectors remain variant-defining; only the two Pregent base_age50 entries are plot_only. Preserve legacy raw fixed_args for article compatibility; derive selection fixed args from role rather than executing plot-only overrides. Resolve any overlap explicitly, never let caller arguments silently override variant identity. |
| Native defaults (derived view, not hand-maintained duplicate values) | Named list from engine formals, plus documented resolved default for multi-choice selectors | Expose engine-native defaults, variant-effective arguments, and plotting-effective arguments separately. Configurable `base_age` changes resolved reference metadata, not just prediction inputs. Do not mistake a formal NULL for scientifically optional input in every operation. |
| Evidence (child rows, referenced by definitions/requirements/rules) | `model_id`, `subject` (field/rule), `evidence_kind`, `locator`, `status`, `note` | evidence_kind `publication`, `reference_implementation`, `engine`, `test`, `extraction_spec`, `plot_configuration`. status `verified`, `secondary_only`, `unresolved`, `contradicted`. Status attaches to the **claim**, not the whole model. Preserve conflicting source and engine claims side by side; a test match proves current behavior, not publication authority. |

For minimal implementation, store these as a small set of typed internal
tibbles and expose nested subsets through registry/info list-columns; do not
expand 51 public rows into a species Cartesian product or create a separate
registry for every operation. Field naming and which nested subsets appear
directly on `si_model_registry()` versus `get_si_model_info()` require schema
approval; child data is authoritative and summaries are derived.

Concrete contracts the schema must express:

- Conventional default: age + height + required species -> native-scale SI;
  age + native-scale SI + required species -> native-scale height. Passing
  both height and SI is an error, not two predictions.
- Parton2018a: default height datum total, SI datum above-BH,
  reference50B; source reference25B retained as evidence. `total_height=FALSE`
  changes only height datum; `base_age` changes the resolved reference age.
- Payandeh: nine species-specific definition rows with reference50/80/100.
  Keep publication input-clock uncertainty even though the engine advertises BHA.
- Cedar GI: age must be a supported integer1:50; hemlock GI: six supported
  ages; age+gi returns SI only. GI definition includes how its source averaging
  interval/age convention is measured, not just units.
- Lafleche: exact species + region + type + conditional subregion match;
  curve_set fixed by ID; unsupported LOGIST3 potential key excluded explicitly.
  Return both evaluated height and IQS, never infer SI from measured height.
- Nigh2009: legacy ID resolves variant1; proposed variant3 requires bec_zone.
  Base-mode applicability is not inferred merely because a zone argument exists.
- Climate: four required positive finite aggregates, species-specific prior
  years and fixed normals, quantity inherent HI; non-climate mode forbids those
  climate arguments. Numeric input presence does not verify temporal aggregation.

Phase3 consistency checks should cover complete/unique IDs; legal args and
fixed-arg roles; every no-default engine argument satisfied per operation;
height/SI exclusivity; species-conditioned definition coverage with no ambiguous
matches; GI/ecological capability consistency; required evidence for scientific
claims; discrete/conditional domains; no conflation of unknown with ALL; and
separation of source, native, variant and plotting definitions. Numerical
identity checks must remain model-specific, not blanket registry invariants.

#### Affected consumers and staged implementation boundary

| Consumer / evidence | Consequence for later phases |
|---|---|
| `vignettes/articles/Site-Index-Models.qmd:51-59,294-306` | Both age-axis and prose switches only label total/BH clocks. Add explicit plantation/corrected/species-dependent/uncertain labels; do not silently use generic Age or one species clock for all panels. |
| Same article `:70-110,163-169,242-246,314-409` | Executes raw fixed_args, titles all generic y axes "Top height", special-cases GI/Lafleche/climate. Lafleche axis is hardcoded BHA and is wrong independently of registry. Preserve Pregent plot reference50 while displaying its role; label above-BH height and D/CD tree height correctly. Do not replace current specialized ecological/climate examples with generic SI curves. |
| `R/si_model_registry.R:1106-1170` | Species helper must retain new metadata/list-column types while expanding coverage; no requirement that species is always an engine formal. Preserve manual subset precedence. |
| `tests/testthat/test-si_model_registry.R:1-41,60-140,239-372` | Current tests protect structure, fixed50 plot arguments, ecological legacy booleans, region metadata and species expansion; they do not audit all scientific definitions or no-default arguments. Add targeted metadata assertions only in approved phases. |
| `tests/testthat/test-si_huang2026.R:168-175,368` and per-engine registry assertions (`test-si_alemdag1991.R:110`, `test-si_carmean2001.R:175`, `test-si_nigh1997.R:95`, `test-si_cieszewskibella1991.R:203`) | Preserve existing total-age and identity assertions; add operation/definition checks rather than changing engine tests to enforce universal SI50. |
| Public registry/docs; external consumers unknown | Additive metadata may still affect consumers that assume only two age_basis values. Document new enum meanings and legacy fixed_args semantics in later roxygen/NEWS updates. No SI dispatcher/discovery API exists yet. |

Search found no other package R consumer selecting SI models by these age
labels/capabilities. `data-raw/preprocess_data.R:1197` also contains `age_basis`,
but that is parameter preprocessing, not an SI-registry consumer. Existing
generated man/docs output is not an independent authority and was not edited.

Safe **metadata-only candidates preserving IDs**, subject to approval: D1;
supported potential-curve portion of D2; D3/D4; annotate plot-only roles in D5;
independent SI/height datum and source/native-reference metadata where verified;
Payandeh species reference ages; noncontroversial sampling/geographic
restrictions, domains and climate quantity/requirements. Correcting a registry
field does not authorize changing an engine's docs, signature, age conversion,
numerical scale, or validation in Phase1.

**Explicitly reviewed variants/legacy meanings:** Scott-Voorhis D11;
Nigh2009 operational/zonal additions D13; species-conditioned Lundgren clocks
and recommendation treatment D8. Proposed new IDs could be
`nigh2009_operational` and `nigh2009_zonal`, keeping `nigh2009` as explicit base
variant1. No alias, rename, extra ID or new default is approved by this audit.
Nigh2002/2004 conditional zone modes need not become extra variants merely
because Nigh2009 is split.

#### Approval recorded — 2026-11-10

The user approved Phase 1 for verified metadata corrections only and approved
the proposed schema subject to unresolved scientific evidence. Metadata
placement should follow the HD registry pattern: shared internal metadata
tables, registry list-columns where appropriate, and detailed access through
an info helper. This is not approval to implement later-phase discovery helpers
or structurally refactor the registry during Phase 1.

Disputed interpretations and new variants remain deferred. Preserve legacy
IDs, numerical behavior, and plot arguments; record unresolved evidence
explicitly rather than choosing an interpretation. No additional scientific
input is required to begin this narrowed Phase 1. No R code has been changed
as part of recording this approval.

#### Scientific decisions deferred / approved scope boundaries

The original approval checklist below records the individual decisions.
The schema shape is approved with HD-style placement as noted above; only
verified corrections are authorized. Requests below to resolve disputed
science or add variants remain deferred, not implicitly approved.

1. Approve the schema shape, clock/quantity/datum vocabulary, per-claim evidence
   status, and species/operation-conditioned definitions. Decide which nested
   metadata belongs on the public registry versus the info helper.
2. Approve plantation-clock corrections and corrected-above-1-m labeling for
   supported Lafleche potential curves. For observed curves, reconcile the
   source's actual age variable before assigning a verified corrected clock.
3. Approve Scott-Voorhis source SI50T for both variants and the proposed
   FALSE=T/direct versus TRUE=B/converted input interpretation, or retain an
   explicitly disputed legacy contract until documentation is separately reviewed.
4. Approve Lundgren's species-dependent clocks (BH fir/spruce; proposed total
   for remaining species) and how to expose source-not-recommended white spruce
   without silently removing engine support. Resolve Payandeh/Parresol source
   age clocks from antecedent curves; do not publish verified BHA by assumption.
5. Approve height/SI datum metadata, especially source-supported above-BH
   Sharma2022; resolve SharmaReid2018's documented above-BH versus source
   height interpretation before declaring compatibility with total height.
   Approve retaining source SI25 versus engine SI50 distinctions, and the
   converted/nominal-reference caveats for alder SI50 and unconstrained fits.
6. Approve preserving all legacy IDs/raw plot arguments and adding only the
   proposed Nigh2009 operational/zonal entries, or defer those additions.
   Confirm Nigh2002/2004 remain conditional modes initially. Verify juvenile
   Nigh2004's GI-derived SI reference lineage before assigning a verified
   reference clock/value; total input age alone does not establish SI50T.
7. Leave Canadian transfer-domain expansion, stand-unknown selection policy,
   automatic regional-mode choice, rank/ties, output normalization and wrapper
   fallback **deferred** to the later approval gates. Nothing here authorizes
   automatic selection or any scientific transformation.

#### Checks and handoff

- Read-only R introspection confirmed **51 unique variant IDs,45 engines**,
  per-row expanded species coverage, valid fixed-argument names, and the two
  missing required-species flags. Read-only calls illustrated Brisco's nominal
  reference and Sharma2022's unchanged paired-height scale.
- The 51 inventory rows were checked against the exact registry IDs and order;
  explicit evidence file paths were checked for existence. The original
  registry has 22 columns (25 after the three species-summary columns).
- No unit tests, package checks, documentation generation, data rebuild or
  website build were needed for this documentation-only phase. Existing tests
  were inspected as evidence, not reported as freshly passing.
- Pre-existing worktree changes in five `docs/articles/*.html` files and
  untracked `README.html` were left untouched. This plan was already untracked.
- **Handoff:** Phase0 research/deliverables complete. The user approved the
  narrowed Phase1 scope recorded above; implementation has not started.
  Phase2-6 statuses remain unchanged. Do not add variants, refactor the
  registry, or resolve disputed science silently.

### Phase 1 — 2026-11-10: verified metadata corrections

**Outcome:** Completed the approved, metadata-only scope. The registry still
contains **51 variants / 45 engines** in the original order. Construction remains
the original parallel-vector tibble; no row-constructor refactor, new variants,
ranking changes, engine changes, parameter edits or scientific transformations
were made. Phases 2-6 remain unimplemented with their existing statuses.

#### Corrections and metadata placement

- Changed exactly **nine cells in existing columns**: the three plantation
  input clocks; Lafleche potential's corrected-above-1-m clock; Lafleche
  observed's explicitly unresolved source clock; Huang1994/Batho2014's required
  species flags; cedar GI's height capability and maximum supported age50.
- Added `R/si_model_metadata.R` with shared internal metadata tables and five
  registry list-columns: `scientific_definitions`, `applicability`,
  `metadata_evidence`, `age_constraints`, and `fixed_arg_roles`. These are
  focused Phase1 records, **not** the complete Phase3 predictor/operation
  contract or discovery API. Scientific definitions currently contain
  42 conditional/species rows for29 variants; empty child tables explicitly
  mean not yet encoded, never unrestricted applicability.
- Separated SI reference/input clocks (including Huang2009's total input age),
  SI/height datums, configurable native references, converted/nominal reference
  caveats, ecological IQS and inherent HI. Payandeh's nine reference ages come
  from the existing compiled `index_age` data; configurable native reference
  ages are derived from engine formals rather than a duplicate defaults table.
- Preserved every raw `fixed_args` value. Only Pregent2010/2016's `base_age=50`
  arguments are marked `plot_only`; their native references remain25.
  Other fixed selectors are marked `variant`.
- Encoded GI discrete support for the SI-from-GI operation: cedar integer1:50,
  hemlock {5,10,20,30,40,50}, with current engine error behavior and no
  interpolation. No general constraint evaluator was introduced.
- Added source-population/geographic/sampling descriptions separately from
  engine region arguments. Stand-origin uncertainty is explicit; existing
  province/species coverage and regional-selection settings are unchanged.
- Retained disputed Scott-Voorhis/Lundgren legacy input labels with explicit
  unresolved claim records, rather than adopting the deferred interpretations.
  Payandeh/Parresol source clocks, Sharma-Reid datum, observed Lafleche clock,
  Nigh2004 reference lineage and alder SI50 conversion provenance remain
  unresolved. No Nigh2009 operational/zonal IDs were added.
- Primary-source reinspection confirmed Sharma2022's above-BH height datum.
  **Locator correction to Phase0:** this is *Forests* **13(3):430**,
  DOI `10.3390/f13030430`, not13:1314 as stated in the inventory/D6.
  The new metadata uses the verified430 locator. Lafleche section4.6.3
  explicitly substitutes50 for corrected `AGEc` for IQSstation; that evidence
  was not extended to the observed family.

#### Directly affected consumers and files

- `R/si_model_registry.R`: corrected legacy cells and appended nested metadata;
  roxygen explains clock meanings, uncertainties, plot-only overrides and
  legacy capability/age-limit semantics.
- `vignettes/articles/Site-Index-Models.qmd`: explicit plantation/corrected/
  species-dependent/unresolved age labels, including specialized ecological
  plots; unresolved legacy source clocks are not plotted as verified BHA/total
  clocks. Above-BH height and dominant/codominant tree height have distinct
  axes; unverified generic sampling is no longer labelled universally "Top
  height". Native definitions, source-population notes and plot-only SI50
  overrides are displayed without changing prediction calls or plotted scales.
- `tests/testthat/test-si_model_registry.R`: focused metadata/uncertainty,
  engine-signature, GI-domain, species-expansion and article-helper regressions.
  Article helpers are evaluated without rendering/building the website.
- `NEWS.md` and regenerated `man/si_model_registry.Rd`: documented corrections.
- Also updated the existing **git-ignored local consumer**
  `tmp/export_models_for_chris.R`: it now uses `devtools::load_all()` instead
  of sourcing two registry files in isolation, so shared metadata/data helpers
  resolve in a fresh R process. It remains ignored, not a tracked package
  artifact; generated exports were not rewritten.

#### Checks and remaining boundary

- New regression tests first failed against the unchanged registry, then
  passed after implementation.
- `devtools::test(filter = "^si_", reporter = "summary",
  stop_on_failure = TRUE)` passed all46 SI test files, including article
  helper tests, without failures, warnings or skips.
- Compared **every existing registry column** against a saved pre-change
  baseline: values/types/list contents are identical outside the nine
  explicitly allowed cells. In particular IDs/order, ranks, raw fixed args,
  species/province coverage and plotting SI values are unchanged.
- `devtools::document()` used installed/configured roxygen2 **8.1.1** and
  changed only `man/si_model_registry.Rd`. `tools::checkRd()` and
  `git diff --check` passed; an initial Rd encoding diagnostic was removed
  by using ASCII in the new roxygen text and regenerating.
  A fresh-process export-startup check successfully loaded the metadata and
  returned all51 registry rows.
- No full package check or article/site build was run. Existing equations and
  numerical regressions were exercised by the SI suite; no data rebuild was
  necessary. Pre-existing five `docs/articles/*.html` changes and untracked
  `README.html` were left untouched. This plan remains untracked as before.
- **Handoff:** Phase1 is complete. Future Phase2 must preserve these corrected
  values and nested metadata while refactoring construction only. Complete
  predictor/conditional-rule coverage, info/discovery helpers, extra variants,
  rankings/selection policy and disputed scientific decisions remain deferred.

### Phase 2 — 2026-11-10: per-variant registry construction

**Outcome:** Completed construction-only refactoring. The registry retains
**51 variants / 45 engines**, with no changes to scientific meaning or output.
Phases 3-6 remain unimplemented with their existing statuses.

#### Implementation and affected files

- `R/si_model_registry.R`: replaced parallel column vectors with one
  `.si_registry_row()` call per variant in `.si_model_definitions()`, following
  the HD registry's general pattern. The small SI-specific constructor wraps
  multi-valued fields as list-column cells and supplies shared typed defaults;
  each variant's exceptions are local to its definition.
- `si_model_registry()` binds those rows and applies the unchanged Phase1
  scientific-metadata helper. Its public signature and the implementation of
  `si_model_registry_species()` are unchanged.
- `tests/testthat/test-si_model_registry.R`: added a focused constructor
  regression covering multi-province/species/region vectors, named fixed
  arguments, plotting values, and preservation of empty lists, NULL and typed
  NA cells when default and populated rows are bound together.
- No engine, coefficient, parameter-data, ranking, discovery, predictor-contract,
  documentation or website changes were made. Existing worktree changes were
  preserved; this plan remains untracked as before.

#### Checks and handoff

- Captured both complete Phase1 outputs before editing, then compared them in
  fresh R processes with the refactored outputs using `identical()`:
  the **51 x 27 registry** and **51 x 30 species-expanded registry** both pass.
  This verifies every column, row/column order, type, attribute and nested
  list/table value, including ranks, fixed arguments, scientific metadata,
  plotting settings and species coverage, rather than only row counts.
  Before/after RDS artifacts are retained as `si-phase2-before.rds` and
  `si-phase2-after.rds` in the executing Copilot session's `files/` directory,
  outside the repository.
- Targeted registry tests passed after refactoring.
  `devtools::test(filter = "^si_", reporter = "summary",
  stop_on_failure = TRUE)` subsequently passed all46 SI test files, including
  the new constructor case and existing article-helper regressions, without
  failures, warnings or skips. Final complete-output equivalence and
  `git diff --check` also passed.
- No full package check, documentation regeneration, data rebuild or website
  build was needed for this internal, output-preserving refactor.
- **Handoff:** Phase2 is complete. Phase3 can build on the per-variant
  definitions, but predictor/operation contracts and consistency checks remain
  its separate scope. No additional variants, ranking policy, scientific
  reinterpretations or wrapper/discovery behaviour were introduced.

### Phase 3 — 2026-11-10: operation-aware input contracts and checks

**Outcome:** Completed the approved metadata/checking scope for all
**51 variants / 45 engines**. Added no variants, dispatcher, discovery API,
ranking changes, engine changes or scientific transformations. Phases 4-6
retain their existing statuses and approval gates.

#### Authoritative contracts and placement

- `R/si_model_predictors.R` contains shared operation-aware input and
  constraint tables, exposed through the registry's new `operation_inputs`
  and `input_constraints` list-columns: **564 input rows / 141 constraint
  rows**. Requirements have units, scientific definitions, evidence kind/status
  and source locators, and use `required`, `optional`, `fixed` or `forbidden`
  roles independently for each operation. Fixed inputs carry their values.
  No single required-input vector conflates the alternative operations.
- Conventional height/SI operations independently require age and exactly the
  appropriate height/SI input, plus species where the engine requires it.
  GI contracts require age and GI only, retain the two discrete age domains,
  and describe the source's above-BH averaging interval and first-year
  correction. RR18 p.5 eq.1 and Nigh1998 eq.3 were checked locally; their
  respective height-growth/growing-season fraction conventions are retained.
- Ecological contracts require species, ecological region/type and an exact
  joint parameter key, not arbitrary height or SI. Subregion requirements
  are derived from species-region-type groups without a missing-subregion
  row. Conditional inputs reference named condition IDs; constraint selectors
  are conjunctions of exact named scalar matches, never executable expressions.
  Explicit NA subregion keys mean missing, not a wildcard. Supported-key
  tables exclude the potential PICE.GLA / 5b / missing / MS22 LOGIST3 row;
  a separate unsupported-form record preserves the engine's explicit error.
- Climate contracts require all four positive finite aggregates in both
  operations, while non-climate HI forbids them. Species-specific PY20/PY10/PY
  windows and observed ranges come from the compiled climate parameter table.
  Semantic prerequisites exclude current/pre-initiation years and retain
  location-specific, fixed 1961-1990 normals. Their engine action is
  `not_enforced`: aggregate values cannot prove history/coverage. Observed
  ranges and the upper age180 warning remain distinct from mathematical
  domains; an unencoded lower calibration-age bound is not invented.
- The new `native_defaults`, `variant_args` and `plot_args` list-columns
  separate formal-derived native defaults, variant-defining selectors and
  plot overrides. Multi-choice selectors resolve to the engine's first
  native choice. Pregent's base_age50 remains plot-only, with native25.
  Legacy `nigh2009` explicitly denotes base variant1, and `huang2026` denotes
  the non-climate fit, without changing their raw fixed_args. No operational
  or zonal Nigh2009 IDs were approved/added. Nigh2002/2004 retain optional
  supplied-zone modes and their existing zone support.
- `R/si_model_metadata.R` adds explicit engine-documented native-scale
  declarations for the remaining22 variants, yielding **64 scientific
  definition rows for all51 variants**. Reference clocks are independently
  declared, not inferred from input clocks. New definition/evidence fields
  identify current engine contracts, not independent publication verification;
  unresolved clocks/datums remain unresolved. Missing definitions cannot be
  silently filled with an assumed universal SI50 scale. Existing source
  disputes, conditional datums, Payandeh references and applicability gaps
  are preserved.

#### Consistency checks and affected files

- `R/si_registry_checks.R` adds `.si_registry_problems()`, following the HD
  pattern of returning actionable problem strings. Checks cover duplicate/
  unknown IDs and input keys; legal engine arguments, complete formal coverage
  and required no-default arguments per operation; height/SI/GI exclusivity;
  capability/quantity consistency; fixed/variant/plot conflicts and native
  defaults; evidence/definition presence; scientific enums, ambiguity and
  species-specific coverage; conditional references/selectors; discrete
  support; ecological exclusions; and species-specific climate requirements,
  domains and aggregation metadata. Unknown scientific metadata is not ALL.
- `tests/testthat/test-si_model_contract.R` adds focused positive contracts
  and negative mutations demonstrating detection of incomplete/contradictory
  metadata, plus representative engine calls for exact ecological matching
  and the unsupported key. Existing SI engine tests remain unchanged.
- `R/si_model_registry.R` wires in the metadata and documents its semantics.
  `NEWS.md` records the additive contract. `man/si_model_registry.Rd` was
  regenerated using configured/installed roxygen2 **8.1.1**, not hand-edited.
  No website, article, parameter-data or dependency changes were made in this
  phase; unrelated pre-existing worktree changes remain untouched.

#### Verification and handoff

- New contract tests failed against the Phase2 implementation, then passed
  after implementation. The targeted registry/contract tests and
  `devtools::test(filter = "^si_", reporter = "summary",
  stop_on_failure = TRUE)` passed; the latter covered all47 SI test files,
  including numerical engine regressions and existing article-helper tests,
  without failures, warnings or skips.
- Saved complete before/after outputs in the executing Copilot session's
  `files/si-phase3-before.rds` and `files/si-phase3-after.rds`. Exact
  `identical()` comparisons preserve every legacy column outside the two
  deliberately enriched scientific/evidence list-columns, in both the
  registry and species expansion. Within those two columns, every existing
  row/value/type is preserved on its original columns; only new declarations,
  evidence rows and fields were added. IDs/order, ranks, raw fixed args,
  species coverage, plotting settings and GI age_constraints are unchanged.
  Registry dimensions are now **51 x 32**, species-expanded **51 x 35**.
- Construction initially created hundreds of individual metadata tibbles;
  batching typed row lists before binding reduced a measured fresh registry
  call from about2.9 to1.0 seconds, without caching or output changes.
  `.si_registry_problems()` returns `character()` for the complete registry.
- `devtools::document()`, `tools::checkRd()` and `git diff --check` passed.
  No full package check, data rebuild or website build was performed.
- **Handoff:** Phase3 is complete. Phase4 may consume the authoritative
  operation inputs, exact-key/selector conditions and argument views for
  discovery/info output. These tables/checks are not a general rule evaluator,
  row-level eligibility validator, automatic mode selector or dispatcher.
  Disputed science, new variants, cross-model SI conversion, ranking/ties and
  wrapper fallback remain deferred; no new scientific decisions were made.

### Phase 4 — 2026-11-10: discovery and scientific model information

**Outcome:** Implemented and exported `find_si_models()` and
`get_si_model_info()` for all **51 variants / 45 engines**. No engines,
coefficients, registry metadata, IDs, ranks or plotting settings were changed.
No `si()` dispatcher, model execution, automatic selection, ranking policy,
scientific conversion or new variant was introduced. Phase5 remains deferred
pending explicit approval.

#### Discovery contract and decisions

- `R/si_model_discovery.R` uses the Phase3 registry child tables directly.
  Species and jurisdiction restrict legacy registered coverage; scientific
  requests annotate rather than hide near-misses. Results retain registry
  order, with one row per model/operation/covered requested species.
  Multi-species queries keep Payandeh's distinct scales in separate rows.
  Jurisdiction coverage remains a registry scope, not source calibration or
  an approved transfer domain.
- An omitted operation lists each supported operation; an explicit operation
  keeps unsupported variants as labelled exclusions. Conventional SI, realized
  HI, climate inherent HI, GI and ecological IQS remain visible and distinct.
  Nothing implies they are ordinary automatic-dispatcher candidates.
- Optional comparisons cover input age clock, reference age and its independent
  clock, productivity quantity, SI/height datums and stand origin.
  `compatibility` records each requested/actual comparison and reason;
  `scientific_compatibility` distinguishes compatible, incompatible, unknown
  and not-requested comparisons. Known mismatches remain in
  `incompatibility_reasons`; missing/unresolved information is separate.
  Disputed claim subjects prevent legacy engine age labels or unresolved
  datums from establishing either a match or a known scientific mismatch.
  Compatibility applies only to requested fields, not complete source
  suitability, sampling/composition, independent publication verification or
  cross-model SI interchangeability.
- Comparisons use native defaults plus variant-defining selectors only.
  Pregent remains native SI25 despite its SI50 plot setting; conditional
  Sharma height definitions resolve at native `total_height`, without
  changing SI datum. Discovery never changes options to force a match.
  Full alternative definitions, source evidence and applicability remain
  available for inspection.
- `available` means argument names the caller could supply, not arguments
  actually passed. A species query supplies the species input name.
  `missing_predictors` contains unconditional missing names; conditional
  requirements and their potentially missing names are separate, restricted
  by known species without guessing ecological selector values.
  `inputs_available` is FALSE for unsupported/missing unconditional inputs,
  NA for potentially missing conditional inputs, and otherwise TRUE.
  Available forbidden names identify arguments not to pass; extra data
  columns alone do not make an observation incompatible.
- `discovery_status` summarizes incompatible, missing-input, needs-review or
  names-complete results. No `usable` flag is offered; `eligibility` is always
  `not_evaluated`. Discrete ages, exact joint ecological keys/unsupported
  forms, selector values, numeric domains, calibration ranges and climate
  aggregation are exposed but not evaluated from names. Free-text population,
  composition, geography and sampling still require review; no unapproved
  vocabulary, regional mode or unknown-origin fallback policy was invented.

#### Information, documentation and affected files

- `get_si_model_info()` returns unwrapped registry/species metadata with
  supported operations, native/variant-effective/plot argument views,
  native and alternative scientific definitions, operation-aware input
  definitions, exact constraints, applicability, evidence, unresolved claims
  and limitations. Variant-effective arguments are derived, not duplicated.
- `tests/testthat/test-si_model_discovery.R` covers all families, normalized
  scopes, operation exclusions, missing inputs, forbidden available columns,
  conditional ecological requirements, climate quantities/inputs, native
  versus plot scales, species-specific references, independent input/reference
  clocks, combined stand origins, disputed science, info views, invalid queries
  and stable typed empty results.
- `NEWS.md` records the feature. `NAMESPACE`,
  `man/find_si_models.Rd` and `man/get_si_model_info.Rd` were generated with
  configured/installed roxygen2 **8.1.1**, not hand-edited.
  No article/site build, dependency change or parameter-data rebuild was made;
  all unrelated/pre-existing worktree changes were preserved.

#### Verification and remaining approval gate

- New discovery tests initially failed because the helpers did not exist.
  After implementation, all48 SI test files passed using
  `devtools::test(filter = "^(si_|si_model_discovery)",
  reporter = "summary", stop_on_failure = TRUE)`, including registry,
  contracts, numerical engine regressions and article-helper tests, without
  failures, warnings or skips. A final targeted discovery run also passed
  after adding independent-clock/combined-origin cases.
- `devtools::document()`, `tools::checkRd()` on both new topics, execution of
  both topics' extracted examples, export checks and `git diff --check` passed.
  Initial cross-topic warnings on the first documentation generation resolved
  after the new topics existed. A representative Ontario black-spruce query
  was inspected: unresolved population/clock claims remain needs-review,
  while known datum/origin mismatches remain incompatible.
- No full package check or website build was performed. Existing SI numerical
  behavior was exercised by the focused SI suite, not reinterpreted.
- **Handoff:** Phase4 is complete. Review discovery output before approving
  the Phase5 wrapper contract. Its requested SI definition/output scale,
  unknown-origin/geographic policy, ranking/ties, fallback/provenance and
  trajectory consistency remain undecided. Disputed source science and new
  variants remain deferred. **Stop here: do not implement `si()` without
  explicit approval.**

### Phase 5 approval — 2026-11-10

The user approved the suggested conservative defaults and the subsequent
native-scale approach: "agree, continue with the suggested defaults and with
the described approach above".

- Require the observation's input age clock and exactly one of height/SI.
  Do not impose total age or BHA package-wide or convert between them.
- SI estimation may return the selected model's native scale without a
  requested output reference; always retain its definition and provenance.
  Explicit definition requests restrict eligibility rather than converting.
- Height prediction requires the supplied SI definition; an explicit model
  ID can declare that model's native SI scale and fixes trajectory identity.
  Identical SI reference labels alone do not prove cross-model interchangeability.
- Select observation-wise, with explicit model IDs for reproducibility.
  Filter compatibility before highest registry rank, breaking ties by model ID.
  Unknown applicability is not unrestricted and is excluded from automatic
  selection; explicit model use must disclose unverified applicability.
- Preserve native warning/extrapolation behavior without clipping or
  inventing new calibration bounds. Expose candidate failures and reasons;
  never hide unexpected engine errors behind a successful fallback.
  Fallback cannot change the SI definition or reinterpret supplied SI.
- Best mode returns one row per observation; comparison mode returns
  per-observation candidate tibbles. Keep scientific provenance and diagnostics.
  GI, ecological IQS and climate inherent HI remain outside this wrapper.

Implementation details and verification will be recorded below. This approval
does not authorize new variants, parameter changes, disputed scientific
reinterpretations or Phase6.

### Phase 5 — 2026-11-10: conventional native-scale dispatch

**Outcome:** Implemented and exported `si()` for conventional non-climate
height-age models, including separately labelled non-climate realized HI.
All **51 registered variants / 45 engines** and their scientific metadata,
coefficients, native defaults, ranks and plotting settings remain unchanged.
GI, ecological IQS and climate inherent HI remain discoverable but are not
dispatcher candidates; explicit specialized IDs are rejected in this initial
wrapper. Phase6 remains deferred.

#### Implemented API and scientific safeguards

- `R/si.R` requires `input_age_basis` and exactly one of `height`/`si`.
  Inputs recycle to a common length, including scalar age with vector
  predictors/metadata, and work inside `dplyr::mutate()`. Missing observations
  produce explicit missing statuses and a summary warning; malformed inputs,
  incompatible lengths and invalid/non-finite supplied numeric values error.
- SI estimation can use native-scale mode without requesting a reference age
  or SI datum. Every returned prediction carries input/reference clocks,
  reference age, productivity quantity, independent SI/height datums,
  reference-age role and native definition ID. Optional scientific requests
  filter candidates; they never change `base_age`, apply plot overrides,
  add a height offset or convert age clocks. Pregent remains native SI25.
- Supplied SI requires either its originating `model_id` (declaring that
  model's native SI) or all four definition fields: reference age, reference
  clock, SI datum and quantity. Unpinned SI emits an interchangeability warning;
  no supplied SI is ever passed to a fallback model after a failed prediction.
  Existing source-supported conversions remain inside their registered
  engines, e.g. Huang2009's total input age / BHA-reference SI variant.
- Automatic selection excludes unknown/mismatching origin, unresolved native
  SI definitions/source-clock disputes and known geographic mismatches before
  preference ranking. BC coast/interior and SBS/ESSF restrictions come from
  existing registry/applicability metadata; required BEC information cannot
  be guessed. Free-text composition, stand structure, local geography and
  sampling still require source review, not an invented compatibility rule.
- Explicit IDs fix the engine/variant for reproducible trajectories. They
  never bypass species/jurisdiction coverage, known mismatches, unresolved SI
  definitions or requested scientific fields. Unverified origin/geographic
  applicability can be explicitly reviewed by the caller and is disclosed
  through an applicability warning and candidate limitations, not silently
  treated as verified. Stand origin is not defaulted to natural.
- Preference uses highest legacy rank then lexical model ID, with radix
  ordering for stable ties. This is observation-wise pragmatic preference,
  not a claim of empirical superiority or a stand-level trajectory lock.
  Fixed model IDs and regional settings provide the explicit trajectory path.
- Best mode returns one tibble row per observation with status/reason and a
  `diagnostics` list-column. Comparison mode returns one candidate tibble per
  observation, retaining exclusions, failures and scientific definitions,
  including different native scales. Empty scope matches retain typed schemas.
  Actual engine arguments, warnings and limitations are inspectable; hiding
  the top-level model ID does not erase diagnostic/definition provenance.

#### Failures, extrapolation and scope

- Native warnings/extrapolation pass through and are recorded without
  clipping or treating legacy plotting age caps as eligibility bounds.
  Non-finite/non-positive returned predictions are explicit failures. For SI
  estimation, fallback is restricted to the first eligible model's quantity,
  reference age/clock, input clock, datums and reference-age role, with a
  fallback warning. Each model independently evaluates measured height.
- Engine errors, including engine-domain errors lacking a reliably distinct
  condition class, abort with model/observation context and the original
  parent condition. They are **not** silently converted into candidate failures
  or a successful fallback. Malformed engine return shapes also abort.
- Only native definitions and registered variants are dispatched.
  Definition-changing engine selectors in `...` are rejected; callers needing
  alternative `base_age`/`total_height` settings use the direct engine.
  `natural_subregion` is supported, and explicitly supplied `bec_zone` is
  forwarded only to engines accepting it; no regional mode is inferred.

#### Affected files and verification

- Added `R/si.R` and `tests/testthat/test-si.R`; updated `NEWS.md`.
  Roxygen2 **8.1.1** generated `man/si.Rd` and the `NAMESPACE` export.
  No engine, registry, parameter, dependency, article or generated website
  files were changed in this phase; pre-existing worktree changes remain.
- Wrapper tests first failed before implementation, then passed. Tests compare
  native SI/height results directly with Pregent2010, Sharma2021, Huang2009,
  non-climate Huang2026 and Batho2014, covering distinct input/reference clocks,
  datums, quantities and native-versus-plot reference ages.
  Focused cases also cover stable ties/rank precedence after compatibility,
  explicit IDs, missing rows, recycling/mutate use, origin/geographic and
  disputed-science exclusions, specialized-family rejection, diagnostics,
  warning/extrapolation preservation, safe/unsafe fallback, propagated
  unexpected errors, malformed returns and invalid arguments.
- `devtools::test(filter = "^si($|_)", reporter = "summary",
  stop_on_failure = TRUE)` passed all49 SI test files without failures,
  warnings or skips, including existing numerical engine, registry, discovery,
  contract and article-helper regressions.
- `devtools::document()`, `tools::checkRd("man/si.Rd")`, execution of the
  generated wrapper examples, export verification and `git diff --check`
  passed. No full package check, website build or data rebuild was performed.
- **Handoff:** Phase5 is complete under the approved native-scale contract.
  Automatic applicability remains intentionally conservative where metadata
  is sparse; direct engines and explicit source-reviewed IDs remain available.
  Do not broaden unknown-origin policy, normalize SI scales, add variants or
  implement specialized dispatchers without a separate decision. Stop before
  Phase6.

### Source reconciliation of the nine blocked variants

**Research handoff; not an implementation approval.** The source re-read and
antecedent tracing below establish the disputed definitions for seven variants.
Payandeh1974 remains partly resolved by species; SharmaReid2018's vertical
datum remains unresolved. These findings supersede the corresponding earlier
research uncertainties, but **the executable metadata and selection gates have
not yet been changed**. Scientific resolution does not itself establish
automatic-selection eligibility, geographic applicability, or stand origin.

| Family / variants | Finding | Remaining implementation issue |
|---|---|---|
| ScottVoorhis1986, both IDs | Equation uses total input age and SI50 total age. The optional conversion accepts BHA and adds YTBH. | Correct the reversed legacy input-clock descriptions, preserving IDs and numerical behavior. |
| LundgrenDolid1970, both forms | Balsam fir and white spruce use BHA/SI50 BHA; the other nine species use total age/SI50 total age. | Encode clocks by species and retain the authors' white-spruce non-recommendation. |
| ParresolVissage1998 | Frothingham's source data use total age; the configurable reference age uses the same clock. | Correct blanket BHA documentation/metadata; no age conversion or equation change. |
| Nigh2004 | Juvenile observation age is total age; supplied SI is total height at BHA50, estimated through the cited GI models. | Record the mixed clocks and GI lineage; do not test this juvenile model by extrapolating to age 50. |
| NighCourtin1998 SI50 | The paper explicitly publishes the implemented SI25-to-SI50 conversion. | Replace the unresolved-provenance record; retain the rounded, source-derived conversion role. |
| Payandeh1974 | Author explicitly requires species-specific antecedent age definitions. Total-age evidence found for Douglas-fir and red spruce. | Inspect the remaining antecedents and the cited Douglas-fir edition before declaring the whole family resolved. |
| SharmaReid2018 | BHA50 reference and natural origin are explicit; above-BH versus total-height response is not explicitly settled by the inspected paper. | Keep the datum dispute; do not add/subtract 1.3 m based on analogy or equation shape. |

#### Scott and Voorhis (1986): source clocks established

- Printed p.145 (PDF p.2), Equation 4 variable definitions and Table 2
  footnote explicitly define `A` as **total age** and SI as **total height of
  dominant/codominant trees at 50 years total age**.
- Printed p.146 explains that BHA observations require estimation of age to
  breast height, and that this step can be omitted when total age is known.
  The published BASIC appendix, pp.147-148, reads BHA and computes
  `TOTAGE = BHAGE + AGE` before evaluating the SI equation.
- Consequently the current selector mapping is:
  `convert_to_total_age = FALSE` accepts **total age**;
  `convert_to_total_age = TRUE` accepts **BHA**, converting it internally.
  Both produce the same native **SI50 total-age** quantity.
- The legacy IDs are misleading: `scottvoorhis1986_bh_age` currently selects
  FALSE, whereas `scottvoorhis1986_total_age` selects TRUE. Do not silently
  swap their selectors or rename public IDs. Correct their scientific labels
  and docs explicitly, with wrapper/direct-engine equivalence tests.
- Source: `sources/site_index/Scott and Voorhis - 1986 - Northeastern Forest
  Survey Site Index Equations and Site Productivity Classes.pdf`.

#### Lundgren and Dolid (1970): all species clocks established

- Printed pp.3-4 (PDF pp.6-7), Table 1 footnote and following discussion
  explicitly identify balsam fir and white spruce as BHA-based.
- Traced the other species to their original two-page curve publications.
  The reverse-side "Site Index--Its Use and Limitations" in the Gevorkiantz
  notes explicitly says the curves use total age except for white spruce and
  balsam fir. Cooley's paper-birch note explicitly specifies total age rather
  than BHA. This is positive evidence, not an inference from absent footnotes.
- For **both forms**, observation and SI-reference clocks are therefore:
  BHA for `ABIE.BAL` and `PICE.GLA`; total age for `PINU.RES`, `PINU.BAN`,
  `PINU.STR`, `PICE.MAR`, `LARI.LAR`, `THUJ.OCC`, `POPU.TRE`, `QUER.RUB`,
  and `BETU.PAP`. Reference age is 50; height/SI are total height.
- Tables 2 and 3, footnote 3 (printed pp.4 and 6; PDF pp.7 and 9), explicitly
  do **not** recommend either fitted form for white spruce. That is a known
  recommendation limitation, not an unresolved age clock. Proposed selection
  treatment: exclude white spruce automatically while retaining discoverability
  and the unchanged direct engines. The p.4 exponential-form balsam-fir
  recommendation is restricted to stands at least 20 years old.
- Original notes read, including reverse-side definitions:
  [red pine, TN484](https://archive.org/details/nfsl_2_264387),
  [jack pine, TN463](https://archive.org/details/nfsl_2_264091),
  [white pine, TN483](https://archive.org/details/nfsl_2_264191),
  [black spruce, TN473](https://archive.org/details/nfsl_2_264203),
  [tamarack, TN498](https://archive.org/details/nfsl_2_264217),
  [white-cedar, TN472](https://archive.org/details/nfsl_2_264393),
  [aspen, TN464](https://archive.org/details/nfsl_2_264082),
  [red oak, TN485](https://archive.org/details/nfsl_2_264214), and
  [paper birch, TN541](https://archive.org/details/CAT31400893).
  Cooley's reverse side is PDF p.4; PDF p.2 is blank.
- Main source:
  `sources/site_index/DolidLundgren1970-Biological_Growth_Functions_Describe_Pub.pdf`.

#### Parresol and Vissage (1998): total-age antecedent established

- RP-SRS-10, printed p.1 (PDF p.3), explicitly identifies Frothingham (1914),
  Tables 3-5, pp.21-22, as the source of the fitted age-height data.
- Read [Frothingham's original USDA Bulletin 13](https://archive.org/details/whitepineunderfo13frot).
  Printed p.19 (PDF p.25) explains age determination from all annual growth,
  including the first year's growth near ground level, and the need to account
  for years below the height of an age observation. This establishes total
  age, rather than the current engine's documented BHA interpretation.
- Observation age and configurable SI-reference age should both be labelled
  total age. Retain the current base-age selector, total-height response,
  coefficients and numeric calculations. Do not insert Frothingham's rough
  YTBH approximation as an automatic conversion.
- Main source: `sources/site_index/rp_srs010.pdf`.

#### Nigh (2004): juvenile total age, mature SI50 BHA

- RR25, printed pp.2-3, describes height above germination and height-total-age
  data; it states that SI was estimated with Nigh (1997, 2004) GI models.
  Its bibliography, pp.17-18, identifies Extension Note 11 and Technical
  Report 013, respectively.
- [Nigh 1997, EN11](https://www.for.gov.bc.ca/hfd/pubs/Docs/En/En11.pdf),
  p.2, Equation 2 and variable definitions, explicitly defines lodgepole-pine
  SI as top height at **BHA50**.
- [Nigh 2004, TR013](https://www.for.gov.bc.ca/hfd/pubs/Docs/Tr/Tr013.pdf),
  pp.2-3, traces the spruce GI models to Nigh (1999).
  [Nigh 1999, EN37](https://www.for.gov.bc.ca/hfd/pubs/Docs/En/En37.pdf),
  p.2, explicitly defines SI as average sample-tree height at **BHA50**;
  its height-minus-1.3 GI definition distinguishes total height from height
  increment above BH.
- Thus `nigh2004` has total-age observations but total-height SI50 **BHA**.
  Its nominal SI parameter is GI-derived; the juvenile equation is not a
  generic mature-height curve constrained at age 50.
- RR25's sampled juvenile stands are described as **managed** (pp.1-2).
  Preserve that context; this research does not verify natural origin or
  establish that all managed stands are plantations.
- Main source: `tmp/si_papers/Nigh2004_Rr25.pdf`.

#### Nigh and Courtin (1998): SI50 provenance resolved

- Printed p.66 (PDF p.8), Equation 7, explicitly publishes
  `SI50 = -0.4063 + 1.313 * SI25` as a conversion derived from the height
  model. These are the coefficients already implemented.
- Printed pp.61-62 define SI25 using BHA; the discussion and Equation 7
  extend that same height-BHA model to SI50. Both indices are total height.
- The prior "independent conversion provenance unresolved" finding missed
  Equation 7 and should be replaced. No new empirical conversion is needed.
  Retain `converted_index` provenance and the source's rounded coefficients;
  lack of machine-exact `H(50) == SI50` is not a scientific incompatibility.
- Source: `sources/site_index/A_1006561502635.pdf`, **Height models for Red
  Alder (Alnus rubra Bong.) in British Columbia**, New Forests 16:59-70.

#### Payandeh (1974): partial resolution, not a blanket BHA family

- Printed p.196 (PDF p.3) explicitly directs users to original publications
  to determine whether total age, BHA, or another definition is required.
  This directly rules out treating the engine's blanket BHA documentation
  as verified publication evidence.
- The available [McArdle report scan](https://archive.org/details/nfsl_1_4682)
  defines stand age as total age since seed (p.10, "Terms used"), and SI as
  total height at age 100 (p.11). **Edition caveat:** the retrieved scan is
  the 1949 revision; Payandeh cites the 1930 report revised in 1941.
  This corroborates Douglas-fir SI100 total age but is not inspection of
  the exact cited edition.
- Carmean et al. (1989), printed p.88 (PDF p.94), Figure 73, explicitly
  identifies Meyer's (1929) red-spruce curves as total-height/total-age
  curves revised from SI65 to SI50. This corroborates red-spruce SI50
  total age, but the Meyer original has not been inspected in this pass.
- Still needed: exact cited Douglas-fir edition; Haig (1932) for western
  white pine; Smithers (1961) for lodgepole pine; Bakuzis and Hansen (1965)
  for balsam fir; Kabzems (1971) for white spruce; Meyer (1929, 1937, 1938)
  for red spruce, Sitka spruce/western hemlock, and ponderosa pine.
  The publication identifies these antecedents on p.194 and in the p.196
  bibliography. Retrieval attempts did not establish the remaining clocks.
- Retain species-specific reference ages 50/80/100 and separately fitted
  height/SI directions. Do not unlock the whole family or alter coefficients
  based on the two corroborated species.

#### Sharma and Reid (2018): initial datum review (superseded below)

- Printed p.34 (PDF p.2) explicitly defines SI using height at 50 years
  **after reaching breast height**, and describes natural-origin site trees.
  This establishes the age clock and stand origin, not the height origin.
- Printed pp.34-35 describe section heights and define the Equation 1
  response as heights `H` and `H1`; neither those passages nor the inspected
  remainder of the paper explicitly identifies subtraction of 1.3 m from
  the modeled heights. Current engine docs nevertheless assert above-BH
  height, without applying an offset.
- Do not infer the datum from the equation's behavior near age zero, from
  other Sharma papers, or from the absence of an offset in this engine.
  Resolving it requires the original analysis/data-processing description,
  author code, or explicit clarification. The antecedent data papers are
  Carmean et al. (2001) for jack pine and (2006) for black spruce; those
  analyses have not been independently reconciled in this pass.
- Source: `sources/site_index/forestscience_64_1_33.pdf`.

#### Handoff boundary

- Only this research handoff was changed. No R code, metadata, model IDs,
  coefficients, registry ranks, tests, generated documentation, or dispatch
  gates were changed. The selection diagram still describes executable
  behavior, not these proposed corrections.
- Publication prose was extracted locally; decisive definitions, equations
  and footnotes were checked against rendered source pages. Downloaded
  antecedents, text extracts and rendered pages are session-only artifacts
  under `files/si-reconcile-*`, not package dependencies.
- After scientific review, apply the established definitions to metadata,
  evidence, engine documentation, discovery tests and dispatch-equivalence
  tests together. Preserve numerical behavior and legacy selectors; handle
  white-spruce recommendations separately from clock compatibility.
- Unknown-origin default policy and specialized-family dispatch remain
  separate work. Neither was implemented by this research.

#### Payandeh antecedent follow-up: downloaded originals and age clocks

This follow-up supersedes the earlier "still needed" list for the retrieved
publications only. Source PDFs are retained under the ignored
`sources/site_index/Payandeh_antecedents/` directory. Printed page numbers and
1-based PDF page numbers below refer to those specific scans.

| Payandeh species | Observation/reference-age clock | Payandeh SI reference age | Evidence |
|---|---|---|---|
| Western white pine | Total age | 50 | Haig (1932), p.2 (PDF p.3), definition of total age; p.7 (PDF p.8), Figure 2, height curves explicitly plotted against total age. |
| Lodgepole pine | Total age | 80 | Smithers (1961), p.143 (PDF p.142), Tables VIII-IX, explicitly labelled Total Age; p.54 (PDF p.53), curves standardized at 80 years. |
| Red spruce | Total age | 50, converted by Payandeh from original SI65 | Meyer (1929), p.5 (PDF p.7), Age of Stand and Site Index; Payandeh (1974), p.194, explains the reference-age conversion. |
| Sitka spruce and western hemlock | Total age | 100 | Meyer (1937), pp.7-8 (PDF pp.13-14), Age of Stand and Site Index definitions. |
| Ponderosa pine | Total age | 100 | Meyer (1938), pp.7-8 (PDF pp.12-13), Age of Stand and Site Index definitions. |
| Douglas-fir | Total age in later editions; exact cited 1941 revision not verified | 100 | McArdle et al., 1949 revision, pp.10-11 (PDF pp.20-21); the separately downloaded 1961 slight revision also uses total age. |
| Balsam fir | Unresolved | 50, reported by Payandeh | Bakuzis and Hansen (1965) full text was not obtained. |
| White spruce | Unresolved | 50, reported by Payandeh | Kabzems (1971) full text was not obtained. |

Decisive passages and interpretation:

- **Haig (1932), `Haig1932_TB323.pdf`:** p.2 defines
  "Total age" as "The age of the oldest dominant tree." Figure 2, p.7,
  labels its horizontal axis "TOTAL AGE (YEARS)"; the index curves agree
  with the 50-year reference age reported by Payandeh. Preserve the
  oldest-dominant age statistic rather than describing it as the average
  dominant/codominant age used in several other reports.
- **Smithers (1961), `Smithers1961_Bulletin127.pdf`:** Tables VIII-IX,
  p.143, describe the Alberta foothills curves with explicit "Total Age"
  columns. Their 80-year rows have dominant heights matching their site
  index classes. This is not merely inference from the British Columbia
  tables: the Alberta tables themselves specify total age. On p.54,
  Smithers states that the Alberta, Oregon and British Columbia curves
  were adjusted to "the same index value of 60 feet at 80 years."
  The same page cautions that this SI combines site quality and density,
  and is unsuitable for predicting stands radically changed by thinning.
  That scientific limitation is separate from the resolved age clock.
- **Meyer (1929), `Meyer1929_TB142.pdf`:** p.5 explicitly says,
  "In the following sets of tables whenever age is mentioned, total age
  is meant, unless otherwise stated." Breast-height ring counts receive
  a 15-year addition; one-foot stump counts receive seven years. The
  same page defines original red-spruce SI at "65 years total age."
  Payandeh p.194 explicitly converts SI65 to SI50 by nonlinear
  interpolation; this changes the reference age, not the age clock.
- **Meyer (1937), `Meyer1937_TB544.pdf`:** p.7 defines stand age as
  average age of sample dominant and codominant trees and says that,
  to obtain total age from breast-height borings in Oregon and
  Washington, seven years are added. P.8 defines SI at age 100.
  The seven-year allowance is a source-specific regional prescription,
  not a generic BHA-to-total conversion for all Canadian applications.
- **Meyer (1938), `Meyer1938_TB630.pdf`:** p.7 likewise defines stand
  age from dominant/codominant trees, with 6-16 years added to
  breast-height counts to obtain total age, depending on site quality.
  P.8 defines SI at age 100. Do not introduce a universal constant
  conversion based on these allowances.
- **McArdle et al., `McArdle_Meyer_Bruce_TB201_1949_revision.pdf`:**
  p.10 explicitly defines age as average total age of dominant and
  codominant trees, "the number of years since the stand started from
  the seed." P.11 defines SI using average total height at age 100.
  The other downloaded copy,
  `McArdle_Meyer_Bruce_TB201_1949_revised1961.pdf`, is clearly marked
  "Slightly revised May 1961"; it must not be cited as the exact 1941
  edition. Both later editions corroborate total age, without resolving
  the exact-edition evidence gap.

Result: six of Payandeh's nine species now have direct total-age evidence
in the cited antecedent publications; Douglas-fir has corroborating
total-age evidence from later editions. Balsam fir and white spruce remain
unresolved. No inspected antecedent supports a BHA classification for
these resolved species. The engine's blanket BHA documentation therefore
cannot be retained as a scientific definition for the whole family.

This is an evidence handoff only. No R code, coefficients, metadata,
documentation generated from R, tests, or dispatch gates were changed.
Decisive age-definition pages, SI-reference definitions and the lodgepole
table headings were checked against rendered PDF pages; extracts and
images are retained in the session files under `age-review-*`.

#### Payandeh total-age implementation decision and handoff

The user approved treating the remaining unresolved Payandeh species as
total age: "ok lets assume the unresolved are also total age. implement."
This supersedes the earlier executable BHA/unresolved contract for
`payandeh1974`, not the source-evidence gaps or other model families.

- Registry, species-specific scientific definitions and engine roxygen now
  consistently use total input and reference age for all nine species.
  SI reference ages remain 50/80/100 from the existing parameter table.
  No numerical equations, coefficients, argument signatures or return types
  changed, and no BHA-to-total conversion was added.
- Balsam fir (`ABIE.BAL`) and white spruce (`PICE.GLA`) are explicitly
  **assumed**, not publication-verified. Douglas-fir retains the exact-1941-
  edition caveat. Discovery/info and dispatch diagnostic limitations retain
  these disclosures, along with Haig's oldest-dominant age statistic,
  Smithers's density limitation and the red-spruce SI65-to-SI50 provenance.
- Evidence separates the verified antecedent claims from the accepted
  `age_basis_assumption` and `antecedent_edition` gaps. Those gaps remain in
  `unresolved_evidence` for inspection, but do not invalidate the approved
  total-age contract. Other models' unresolved clock/datum claims still
  block scientific compatibility as before.
- `si(..., model_id = "payandeh1974", input_age_basis = "total_age")`
  can now dispatch in either direction, on each species' native scale.
  BHA requests are incompatible, not converted. Origin/applicability
  uncertainty is still disclosed with the existing explicit-use warning.
- Automatic origin screening is unchanged: Payandeh still requires an
  explicit ID because its stand-origin applicability is not encoded.
  The selection diagram now shows 28 explicit-use and eight
  science-blocked conventional variants; the ten potentially automatic
  and five specialized variants are unchanged.
- Regenerated `man/si_payandeh1974.Rd`, updated NEWS and the selection
  diagram, and revised registry/discovery/contract expectations. Focused
  tests cover both directions against direct calls for all nine species,
  all native reference ages, BHA rejection, retained assumptions and the
  unchanged automatic-origin gate. The five affected test files passed:
  `test-si.R`, `test-si_payandeh1974.R`, `test-si_model_registry.R`,
  `test-si_model_contract.R`, and `test-si_model_discovery.R`.

Other publication-reconciled model corrections, the natural-origin default
proposal and Phase 6 specialized dispatch remain separate, unimplemented work.

#### Sharma-Reid follow-up: explicit above-BH evidence located

The follow-up located decisive evidence missed by the initial review.
Printed **p.38 (PDF p.6), Figure 2**, in Sharma and Reid (2018), explicitly
states: **"All stand height values are from breast height."** The caption
identifies both species' curves as generated using **Equation 1**, the
fixed-effects equation implemented by `si_sharmareid2018()`. This was
verified on the rendered page, not inferred from the curves' age-zero
behavior or another Sharma paper.

- Together with the BHA50 definition on p.34 and the explanation on p.37
  that stand heights at BHA50 supply SI, this supports **above-breast-height
  height and SI** for the implemented native equation. Breast height is
  1.3 m. The existing engine's documented height origin is therefore
  supported; the earlier unresolved-datum assessment was too conservative
  because it missed the explicit caption.
- The antecedents do not justify substituting a total-height contract.
  Carmean et al. (2001), printed p.143 (PDF p.3), Table 1 footnote,
  explicitly defines SI as total height at BHA50. Carmean et al. (2006),
  printed p.233 (PDF p.3), describes ground-referenced section heights and
  the stem-analysis/SI construction. Those are antecedent data and model
  definitions, not proof that the later Sharma-Reid equation retained the
  same height origin. Do not mix the native SI scales without accounting
  for the height datum.
- The caption resolves the operational datum for Equation 1; it does not
  document every intermediate data-processing step or establish the
  meaning of every summary-table value in the paper.

Verified publication links:

- Sharma and Reid (2018), Forest Science **64(1):33-40**:
  <https://doi.org/10.5849/FS-2016-133>;
  <https://academic.oup.com/forestscience/article/64/1/33/4804510>.
- Carmean, Niznowski and Hazenberg (2001), The Forestry Chronicle
  **77(1):141-150**: <https://doi.org/10.5558/tfc77141-1>;
  PDF: <https://pubs.cif-ifc.org/doi/pdf/10.5558/tfc77141-1>.
- Carmean, Hazenberg and Deschamps (2006), The Forestry Chronicle
  **82(2):231-242**: <https://doi.org/10.5558/tfc82231-2>;
  PDF: <https://pubs.cif-ifc.org/doi/pdf/10.5558/tfc82231-2>.
  Crossref confirms this DOI/page range; the current `Carmean2006`
  bibliography entry's `tfc82213-2` / 213-231 is incorrect and remains
  unchanged in this research-only pass.

Publisher requests returned HTTP 403 in this environment, so no fresh
publisher PDFs were downloaded. OpenAlex identifies public PDF links for
both Carmean papers but no open copy of Sharma-Reid. All three full PDFs
were already present under `sources/site_index/`; their prose and the
decisive rendered pages were inspected locally. Text extracts and page
images are retained in the session files as `sharmareid2018-datum-*` and
`carmean2001-datum-*` / `carmean2006-datum-*`.

**Implementation boundary:** only this research handoff changed. The
registry still marks Sharma-Reid's height/SI datum unresolved and the
wrapper gate is unchanged. The next implementation should reconcile the
definition/evidence rows with the explicit Figure 2 statement, preserve
the native above-BH scale and numerical engine, and update the affected
discovery/dispatch tests and selection diagram. No total-age conversion
or numerical refit is warranted by this finding.

#### Sharma-Reid implementation handoff

The user approved implementation after reviewing the explicit Figure 2
evidence. The previous research-only boundary is superseded for
`sharmareid2018`:

- Scientific definitions now declare **above-breast-height height and SI**,
  BHA input/reference clocks and the unchanged native SI50 reference.
  Claim-level publication evidence cites p.37 and p.38, Figure 2
  (PDF p.6), as verified; the earlier unresolved datum claim is replaced,
  not bypassed by weakening the common compatibility checks.
- Applicability retains natural-origin, even-aged, fully stocked,
  undisturbed jack pine/black spruce stands in northern Ontario. Its
  height-sampling description no longer says the datum is unresolved.
  These free-text stand/sampling restrictions still require source review;
  encoded origin/species/jurisdiction and scientific compatibility are
  screened as before.
- Both explicit-ID and automatic dispatch now permit compatible requests.
  Callers must specify `height_datum = "above_breast_height"` in either
  direction. The default total-height request remains incompatible.
  Total-height SI, total-age clocks and plantation origin remain
  incompatible; no 1.3 m offset, age conversion or cross-model SI
  substitution is performed.
- Engine equations, coefficients, argument signatures, defaults, ranks
  and parameter data are unchanged. Existing metadata-driven dispatch
  requires no algorithm changes. Tests compare wrapper predictions
  directly with the engine for both species in both directions and
  exercise automatic eligibility and incompatible near-misses.
- Engine roxygen and generated `man/si_sharmareid2018.Rd` now explicitly
  describe both above-BH quantities, the Figure 2 evidence and wrapper
  datum declaration. NEWS and the selection diagram are updated:
  **11 automatic-possible, 28 explicit-use, seven science-blocked,
  five specialized** variants. Specialized dispatch remains deferred.
- Corrected the directly reviewed `Carmean2006` bibliography entry to
  **231-242**, DOI **10.5558/tfc82231-2**; no Carmean numerical models
  or SI definitions changed.

Validation completed with `devtools::document()` and
`devtools::test(filter = "^si(_sharmareid2018|_model_(registry|contract|discovery))?$",
reporter = "summary", stop_on_failure = TRUE)`. All five affected test
files passed without warnings or failures. The new regression cases were
first observed failing against the unresolved metadata.

Other publication-reconciled model corrections, the natural-origin
default proposal and specialized-family dispatch remain separate work.

### Remaining conventional SI reconciliation: implemented

The user approved implementing all remaining conventional source corrections
and the previously agreed non-plantation natural-origin policy. This section
supersedes the earlier research-only and pending-implementation boundaries
for those items. Specialized-family dispatch is still intentionally excluded.

- **Scott-Voorhis, both legacy IDs:** the `bh_age` ID still selects
  `convert_to_total_age = FALSE`, now correctly documented/registered as
  total-age input. The `total_age` ID still selects `TRUE`, now correctly
  documented/registered as BHA input with the source YTBH calculation.
  Both native SI50 definitions use total age. IDs, selectors, numerical
  computations and nominal/non-exact reference conditioning are preserved.
- **Lundgren-Dolid, both forms:** added 11 species-specific definitions per
  form. Balsam fir/white spruce use BHA input and BHA50; the other nine
  species use total age and total-age SI50. Registry `age_basis` is
  `species_dependent`; all height/SI datums remain total height.
  Published recommendations are typed, species-conditioned constraints:
  white spruce is excluded automatically for either form; exponential-
  monomolecular balsam fir is excluded automatically below BHA20.
  An explicit model ID permits these recommendation violations only with
  the existing applicability-warning class and diagnostic limitations.
  Direct engines remain unchanged.
- **Parresol-Vissage:** total input/reference age, native SI50, total height;
  verified antecedent provenance replaces the unresolved BHA claim.
  The existing engine minimum ages of 10 are unchanged.
- **Nigh2004:** total-age juvenile observations and GI-derived total-height
  SI50 at BHA50 are independently encoded. Reference role remains nominal;
  the juvenile model is not tested by extrapolating to age 50 or treated as
  an exact reference-age-conditioned mature trajectory. Province-wide and
  supplied-zone modes retain their existing coefficients and behavior.
- **Nigh-Courtin SI50:** Equation 7 is now verified conversion evidence.
  Retained source coefficients, BHA reference clock, total-height datum
  and `converted_index` role; no machine-exact H(50)=SI50 requirement.
- **Origin policy:** missing/unresolved model origin now defaults to natural,
  while known plantation and natural-and-plantation classifications remain
  unchanged. Each default has a `stand_origin_assumption`, kind `assumption`,
  status `unresolved`, explaining that it is accepted policy rather than
  independent publication verification. Such disclosures do not reopen
  accepted scientific-definition gates. The 35 previously unclassified
  conventional variants can therefore participate in automatic selection.
  Missing *observation* origin is not inferred, and sampling, geography,
  composition and stand structure are not assumed unrestricted.
- **Discovery/dispatch:** all 46 conventional variants now have resolved
  clock/datum/conversion contracts. Recommendations are discoverable;
  white-spruce non-recommendations remain explicit near-misses, and numeric
  age limits are evaluated by dispatch, not name-only discovery.
  Known clock/datum/origin/geography mismatches remain exclusions even
  with an explicit ID; no generic age, height or cross-model SI conversion
  was added. Registry ranks are unchanged.
- **Documentation:** corrected five engines' roxygen contracts and regenerated
  their Rd files, plus wrapper/registry/discovery docs. The SI article explains
  origin assumptions and uses corrected metadata for age-axis labels.
  NEWS and `si_model_selection.md` reflect **46 automatic-possible,
  zero origin-only explicit-use, zero conventional science-blocked,
  five specialized** variants. The automatic-possible group contains
  38 natural-origin (source-classified or assumed), seven plantation,
  and one mixed-origin variant. Every registered ID appears exactly once
  in the model map.

Validation:

- New regression expectations were observed failing before the corrections.
- `devtools::document()` regenerated the affected documentation.
- `devtools::test(filter = "^si(_(model_(registry|contract|discovery)|scottvoorhis1986|lundgrendolid1970|parresolvissage1998|nigh2004|nighcourtin1998))?$",
  reporter = "summary", stop_on_failure = TRUE)` passed all nine selected
  files without warnings or failures.
- Wrapper/direct-engine equivalence covers both directions for every newly
  reconciled variant, both juvenile Nigh species, and all recommended
  Lundgren-Dolid species. Tests cover clock mismatch rejection, origin
  assumptions versus known plantation/mixed origins, white-spruce automatic
  exclusion/explicit warning and the BHA19/BHA20 recommendation boundary.
- Registry consistency and complete diagram ID coverage/counts were checked.

The five specialized variants remain available through direct engines and
discovery only. The observed Lafleche family's clock remains unresolved
within that specialized group. Those limitations, Payandeh's accepted
clock assumptions/edition gap and all source applicability caveats remain
visible; they were not relabelled as verified science.
