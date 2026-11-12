# How `si()` selects a model

These diagrams describe the implemented dispatcher in [R/si.R](R/si.R), not a
proposed selection policy. Mermaid diagrams render on GitHub and in
Mermaid-enabled Markdown previews.

**Current implementation:** all **46 conventional variants** have resolved
scientific contracts and can participate in automatic selection for compatible
observations. The previous unknown-origin and unresolved-definition groups are
gone. Models not identified as plantation or mixed-origin now default to
**assumed natural origin**, with the assumption retained in evidence.
The **five specialized variants** remain outside `si()`.

**The key rule:** scientific compatibility comes before rank. Input age and
the SI reference have independent clocks. The wrapper does not convert clocks,
change reference ages, add height offsets or make one model's SI interchangeable
with another's.

## 1. Inputs and candidate screening

Selection is repeated independently for each observation after common-length
recycling. A fixed `model_id` prevents observation-wise switching between models.

```mermaid
flowchart TD
    start(["Call si()"]) --> valid{"Valid types, choices, lengths,<br/>positive finite numeric values,<br/>and exactly one of height / si?"}
    valid -->|No| error["Abort with an explicit error"]
    valid -->|Yes| idcheck{"Explicit model IDs valid<br/>and conventional?"}
    idcheck -->|No: unknown or specialized| error
    idcheck -->|Yes, or no ID supplied| direction{"Which input is supplied?"}

    direction -->|Measured height| infer["Estimate SI<br/>Native scale allowed;<br/>requested definition fields are filters"]
    direction -->|Supplied SI| declaration{"Originating model_id supplied?"}
    declaration -->|Yes| native["ID declares that model's native SI<br/>Additional requests must still match"]
    declaration -->|No| full{"All four SI fields supplied?<br/>reference_age, reference_age_basis,<br/>si_datum, productivity_quantity"}
    full -->|No| error
    full -->|Yes| unpinned["Warn: matching labels do not<br/>prove SI interchangeability<br/>No fallback for supplied SI"]

    infer --> rows
    native --> rows
    unpinned --> rows
    rows["For each observation"] --> missing{"Required observation values missing?<br/>age, height / si, species, jurisdiction,<br/>input_age_basis, height_datum"}
    missing -->|Yes| na["Return NA + missing_input status<br/>and a summary warning"]
    missing -->|No| pinned{"Explicit model_id for this row?"}
    pinned -->|Yes| one["Screen only that registered model"]
    pinned -->|No| scope["Find registered species + jurisdiction matches<br/>Exact species match; no genus fallback"]

    one --> science
    scope --> science
    science["Screen every candidate:<br/>1. Conventional non-climate family<br/>2. Registered species / jurisdiction<br/>3. Unambiguous native SI definition<br/>4. Input clock + height datum match<br/>5. Requested SI fields match<br/>6. No unresolved claims for these definitions"]
    science --> pass{"All scientific checks pass?"}
    pass -->|No| excluded["Keep candidate as excluded<br/>Record incompatibility / uncertainty reasons"]
    pass -->|Yes| applicability{"Origin and encoded geography<br/>are compatible and known?"}
    applicability -->|Known mismatch| excluded
    applicability -->|Unknown, automatic selection| excluded
    applicability -->|Unknown, explicit ID| review["Permit source-reviewed explicit use<br/>Warn if attempted; retain limitations"]
    applicability -->|Known match| optional
    review --> optional
    optional["Check supplied discrete optional inputs<br/>Forward supported regional arguments;<br/>otherwise use native defaults"]
    optional --> supported{"Supported inputs?"}
    supported -->|No| excluded
    supported -->|Yes| recommendation{"Published recommendation violated?<br/>Lundgren-Dolid white spruce;<br/>exponential balsam fir below BHA20"}
    recommendation -->|Yes, automatic selection| excluded
    recommendation -->|Yes, explicit ID| cautionuse["Permit explicit use with warning<br/>Retain source recommendation limitation"]
    recommendation -->|No| eligible["Eligible candidate"]
    cautionuse --> eligible
    eligible --> execution["Continue to execution diagram"]
    excluded -.-> diagnostics["Exclusions remain in candidate diagnostics;<br/>other candidates may still qualify"]

    classDef stop fill:#fee2e2,stroke:#b91c1c,color:#111827;
    classDef caution fill:#fef3c7,stroke:#b45309,color:#111827;
    classDef good fill:#dcfce7,stroke:#15803d,color:#111827;
    class error,excluded,na stop;
    class unpinned,review,cautionuse,diagnostics caution;
    class eligible good;
```

Scientific requests use `reference_age`, `reference_age_basis`,
`productivity_quantity` and `si_datum`. An omitted field means the model's native
value when estimating SI. `input_age_basis` is always required; `height_datum`
defaults to total height and remains independent of SI datum.

Encoded BC geography checks include coast/interior and SBS/ESSF restrictions.
Free-text stand composition, structure, local geography and height sampling
still need source review; passing these checks is not universal ecological
validation.
Observation origin must still be supplied for automatic selection. The natural-
origin default applies to model applicability, not to missing observations.

## 2. Execution, fallback and output

```mermaid
flowchart TD
    screened(["Screened candidates"]) --> any{"Any eligible candidates?"}
    any -->|No| none["No prediction<br/>NA + no_model status + warning<br/>Retain exclusion diagnostics"]
    any -->|Yes| order["Order by highest registry rank<br/>Then lexical model_id, using stable radix order"]
    order --> mode{"pick_best?"}

    mode -->|TRUE: default| anchor["Anchor on the first eligible model's<br/>native scientific definition"]
    mode -->|FALSE| compare["Evaluate all eligible models independently<br/>Different native scales remain labelled<br/>This is comparison, not SI conversion"]
    anchor --> run["Call candidate engine with native defaults<br/>and registered variant arguments"]
    compare --> run

    run --> outcome{"Engine outcome?"}
    outcome -->|Error or malformed return| abort["Abort with model / observation context<br/>Do not hide errors behind fallback"]
    outcome -->|Finite positive prediction| success["Keep prediction + scientific provenance<br/>Native warnings pass through and are recorded<br/>No extrapolation clipping"]
    outcome -->|Non-finite or non-positive prediction| failed["Record candidate as failed"]

    failed --> failedmode{"pick_best?"}
    failedmode -->|FALSE| nextcompare{"More eligible comparison candidates?"}
    nextcompare -->|Yes| run
    nextcompare -->|No| listout
    failedmode -->|TRUE| fallback{"Estimating SI from measured height<br/>AND another eligible candidate has<br/>the SAME anchored definition?"}
    fallback -->|Yes| nextbest["Try the next matching candidate<br/>Re-evaluate measured height independently<br/>Warn if fallback succeeds"]
    nextbest --> run
    fallback -->|No, including all supplied-SI cases| noresult["No prediction<br/>NA + failed status + warning<br/>Retain candidate diagnostics"]

    success --> successmode{"pick_best?"}
    successmode -->|TRUE| bestout["One tibble row per observation<br/>si / height + native scientific definition<br/>status + reason + diagnostics list-column<br/>Lower candidates remain not_tried"]
    successmode -->|FALSE| nextcompare
    listout["One candidate tibble per observation<br/>Include successful, failed and excluded models<br/>Warn if no prediction succeeds"]
    bestout --> flag["keep_model_id controls the top-level ID column<br/>Diagnostic IDs and definition provenance remain"]
    listout --> flag

    classDef stop fill:#fee2e2,stroke:#b91c1c,color:#111827;
    classDef caution fill:#fef3c7,stroke:#b45309,color:#111827;
    classDef good fill:#dcfce7,stroke:#15803d,color:#111827;
    class abort,none,noresult stop;
    class failed,fallback,nextbest caution;
    class success,bestout,listout good;
```

Fallback requires the same **input clock, reference age, reference clock,
productivity quantity, SI datum, height datum and reference-age role** as the
anchor. Identical labels still do not establish cross-model SI interchangeability:
fallback estimates SI afresh from measured height; it never transfers an SI
value between engines.

Comparison mode can evaluate supplied SI under multiple models only as an
explicitly warned, unpinned comparison. Use its originating `model_id` for a
reproducible height trajectory.

## 3. Current routes for every registered model

All **51 registered variants** appear below exactly once. These are current
metadata-dependent routes, not rankings or claims that a model is intrinsically
more suitable. "Automatic possible" assumes matching species, jurisdiction,
native height datum, origin and required regional information; it does not mean
the model is eligible for every observation.

```mermaid
flowchart LR
    registry["51 registered variants"] --> conventional["46 conventional non-climate variants<br/>45 site_index + 1 realized height_index"]
    registry --> specialized["5 specialized variants<br/>Discovery and direct engines only"]

    conventional --> automatic["Automatic selection possible for all 46<br/>Scientific compatibility, origin, geography<br/>and recommendations still screen each observation"]

    automatic --> plantations["Plantation origin; total_age (years since planting); SI25<br/>Height and SI: total height<br/>augerward2021<br/>pregent2010<br/>pregent2016"]
    automatic --> plantedbha["Plantation origin; BHA; SI50<br/>Height: total; SI: above BH<br/>sharma2015<br/>sharmaparton2018a<br/>sharmaparton2018b<br/>sharmaparton2019"]
    automatic --> eitherorigin["Natural or plantation origin; BHA; SI50<br/>Height and SI: total; BC SBS zone required<br/>batho2014"]

    automatic --> natural["38 natural-origin variants<br/>Source-classified or explicitly assumed"]
    natural --> total["Total input age; 10 variants<br/>Native SI reference clocks vary<br/>buckman2006<br/>carmean1989<br/>carmeanhahn1981<br/>huang2009_bh_age<br/>huang2009_total_age<br/>huang2026<br/>nigh2004<br/>parresolvissage1998<br/>payandeh1974<br/>scottvoorhis1986_bh_age"]
    natural --> bha["BHA input; total height and SI; 23 variants<br/>SI reference ages/clocks vary<br/>alemdag1991<br/>brisco2002<br/>carmean1996<br/>carmean2001<br/>carmean2006<br/>chenklinka2000<br/>cieszewski1993<br/>cieszewskibella1991<br/>goelz1992<br/>goudie1984<br/>huang1994<br/>hugarcia2009<br/>kerbowling1991<br/>nigh1997<br/>nigh1998<br/>nigh2000<br/>nigh2002<br/>nigh2009<br/>nigh2017<br/>nighcourtin1998_si25<br/>nighcourtin1998_si50<br/>scottvoorhis1986_total_age<br/>thrower1994"]
    natural --> naturalheight["BHA; SI50; total height / above-BH SI<br/>sharma2021"]
    natural --> naturalabove["BHA; SI50; above-BH height and SI<br/>sharmareid2018<br/>sharma2022"]
    natural --> speciesclock["Species-dependent input and SI clocks; SI50<br/>BHA: balsam fir / white spruce; total: other species<br/>White spruce excluded automatically<br/>Exponential balsam fir requires BHA >=20<br/>lundgrendolid1970_exponential_monomolecular<br/>lundgrendolid1970_monomolecular"]

    specialized --> gi["Growth intercept to SI only<br/>nigh1998_gi<br/>nigh2000_gi"]
    specialized --> eco["Ecological curve evaluation<br/>lafleche2013_potential<br/>lafleche2013_observed"]
    specialized --> climate["Climate inherent HI<br/>huang2026_climate"]

    classDef good fill:#dcfce7,stroke:#15803d,color:#111827;
    classDef caution fill:#fef3c7,stroke:#b45309,color:#111827;
    classDef separate fill:#dbeafe,stroke:#1d4ed8,color:#111827;
    class automatic,plantations,plantedbha,eitherorigin good;
    class natural,total,bha,naturalheight,naturalabove,speciesclock caution;
    class specialized,gi,eco,climate separate;
```

Important details behind this map:

- **BHA** means breast-height age. Height and SI datums in the automatic
  branches are the native wrapper settings, not optional direct-engine modes.
- The three models using years since planting are classified as `total_age`,
  without a nursery-age adjustment, and use native SI25. Registry SI50 plotting
  overrides do not change dispatch.
- The automatic breast-height-age group does **not** share one height/SI datum:
  Sharma/Parton SI is above breast height; `sharma2022` and `sharmareid2018` require
  `height_datum = "above_breast_height"`. Batho requires the SBS zone and supports
  natural or plantation origin.
- `sharmareid2018` uses natural-origin jack pine/black spruce, BHA50 and
  above-BH height/SI, explicitly supported by Figure 2 (p. 38). Both
  automatic and explicit dispatch require the above-BH height datum;
  no 1.3 m offset is applied and Carmean's native SI is not substituted.
- `huang2009_bh_age` still takes **total input age**, despite its ID. Its native
  SI reference is breast-height age. `huang2026` returns non-climate realized HI,
  not conventional SI or climate inherent HI.
- `payandeh1974` uses total input/reference age with species-specific
  SI50/80/100. Balsam fir and white spruce clocks are explicit assumptions,
  not publication-verified; Douglas-fir retains an exact-edition caveat.
  These disclosed gaps do not block the approved total-age contract.
- Natural-origin assumptions are not publication verification, and do not
  expand geographic, stand-structure or sampling applicability. Known plantation
  and mixed-origin classifications are preserved. No plantation/natural fallback
  or inference of missing observation origin is performed.
- An explicit ID still pins a model and its native scale. It can permit
  disclosed geographic uncertainty or a published recommendation violation
  with a warning; it cannot override a clock, datum or known origin/geography
  mismatch.

## 4. Implemented source reconciliations and remaining limits

The seven previously blocked conventional variants are now reconciled in code.

| Model IDs | Native contract | Implemented treatment |
|---|---|---|
| `scottvoorhis1986_bh_age`, `scottvoorhis1986_total_age` | SI50 uses total age. The legacy `bh_age` ID takes total input age (`FALSE`); the legacy `total_age` ID takes BHA (`TRUE`, source YTBH added internally). | Corrected scientific labels and docs; IDs, selectors and equations retained. Do not infer input clocks from suffixes. |
| `lundgrendolid1970_exponential_monomolecular`, `lundgrendolid1970_monomolecular` | Balsam fir/white spruce use BHA; the other nine species use total age. SI50 uses the corresponding clock. | Species-specific definitions; white spruce excluded automatically; exponential balsam fir excluded automatically below BHA20. Explicit recommendation violations warn. |
| `parresolvissage1998` | Total input/reference age; native SI50, total height. | Corrected docs and verified clock evidence; native minimum ages of 10 unchanged. |
| `nigh2004` | Juvenile total-age observations; GI-derived total-height SI50 at BHA50. | Independent clocks and GI provenance encoded. Not a mature trajectory or exact base-age identity. |
| `nighcourtin1998_si50` | Equation 7 publishes the implemented total-height SI25-to-SI50 conversion. | Verified provenance; rounded conversion and `converted_index` role retained. |

The detailed publication evidence and implementation handoffs are in
[si_improvements_plan.md](si_improvements_plan.md). Uncertainty that remains
visible includes assumed natural origins, the approved Payandeh clock
assumptions/edition gap and incompletely encoded population/sampling limits.

The five specialized variants remain outside `si()` for a separate reason:
GI input, ecological IQS and climate inherent HI are different operations or
quantities. Resolving their metadata does not enable conventional dispatch.
The observed Lafleche family's source clock remains unresolved within that
specialized group.

## 5. Options at a glance

| Input / option | Effect on selection |
|---|---|
| `age` + exactly one of `height` / `si` | Chooses SI estimation or height prediction. |
| `species`, `jurisdiction` | Restrict registered coverage; no species genus fallback. |
| `input_age_basis` | Required observation clock; must match, without conversion. |
| `reference_age`, `reference_age_basis` | Filter native SI reference definitions; do not set `base_age`. |
| `si_datum`, `productivity_quantity` | Filter SI scale/quantity; no offsets or quantity substitution. |
| `height_datum` | Measured/predicted height datum, independently of SI; defaults to total height. |
| `stand_origin` | Observation origin required for automatic selection; match source-classified or explicitly assumed model origin. No natural/plantation fallback. |
| `bec_zone` | Checks encoded BC restrictions; explicitly selects supplied-zone modes where accepted. |
| `natural_subregion` in `...` | Passed only to accepting engines; unsupported discrete values are excluded where encoded. |
| `model_id` | Pins a conventional model/variant; declares its native SI when SI is supplied. |
| `pick_best = TRUE` / `FALSE` | One preferred result versus per-observation candidate comparisons. |
| `keep_model_id` | Controls the result ID column, not scientific provenance or eligibility. |
| `base_age`, `total_height`, `variant`, other definition selectors in `...` | Rejected by the wrapper; use a registered variant or the direct engine. |

Use `find_si_models()` to browse coverage, scientific near-misses and name-level
input requirements without running engines. Use `get_si_model_info(model_id)`
to inspect source definitions, applicability, native defaults, constraints and
unresolved evidence before interpreting a model's SI.
