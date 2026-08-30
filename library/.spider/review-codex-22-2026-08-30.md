# Wave-1 semantic review — codex-22 half

Date: 2026-08-30

The evidence-kind judgment precedes the directive-kind judgment. A spider
turn that reproduces the source pattern is `self-text`; it is not independent
evidence merely because it was stored as an Evidence Landscape record. An
authored sentence in the pattern body that explicitly relates the two concepts
can nevertheless warrant the edge under the review rule.

| Edge | Kind | Evidence kind | Verdict | Reason |
|---|---|---|---|---|
| `aif/belief-aware-risk-term` → `aif/expected-free-energy-scorecard` | `@see-also` | stated | attested | The source defines risk as the pragmatic EFE term (`belief-aware-risk-term.flexiarg:11-13,24-30`), while the target is explicitly the named-term EFE composition (`expected-free-energy-scorecard.flexiarg:12-26`); peer linkage is accurate. |
| `aif/currency-before-merge` → `aif/expected-free-energy-scorecard` | `@see-also` | self-text | sent back | Evidence `e-3fb820aa-3e22-4a67-a514-7ce2478ce63e` is the spider turn containing the source's own context (`currency-before-merge.flexiarg:12`); it names an EFE blend but neither the target nor a specific relation. |
| `aif/decomposed-prediction-noise` → `aif/belief-aware-risk-term` | `@see-also` | self-text | sent back | Evidence `e-c6ac0857-5a23-4c22-866c-2775fb358c6e` repeats the generic consumer sentence (`decomposed-prediction-noise.flexiarg:36`), not a relation to the named risk pattern. |
| `aif/expected-free-energy-scorecard` → `structure/mana-allostasis` | `@see-also` | co-mention | refused | Evidence `e-991c1415-3a8b-4889-a268-5586ba5525f4` lists both ids in a cross-reference table but states no relation; the source's scoring IF/HOWEVER/THEN (`expected-free-energy-scorecard.flexiarg:16-26`) and the target's operator-load IF/HOWEVER/THEN (`mana-allostasis.flexiarg:36-55,76-83`) do not supply one. |
| `aif/free-energy-as-tick-scalar` → `aif/expected-free-energy-scorecard` | `@see-also` | stated | attested | The source explicitly contrasts per-tick F with per-candidate expected free energy G (`free-energy-as-tick-scalar.flexiarg:14-26`); the target defines that G scorecard (`expected-free-energy-scorecard.flexiarg:12-26`). |
| `aif/hierarchical-and-temporal-depth` → `aif/policy-precision-commitment-temperature` | `@see-also` | stated | attested | The source says temporal discount must not overload the R14 commitment dial (`hierarchical-and-temporal-depth.flexiarg:23-35`), directly distinguishing the target's quantity; peer comparison is the correct kind. |
| `aif/interoceptive-tripwires` → `aif/structured-observation-vector` | `@see-also` | self-text | sent back | Evidence `e-ef28b7b2-cb6d-4a1c-8c08-12b36a08a796` repeats the source's task-only observation-channel diagnosis (`interoceptive-tripwires.flexiarg:22-27`) without naming the target pattern. |
| `aif/no-self-certification` → `aif/interoceptive-tripwires` | `@see-also` | self-text | sent back | Evidence `e-75ef042f-5000-4e48-aec1-f70fd23e12a1` repeats the source's structural-tag rule (`no-self-certification.flexiarg:31-43`); it does not state a relation to tripwires. |
| `aif/placeholder-is-load-bearing` → `aif/term-to-channel-traceability` | `@see-also` | self-text | sent back | Evidence `e-9ed973f1-edbe-4073-a7c4-25aabbec4a60` repeats “provenance field, never bare” (`placeholder-is-load-bearing.flexiarg:21-25`) but does not name the target or its channel-level receipt discipline (`term-to-channel-traceability.flexiarg:14-24`). |
| `aif/posterior-variance-as-epistemic-value` → `aif/predictive-entropy-as-ambiguity` | `@see-also` | stated | attested | The source contains an authored “Relation to” paragraph naming the target and distinguishing reducible EIG from irreducible ambiguity (`posterior-variance-as-epistemic-value.flexiarg:28-32`); peer contrast is exact. |
| `problems/tension-proposes-candidates` → `futon-theory/reverse-morphogenesis` | `@see-also` | stated | attested | The source explicitly names the target as the earlier method that may supersede its tension framing (`tension-proposes-candidates.flexiarg:19-25`); `@see-also` preserves that open comparison without claiming authority. |

## Counts

- Attested: 5
- Sent back: 5
- Refused: 1
- Evidence kinds: stated 5; self-text 5; co-mention 1; listing 0

## Difference noticed in the other half

I did not edit the other half. I would have sent back the reciprocal
`candidate-pattern-action-space` / `evidence-precision-registry` edges: their
record excerpt is a list of three “likely prior-art patterns,” which is a
co-mention and not a stated relation. The concepts may genuinely be peers, but
the cited record does not establish that edge under F3.
