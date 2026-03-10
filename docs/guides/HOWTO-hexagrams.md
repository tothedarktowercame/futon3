# HOWTO: Complete the I Ching Hexagram Pattern Library

## Context

We have discovered that the I Ching hexagram structure predicts which sigils produce edge-of-chaos (EoC) dynamics in MMCA. The hexagram 泰 (Peace) - dynamic balance between 乾 (Creative) and 坤 (Receptive) - corresponds to the parameter zone where stable EoC emerges.

Three hexagrams are complete as examples:
- `library/iching/hexagram-01-qian.flexiarg` - ䷀ 乾 (The Creative)
- `library/iching/hexagram-02-kun.flexiarg` - ䷁ 坤 (The Receptive)
- `library/iching/hexagram-11-tai.flexiarg` - ䷊ 泰 (Peace)

**Task:** Complete the remaining 61 hexagrams following the same format.

## Hexagram Structure

Each hexagram has:
- **Number** (1-64)
- **Binary** (6 lines, each yin/yang → 6 bits)
- **Upper trigram** (lines 4-6)
- **Lower trigram** (lines 1-3)
- **Name** (Chinese character + pinyin + English)
- **Traditional meaning** (judgment, image, line texts)

The 8 trigrams are:
```
☰ 乾 qián  heaven   111
☱ 兌 duì   lake     110
☲ 離 lí    fire     101
☳ 震 zhèn  thunder  100
☴ 巽 xùn   wind     011
☵ 坎 kǎn   water    010
☶ 艮 gèn   mountain 001
☷ 坤 kūn   earth    000
```

## Format Template

Each hexagram file should follow this structure:

```
@flexiarg iching/hexagram-NN-name
@title ䷊ 字 (Pīnyīn) - English Name
@sigils [☰☷/字]
@binary NNNNNN
@trigrams [upper/lower]
@number NN
@audience improvisers, system designers, CT practitioners
@tone foundational
@style pattern

! conclusion: [Core meaning in one sentence]
  + context: [When this pattern applies]
  + IF: [Conditions that call for this pattern]
  + HOWEVER: [The tension or limitation]
  + THEN: [The recommended response]
  + BECAUSE: [Why this works - the universal principle]
    + evidence: Traditional: "[Quote from I Ching]"
  + NEXT-STEPS: [Practical applications]

@ct-interpretation
  :vision {...}
  :as-morphism {...}
  :adapt {...}

@ant-interpretation
  :policy {...}
  :precision {...}
  :pattern-sense {...}
  :adapt-trigger {...}
  :failure-mode "..."

@mmca-interpretation
  :sigil-encoding {...}
  :exotype-params {...}
  :tensor-interpretation {...}
  :regime-prediction {...}

@morphisms-to-other-hexagrams
  :primary-pair {...}
  :generates [...]

@domain-transfer-notes
  "..."
```

## Interpretation Guidelines

### CT Interpretation

Map the hexagram meaning to category-theoretic concepts:
- **VISION objects**: What states/modes does this hexagram recognize?
- **Morphism type**: Is it identity-like, projection-like, or compositional?
- **ADAPT triggers**: What causes transition to other hexagrams?

### Ant Interpretation

Map to AIF cyberant behavior:
- **Policy priors**: Weight distribution over {forage, return, hold, pheromone}
- **Precision (Pi-o)**: How much to trust observations (0=ignore, 1=fully attend)
- **Temperature (tau)**: How committed vs exploratory (0=committed, 1=exploratory)
- **Pattern-sense**: Trail-follow, gradient-use, novelty-seek weights

Use the poles as reference:
- 乾 (pure yang): forage=0.8, Pi-o=0.1, tau=0.1, novelty=0.9
- 坤 (pure yin): return=0.7, Pi-o=0.9, tau=0.9, trail-follow=0.9
- 泰 (balance): all ~0.5, balanced cycling

### MMCA Interpretation

Map to exotype parameters:
- **update-prob**: How often to intervene (0=never, 1=always)
- **match-threshold**: How strict the context matching (0=any, 1=exact)
- **mix-mode**: Coordination style (none, rotate, majority, scramble, etc.)

The 泰 zone is approximately: update-prob ∈ [0.3, 0.7], match-threshold ∈ [0.3, 0.7]

### Tensor Interpretation

- **Identity**: I (preserve state)
- **Projection**: |target⟩⟨target| (collapse to attractor)
- **Convex combination**: αP + (1-α)I (partial intervention)

## Priority Order

Suggested order for completing hexagrams:

### First: The structural hexagrams
1. ䷋ 否 (12, Pǐ) - Obstruction (inverse of 泰)
2. ䷾ 既濟 (63, Jì Jì) - After Completion
3. ䷿ 未濟 (64, Wèi Jì) - Before Completion

### Second: The trigram doubles (pure forms)
4. ䷹ 兌 (58) - Lake/Lake - The Joyous
5. ䷝ 離 (30) - Fire/Fire - The Clinging
6. ䷲ 震 (51) - Thunder/Thunder - The Arousing
7. ䷸ 巽 (57) - Wind/Wind - The Gentle
8. ䷜ 坎 (29) - Water/Water - The Abysmal
9. ䷳ 艮 (52) - Mountain/Mountain - Keeping Still

### Third: Fill in systematically by number

## Resources

- Wilhelm/Baynes translation for traditional meanings
- https://www.iching-online.com/ for quick reference
- futon5 `futon5.mmca.exotype/lift` to check sigil → param mappings

## Validation

After completing a hexagram, verify:
1. The CT interpretation is internally consistent
2. The ant params make sense given the meaning
3. The MMCA params predict a plausible regime
4. Morphisms to related hexagrams are bidirectional

## Output

Files go in: `library/iching/hexagram-NN-name.flexiarg`

Naming convention: two-digit number, then pinyin name (lowercase, no tone marks)
- hexagram-01-qian.flexiarg
- hexagram-12-pi.flexiarg
- hexagram-63-jiji.flexiarg

## The Big Picture

These 64 hexagrams form the "candidate core set" of patterns. All other patterns in the library are "unfoldings" into domain-specific detail. The hexagram structure should:

1. Predict parameter zones for different dynamics
2. Provide morphism structure (how patterns transform)
3. Enable domain-general transfer (CA → ants → music → argument → ...)

The 泰 finding validates this approach: ancient pattern language predicts modern simulation behavior.
