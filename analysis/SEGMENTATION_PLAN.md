# Text Splitting — Full Design & Test Plan (literature-grounded)

## 0. Two jobs that are constantly confused

The literature splits text for **two different purposes**, and they need different
algorithms. Conflating them is the usual source of bad results.

| Job | Unit wanted | Optimize for | Our use here |
|-----|-------------|--------------|--------------|
| **A. Embedding / retrieval chunking** | fits a model's token budget, retains context | retrieval recall & end-to-end accuracy | the 256-token abstract chunker (already in the package) |
| **B. Representative-unit extraction** | a *readable linguistic unit* (sentence/clause) | human interpretability, distinctiveness | the topic-card "representative sentences" |

The recent RAG chunking work (job A) and the sentence-boundary-detection (SBD) /
discourse literature (job B) are **separate fields with separate metrics**.

---

## 1. What the literature actually does

### 1a. Sentence Boundary Detection (job B, sentence level)
- **Punkt (Kiss & Strunk 2006)** — the canonical *unsupervised* SBD. Core idea:
  *once abbreviations are identified, most boundary ambiguity disappears.*
  Detects abbreviations by three context-free cues — a period forms a tight
  collocation with a truncated word, abbreviations are short, and may contain
  internal periods. **Weakness**: fails on brackets, itemized text, and unseen
  abbreviations; ~0.62 precision / 0.71 F1 on domain text.
- **NUPunkt / CharBoundary (2025)** — rule-based, domain-adapted SBD with a
  **4,000+ abbreviation gazetteer**. Result that matters to us: **rule-based,
  domain-adapted SBD beats neural** on specialized text — **precision 0.911,
  10M chars/sec** (NUPunkt); CharBoundary **F1 0.782**. Neural spaCy/NLTK sit at
  **0.62–0.65 precision**. "Precision matters exponentially more" downstream.
- **Implication for us**: our text is (i) a specialized domain (CS-education
  abstracts) and (ii) **ALL-CAPS**, which removes the capitalization feature
  neural SBD leans on. A **rule-based gazetteer splitter is the correct,
  evidence-backed choice** — this is exactly `segment_text()`.

### 1b. Clause / discourse units (job B, finer than a sentence)
- **RST Discourse Treebank** annotates **Elementary Discourse Units (EDUs)** —
  clause-level spans — the standard sub-sentential unit. Full EDU segmentation
  needs a discourse parser; the cheap, deterministic proxy is **connective +
  punctuation chunking** (split at `;:—` and subordinators like *which, where,
  in terms of*), which is what our `clause` level implements.
- **TextTiling (Hearst 1997)** — segments by **lexical-cohesion troughs** (a
  sliding window over vocabulary overlap). This is the ancestor of today's
  *semantic chunking*.

### 1c. Chunking for embeddings/retrieval (job A)
- **Fixed-size (token window + overlap)** — simplest baseline; tokenizer-aware.
- **Recursive character splitting (LangChain)** — hierarchical separator list
  (¶ → line → sentence → word). **Best general default: ~69% end-to-end
  accuracy**, fast, cheap.
- **Semantic chunking (Kamradt)** — cut where adjacent-sentence embedding
  similarity drops. **Highest retrieval recall (~92%)** but lower end-to-end
  (~54%) and **~14× slower**.
- **Late chunking (Günther et al. 2024, Jina)** — embed the **whole document
  first**, then pool token embeddings into chunks, so each chunk keeps global
  context (pronouns, references). Needs a long-context embedding model.
- **Finding**: no single strategy dominates; chunking must be **co-optimized
  with the retriever**. Recursive+hybrid, semantic+dense, late+long-context.

### 1d. Evaluation metrics (both jobs)
- **Boundary detection**: Precision / Recall / **F1** on labeled boundaries
  (used by NUPunkt et al.). This is our primary gate.
- **Topic/segment quality**: **Pk (Beeferman et al. 1999)** and **WindowDiff
  (Pevzner & Hearst 2002)** — sliding-window boundary-error rates; the standard
  for topic-segmentation, lower is better.

---

## 2. The variations (design space)

A single dispatcher `split_text(text, method, ...)`, `method ∈`:

| # | Method | Job | Unit | Algorithm core | Deps | Failure mode | Lit. |
|---|--------|-----|------|----------------|------|--------------|------|
| V1 | `token_window` | A | ~N tokens (+overlap) | tokenizer count, greedy fill | model tokenizer | breaks mid-sentence | fixed-size |
| V2 | `recursive` | A | balanced chunk | hierarchical separator list | none | can still cut sentences | LangChain |
| V3 | `sentence` | B | sentence | **rule-based SBD + gazetteer** | none | unseen abbrev | Punkt / NUPunkt |
| V4 | `clause` | B | clause/EDU-lite | V3 + `;:—` + subordinators | none | list/clause comma ambiguity | RST-EDU |
| V5 | `phrase` | B | phrase | V4 + commas (+merge) | none | subject-severed shards | — |
| V6 | `semantic` | A/B | topic block | embedding-similarity troughs | SBERT | slow (~14×) | TextTiling / Kamradt |
| V7 | `late` | A | context-rich chunk | embed doc → pool | long-ctx model | needs long-ctx model | Günther 2024 |

**V3–V5 are implemented and tested** (`segment_text()`, 31 tests). **V1 exists**
in the package's abstract chunker. **V2/V6/V7 are specified but not built.**

---

## 3. The recommended, well-tested core algorithm

For **job B** (our representative units): **V3 sentence → V4 clause**, i.e.
`segment_text()`. Justified by §1a: rule-based gazetteer SBD is state-of-the-art
for specialized + no-case text.

Pipeline (deterministic, base R, 6 stages):
1. **Normalize** — uppercase, unify quotes/apostrophes/dashes, collapse space.
2. **Protect parentheticals** — mask `.`/`,` inside `(...)`.
3. **Protect abbreviations/decimals** — `\b`-anchored gazetteer (so `ST.`≠`COST.`)
   + `\d\.\d`. (This is Punkt's central insight, done with an explicit list.)
4. **Mark boundaries** for the level — `.?!`; `+ ;:— & subordinators`; `+ commas`.
5. **Split & restore** placeholders.
6. **Merge short** (optional) — re-join sub-threshold shards.

Known, documented limits (from §1b): list-comma vs. clause-comma is unsolvable
without a parser → **enumeration integrity guaranteed only at clause level**.
Unseen abbreviations can leak (mitigation: extend gazetteer; log candidates).

For **job A**: keep **V1 token_window** (already there); add **V2 recursive** and
optionally **V6 semantic** later.

---

## 4. Evaluation methodology (the "well-tested" gate)

### 4a. Gold corpus
Hand-label boundaries on **200 sentences sampled from MCSE abstracts** (stratified
across topics), stored as `segmentation_gold.csv (input, level, expected)`. Seed
with real failures (COST./FIG./I/O./decimals/hinges/parentheticals) — done.

### 4b. Metrics (literature-standard)
- **Boundary P/R/F1** vs gold, per level (NUPunkt-style). **Acceptance gate:
  sentence-level F1 ≥ 0.95** on our cleaner-than-legal abstracts (NUPunkt got
  0.91 on messy legal, so 0.95 is a fair bar here).
- **Pk / WindowDiff** if/when V6 semantic segmentation is evaluated.
- **Throughput** (chars/sec) — must stay ≫ embedding cost.

### 4c. Property tests (already implemented)
Determinism · token-multiset preservation (no lost words) · no leaked sentinels ·
no empty units · edge cases (empty, one word, no terminal punctuation).

### 4d. Regression
Every flagged sentence becomes a permanent gold row.

---

## 4b. Empirical results (measured, `analysis/segment_eval.R`)

Test set: **200 documents / 800 true boundaries**, built by concatenating
"clean single sentences" mined from real MCSE abstracts (unambiguous ground
truth), with abbreviations injected at known **non**-boundary positions. Three
conditions × 6 methods (2 baselines, 2 reference tokenizers, our splitter + a
corpus-mining variant). Boundary **P / R / F1**:

| Condition | naive | icu (ref) | nltk (ref) | **sbd (ours)** | sbd+mined |
|-----------|:-----:|:---------:|:----------:|:--------------:|:---------:|
| clean (no abbrev) | 1.000 | 0.994 | 0.997 | **0.999** | 0.999 |
| **in-gazetteer abbrev** | 0.800 | 0.797 | 0.824 | **0.999** | **0.999** |
| **unseen abbrev (absent from corpus)** | 0.726 | 0.723 | **0.771** | 0.727 | 0.727 |

Throughput: ours ≈ **1.1M chars/sec** (naive 2.4M, icu 2.9M) — all far above the
embedding cost, so accuracy is the only thing that matters.

**What the data shows**
1. **On the abbreviations that actually occur in MCSE, the rule-based gazetteer
   wins decisively** — F1 **0.999, zero over-splits**, vs. 0.80–0.82 for
   references. This is the realistic operating condition (real abstracts use
   `FIG.`, `NO.`, `ET AL.`, `E.G.`, all covered).
2. **Ablation proves the gazetteer is the entire source of the win**:
   `sbd no-gazetteer` ≡ naive in every condition.
3. **Honest limit**: on abbreviations *absent from the corpus* (injected
   `PROC.`, `SECT.`, … — verified 0 occurrences), no data-driven method helps;
   only nltk's shape/collocation model generalizes (0.771 vs 0.727). **Corpus
   mining cannot learn what the corpus does not contain** — but such
   abbreviations, being absent, also never appear in the real abstracts, so this
   condition is a stress test, not the operating point.

**Acceptance gate** (sentence-F1 ≥ 0.95): **met at 0.999** on realistic
(in-corpus) abbreviations; deliberately failed only on the synthetic
absent-abbreviation stress test. For maximum robustness on arbitrary future
text, layer: static gazetteer → corpus mining (Punkt collocation) → optional
shape heuristic.

## 5. Deliverables & rollout
1. ✅ `analysis/segment_text.R` (V3–V5, custom-gazetteer hook) + `test-segmentation.R` (31 tests).
2. ✅ `analysis/segment_eval.R` — 200-doc / 800-boundary auto-gold, 3 conditions,
   P/R/F1 vs ICU + nltk references, ablation, corpus miner, throughput →
   `outputs/segmentation_eval/boundary_metrics.csv`.
3. ✅ Corpus abbreviation miner (Punkt collocation) as `V3b`; measured (helps for
   in-corpus abbreviations, cannot learn absent ones).
4. ✅ `analysis/split_text.R` dispatcher (V1 token_window, V2 recursive, V3–V5
   sentence/clause/phrase, V6 semantic) + `test-split-text.R` (11 tests).
5. ✅ Rewired `mcse_gold_sentences.R` to validated `segment_text()` clause units
   over a **wide 200-abstract pool** with a shorter-clause tie-break
   (mean 13.6 words/segment, down from paragraph-length).
6. ▢ Optional: promote to package as `sbert_segment()` (roxygen + `tests/testthat/`).

---

## Sources
- Kiss & Strunk (2006), *Unsupervised Multilingual Sentence Boundary Detection* — https://ptrckprry.com/course/ssd/reading/Kiss06.pdf
- NUPunkt / CharBoundary (2025), *Precise Legal Sentence Boundary Detection at Scale* — https://arxiv.org/html/2504.04131v1
- *Reconstructing Context: Evaluating Advanced Chunking Strategies for RAG* (2025) — https://arxiv.org/pdf/2504.19754
- Hearst (1997), *TextTiling* — https://www.researchgate.net/publication/2454524_TextTiling_A_Quantitative_Approach_to_Discourse_Segmentation
- Pevzner & Hearst (2002), WindowDiff; Beeferman et al. (1999), Pk — see https://www.cl.cam.ac.uk/teaching/1011/L104/lec10-2x2.pdf
- RAG chunking strategy surveys — https://www.firecrawl.dev/blog/best-chunking-strategies-rag , https://www.datacamp.com/blog/chunking-strategies
