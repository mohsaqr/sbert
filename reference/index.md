# Package index

## Encoding text

Turn text into Sentence-BERT embeddings.

- [`encode()`](https://pak.dynasite.org/sbert/reference/encode.md) :
  Encode Text with Sentence-BERT
- [`pool()`](https://pak.dynasite.org/sbert/reference/pool.md) : Pool
  Token Embeddings into Sentence Embeddings

## Models and runtime

Install the ONNX Runtime, download revision-pinned models, load custom
Hugging Face exports, and manage the on-disk cache.

- [`models()`](https://pak.dynasite.org/sbert/reference/models.md) :
  List the Available Pinned Sentence-BERT Models
- [`model_download()`](https://pak.dynasite.org/sbert/reference/model_download.md)
  : Download a Pinned Sentence-BERT Model
- [`load_model()`](https://pak.dynasite.org/sbert/reference/load_model.md)
  : Load a Pinned Sentence-BERT Model
- [`load_custom()`](https://pak.dynasite.org/sbert/reference/load_custom.md)
  : Load an Arbitrary Hugging Face Embedding Model
- [`model_status()`](https://pak.dynasite.org/sbert/reference/model_status.md)
  : Inspect an Installed Model
- [`model_remove()`](https://pak.dynasite.org/sbert/reference/model_remove.md)
  : Remove an Installed Model from the Cache
- [`install_runtime()`](https://pak.dynasite.org/sbert/reference/install_runtime.md)
  : Install ONNX Runtime
- [`cache_dir()`](https://pak.dynasite.org/sbert/reference/cache_dir.md)
  : Locate the sbert Model Cache
- [`cache_size()`](https://pak.dynasite.org/sbert/reference/cache_size.md)
  : Measure the sbert Cache

## Preparing a corpus

Deduplicate, segment, and context-blend documents before modeling.

- [`dedupe()`](https://pak.dynasite.org/sbert/reference/dedupe.md) :
  Deduplicate a Text Corpus with Frequencies
- [`segment()`](https://pak.dynasite.org/sbert/reference/segment.md) :
  Segment Text into Sentences, Clauses, or Phrases
- [`abbreviations()`](https://pak.dynasite.org/sbert/reference/abbreviations.md)
  : Obtain the Built-in Abbreviation Gazetteer
- [`blend()`](https://pak.dynasite.org/sbert/reference/blend.md) : Blend
  Segment Embeddings with Their Document Context
- [`stop_words()`](https://pak.dynasite.org/sbert/reference/stop_words.md)
  : Obtain and Adjust the Topic Stop-Word List

## Topic modeling

Fit topic models, sweep the topic count, and merge topics down.

- [`topics()`](https://pak.dynasite.org/sbert/reference/topics.md) :
  Discover Semantic Topics in Documents
- [`select_topics()`](https://pak.dynasite.org/sbert/reference/select_topics.md)
  : Compare Topic Counts Before Committing to One
- [`fitted(`*`<sbert_topic_sweep>`*`)`](https://pak.dynasite.org/sbert/reference/fitted.sbert_topic_sweep.md)
  : Extract One Fitted Model from a Topic-Count Sweep
- [`reduce_topics()`](https://pak.dynasite.org/sbert/reference/reduce_topics.md)
  : Reduce a Fitted Topic Model to Fewer Topics

## Describing topics

Distinctive terms, representative documents, sizes, and keywords.

- [`terms(`*`<sbert_topic_model>`*`)`](https://pak.dynasite.org/sbert/reference/terms.sbert_topic_model.md)
  : Topic Terms, Retuned Without Refitting
- [`representatives()`](https://pak.dynasite.org/sbert/reference/representatives.md)
  : Representative Text Units for Every Topic
- [`topic_sizes()`](https://pak.dynasite.org/sbert/reference/topic_sizes.md)
  : Topic Sizes on the Distinct and Weighted Scales
- [`keywords()`](https://pak.dynasite.org/sbert/reference/keywords.md) :
  Extract Keywords from Documents by Embedding Similarity

## Topic inference

Assign new documents and recover soft, generative, and mixed
memberships.

- [`predict(`*`<sbert_topic_model>`*`)`](https://pak.dynasite.org/sbert/reference/predict.sbert_topic_model.md)
  : Assign New Documents to Fitted Topics
- [`topic_membership()`](https://pak.dynasite.org/sbert/reference/topic_membership.md)
  : Soft Topic Membership Probabilities
- [`topic_gamma()`](https://pak.dynasite.org/sbert/reference/topic_gamma.md)
  : Document-Topic Distributions from Segment Assignments

## Evaluating topics

Intrinsic coherence and topic diversity.

- [`coherence()`](https://pak.dynasite.org/sbert/reference/coherence.md)
  : Score Topic Coherence
- [`topic_diversity()`](https://pak.dynasite.org/sbert/reference/topic_diversity.md)
  : Measure Topic Diversity

## Visualizing and summarizing

Deterministic base-graphics plots, hierarchies, and reports.

- [`summary(`*`<sbert_topic_model>`*`)`](https://pak.dynasite.org/sbert/reference/summary.sbert_topic_model.md)
  : Summarize a Semantic Topic Model
- [`plot(`*`<sbert_topic_model>`*`)`](https://pak.dynasite.org/sbert/reference/plot.sbert_topic_model.md)
  : Plot a Semantic Topic Model
- [`plot(`*`<sbert_topic_sweep>`*`)`](https://pak.dynasite.org/sbert/reference/plot.sbert_topic_sweep.md)
  : Plot a Topic-Count Sweep
- [`topic_hierarchy()`](https://pak.dynasite.org/sbert/reference/topic_hierarchy.md)
  : Build the Topic Hierarchy of a Fitted Model
- [`topic_similarity()`](https://pak.dynasite.org/sbert/reference/topic_similarity.md)
  : Compute Cosine Similarity
- [`topic_palette()`](https://pak.dynasite.org/sbert/reference/topic_palette.md)
  : Qualitative Colour Palette for Topics

## Data

A bundled multilingual corpus for offline examples.

- [`feedback_translations`](https://pak.dynasite.org/sbert/reference/feedback_translations.md)
  : Levebee AI Mathematics Feedback with English Translations
