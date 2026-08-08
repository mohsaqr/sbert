# Plot a Topic-Count Sweep

Draws coherence, topic_diversity, and between-topic variance against the
candidate topic counts, marking the count with the highest coherence.
There is no single correct topic count; the useful signal is the count
after which coherence stops improving, not a global maximum.

## Usage

``` r
# S3 method for class 'sbert_topic_sweep'
plot(x, main = "Topic-count comparison", ...)
```

## Arguments

- x:

  A sweep returned by \[select_topics()\].

- main:

  Overall title.

- ...:

  Passed to the underlying plotting calls.

## Value

\`x\`, invisibly.
