# Honest crosswalk between two 29-topic models of the SAME CER corpus:
#   - STM on keyword metadata (Apiola, Lopez-Pernas & Saqr, Table 1)   [reference]
#   - SBERT abstract embeddings + k-means (this session)               [ours]
# Both topic representations are embedded with the same MiniLM model and aligned
# by cosine similarity, so the comparison lives in one common space.
devtools::load_all(quiet = TRUE)

model <- readRDS("outputs/mcse_gold_topics/mcse_gold_topic_model.rds")
quality <- read.csv("outputs/mcse_gold_topics/topic_quality.csv", stringsAsFactors = FALSE)
sbert_model <- sbert_load_model("/private/tmp/sbert-package-download-test", threads = 2L)

# ---- STM topics from the published Table 1 (label -> frequent keywords) ------
stm <- list(
  "Programming languages" = "programming languages, java, computer programming, high level languages, c programming language",
  "Programming" = "programming, programming education, novice, block-based programming, novice programmer, programming environment, scratch",
  "Introductory courses" = "cs1 cs2, introductory programming, motivation, introductory computer science, courses, computer science courses",
  "OOP" = "object-oriented programming, object-oriented, abstracting, innovation",
  "Curriculum" = "curriculum, information technology, computing curricula, societies and institutions, curriculum development, interdisciplinary, computer science curricula",
  "Classroom pedagogy" = "classroom, flipped, blended, e-learning, distance learning, environments",
  "Pedagogy" = "pedagogy, cs education, pedagogical approach, teaching assistants, qualitative research",
  "Educational psychology" = "self-efficacy, attitudes, surveys, undergraduate students, behavioral research",
  "Collaborative learning" = "collaborative learning, pair, pair programming, computer supported collaborative work, empirical studies",
  "Assessment" = "assessment, algorithms, data structures, automatic assessment, grading",
  "Games" = "games, game-based learning, computer games, game design and development, interactive computer graphics",
  "STEM" = "stem, science, engineering, physical, computational, high school students",
  "Tools" = "educational tools, tool, interactive, interactive learning, systems, environments",
  "Educational Technology" = "educational technology, websites, technology, world wide web, multimedia systems, social networking, human computer interaction",
  "Visualisation" = "visualization, animation, data visualization, parallel programming",
  "Online learning" = "e-learning, computer aided instruction, online, learning environments, distance education, intelligent tutoring systems",
  "Robotics" = "robotics, robot programming, educational robotics, databases",
  "Software engineering" = "software engineering, computer software, problem solving, software design, project management, development",
  "Projects" = "projects, project-based learning, capstone, experiential learning, open source software",
  "Design" = "design, human factors, human engineering, instructional design, user experience",
  "Software testing" = "software testing, test-driven development, testing",
  "Information systems" = "information systems, formal methods, information use",
  "Data mining" = "data mining, learning analytics, educational data mining, source codes",
  "AI & ML" = "learning systems, artificial intelligence, machine learning, mathematical models, active learning",
  "Computer architecture" = "computer architecture, computer hardware, program compilers, simulation, hardware, embedded systems",
  "Operating systems" = "operating systems, security and privacy, cryptography, computer systems, computer operating systems",
  "Computational thinking" = "k-12, computational thinking, teachers, training, high school, professional development, primary school",
  "Gender and diversity" = "gender and diversity, women, broadening participation, diversity",
  "Other" = "computing, computation theory, computer applications, research questions, mobile, teaching and learning"
)
stm_labels <- names(stm)
stm_embeddings <- sbert_encode(unlist(stm, use.names = FALSE), sbert_model,
                               batch_size = 32L, normalize = TRUE)

# ---- Align in the common MiniLM space --------------------------------------
sbert_labels <- model$topics$label
similarity <- sbert_similarity(stm_embeddings, model$centers)   # 29 STM x 29 SBERT
rownames(similarity) <- stm_labels
colnames(similarity) <- sbert_labels

best_stm_for_sbert <- apply(similarity, 2L, which.max)          # per SBERT topic
best_sbert_for_stm <- apply(similarity, 1L, which.max)          # per STM topic

crosswalk <- data.frame(
  sbert_topic = sbert_labels,
  sbert_npmi = quality$npmi,
  nearest_stm = stm_labels[best_stm_for_sbert],
  cosine = round(vapply(seq_along(sbert_labels),
    function(j) similarity[best_stm_for_sbert[j], j], numeric(1L)), 3L),
  mutual = vapply(seq_along(sbert_labels), function(j) {
    best_sbert_for_stm[[best_stm_for_sbert[j]]] == j
  }, logical(1L)),
  stringsAsFactors = FALSE
)
crosswalk <- crosswalk[order(-crosswalk$cosine), ]

unmatched_stm <- setdiff(stm_labels, stm_labels[best_stm_for_sbert])
stm_merges <- table(stm_labels[best_stm_for_sbert])
stm_merges <- names(stm_merges[stm_merges > 1L])

cat("\n================ SBERT (ours) -> nearest STM (Table 1) ================\n")
print(crosswalk, row.names = FALSE)
cat(sprintf("\nmutual nearest-neighbour matches: %d / 29\n", sum(crosswalk$mutual)))
cat(sprintf("median alignment cosine: %.3f\n", median(crosswalk$cosine)))
cat(sprintf("\nSTM topics with NO SBERT topic mapping to them (%d):\n  %s\n",
            length(unmatched_stm), paste(unmatched_stm, collapse = "; ")))
cat(sprintf("\nSTM topics that ABSORB several SBERT topics (merges):\n  %s\n",
            paste(stm_merges, collapse = "; ")))

dir.create("outputs/stm_comparison", showWarnings = FALSE, recursive = TRUE)
write.csv(crosswalk, "outputs/stm_comparison/crosswalk.csv", row.names = FALSE)
cat("\nwrote outputs/stm_comparison/crosswalk.csv\n")
