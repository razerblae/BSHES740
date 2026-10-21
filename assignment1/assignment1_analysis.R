# Assignment 1: simple keyword matching and frequency analysis.
# Run from assignment1: Rscript assignment1_analysis.R
# Methods follow Labs 2, 4, and 5. No model training is used.
.libPaths(c("../lab2/.r_libs", .libPaths()))
library(tm)
library(SnowballC)
library(slam)
library(wordcloud)

dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
options(stringsAsFactors = FALSE)

# One unquoted carriage return in set 2 otherwise creates an extra CSV row.
# Convert standalone CRs to spaces; leave normal CRLF line endings alone.
read_posts <- function(path, set_name) {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  csv_text <- gsub("\r(?!\n)", " ", rawToChar(raw), perl = TRUE)
  d <- read.csv(text = csv_text, colClasses = "character",
                na.strings = "", check.names = FALSE)
  d$set <- set_name
  stopifnot(all(grepl("^[0-9]+$", d$id)), !anyDuplicated(d$id),
            all(grepl("^\\d{4}-\\d{2}-\\d{2}$", d$date)),
            !anyNA(d$text))
  d
}
set1 <- read_posts("TADA_A1_set1.csv", "Set 1")
set2 <- read_posts("TADA_A1_set2.csv", "Set 2")
posts <- rbind(set1, set2)
stopifnot(!anyDuplicated(posts$id))

dataset_summary <- do.call(rbind, lapply(list(set1, set2), function(d) {
  data.frame(set = d$set[1], posts = nrow(d), start = min(d$date),
             end = max(d$date), english_posts = sum(d$lang == "en"),
             language_codes = length(unique(d$lang)),
             missing_locations = sum(is.na(d$location)))
}))
write.csv(dataset_summary, "results/dataset_summary.csv", row.names = FALSE)
languages <- as.data.frame(table(posts$lang, posts$set))
names(languages) <- c("language_code", "set", "posts")
languages <- languages[languages$posts > 0, ]
write.csv(languages, "results/languages.csv", row.names = FALSE)

# Drop URLs and @handles BEFORE keyword matching. A handle such as
# @Methadone_Cat alone should not make a tweet a methadone discussion.
clean_for_matching <- function(x) {
  x <- tolower(x)
  x <- gsub("https?://\\S+|www\\.\\S+", " ", x, perl = TRUE)
  x <- gsub("@[[:alnum:]_]+", " ", x, perl = TRUE)
  x
}
posts$match_text <- clean_for_matching(posts$text)
drug_patterns <- c(
  Methadone = "\\b(methadone|methadones|methadon|methadose)\\b",
  Suboxone = "\\b(suboxone|suboxones|suboxon|subox|subuxone)\\b",
  Fentanyl = "\\b(fentanyl|fentanyls|fentanil|fentanilo|fent)\\b"
)
for (drug in names(drug_patterns)) {
  posts[[drug]] <- grepl(drug_patterns[drug], posts$match_text, perl = TRUE)
}
write.csv(data.frame(substance = names(drug_patterns), pattern = drug_patterns),
          "results/keyword_rules.csv", row.names = FALSE)

# Count tweets, once per substance, rather than repeated words in a tweet.
# Categories can overlap. All-language counts are keyword hits, not validated
# drug discussions: in particular, 'fent' is a common Catalan word.
mention_counts <- do.call(rbind, lapply(c("All languages", "English"), function(scope) {
  d <- if (scope == "English") posts[posts$lang == "en", ] else posts
  do.call(rbind, lapply(c("Set 1", "Set 2"), function(s) {
    a <- d[d$set == s, ]
    data.frame(scope = scope, set = s, substance = names(drug_patterns),
               tweets = vapply(names(drug_patterns), function(drug) sum(a[[drug]]), 0L),
               denominator = nrow(a))
  }))
}))
mention_counts$percent <- 100 * mention_counts$tweets / mention_counts$denominator
write.csv(mention_counts, "results/mention_counts.csv", row.names = FALSE)
# Show how much the ambiguous abbreviation changes the fentanyl counts.
fentanyl_long <- "\\b(fentanyl|fentanyls|fentanil|fentanilo)\\b"
fentanyl_sensitivity <- do.call(rbind, lapply(c("All languages", "English"), function(scope) {
  d <- if (scope == "English") posts[posts$lang == "en", ] else posts
  do.call(rbind, lapply(c("Set 1", "Set 2"), function(s) {
    a <- d[d$set == s, ]
    long_hit <- grepl(fentanyl_long, a$match_text, perl = TRUE)
    data.frame(scope = scope, set = s, long_form = sum(long_hit),
               fent_only = sum(a$Fentanyl & !long_hit))
  }))
}))
write.csv(fentanyl_sensitivity, "results/fentanyl_sensitivity.csv", row.names = FALSE)
english <- posts[posts$lang == "en", ]
write.csv(data.frame(id = english$id, set = english$set,
                    Methadone = english$Methadone, Suboxone = english$Suboxone,
                    Fentanyl = english$Fentanyl),
          "results/english_tweet_flags.csv", row.names = FALSE)

# Keep bare fentanyl separate from its named analogs. This is an explicit,
# finite dictionary, not an exhaustive search for every possible analog.
analog_patterns <- c(
  Carfentanil = "\\b(carfentanil|carfentanyl)\\b",
  Sufentanil = "\\b(sufentanil|sufentanyl)\\b",
  Alfentanil = "\\b(alfentanil|alfentanyl)\\b",
  Remifentanil = "\\b(remifentanil|remifentanyl)\\b",
  Acetylfentanyl = "\\b(acetylfentanyl|acetyl[ -]fentanyl)\\b",
  Valerylfentanyl = "\\b(valerylfentanyl|valeryl[ -]fentanyl)\\b",
  Furanylfentanyl = "\\b(furanylfentanyl|furanyl[ -]fentanyl)\\b",
  Cyclopropylfentanyl = "\\b(cyclopropylfentanyl|cyclopropyl[ -]fentanyl)\\b"
)
analog_counts <- do.call(rbind, lapply(names(analog_patterns), function(a) {
  data.frame(analog = a, set = c("Set 1", "Set 2"),
             tweets = vapply(c("Set 1", "Set 2"), function(s) {
               sum(grepl(analog_patterns[a],
                         english$match_text[english$set == s], perl = TRUE))
             }, 0L))
}))
write.csv(analog_counts, "results/analog_counts.csv", row.names = FALSE)

# Lab 2 preprocessing. Keep COVID words because they matter to this question.
# Stemmed unigram counts describe common discussion words, not formal topics.
corpus <- VCorpus(VectorSource(english$match_text))
corpus <- tm_map(corpus, content_transformer(function(x) {
  x <- gsub("&amp;|&lt;|&gt;|&quot;|&#39;", " ", x)
  gsub("[^a-z[:space:]]", " ", x)
}))
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "rt", "via"))
corpus <- tm_map(corpus, stripWhitespace)

# Lab 4 bigrams: adjacent retained words within a tweet, after stopword
# removal. Keep full word forms for readable phrase labels.
clean_text <- vapply(corpus, content, character(1))
tokens <- lapply(strsplit(trimws(clean_text), "\\s+"),
                 function(x) x[nchar(x) >= 3])
count_bigrams <- function(token_list) {
  grams <- unlist(lapply(token_list, function(x) {
    if (length(x) < 2) return(character(0))
    paste(head(x, -1), tail(x, -1))
  }), use.names = FALSE)
  counts <- table(grams)
  d <- data.frame(bigram = names(counts), frequency = as.integer(counts))
  d <- d[order(-d$frequency, d$bigram), ]
  rownames(d) <- NULL
  d
}

stemmed <- tm_map(corpus, stemDocument, language = "english")
dtm <- DocumentTermMatrix(stemmed, control = list(wordLengths = c(3, Inf)))
# Sparse column sums avoid converting a large matrix to a dense matrix.
freq_table <- function(index) {
  values <- slam::col_sums(dtm[index, ])
  d <- data.frame(term = names(values), frequency = as.integer(values))
  d <- d[d$frequency > 0, ]
  d <- d[order(-d$frequency, d$term), ]
  rownames(d) <- NULL
  d
}
topic_tables <- list()
bigrams <- list()
drug_stems <- unique(wordStem(c("methadone", "methadones", "methadon", "methadose",
                               "suboxone", "suboxones", "suboxon", "subox", "subuxone",
                               "fentanyl", "fentanyls", "fentanil", "fentanilo", "fent"),
                              language = "english"))
# Only the topic summary omits these generic conversation words; saved term
# frequencies, word clouds, and bigrams retain them.
topic_exclusions <- c(drug_stems, wordStem(c(
  "get", "can", "use", "people", "like", "just", "take", "need", "one", "will",
  "dont", "know", "now", "make", "also", "time", "want", "say", "said", "good",
  "much", "even", "day", "see", "really", "still", "well", "back", "think", "got"
), language = "english"))
for (drug in names(drug_patterns)) {
  index <- which(english[[drug]])
  f <- freq_table(index)
  write.csv(f, paste0("results/", tolower(drug), "_terms.csv"), row.names = FALSE)
  # Remove the three drug labels so they do not dominate the topic list.
  topic_tables[[drug]] <- head(f[!f$term %in% topic_exclusions, ], 10)
  bigrams[[drug]] <- count_bigrams(tokens[index])
  write.csv(bigrams[[drug]], paste0("results/", tolower(drug), "_bigrams.csv"),
            row.names = FALSE)
}
topics <- do.call(rbind, lapply(names(topic_tables), function(drug) {
  data.frame(substance = drug, topic_tables[[drug]], row.names = NULL)
}))
write.csv(topics, "results/top_discussion_terms.csv", row.names = FALSE)

period_terms <- lapply(c("Set 1", "Set 2"), function(s) freq_table(which(english$set == s)))
names(period_terms) <- c("Set 1", "Set 2")
for (i in 1:2) {
  write.csv(period_terms[[i]], paste0("results/set", i, "_terms.csv"), row.names = FALSE)
}
period_topics <- do.call(rbind, lapply(names(drug_patterns), function(drug) {
  do.call(rbind, lapply(c("Set 1", "Set 2"), function(s) {
    f <- freq_table(which(english[[drug]] & english$set == s))
    data.frame(substance = drug, set = s,
               head(f[!f$term %in% topic_exclusions, ], 10), row.names = NULL)
  }))
}))
write.csv(period_topics, "results/discussion_terms_by_period.csv", row.names = FALSE)

# A small, literal check for discussion of access, with the same words in
# both periods. This is a word-occurrence summary, not an access classifier.
access_words <- c("clinic", "treatment", "withdrawal", "take home", "covid", "coronavirus")
access_patterns <- c("\\bclinics?\\b", "\\btreatments?\\b", "\\bwithdrawals?\\b",
                     "\\btake[ -]?homes?\\b", "\\bcovid(?:[ -]?19)?\\b", "\\bcoronavirus\\b")
access_counts <- do.call(rbind, lapply(c("Methadone", "Suboxone"), function(drug) {
  do.call(rbind, lapply(c("Set 1", "Set 2"), function(s) {
    t <- english$match_text[english[[drug]] & english$set == s]
    data.frame(substance = drug, set = s, term = access_words,
               tweets = vapply(access_patterns, function(p) sum(grepl(p, t, perl = TRUE)), 0L),
               denominator = length(t))
  }))
}))
access_counts$percent <- 100 * access_counts$tweets / access_counts$denominator
write.csv(access_counts, "results/access_word_counts.csv", row.names = FALSE)

# Base R plots, as used in the labs.
pdf("figures/wordclouds.pdf", width = 9, height = 4.8)
par(mfrow = c(1, 2), mar = c(0, 0, 3, 0))
set.seed(740)
for (i in 1:2) {
  f <- head(period_terms[[i]], 70)
  wordcloud(f$term, f$frequency, scale = c(3.2, 0.6), min.freq = 1,
            max.words = 70, random.order = FALSE, rot.per = 0,
            colors = c("#2C3E50", "#327D99", "#707070"))
  title(paste("Set", i))
}
dev.off()

en_counts <- mention_counts[mention_counts$scope == "English", ]
count_matrix <- t(sapply(names(drug_patterns), function(drug) en_counts$tweets[en_counts$substance == drug]))
pct_matrix <- t(sapply(names(drug_patterns), function(drug) en_counts$percent[en_counts$substance == drug]))
colnames(count_matrix) <- colnames(pct_matrix) <- c("Set 1", "Set 2")
pdf("figures/substance_comparison.pdf", width = 9, height = 4.2)
par(mfrow = c(1, 2), mar = c(5, 4.5, 3, 1))
colors <- c("#46789A", "#C98D45")
for (measure in c("counts", "percent")) {
  m <- if (measure == "counts") count_matrix else pct_matrix
  positions <- barplot(t(m), beside = TRUE, col = colors,
                       names.arg = rownames(m), ylim = c(0, max(m) * 1.25),
                       ylab = if (measure == "counts") "Number of tweets" else "Percent of tweets",
                       main = if (measure == "counts") "Tweet counts" else "Share of each set")
  text(positions, t(m), labels = if (measure == "counts") format(t(m), big.mark = ",", trim = TRUE) else sprintf("%.1f", t(m)),
       pos = 3, cex = 0.72)
  legend("topright", legend = c("Set 1", "Set 2"), fill = colors, bty = "n", cex = 0.8)
}
dev.off()

pdf("figures/top_bigrams.pdf", width = 9, height = 9)
par(mfrow = c(3, 1), mar = c(3.5, 13, 2, 1), mgp = c(2, 0.6, 0))
for (drug in names(bigrams)) {
  b <- head(bigrams[[drug]], 10)
  barplot(rev(b$frequency), names.arg = rev(b$bigram), horiz = TRUE,
          las = 1, col = "#46789A", border = NA, cex.names = 0.85,
          xlim = c(0, max(b$frequency) * 1.17),
          xlab = "Bigram occurrences (both periods)", main = drug) -> positions
  text(rev(b$frequency), positions, labels = rev(b$frequency), pos = 4, cex = 0.75)
}
dev.off()

saveRDS(list(summary = dataset_summary, mentions = mention_counts,
             analogs = analog_counts, topics = topics, bigrams = bigrams,
             period_topics = period_topics, access = access_counts,
             languages = languages), "results/report_data.rds")
capture.output(sessionInfo(), file = "results/sessionInfo.txt")
print(dataset_summary, row.names = FALSE)
print(en_counts, row.names = FALSE)
print(topics, row.names = FALSE)
