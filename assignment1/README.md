# Assignment 1: drug-related chatter

Code: [assignment1_analysis.R](https://github.com/razerblae/BSHES740/blob/main/assignment1/assignment1_analysis.R)

## Files

- `assignment1_analysis.R`: keyword matching, text cleaning, term frequencies, word clouds, and bigrams.
- `figures/`: word clouds, substance comparisons, and the top 10 bigrams for each substance.
- `results/`: summary counts, matching rules, language counts, and discussion terms.

Running the script also creates full term and bigram frequency tables, tweet flags, and cached report data. These larger files stay local. The local submission ZIP includes the report PDF, its editable R Markdown source, the code, figures, and results. Report files and the supplied datasets are not published in this repository.

## Run

Place both supplied CSV files beside the R script, then run from this directory:

```sh
Rscript assignment1_analysis.R
```

Required packages: `tm`, `SnowballC`, `slam`, and `wordcloud`. Install once in R:

```r
install.packages(c("tm", "SnowballC", "slam", "wordcloud"))
```

## Analysis notes

- Set 1 has 171,012 posts; Set 2 has 226,851. A standalone carriage return splits one Set 2 tweet during a normal CSV import; the script fixes this before reading it.
- The main analysis uses the 66,592 and 62,132 English posts. This reduces unrelated Catalan matches for “fent.”
- Drug counts are numbers of tweets. Word and bigram counts are total occurrences. Substance groups can overlap.
- The three bigram groups are methadone, Suboxone, and fentanyl, combining both periods. Pairs are formed within tweets after stopword removal.
- Keyword rules include spelling variants and remove usernames and links before matching. Bare “bupe,” buprenorphine, and “subs” do not identify Suboxone specifically and are excluded.
- Topic summaries use common terms after removing drug names and generic conversational words. Full unfiltered frequencies are also generated.

## Local report

To render the local `assignment1_report.Rmd`, run:

```r
rmarkdown::render("assignment1_report.Rmd")
```

This also requires `rmarkdown`, `knitr`, Pandoc, XeLaTeX, and Times New Roman. On this computer:

```sh
RSTUDIO_PANDOC='/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64' Rscript -e 'rmarkdown::render("assignment1_report.Rmd", quiet = TRUE)'
```

AI assistance: ChatGPT assisted with the code, figures, and report draft. The course syllabus requires students to write the final report in their own words and disclose AI assistance.
