# BSHES 740

Course analysis code for BSHES 740.

## Assignment 1

[R analysis script](assignment1/assignment1_analysis.R) comparing tweets about methadone, Suboxone, and fentanyl in two periods around the emergence of COVID-19.

The analysis follows the labs: keyword matching, text cleaning, stemming, term frequencies, word clouds, and bigrams. Figures and summary tables are included in `assignment1/figures` and `assignment1/results`.

### Run

Install the R packages once:

```r
install.packages(c("tm", "SnowballC", "slam", "wordcloud"))
```

Place the supplied `TADA_A1_set1.csv` and `TADA_A1_set2.csv` files in `assignment1`, then run from the repository root:

```sh
cd assignment1
Rscript assignment1_analysis.R
```

The script generates the full frequency tables and figures. It reads tweet IDs as text, repairs a split CSV record, and uses English posts for the main comparison. Tweets may match more than one substance.

The supplied datasets, course slides, tweet-level outputs, and report files are kept outside version control. See [assignment notes](assignment1/README.md) for output details.

AI assistance: ChatGPT assisted with the analysis code, figures, and report draft.
