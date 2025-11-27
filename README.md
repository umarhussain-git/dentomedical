# dentomedical

**Advanced Descriptive and Bivariate Analysis Tools for Medical and Clinical Data**

---

## 🎯 What is dentomedical

`dentomedical` is an R package built for **medical, dental, and clinical researchers** who want to streamline their data analysis.  
It provides **automated summary statistics, normality testing, and bivariate analysis**, with intelligent test selection based on variable types and distributions.  
All outputs are formatted into **clean, publication‑ready tables** (via `flextable`), making it easy to include results directly in manuscripts, reports, or thesis documents.  

---

## 🚀 Features

- Automatically summarises both numeric and categorical variables.  
- Intelligent selection of statistical tests:
  - Continuous variables: t‑test, Wilcoxon, ANOVA, or Kruskal–Wallis.  
  - Categorical variables: Chi‑square or Fisher’s exact test.  
- Normality assessment via Shapiro–Wilk test (with safe handling of large sample sizes).  
- Publication‑ready output tables using `flextable`.  
- Built with tidy workflows in mind (`dplyr`, `tidyr`, `tibble`).  

---

## 📦 Installation

You can install the development version directly from GitHub:

```r
# install remotes if not already installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("umarhussain-git/dentomedical")
```
```r
library(dentomedical)

## 1. Normality Test Summary for numeric variables
norm.sum(iris)

## 2. Summary table with p‑values (bivariate analysis)
# Example using built-in dataset CO2
sum.stat.p(CO2, by = "Type", statistic = "med_iqr")

## 3. Force specific test (e.g. Wilcoxon for two-group comparison)
sum.stat.p(CO2, by = "Type", statistic = "med_iqr", test_type = "wilcox")
```
🧰 Dependencies

dentomedical relies on the following R packages:

dplyr, tidyr, tibble — for data manipulation

flextable — for formatted tables

stats (base) — for statistical tests

These dependencies are declared in the DESCRIPTION, so installation should handle them automatically.

👤 Author & Maintainer

Umar Hussain
📧 umar.hussain@gmail.com

📄 License

This project is licensed under the MIT License — see the LICENSE
 file for details.

## 📚 Contributing & Feedback

Feel free to open issues or submit pull requests.
I welcome suggestions for new features (e.g. more statistical tests, custom formatting, additional table output types) or bug reports.

## 🔖 Why dentomedical

As a clinician‑researcher or dental academic, you’re often juggling data cleaning, statistical tests, manuscript formatting, and time constraints.
dentomedical cuts down repetition:

automate the routine summarization and testing,

get instantly formatted tables,

avoid manual copying/pasting errors,

and produce output ready for publishing or reporting.

Let the package do the heavy lifting — you focus on interpreting the data.
