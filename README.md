# dentomedical

**Publication-Ready Descriptive, Bivariate, Regression, and Diagnostic Accuracy Tools for Medical and Dental Data**

---

## 🎯 What is dentomedical

The 'dentomedical' package provides a comprehensive suite of tools for
 medical and dental research. It includes automated descriptive statistics,
 bivariate analysis with intelligent test selection, logistic regression,
 and diagnostic accuracy assessment. All functions generate structured,
 publication-ready tables using 'flextable', ensuring reproducibility and
 clarity suitable for manuscripts, reports, and clinical research workflows.


## 🚀 Features of dentomedical

* Automatically summarizes both numeric and categorical variables.
* Intelligent selection of statistical tests:

  * Continuous variables: t‑test, Wilcoxon, ANOVA, or Kruskal–Wallis.
  * Categorical variables: Chi‑square or Fisher’s exact test.
* Normality assessment via Shapiro–Wilk test (with safe handling of large sample sizes).
* Uni‑ and multivariate regression analyses (logistic and linear regression) with formatted ORs, CIs, and p-values.
* Diagnostic accuracy metrics for medical tests, including sensitivity, specificity, and predictive values.
* Publication‑ready output tables using `flextable`.
* Built with tidy workflows in mind (`dplyr`, `tidyr`, `tibble`).
* Designed for reproducible, CRAN-ready medical and clinical data analyses.
 ---

## 📦 Installation

You can install the development version directly from GitHub:

# download new version of the dentomedical package
```r
# install.packages("devtools")
devtools::install_github("umarhussain-git/dentomedical1")
```
# old version 
```r

install.packages("dentomedical")
```

# read its guide on Rpubs
https://rpubs.com/umarhussain/1378450

```r
library(dentomedical)

## 1. Normality Test Summary for numeric variables
norm_sum(iris)

## 2. Summary table with p‑values (bivariate analysis)
# Example using built-in dataset CO2
sum_stat.p(CO2, by = "Type", statistic = "med_iqr")

## 3. Force specific test (e.g. Wilcoxon for two-group comparison)
sum_stat.p(CO2, by = "Type", statistic = "med_iqr", test_type = "wilcox")
```

```r
library(dentomedical)
# uni- and multivaiate linear regression
#Example using built-in iris dataset
linreg(iris, outcome = "Sepal.Length",
       predictors = c("Sepal.Width", "Petal.Length", "Species"))
```

```r
# uni- and multivaiate logistic regression
logreg(data=medical_data(), outcome="case" ,
   predictors= c("age" ,  "parity" ,    "induced" ))

# *Note*: please make sure your all categorical variables should be  factor (ordinal)
# if not then run this code first (lets say your data name is 'df')
df <- df %>%
  mutate(across(where(is.character), as.factor))
```
```r
# Diagnostic accuracy
diagnostic_data <- data.frame(
  test = c("positive","negative","positive","negative","positive","negative","positive","negative"),
  goldstandard = c("positive","positive","negative","negative","positive","negative","positive","negative")
)
diag_accuracy(diagnostic_data, test_col = "test", gold_col = "goldstandard")
```

🧰 Dependencies

dentomedical relies on the following R packages:

dplyr, tidyr, tibble — for data manipulation

flextable m=, broom— for formatted tables

stats (base) — for statistical tests

These dependencies are declared in the DESCRIPTION, so installation should handle them automatically.

👤 Author & Maintainer

Umar Hussain
📧 drumarhussain@gmail.com

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

and avoid manual copying/pasting errors

Let the package do the heavy lifting — you focus on interpreting the data.
