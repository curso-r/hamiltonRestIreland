get_assumptions_text <- function(){
  HTML(
    paste0("<div style = 'color: black;'><p> This visualisation plots the raw number of cases in a selected ",
    "province and calculates the R number for that period using the methods ",
    "described in <a href = 'https://bmcmedinformdecismak.biomedcentral.com/",
    "articles/10.1186/1472-6947-12-147'>Obadia et al, BMC Medical Informatics ",
    "and Decision Making, 2012</a>.<p> The method relies on an estimate of the ",
    "Generation Time of the disease; this is the time from becoming infected ",
    "with COVID-19 to the time of generating a secondary case. The estimated ",
    "generation time distribution and its parameters have been taken from <a ",
    "href = 'https://onlinelibrary.wiley.com/doi/full/10.1111/biom.13325'>Yuhao ",
    "et al Biometrics, 2020</a>. The values can be changed by clicking the ",
    "'show extra options' button.<p> The R0 package allows for different methods ",
    "to calculate the R value. We use the Sequential Bayesian method which also ",
    "provides a 95% confidence interval. Other methodsrun can be selected in the ",
    "extra options.<p> If there are large number of zero cases, or the date range ",
    "is too large/small, the estimate may fail and an R0 number will not be ",
    "shown. Try increasing/decreasing the date range to get a valid estimate.",
    "<p> Be aware that most of these methods have hidden assumptions (e.g. that ",
    "the date range shows a period of exponential growth). If you are changing ",
    "the method, we would recommend reading the above papers first to avoid ",
    "mistaken readings.</div>")
  )
}