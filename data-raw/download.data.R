## ----- getting_data
options(encoding = 'UTF-8')
library(tm)

use.sample <- FALSE
set.seed(2016-11-27)
sample.lines <- 20000

data.url <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
data.zip <- 'data-raw/Coursera-SwiftKey.zip'
data.local.dir <- 'data-raw'
data.local <- file.path(data.local.dir, 'final')
if (!file.exists(data.zip)) {
    download.file(data.url, data.zip)
}
if (!file.exists(data.local)) {
    unzip(data.zip, exdir = data.local.dir)
}

# Not strictly necessary, allows for faster access to (possibly a subset of)
# the raw source files
for (fname in dir(data.local, pattern = '*.txt', recursive = TRUE)) {
    varname <- gsub('_', '.', gsub('.txt', '', basename(fname)))
    temp <- readLines(file.path(data.local, fname))
    if (use.sample) {
        temp <- sample(temp, size = min(length(temp), sample.lines))
    }
    assign(varname, temp)
    writeLines(temp, file.path('data', basename(fname)))
    save(list = varname,
         file = file.path('data', gsub('.txt', '.RData', basename(fname))))
    rm(list = varname)
}
rm(data.url, data.zip, data.local.dir, data.local,
   fname, temp, varname, use.sample, sample.lines)
