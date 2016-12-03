options(mc.cores = 1)
options(encoding = 'UTF-8')

library('ProjectTemplate')
reload.project()

# de.Tweets.source <- VectorSource(de.DE.twitter)
# de.Tweets.corpus <- VCorpus(de.Tweets.source)

## ----- load_data
corpus.file <- 'data/en.corpus.RData'
if (!file.exists(corpus.file)) {
    en.source <- DirSource('data-raw/final/en_US',
                           pattern = 'en.*\\.txt',
                           encoding = 'UTF-8')
    en.corpus <- VCorpus(en.source, readerControl = list(language = 'en'))
    save(en.corpus, file = corpus.file)
} else {
    load(corpus.file)
}

## ----- summarise_data
# words <- lapply(en.corpus, function(x)stri_extract_all_words(x$content))
# en.corpus.summary <- data.frame(
#     documents = sapply(en.corpus, function(x)length(x$content)),
#     source_size = sapply(en.corpus, function(x)format(object.size(x), units = 'Mb')),
#     word_count = sapply(words, function(x)length(unlist(x))),
#     unique_words = sapply(words, function(x)length(unique(unlist(x)))),
#     average_words = sapply(words, function(x)mean(sapply(x, length)))
# )
# names(en.corpus.summary) <- c('Number of documents', 'Source size',
#                               'Number of words', 'Number of unique words',
#                               'Average words per document')

## ----- clean_data
set.seed(2016 - 12 - 03)
# Subset each source to 20000 lines
for (s in seq_along(en.corpus)) {
    en.corpus[[s]]$content %<>% sample(size = min(length(.), 20000))
}

# define function to remove substrings using regular expressions
removePattern <- content_transformer(function(x, pattern)gsub(pattern, ' ', x, perl = T))

en.corpus %<>%
    tm_map(content_transformer(tolower)) %>%
    # Remove urls
    tm_map(removePattern, '(https?|ftp)://(\\w+[\\.|/?&=#-]*)+') %>%
    # Remove mentions, retweets, etc.
    tm_map(removePattern, '((rt|/?via)( |:)*)?(@\\w+)?(#\\w+)') %>%
    # tm_map(removeWords, stopwords('en')) %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation)

## ----- dtm
# en.dtm <- as.list(seq_along(en.corpus))
# en.freqs <- as.list(seq_along(en.corpus))
# for (k in seq_along(en.corpus)) {
#     en.dtm[[k]] <- DocumentTermMatrix(VCorpus(VectorSource(en.corpus[[k]]$content)))
#     en.dtm[[k]] %<>% removeSparseTerms(.999)
#     en.freqs[[k]] <- sort(colSums(as.matrix(en.dtm[[k]])), decreasing = TRUE)
# }
# names(en.dtm) <- names(en.corpus)
# names(en.freqs) <- names(en.corpus)

# freqs <- data.frame(freq = unlist(en.freqs, use.names = T))
# freqs %<>%
#     mutate(
#         name = row.names(.)
#     ) %>%
#     separate(
#         col = name,
#         into = c('language', 'source', 'filetype', 'word'),
#         sep = '\\.'
#     ) %>%
#     select(
#         -filetype
#     )

## ------------------------ n-grams
bigram.tokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
trigram.tokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
quadgram.tokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))
uni.quadgram.tokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 4))

# en.corpus2 <- en.corpus %>%
#     lapply(function(x)x$content) %>%
#     unlist %>%
#     VectorSource %>%
#     VCorpus
# dtm <- en.corpus %>%
#     DocumentTermMatrix
# dtm.bi <- en.corpus %>%
#     DocumentTermMatrix(control = list(tokenizer = bigram.tokenizer))
# dtm.tri <- en.corpus %>%
#     DocumentTermMatrix(control = list(tokenizer = trigram.tokenizer))
# dtm.quad <- en.corpus %>%
#     DocumentTermMatrix(control = list(tokenizer = quadgram.tokenizer))
dtm.uni.quad <- en.corpus %>%
    DocumentTermMatrix(control = list(tokenizer = uni.quadgram.tokenizer))

freqs <- colSums(as.matrix(dtm.uni.quad))
quadgrams <- data.frame(quads = colnames(dtm.uni.quad))
quadgrams %<>%
    separate(quads, c("one", "two", "three", "four"), sep = " ", fill = "left") %>%
    unite(trigram, -four, sep = " ")
qmod <- quadgrams %>% lm(four~trigram, ., weights = freqs)
