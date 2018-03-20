require(quanteda)
quanteda_options(threads = 4)
load("/home/kohei/Documents/Brexit/Analysis/data_tokens_guardian.RData")
toks <- data_tokens_guardian[1:50000]
xtoks <- as.xtokens(toks)

print(object.size(toks), units = 'MB')
print(object.size(xtoks) + sum(ntoken(toks)) * 4, units = 'MB')

microbenchmark::microbenchmark(
    tokens = tokens_select(toks, stopwords(), valuetype = 'fixed', padding = TRUE),
    xtokens = tokens_select(xtoks, stopwords(), valuetype = 'fixed', padding = TRUE),
    times = 10, unit = 'relative'
)

toks_sent <- tokens_segment(toks, c(".", "?", "!"), valuetype = "fixed", pattern_position = "after")
xtoks_sent <- as.xtokens(toks_sent)

microbenchmark::microbenchmark(
    tokens = tokens_select(toks_sent, stopwords(), valuetype = 'fixed', padding = TRUE),
    xtokens = tokens_select(xtoks_sent, stopwords(), valuetype = 'fixed', padding = TRUE),
    times = 10, unit = 'relative'
)