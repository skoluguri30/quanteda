# Test wordfish
# R -d "valgrind --leak-check=full --show-reachable=yes" --log-file="tests/valgrind.out" -f tests/valgrind/textmodel_wordfish.R
require(quanteda)
textmodel_wordfish(data_dfm_lbgexample, dir = c(1,5))