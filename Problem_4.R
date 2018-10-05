# Thomas Waters
# Assignment 2
# Problem 6, 7, and 8
library(tidyverse)
library(ggplot2)

getExtendedElements = function() {
  gamma = 1.73
  scale = 3.46*10^8

  k = 199:1
  nk = scale * k^(-gamma)

  log10_k = log10(k)
  log10_nk = log10(nk)

  return(data.frame(k, nk, log10_k, log10_nk))
}

# ---- Create the data set ----

google_vocab_filename = 'vocab_k_nk.txt'
vocab_freqs = read.table(google_vocab_filename, sep=' ', col.names=c('k', 'nk')) %>%
  mutate(log10_k = log10(k)) %>%
  mutate(log10_nk = log10(nk)) %>%
  bind_rows(getExtendedElements())

# ---- Plot it in log-log space ----

ggplot(vocab_freqs,
       aes(x=log10_k, y=log10_nk)) +
  labs(title='Frequency Distribution of Words',
       x='log10 Nk, Frequency',
       y='log10 k, Number of Words') +
  geom_point()

img_width = 4
img_height = img_width * 7 / 8
ggsave('images/problem_4.png', device='png', units='in', width=img_width, height=img_height)

# ---- Problem 7 ---------------------------------------------------------------

power_law_cutoff = 4.25
restricted_vocab_freqs = vocab_freqs %>%
  filter(log10_k < power_law_cutoff)

lm(log10_nk ~ log10_k, restricted_vocab_freqs)

# ggplot(restricted_vocab_freqs,
#        aes(x=log10_k, y=log10_nk)) +
#   labs(title='Frequency Distribution of Words',
#        x='log10 Nk, Frequency',
#        y='log10 k, Number of Words') +
#   geom_point() +
#   geom_smooth(method='lm')

# ---- Problem 8 ---------------------------------------------------------------



mean(vocab_freqs$nk)
sd(vocab_freqs$nk)
