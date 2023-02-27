n <- 10000

head_ctr <- 0
monday_ctr <- 0
tuesday_ctr <- 0

for(ind in 1:n) {
  coin_flip <- sample(c('H','T'), prob = c(0.5,0.5))
  if (coin_flip[1] == 'T') {
    monday_ctr <- monday_ctr + 1
    tuesday_ctr <- tuesday_ctr + 1
  } else {
    head_ctr <- head_ctr + 1
  }
}

(monday_ctr + tuesday_ctr) / (monday_ctr + tuesday_ctr + head_ctr)