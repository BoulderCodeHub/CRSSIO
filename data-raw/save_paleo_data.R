zz <- read.csv('data-raw/woodhouse_leesB.csv')
colnames(zz) <- c('year', 'LeesFerry')
zz$month <- 'September'

woodhouse <- as_nfd(zz, flow_space = 'total', time_step = 'annual', year = 'wy')

usethis::use_data(woodhouse)

zz <- read.csv('data-raw/meko2017.csv')
colnames(zz) <- c('year', 'LeesFerry')
zz$month <- 'September'

meko2017 <- as_nfd(zz, flow_space = 'total', time_step = 'annual', year = 'wy')

usethis::use_data(meko2017)
