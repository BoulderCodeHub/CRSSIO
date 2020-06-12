# for vignette
tmp <- CoRiverNF::monthlyInt["1906/"]

allNodes <- nfShortNames()

nf <- lapply(allNodes, function(x) createISMMatrix(tmp[,which(names(tmp) == x)], startMonth = "2018-01-31"))
names(nf) <- allNodes

# "create" should create files
# "get" will return data
# nouns:
# crssi, crsso, ism, nf, natsalt

# ism on crssi vs ism on nf and sac, then creating crssi
# if data are cy, the two processees are the same
# if original data are wy, then they are not. The first, trims to full cy, then
# applies ism. The second, applies ism on the full wy of data, and then trims 
# to cy data.

nfd_mon <- crss_nf(-99, n_trace = 1, flow_space = "both", time_step = "monthly", 
                   year = "wy")
nfd_mon$monthly$intervening[[1]] <- CoRiverNF::monthlyInt["1970-10/1980-09"]
nfd_mon$monthly$total[[1]] <- CoRiverNF::monthlyTot["1970-10/1980-09"]

sac2 <- CRSSIO:::sacYT["1971/1979"]

crssi2 <- crssi(nfd_mon, sac2, 1.2, "ok")

# this will fail. 
expect_identical(
  ism(crssi2),
  crssi(nfd_extract(ism(nfd_mon), "1971-01/1979-12"), ism(CRSSIO:::sacYT["1970/1979"]), 1.2, "ok")
)

# the other way passes. See test-ism.R.
