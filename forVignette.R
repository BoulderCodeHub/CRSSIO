# for vignette
tmp <- CoRiverNF::monthlyInt["1906/"]

allNodes <- nfShortNames()

nf <- lapply(allNodes, function(x) createISMMatrix(tmp[,which(names(tmp) == x)], startMonth = "2018-01-31"))
names(nf) <- allNodes

# "create" should create files
# "get" will return data
# nouns:
# crssi, crsso, ism, nf, natsalt
