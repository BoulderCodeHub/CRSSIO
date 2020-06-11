# xx
# xx2 (see test-nfd_stats)

t1 <- nfd_get_site(xx, "LeesFerry", "total", "annual")
t2 <- nfd_get_site(xx2, "LeesFerry", "total", "annual")
t3 <- nfd_get_site(mm, "Cameo", "intervening", "monthly")
t4 <- nfd_get_site(mm2, "Cameo", "intervening", "monthly")

d1 <- density(t1[,1], n = 25)
d2 <- density(t2, n = 25)
d3 <- density(t2[,1], n = 25)

d4 <- density(t1[,1], n = 25, from = min(d2$x), to = max(d2$x))

dd <- nfd_ann_pdf(nfd_get_site(xx, "LeesFerry", "total", "annual"), breaks = NULL, "cy") 

dd2 <- nfd_ann_pdf(nfd_get_site(xx2, "LeesFerry", "total", "annual"), breaks = dd$x, "cy")


dd3 <- nfd_mon_pdf(t3, c(4, 7), breaks = NULL, "cy")
#my_breaks <- cbind(filter(dd3, month == 4)$x, filter(dd3, month == 7)$x)
my_breaks2 <- nfd_pdf_get_monthly_breaks(dd3)
dd4 <- nfd_mon_pdf(t4, c(4, 7), breaks = my_breaks, "cy")
                  