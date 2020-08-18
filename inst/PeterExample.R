#  Mega-Example
library(Mar.bycatch)
# this uses local data
Peter <- sp_halibut(dateStart = "2018-01-01", dateEnd = "2018-12-31",
                    useLocal = TRUE, data.dir= "c:/git/wrangledData/")
# this queries oracle
#Peter1 <- sp_halibut(dateStart = "2018-01-01", dateEnd = "2018-12-31",
#                    useLocal = FALSE, oracle.username="mcmahonm", oracle.dsn="ptran", oracle.password = "KimoIsG00d", usepkg = "roracle")

