R.utils::sourceDirectory("c:/git/Maritimes/Mar.bycatch/R/")
load("c:/git/Maritimes/Mar.bycatch/data/spLookups.rda")
WF_loc <- sp_winterflounder(year = 2015, data.dir = "C:/git/wrangledData/", useLocal=T, debug=T, quiet=T)
WF_loc <- sp_winterflounder(year = 2016, data.dir = "C:/git/wrangledData/", useLocal=T, debug=T, quiet=T)
WF_loc <- sp_winterflounder(year = 2017, data.dir = "C:/git/wrangledData/", useLocal=T, debug=T, quiet=T)
WF_loc <- sp_winterflounder(year = 2018, data.dir = "C:/git/wrangledData/", useLocal=T, debug=T, quiet=T)

WF_rem <- sp_winterflounder(year = 2015, data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=T, quiet=F)
WF_rem <- sp_winterflounder(year = 2016, data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=T, quiet=F)
WF_rem <- sp_winterflounder(year = 2017, data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=T, quiet=F)
WF_rem <- sp_winterflounder(year = 2018, data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=T, quiet=F)

SHake_loc <- sp_silverhake(year = 2015, data.dir = "C:/git/wrangledData/", useLocal=T, debug=T, quiet=T)
SHake_loc <- sp_silverhake(year = 2016, data.dir = "C:/git/wrangledData/", useLocal=T, debug=T, quiet=T)
SHake_loc <- sp_silverhake(year = 2017, data.dir = "C:/git/wrangledData/", useLocal=T, debug=T, quiet=T)
SHake_loc <- sp_silverhake(year = 2018, data.dir = "C:/git/wrangledData/", useLocal=T, debug=T, quiet=T)

SHake_rem <- sp_silverhake(year = 2015, data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=T, quiet=F)
SHake_rem <- sp_silverhake(year = 2016, data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=T, quiet=F)
SHake_rem <- sp_silverhake(year = 2017, data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=T, quiet=F)
SHake_rem <- sp_silverhake(year = 2018, data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=T, quiet=F)


Poll_rem <- sp_pollock(year = 2015, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)
Poll_rem <- sp_pollock(year = 2016, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)
Poll_rem <- sp_pollock(year = 2017, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)
Poll_rem <- sp_pollock(year = 2018, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)

Poll_loc <- sp_pollock(year = 2015, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)
Poll_loc <- sp_pollock(year = 2016, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)
Poll_loc <- sp_pollock(year = 2017, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)
Poll_loc <- sp_pollock(year = 2018, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)


Poll_rem <- sp_pollock(year = 2015, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)
Poll_rem <- sp_pollock(year = 2016, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)
Poll_rem <- sp_pollock(year = 2017, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)
Poll_rem <- sp_pollock(year = 2018, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)

Poll_loc <- sp_pollock(year = 2015, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)
Poll_loc <- sp_pollock(year = 2016, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)
Poll_loc <- sp_pollock(year = 2017, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)
Poll_loc <- sp_pollock(year = 2018, type = "MOBILE", component = "EASTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)


Poll_rem <- sp_pollock(year = 2015, type = "MOBILE", component = "WESTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)
Poll_rem <- sp_pollock(year = 2016, type = "MOBILE", component = "WESTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)
Poll_rem <- sp_pollock(year = 2017, type = "MOBILE", component = "WESTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)
Poll_rem <- sp_pollock(year = 2018, type = "MOBILE", component = "WESTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=F, oracle.username="mcmahonm", oracle.password="KimoIsG00d", oracle.dsn="PTRAN", usepkg="roracle", debug=F, quiet=T)

Poll_loc <- sp_pollock(year = 2015, type = "MOBILE", component = "WESTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)
Poll_loc <- sp_pollock(year = 2016, type = "MOBILE", component = "WESTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)
Poll_loc <- sp_pollock(year = 2017, type = "MOBILE", component = "WESTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)
Poll_loc <- sp_pollock(year = 2018, type = "MOBILE", component = "WESTERN", mesh = "ALL", data.dir = "C:/git/wrangledData/", useLocal=T, debug=F, quiet=T)


northernShrimp <- sp_northernShrimp(year = 2018, data.dir = "C:/git/wrangledData/", useLocal=FALSE, debug=FALSE, oracle.username = "mcmahonm", oracle.password = "KimoIsG00d", oracle.dsn = "PTRAN", usepkg = "roracle")
northernShrimp17 <- sp_northernShrimp(year = 2017, data.dir = "C:/git/wrangledData/", useLocal=FALSE, debug=FALSE, oracle.username = "mcmahonm", oracle.password = "KimoIsG00d", oracle.dsn = "PTRAN", usepkg = "roracle")
northernShrimp16 <- sp_northernShrimp(year = 2016, data.dir = "C:/git/wrangledData/", useLocal=FALSE, debug=FALSE, oracle.username = "mcmahonm", oracle.password = "KimoIsG00d", oracle.dsn = "PTRAN", usepkg = "roracle")
northernShrimp15 <- sp_northernShrimp(year = 2015, data.dir = "C:/git/wrangledData/", useLocal=FALSE, debug=FALSE, oracle.username = "mcmahonm", oracle.password = "KimoIsG00d", oracle.dsn = "PTRAN", usepkg = "roracle")
