R.utils::sourceDirectory("C:/git/Maritimes/Mar.utils/R/")
R.utils::sourceDirectory("C:/git/Maritimes/Mar.bycatch/R/")

isolated=T
yr =c(2015:2018)
Haddock_m_5Z_ng <-list()
Haddock_f_5Z <-list()
Haddock_m_4X <-list()

for (y in 1:length(yr)){
  Haddock_m_5Z_ng[[paste0("haddock_",yr[y])]]<- sp_haddock(data.dir = data.dir, year = yr[y], type = "mobile", area = "5Zjm")
   # Haddock_f_5Z[[paste0("haddock_",yr[y])]]<- sp_haddock(data.dir = data.dir, year = yr[y], type = "fixed", area = "5Zjm")
   # Haddock_m_4X[[paste0("haddock_",yr[y])]]<- sp_haddock(data.dir = data.dir, year = yr[y], type = "mobile", area = "4X5Y")
}
# Haddock_m_5Z_ng$haddock_2015$catch_T #3563.058
# Haddock_m_5Z_ng$haddock_2016$catch_T #2780.592
# Haddock_m_5Z_ng$haddock_2017$catch_T #1519.366
# Haddock_m_5Z_ng$haddock_2018$catch_T #498.6487
#
# Haddock_f_5Z$haddock_2015$catch_T281.6728
# Haddock_f_5Z$haddock_2016$catch_T95.53995
# Haddock_f_5Z$haddock_2017$catch_T52.86891
# Haddock_f_5Z$haddock_2018$catch_T33.90602
#
# Haddock_m_4X$haddock_2015$catch_T2485.622
# Haddock_m_4X$haddock_2016$catch_T2981.56
# Haddock_m_4X$haddock_2017$catch_T4385.83
# Haddock_m_4X$haddock_2018$catch_T3352.377
#
# Haddock_m_5Z$haddock_2015$ntrips136
# Haddock_m_5Z$haddock_2016$ntrips145
# Haddock_m_5Z$haddock_2017$ntrips64
# Haddock_m_5Z$haddock_2018$ntrips33
#
# Haddock_f_5Z$haddock_2015$ntrips53
# Haddock_f_5Z$haddock_2016$ntrips37
# Haddock_f_5Z$haddock_2017$ntrips26
# Haddock_f_5Z$haddock_2018$ntrips22
#
# Haddock_m_4X$haddock_2015$ntrips415
# Haddock_m_4X$haddock_2016$ntrips456
# Haddock_m_4X$haddock_2017$ntrips435
# Haddock_m_4X$haddock_2018$ntrips396
