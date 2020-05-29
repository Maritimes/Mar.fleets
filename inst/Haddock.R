yr = 2017

Haddock <- sp_haddock(data.dir = data.dir, year = yr, type = "mobile", area = "5Zjm")
plot_Bycatch(obsSpp = Haddock$bycatch[1,1],
             df = Haddock$bycatch,
             showXSpp = 20,
             title ="5Zjm Haddock (mobile)", subtitle = yr)
coverage <- calc_Coverage(get_MARFIS = Haddock$marf, get_OBS = Haddock$obs, quietly = T)
