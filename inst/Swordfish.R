yr =2018

Swordfish <- sp_swordfish(data.dir = data.dir, year = yr)
plot_Bycatch(obsSpp = Swordfish$bycatch[1,1],
             df = Swordfish$bycatch,
             showXSpp = 20,
             title ="Swordfish", subtitle = yr)
coverage <- calc_Coverage(get_MARFIS = Swordfish$marf, get_OBS = Swordfish$obs, quietly = T)
