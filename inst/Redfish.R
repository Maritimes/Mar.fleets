yr = 2017

Redfish <- sp_redfish(data.dir = data.dir, year = yr, unit=99)
plot_Bycatch(obsSpp = Redfish$bycatch[1,1],
             df = Redfish$bycatch,
             showXSpp = 12,
             title ="Unit 2 Redfish", subtitle = yr)
coverage <- calc_Coverage(get_MARFIS = Redfish$marf, get_OBS = Redfish$obs, quietly = T)
