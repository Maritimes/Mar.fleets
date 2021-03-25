--By plopping a known list of accepted VRs from a fleet into this SQL, we get the associated licence types and subtypes
SELECT distinct licence_type, licence_type_id, licence_subtype, gear_code,     licence_subtype_id, species_code, species FROM MARFISSCI.marbycatch_lic WHERE LICENCE_ID IN ( 
--    SELECT DISTINCT LICENCE_ID FROM MARFISSCI.LICENCE_VESSELS WHERE VR_NUMBER IN (
-- 1068,3488,3660,4038,5675,5711,5744,5752,6860,13982,18071,18107,19308,19545,19739,19832,22422,27473,100059,100216,100227,100247,100570,100571,100827,100978,100998,101040,101155,101209,101293,101434,101454,101496,101544,101550,101595,101608,101698,101761,101804,102339,102380,102408,102756,102789,102865,102880,103210,103394,103431,103445,103580,103634,103971,104044,104159,104234,104244,104270,104282,104317,104428,104482,104688,104698,104735,104886,104953,104962,105119,105135,105163,105177,105184,105222,105326,105329,105351,105367,105372,105429,105468,105529,105531,105538,105550,105574,105627,105640,105655,105661,105708,105735,105763,105765,105776,105780,105783,105802,105820,105833,105847,105892,105911,105917,105935,105952,105966,105969,105971,106001,106009,106014,106033,106062,106074,106109,106117,106183,106186,106196,106215,106225,106262,106268,106295,106296,106307,106331,106334,106347,106350,106358,106363,106373,106381,106387,106457,106504,106523,106533,106573,106581,106598,106607,106614,106633,106635,106688,106709,106715,106739,106776,106793,106906,106923,106951,106969,107018,107019,107029,107034,107089,107140,107152,107157,107164,107168,107181,107218,107229,107261,107262,107269,107271,107372,107381,107392,107478,107508,107553,107679,107698,107743,107829,107841,107878,107881,107883,107899,107901,107911,107918,107919,107953,107955,107965,107971,107990,108011,108043,108066,108072,108140,108157,108248,108252,108262,108282,108287,108297,108303,108371,108390,108425,108441,108442,108463,108486,108503,108553,108572,108581,108590,108620,108629,108660,108709,108725,138187
-- )
100527, 100871, 101087, 102087, 100677, 100977, 101133, 100777, 101134, 100387, 102494, 100327, 100644, 100646, 100548, 100616, 100668, 100998, 100338, 100268, 301179, 102199, 100789, 100362, 100768, 101849, 101550, 100629, 101599, 103539, 100697, 103546, 100583, 300961, 101668, 100414, 100896, 101219, 101929, 100848, 103499, 100517, 100449, 100665, 100687, 100984, 103568, 100267, 100458, 101242, 100672, 100588, 103560, 101055, 303240, 101005, 100247, 102512, 142344, 100759, 144278, 102048, 100305, 100708, 300962, 100239, 103511, 100274, 100318, 100729, 100726, 142087, 102446, 100599, 101993, 100794, 100747, 102455, 100698, 100774, 101986, 100701, 100278, 100353, 101029, 100426, 100679, 102467, 102260, 100462, 142082, 100716, 101621, 101651, 101130, 102277, 101880, 102440, 101063, 101126, 100838, 303775, 101964, 102241, 100765, 101108, 101827, 100725, 100263, 100303, 103468, 101085, 100656, 102533, 100467, 101132, 100972, 103453, 100613, 100287, 101991, 304929, 100662, 304926, 305359, 305360, 305357, 101535, 100258, 100666, 142086, 142080, 142085, 142083, 142343, 142077, 044986, 142341, 101173, 100674, 305356, 304715, 306810, 100624, 100257, 100653, 304927, 100295, 142081, 043688, 103564, 142078, 100617, 100270, 100654, 100681, 102469, 100277, 304928, 305358, 100451, 101038, 100377, 308907, 101066, 100253, 309866, 101208, 100884, 101601, 308618, 315732, 101945, 103562, 318054, 318053, 100713, 101116, 100568, 100425, 100700, 100640, 101106, 100657, 101091, 100739, 100246, 101402, 100772, 101714, 100405, 100694, 101988, 322454, 100893, 102281, 325524, 329857, 100513, 101930, 325602, 323755, 103515, 100686, 101890, 100299, 101028, 100608, 100746, 100683, 100669, 100255, 339708, 339707, 339709, 101614, 100430, 101047, 339706, 100544
) AND GEAR_CODE IN (12)


SELECT distinct licence_id, licence_type, licence_type_id, licence_subtype, gear_code, licence_subtype_id, species_code, species FROM MARFISSCI.marbycatch_lic WHERE LICENCE_ID IN ( 
102663, 102727, 102789, 102897, 142074, 142075, 142076, 142077, 142327, 142514, 318054, 322474, 700065
)