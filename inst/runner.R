redunit3_2018v2 = fleet_redfish(unit=3, useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/",
                                quietly=T, debuggit=T, returnMARFIS = F, returnISDB = F)
# redunit3_2018vDog = fleet_redfish(unit=3, useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/",
#                                 quietly=T, debuggit=T, returnMARFIS = F, returnISDB =F)
# redunit3_2018vNoG = fleet_redfish(unit=3, useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/",
#                                   quietly=T, debuggit=T, returnMARFIS = F, returnISDB =F)


# length(unique(redunit3_2018vDog$fleet$FLEET$LICENCE_ID))
# length(unique(redunit3_2018vNoG$fleet$FLEET$LICENCE_ID))

####

# Load required files and functions -----------------------------------------------------------------------------------------------------------------------
R.utils::sourceDirectory("c:/git/Maritimes/Mar.fleets/R/", modifiedOnly=F)
load("C:/DFO-MPO/BycatchCourse/SessionFiles/SESSION_redfish_2002_2019.RData")
rm(list=(setdiff(ls(), "redunit3_2017")))

# Extract 2018 fleet --------------------------------------------------------------------------------------------------------------------------------------
redunit3_2017_new = fleet_redfish(unit=3, useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/",
                                  quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F)

# Do some comparisons -------------------------------------------------------------------------------------------------------------------------------------
length(unique(redunit3_2018$fleet$LICENCE_ID))             #62
length(unique(redunit3_2018v3_1$fleet$FLEET$LICENCE_ID))   #51

quick <-setdiff(redunit3_2017$fleet$LICENCE_ID,redunit3_2017_new$fleet$FLEET$LICENCE_ID)
redunit3_2018v3_1_db = fleet_redfish(unit=3, useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/",
quietly=T, debuggit=T, returnMARFIS = F, returnISDB = F, debugLics = quick)



# Halibut -------------------------------------------------------------------------------------------------------------------------------------------------


load("C:/DFO-MPO/BycatchCourse/SessionFiles/SESSION_halibut_2002_2020.RData")
rm(list=(setdiff(ls(), "halibut2018")))
R.utils::sourceDirectory("c:/git/Maritimes/Mar.fleets/R/", modifiedOnly=F)
halibut2018_new = fleet_halibut(useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/",
                          quietly=T, debuggit=T, returnMARFIS = F, returnISDB = F)

# quick <-setdiff(halibut2018_new$fleet$LICENCE_ID,halibut2018$fleet$FLEET$LICENCE_ID)
quick <-setdiff(halibut2018$fleet$LICENCE_ID,halibut2018_new$fleet$FLEET$LICENCE_ID)

halibut2018_new_all = fleet_halibut(useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/",
                                quietly=T, debuggit=T, returnMARFIS = T, returnISDB = T)
hal_45under <- c(100237,100259,100260,100281,100308,100314,100328,100335,100348,100386,100418,100440,100442,100461,100466,100468,100475,100479,100487,100495,100496,100497,100512,100521,100553,100566,100571,100724,100728,100755,100767,100781,100782,100807,100816,100824,100827,100832,100835,100862,100867,100878,100880,100889,100901,100912,100922,100925,100933,100934,100947,100952,100969,100975,100976,100987,100989,100991,101000,101009,101017,101018,101031,101035,101052,101076,101081,101107,101111,101112,101128,101136,101154,101156,101158,101170,101178,101188,101211,101212,101220,101237,101239,101243,101255,101265,101266,101268,101269,101279,101287,101290,101295,101299,101300,101309,101311,101325,101331,101332,101344,101348,101367,101371,101380,101383,101384,101396,101407,101411,101430,101460,101470,101479,101490,101499,101502,101518,101529,101539,101554,101566,101575,101582,101584,101587,101624,101630,101663,101664,101670,101693,101701,101703,101707,101731,101737,101742,101750,101753,101776,101780,101783,101784,101790,101800,101806,101808,101809,101812,101815,101818,101831,101834,101838,101861,101870,101871,101872,101936,101946,101947,101960,101970,101974,101981,101983,101990,102007,102012,102013,102014,102023,102025,102035,102037,102041,102046,102049,102051,102063,102067,102074,102081,102093,102094,102095,102103,102108,102120,102121,102124,102131,102137,102159,102165,102179,102193,102203,102205,102207,102209,102210,102230,102236,102238,102245,102252,102256,102263,102270,102296,102302,102315,102318,102322,102346,102350,102353,102370,102380,102387,102396,102401,102405,102407,102413,102453,102460,102466,102472,102477,102488,102492,102497,102500,102515,102518,102528,102530,103140,103284,103455,103463,103466,103472,103473,103474,103483,103500,103509,103520,103535,103541,103550,103569,102534,100262,100280,100290,100293,100315,100321,100323,100346,100349,100354,100372,100410,100424,100427,100441,100444,100450,100455,100464,100471,100493,100494,100501,100502,100508,100532,100556,100563,100565,100576,100587,100605,100625,100742,100745,100749,100753,100754,100757,100760,100762,100780,100783,100785,100791,100808,100812,100813,100815,100830,100846,100858,100873,100892,100899,100914,100918,100943,100954,100956,100974,100979,101014,101015,101020,101030,101039,101041,101042,101054,101056,101058,101071,101089,101099,101117,101121,101153,101166,101167,101168,101176,101189,101190,101194,101203,101214,101222,101224,101229,101230,101232,101240,101241,101250,101256,101267,101276,101281,101298,101306,101320,101326,101327,101339,101351,101353,101358,101366,101369,101377,101390,101398,101406,101433,101441,101451,101484,101506,101527,101530,101536,101542,101544,101545,101552,101556,101560,101565,101567,101571,101572,101574,101578,101580,101581,101606,101619,101623,101632,101636,101641,101649,101662,101665,
                 101671,101673,101679,101682,101684,101686,101689,101712,101722,101724,101726,101730,101736,101739,101755,101768,101770,101772,101777,101779,101791,101796,101802,101826,101848,101869,101879,101891,101893,101894,101905,101924,101933,101953,101961,101963,101965,101973,101997,101998,102001,102005,102008,102011,102017,102024,102027,102028,102047,102057,102060,102064,102096,102112,102113,102117,102128,102135,102136,102139,102143,102146,102152,102155,102162,102167,102219,102223,102224,102237,102242,102246,102253,102255,102261,102265,102285,102295,102309,102319,102320,102331,102348,102355,102366,102377,102421,102422,102441,102464,102470,102471,102473,102485,102498,102508,102516,102521,102538,102687,102746,102824,103031,103156,103397,103452,103454,103457,103470,103480,103487,103506,103517,103521,103525,103528,103529,103537,103558,103571,101885,102216,103540,102491,102172,100733,101825,100282,100285,100301,100307,100319,100371,100373,100376,100381,100393,100396,100399,100409,100446,100448,100454,100474,100478,100490,100520,100523,100524,100535,100538,100558,100564,100577,100580,100584,100592,100602,100626,100723,100738,100766,100771,100787,100810,100821,100839,100841,100843,100857,100869,100870,100875,100881,100883,100888,100904,100905,100906,100929,100958,100959,100962,100964,100971,100981,100999,101006,101013,101022,101026,101027,101036,101069,101077,101093,101095,101120,101140,101147,101149,101150,101152,101159,101165,101169,101196,101210,101223,101244,101246,101259,101261,101270,101282,101286,101288,101289,101303,101314,101337,101338,101342,101365,101368,101382,101389,101394,101400,101404,101409,101410,101414,101420,101427,101428,101432,101434,101437,101438,101454,101458,101466,101471,101474,101481,101483,101487,101495,101497,101507,101519,101526,101541,101562,101569,101573,101583,101586,101588,101592,101603,101609,101611,101628,101639,101646,101653,101672,101681,101685,101696,101699,101708,101713,101741,101758,101760,101766,101769,101788,101797,101799,101807,101817,101822,101823,101829,101835,101839,101842,101844,101846,101878,101884,101886,101926,101934,101942,101950,101956,101962,101971,101972,101975,101984,101993,102000,102042,102044,102052,102056,102058,102072,102073,102085,102088,102105,102119,102123,102129,102147,102170,102178,102184,102185,102196,102201,102211,102225,102227,102228,102231,102233,102234,102272,102280,102284,102294,102297,102299,102300,102304,102305,102307,102312,102338,102358,102368,102371,102382,102397,102415,102420,102428,102436,102438,102443,102447,102461,102468,102482,102487,102501,102511,102517,102519,102539,102904,103176,103179,
                 103448,103459,103467,103477,103502,103504,103505,103514,103518,103519,103530,103531,103532,103533,103534,103543,103565,103573,101485,101274,100554,101733,101341,101418,101233,101049,101852,100242,100256,100264,100297,100331,100332,100336,100345,100356,100359,100361,100369,100374,100375,100378,100379,100403,100411,100415,100423,100428,100439,100456,100457,100488,100489,100518,100534,100536,100543,100555,100561,100595,100596,100604,100612,100717,100732,100736,100741,100756,100773,100778,100784,100790,100795,100849,100860,100866,100868,100895,100910,100930,100936,100948,100978,100983,100992,101033,101043,101048,101060,101061,101101,101105,101114,101129,101141,101160,101163,101184,101198,101200,101202,101204,101206,101218,101231,101248,101258,101263,101293,101294,101308,101310,101323,101336,101345,101357,101364,101376,101392,101401,101416,101417,101435,101445,101453,101473,101475,101480,101486,101496,101503,101531,101532,101563,101570,101593,101596,101600,101605,101607,101610,101613,101629,101631,101635,101643,101644,101647,101650,101654,101661,101675,101676,101691,101694,101716,101746,101747,101748,101754,101773,101789,101813,101824,101843,101863,101866,101875,101876,101881,101882,101895,101896,101907,101911,101915,101922,101949,101958,101959,101967,101976,101980,101982,101987,101999,102004,102031,102043,102054,102068,102069,102076,102092,102107,102116,102118,102134,102149,102156,102166,102173,102176,102197,102213,102217,102221,102239,102243,102249,102283,102292,102298,102303,102316,102327,102334,102357,102363,102374,102376,102383,102395,102398,102400,102403,102423,102431,102442,102457,102462,102463,102474,102484,102489,102504,102506,102524,102527,102592,102808,102846,102898,103458,103462,103476,103490,103495,103496,103507,103516,103536,103548,103554,103556,103561,103563,101612,102406,100805,102411,101719,100243,100248,100261,100341,100343,100350,100358,100363,100364,100368,100388,100389,100394,100395,100404,100416,100417,100422,100431,100435,100452,100469,100472,100480,100481,100482,100491,100492,100525,100528,100529,100547,100552,100579,100582,100585,100589,100730,100734,100740,100744,100752,100788,100799,100820,100845,100850,
                 100852,100863,100864,100876,100879,100890,100911,100917,100926,100939,100951,100957,100963,100973,100985,100986,100996,101001,101007,101008,101021,101046,101065,101090,101109,101118,101125,101146,101157,101175,101187,101192,101199,101205,101227,101235,101249,101296,101316,101333,101340,101346,101347,101354,101355,101362,101373,101399,101402,101429,101431,101440,101443,101465,101467,101472,101477,101482,101489,101494,101498,101510,101511,101513,101520,101528,101533,101590,101598,101608,101637,101638,101640,101652,101655,101660,101687,101692,101702,101704,101705,101711,101721,101725,101729,101735,101744,101749,101756,101759,101761,101804,101814,101837,101854,101858,101862,101868,101873,101883,101890,101902,101910,101938,101940,101941,101951,102006,102021,102039,102055,102059,102071,102086,102089,102097,102100,102104,102125,102132,102138,102148,102163,102181,102186,102187,102192,102195,102200,102218,102220,102222,102259,102276,102286,102291,102306,102332,102342,102345,102352,102362,102364,102378,102381,102388,102392,102402,102417,102425,102426,102432,102435,102437,102439,102450,102479,102480,102490,102499,102563,102801,103380,103449,103464,103469,103471,103482,103488,103489,103493,103494,103510,103524,300005,100551,101315,101183,101185,100367,101045,100269,100286,100296,100310,100311,100320,100322,100324,100325,100334,100339,100370,100413,100421,100470,100473,100477,100483,100484,100498,100511,100516,100530,100531,100533,100537,100540,100557,100574,100575,100578,100593,100600,100691,100718,100751,100763,100764,100770,100796,100801,100817,100818,100822,100825,100828,100829,100833,100842,100854,100859,100865,100872,100877,100903,100909,100920,100937,100940,100942,100945,100953,100960,100965,100966,100980,100993,100995,101023,101044,101062,101067,101078,101110,101127,101139,101144,101148,101162,101171,101180,101191,101197,101201,101213,101217,101226,101228,101236,101251,101252,101253,101262,101273,101277,101278,101284,101285,101317,101318,101319,101321,101324,101328,101330,101359,101361,101372,101381,101387,101391,101395,101405,101412,101423,101442,101456,101463,101468,101476,101478,101491,101500,101514,101522,101538,101561,101579,
                 101585,101626,101648,101657,101667,101680,101690,101695,101700,101723,101745,101751,101752,101763,101774,101781,101782,101786,101794,101795,101798,101819,101821,101828,101832,101847,101850,101859,101865,101877,101898,101917,101948,101952,101954,101968,101969,101994,101996,102002,102010,102016,102018,102020,102030,102034,102040,102050,102066,102070,102080,102084,102091,102101,102102,102114,102130,102140,102145,102150,102151,102158,102160,102161,102189,102198,102212,102214,102215,102240,102248,102254,102266,102267,102275,102288,102293,102321,102324,102336,102339,102351,102356,102360,102361,102365,102372,102385,102399,102408,102410,102416,102430,102448,102456,102459,102465,102486,102496,102503,102510,102513,102514,102522,102525,102526,102532,102536,102713,103041,103305,103433,103451,103478,103481,103497,103498,103512,103526,103538,103544,103551,103555,103566,103572,305863,101678,100748,103446,101271,101408,100238,100240,100291,100292,100333,100351,100352,100360,100365,100401,100402,100407,100408,100412,100433,100434,100437,100486,100506,100510,100515,100519,100522,100526,100541,100542,100546,100570,100581,100597,100598,100601,100633,100737,100743,100761,100769,100786,100797,100804,100806,100809,100823,100831,100834,100836,100840,100861,100887,100900,100902,100916,100919,100924,100928,100935,100938,100982,100988,101004,101019,101025,101032,101040,101084,101096,101124,101143,101155,101174,101177,101193,101209,101221,101225,101245,101247,101254,101257,101260,101264,101272,101275,101291,101297,101301,101302,101307,101312,101322,101329,101352,101360,101363,101386,101393,101413,101415,101421,101422,101425,101426,101439,101444,101446,101449,101455,101459,101462,101464,101493,101505,101512,101517,101523,101525,101537,101543,101546,101547,101549,101551,101553,101557,101559,101577,101595,101604,101618,101620,101627,101656,101658,101674,101688,101697,101698,101718,101720,101727,101732,101734,101740,101743,101764,101765,101767,
                 101775,101793,101805,101810,101811,101840,101853,101874,101887,101888,101889,101892,101899,101901,101912,101914,101921,101923,101928,101930,101931,101932,101937,101939,101944,101957,101966,101977,101979,101992,101995,102033,102036,102079,102082,102083,102090,102109,102110,102111,102133,102141,102154,102157,102164,102171,102180,102190,102208,102226,102229,102235,102251,102262,102264,102274,102279,102282,102289,102333,102335,102337,102341,102359,102367,102369,102373,102384,102389,102390,102394,102409,102424,102427,102445,102476,102523,102646,102899,103033,103348,103405,103444,103447,103475,103479,103492,103503,103549,103559,101677,101469,102343,100750,100300,100302,100304,100309,100313,100316,100340,100344,100347,100355,100380,100383,100384,100385,100397,100400,100429,100432,100438,100447,100463,100465,100500,100505,100507,100509,100514,100539,100550,100559,100560,100562,100586,100590,100719,100722,100731,100735,100758,100775,100779,100793,100814,100826,100844,100851,100885,100886,100908,100921,100923,100927,100932,100944,100946,100949,100961,100970,100990,100997,101002,101016,101024,101034,101037,101053,101068,101075,101086,101102,101115,101123,101138,101145,101195,101215,101216,101234,101238,101283,101292,101305,101313,101334,101335,101343,101349,101350,101370,101374,101375,101378,101379,101385,101388,101397,101403,101419,101424,101436,101448,101450,101452,101457,101461,101488,101501,101509,101515,101516,101524,101534,101548,101558,101564,101576,101589,101591,101594,101617,101633,101645,101659,101669,101683,101706,101709,101710,101715,101717,101738,101757,101771,101778,101785,101787,101792,101801,101803,101816,101820,101830,101836,101841,101851,101856,101857,101897,101900,101904,101908,101909,101913,101916,101918,101919,101920,101925,101927,101943,101955,101978,102003,102009,102015,102019,102022,102026,102038,102053,102061,102062,102075,102077,102078,102099,102106,102115,102122,102127,102142,102144,102168,102174,102177,102182,102183,102191,102194,102202,102204,102244,102247,102257,102258,102268,102269,102271,102273,102290,102301,102310,102311,102313,102314,102325,102328,102330,102340,102344,102347,102354,102386,102391,102393,102412,102414,102418,102429,102434,102444,102452,102478,102481,102483,102495,102502,102505,102507,102509,102531,102535,102736,102839,102909,102953,103243,103450,103456,103501,103508,103527,103545,103557,103567,102029,100567,100357,102232,102329,101356,100931,100317)
  hal_45bigger <- c(103553,103486,100712,102126,100630,100615,100690,100715,100703,100642,100627,100283,100294,101011,101010,100710,100684,100647,101082,100693,100676,100645,100641,103485,100682,100251,100688,100685,100639,100606,100705,100623,100611,103552,100288,100252,101012,100663,101447,102281,100635,100634,100631,100628,100622,100620,100245,100244,100706,100651,100619,100607,101074,100704,101103,101080,101057)

  mike = halibut2018$marf$MARF_TRIPS[,c("LICENCE_ID", "TRIP_ID_MARF")]
  Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = "c:/DFO-MPO/BycatchCourse/wrangledData/", tables = c("MARBYCATCH_LIC"))
  MARBYCATCH_LIC = MARBYCATCH_LIC[MARBYCATCH_LIC$LICENCE_ID %in% mike$LICENCE_ID,]
  MARBYCATCH_LIC<-MARBYCATCH_LIC[MARBYCATCH_LIC$GEAR_CODE ==51,]
  MARBYCATCH_LIC<-MARBYCATCH_LIC[MARBYCATCH_LIC$SPECIES_CODE ==199,]
  MARBYCATCH_LIC <- unique(MARBYCATCH_LIC[,c("LICENCE_ID","LICENCE_TYPE_ID","LICENCE_SUBTYPE_ID", "GEAR_CODE", "SPECIES_CODE", "LICENCE_TYPE", "LICENCE_SUBTYPE", "GEAR", "SPECIES")])
mike <- merge(mike, MARBYCATCH_LIC, all.x = T)



pollock_2015_East_Mob_Sm_HS = fleet_pollock(component="eastern", mesh = "SMALL", type= "MOBILE", useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
pollock_2016_East_Mob_Sm_HS = fleet_pollock(component="eastern", mesh = "SMALL", type= "MOBILE", useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
pollock_2017_East_Mob_Sm_HS = fleet_pollock(component="eastern", mesh = "SMALL", type= "MOBILE", useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
pollock_2018_East_Mob_Sm_HS = fleet_pollock(component="eastern", mesh = "SMALL", type= "MOBILE", useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
beepr::beep(sound = 2)
pollock_2015_West_Mob_Lg_HS = fleet_pollock(component="western", mesh = "LARGE", type= "MOBILE", useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
pollock_2016_West_Mob_Lg_HS = fleet_pollock(component="western", mesh = "LARGE", type= "MOBILE", useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
pollock_2017_West_Mob_Lg_HS = fleet_pollock(component="western", mesh = "LARGE", type= "MOBILE", useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
pollock_2018_West_Mob_Lg_HS = fleet_pollock(component="western", mesh = "LARGE", type= "MOBILE", useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
beepr::beep(sound = 2)
pollock_2015_East_Mob_Lg_HS = fleet_pollock(component="eastern", mesh = "LARGE", type= "MOBILE", useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
pollock_2016_East_Mob_Lg_HS = fleet_pollock(component="eastern", mesh = "LARGE", type= "MOBILE", useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
pollock_2017_East_Mob_Lg_HS = fleet_pollock(component="eastern", mesh = "LARGE", type= "MOBILE", useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
pollock_2018_East_Mob_Lg_HS = fleet_pollock(component="eastern", mesh = "LARGE", type= "MOBILE", useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=T, returnMARFIS = T, returnISDB = F, HS=T)
beepr::beep(sound = 2)

pollock_2015_East_Fix_Lg_HS = fleet_pollock(component="eastern", mesh = "LARGE", type= "FIXED", useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2015_East_Fix_Lg_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2015_East_Fix_Lg_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2016_East_Fix_Lg_HS = fleet_pollock(component="eastern", mesh = "LARGE", type= "FIXED", useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2016_East_Fix_Lg_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2016_East_Fix_Lg_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2017_East_Fix_Lg_HS = fleet_pollock(component="eastern", mesh = "LARGE", type= "FIXED", useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2017_East_Fix_Lg_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2017_East_Fix_Lg_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2018_East_Fix_Lg_HS = fleet_pollock(component="eastern", mesh = "LARGE", type= "FIXED", useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2018_East_Fix_Lg_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2018_East_Fix_Lg_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2015_west_Fix_Lg_HS = fleet_pollock(component="western", mesh = "LARGE", type= "FIXED", useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2015_west_Fix_Lg_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2015_west_Fix_Lg_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2016_west_Fix_Lg_HS = fleet_pollock(component="western", mesh = "LARGE", type= "FIXED", useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2016_west_Fix_Lg_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2016_west_Fix_Lg_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2017_west_Fix_Lg_HS = fleet_pollock(component="western", mesh = "LARGE", type= "FIXED", useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2017_west_Fix_Lg_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2017_west_Fix_Lg_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2018_west_Fix_Lg_HS = fleet_pollock(component="western", mesh = "LARGE", type= "FIXED", useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2018_west_Fix_Lg_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2018_west_Fix_Lg_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
##
pollock_2015_East_Fix_sm_HS = fleet_pollock(component="eastern", mesh = "SMALL", type= "FIXED", useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2015_East_Fix_sm_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2015_East_Fix_sm_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2016_East_Fix_sm_HS = fleet_pollock(component="eastern", mesh = "SMALL", type= "FIXED", useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2016_East_Fix_sm_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2016_East_Fix_sm_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2017_East_Fix_sm_HS = fleet_pollock(component="eastern", mesh = "SMALL", type= "FIXED", useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2015_East_Fix_Lg_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2015_East_Fix_Lg_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2018_East_Fix_sm_HS = fleet_pollock(component="eastern", mesh = "SMALL", type= "FIXED", useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2018_East_Fix_sm_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2018_East_Fix_sm_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2015_west_Fix_sm_HS = fleet_pollock(component="western", mesh = "SMALL", type= "FIXED", useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2015_west_Fix_sm_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2015_west_Fix_sm_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2016_west_Fix_sm_HS = fleet_pollock(component="western", mesh = "SMALL", type= "FIXED", useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2016_west_Fix_sm_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2016_west_Fix_sm_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2017_west_Fix_sm_HS = fleet_pollock(component="western", mesh = "SMALL", type= "FIXED", useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2017_west_Fix_sm_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2017_west_Fix_sm_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

pollock_2018_west_Fix_Lg_HS = fleet_pollock(component="western", mesh = "SMALL", type= "FIXED", useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(pollock_2018_west_Fix_Lg_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(pollock_2018_west_Fix_Lg_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)


wf_2015_HS = fleet_winterflounder(useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(wf_2015_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(wf_2015_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
wf_2016_HS = fleet_winterflounder(useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(wf_2016_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(wf_2016_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
wf_2017_HS = fleet_winterflounder(useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(wf_2017_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(wf_2017_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
wf_2018_HS = fleet_winterflounder(useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(wf_2018_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(wf_2018_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)


shake_2015_HS = fleet_silverhake(useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2015_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2015_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
shake_2016_HS = fleet_silverhake(useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2016_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2016_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
shake_2017_HS = fleet_silverhake(useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2017_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2017_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
shake_2018_HS = fleet_silverhake(useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2018_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2018_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)


shake_2015_sm_HS = fleet_silverhake(useLocal=T, mesh = "small", year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2015_sm_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2015_sm_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
shake_2016_sm_HS = fleet_silverhake(useLocal=T, mesh = "small", year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2016_sm_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2016_sm_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
shake_2017_sm_HS = fleet_silverhake(useLocal=T, mesh = "small", year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2017_sm_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2017_sm_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
shake_2018_sm_HS = fleet_silverhake(useLocal=T, mesh = "small", year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2018_sm_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2018_sm_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

shake_2015_HS = fleet_silverhake(useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2015_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2015_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
shake_2016_HS = fleet_silverhake(useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2016_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2016_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
shake_2017_HS = fleet_silverhake(useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2017_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2017_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
shake_2018_HS = fleet_silverhake(useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(shake_2018_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(shake_2018_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)


haddock_2015_mob_4X5Y_HS = fleet_haddock(type = "MOBILE", area = "4X5Y", useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2015_mob_4X5Y_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2015_mob_4X5Y_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
haddock_2016_mob_4X5Y_HS = fleet_haddock(type = "MOBILE", area = "4X5Y", useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2016_mob_4X5Y_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2016_mob_4X5Y_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
haddock_2017_mob_4X5Y_HS = fleet_haddock(type = "MOBILE", area = "4X5Y", useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2017_mob_4X5Y_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2017_mob_4X5Y_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
haddock_2018_mob_4X5Y_HS = fleet_haddock(type = "MOBILE", area = "4X5Y", useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2018_mob_4X5Y_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2018_mob_4X5Y_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

haddock_2015_mob_5ZJM_HS = fleet_haddock(type = "MOBILE", area = "5ZJM", useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2015_mob_5ZJM_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2015_mob_5ZJM_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
haddock_2016_mob_5ZJM_HS = fleet_haddock(type = "MOBILE", area = "5ZJM", useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2016_mob_5ZJM_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2016_mob_5ZJM_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
haddock_2017_mob_5ZJM_HS = fleet_haddock(type = "MOBILE", area = "5ZJM", useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2017_mob_5ZJM_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2017_mob_5ZJM_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
haddock_2018_mob_5ZJM_HS = fleet_haddock(type = "MOBILE", area = "5ZJM", useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2018_mob_5ZJM_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2018_mob_5ZJM_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)


haddock_2015_fix_5ZJM_HS = fleet_haddock(type = "FIXED", area = "5ZJM", useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2015_fix_5ZJM_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2015_fix_5ZJM_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
haddock_2016_fix_5ZJM_HS = fleet_haddock(type = "FIXED", area = "5ZJM", useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2016_fix_5ZJM_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2016_fix_5ZJM_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
haddock_2017_fix_5ZJM_HS = fleet_haddock(type = "FIXED", area = "5ZJM", useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2017_fix_5ZJM_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2017_fix_5ZJM_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
haddock_2018_fix_5ZJM_HS = fleet_haddock(type = "FIXED", area = "5ZJM", useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(haddock_2018_fix_5ZJM_HS$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(haddock_2018_fix_5ZJM_HS$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)




clam_2015 = fleet_surfclam(useLocal=T, year = "2015", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(clam_2015$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(clam_2015$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
clam_2016 = fleet_surfclam(useLocal=T, year = "2016", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(clam_2016$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(clam_2016$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
clam_2017 = fleet_surfclam(useLocal=T, year = "2017", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(clam_2017$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(clam_2017$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)
clam_2018 = fleet_surfclam(useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/", quietly=T, debuggit=F, returnMARFIS = T, returnISDB = F, HS=T)
length(unique(clam_2018$marf$MARF_TRIPS$TRIP_ID_MARF))
sum(clam_2018$marf$MARF_TRIPS$RND_WEIGHT_KGS)/1000
beepr::beep(sound = 2)

source('C:/git/Maritimes/Mar.utils/R/changeDetector.R', echo=TRUE)
source('C:/git/Maritimes/Mar.utils/R/updateExpected.R', echo=TRUE)

redunit3_2018v2 = fleet_redfish(unit=3, useLocal=T, year = "2018", data.dir="c:/DFO-MPO/BycatchCourse/wrangledData/",
quietly=T, debuggit=T, returnMARFIS = F, returnISDB =F, debugLics=c(100739,100599,100616,102048,101116,102241,100583,102087,100665,100681,100299))
