localCheck<-function(){
  get_data_custom(schema = "MARFISSCI", data.dir = data.dir,
                  tables = c("GEARS",
                             "LICENCE_SUBTYPES",
                             "LICENCES",
                             "LOG_EFRT_ENTRD_DETS",
                             "LOG_EFRT_STD_INFO",
                             "MON_DOCS",
                             "NAFO_UNIT_AREAS",
                             "PRO_SPC_INFO"
                             ), quiet = T)



}
