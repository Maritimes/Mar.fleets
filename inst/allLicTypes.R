Mar.utils::get_data_tables(schema = "MARFISSCI", data.dir = "c:/git/wrangledData/",
                           tables = c("LICENCES", "LICENCE_TYPES", "LICENCE_SUBTYPES", "LICENCE_GEARS", "SPECIES", "GEARS"))
#, "PRO_SPC_INFO"))
# PRO_SPC_INFO = PRO_SPC_INFO[PRO_SPC_INFO$YEAR ==2017,]

#Cleaning
LICENCES <- LICENCES[,c("LICENCE_ID","LICENCE_TYPE_ID", "LICENCE_SUBTYPE_ID", "SPECIES_CODE", "START_DATE_TIME", "ORIGIN_DATE", "EXPIRY_DATE")]
LICENCES$L_START_DATE <- as.Date(LICENCES$START_DATE_TIME)
LICENCES$L_ORIGIN_DATE <- as.Date(LICENCES$ORIGIN_DATE)
LICENCES$L_EXPIRY_DATE <- as.Date(LICENCES$EXPIRY_DATE)
LICENCES$START_DATE_TIME<- LICENCES$ORIGIN_DATE <- LICENCES$EXPIRY_DATE <- NULL

LICENCE_GEARS <- LICENCE_GEARS[,c("LICENCE_ID", "GEAR_CODE", "START_DATE", "END_DATE")]
LICENCE_GEARS$LG_START_DATE <- as.Date(LICENCE_GEARS$START_DATE)
LICENCE_GEARS$LG_END_DATE <- as.Date(LICENCE_GEARS$END_DATE)
LICENCE_GEARS$START_DATE <- LICENCE_GEARS$END_DATE <- NULL
LICENCE_GEARS <- unique(LICENCE_GEARS)

GEARS <- GEARS[,c("GEAR_CODE", "GEAR_TYPE_ID", "DESC_ENG")]
colnames(GEARS)[colnames(GEARS)=="DESC_ENG"] <- "GEAR"

LICENCE_TYPES <- unique(LICENCE_TYPES[,c("LICENCE_TYPE_ID","DESC_ENG","VESSEL_BASED")])
colnames(LICENCE_TYPES)[colnames(LICENCE_TYPES)=="DESC_ENG"] <- "TYPE"

LICENCE_SUBTYPES <- unique(LICENCE_SUBTYPES[,c("LICENCE_SUBTYPE_ID", "DESC_ENG")])
colnames(LICENCE_SUBTYPES)[colnames(LICENCE_SUBTYPES)=="DESC_ENG"] <- "SUBTYPE"

SPECIES <- SPECIES[,c("SPECIES_CODE","DESC_ENG")]
colnames(SPECIES)[colnames(SPECIES)=="DESC_ENG"] <- "SPECIES"


# LICENCES <- LICENCES[which(LICENCES$EXPIRY_DATE >= as.Date('2017-12-31') & LICENCES$START_DATE_TIME <= as.Date('2017-01-01')) ,]
# LICENCE_GEARS <- LICENCE_GEARS[which(LICENCE_GEARS$END_DATE >= as.Date('2017-12-31') & LICENCE_GEARS$START_DATE <= as.Date('2017-01-01')) ,]
# LICENCES <- LICENCES[LICENCES$LICENCE_ID %in% PRO_SPC_INFO$LICENCE_ID,]

data <- merge(LICENCES, LICENCE_GEARS)
data <- merge(data, LICENCE_TYPES, all.x=T)
data <- merge(data, LICENCE_SUBTYPES, all.x =T)
data <- merge(data, GEARS, all.x =T)
data <- merge(data, SPECIES, all.x =T)
data[is.na(data)] <- -99

# rm(list=c("LICENCE_GEARS", "LICENCE_SUBTYPES", "LICENCE_TYPES", "LICENCES", "GEARS"))

 # data<- data[!data$LICENCE_TYPE_ID %in% c(4,6,8,9,16,18,19,21,22,26),] #5,14 are "relay"
 # data<- data[!data$LICENCE_SUBTYPE_ID %in% c(10,11,12,13,37,40,42,46),]
allTypes <- unique(data[,c("TYPE", "LICENCE_TYPE_ID", "SUBTYPE", "LICENCE_SUBTYPE_ID","GEAR","GEAR_CODE","SPECIES")])
#allTypes <- 	allTypes[with(allTypes,order(SPECIES, GEAR, TYPE, SUBTYPE)),]
SPECIES = SPECIES[SPECIES$SPECIES_CODE %in% data$SPECIES_CODE,]
test= data[data$SPECIES_CODE ==259,]

thisSpec <- aggregate(
  x = list(cnt = test$LICENCE_ID),
  by = list(SPECIES_CODE= test$SPECIES_CODE,
            GEAR_CODE = test$GEAR_CODE,
            LICENCE_TYPE_ID = test$LICENCE_TYPE_ID,
            LICENCE_SUBTYPE_ID = test$LICENCE_SUBTYPE_ID,
            GEAR = test$GEAR,
            TYPE = test$TYPE,
            SUBTYPE = test$SUBTYPE
  ),
  length
)
thisSpec <- thisSpec[with(thisSpec,order(cnt, GEAR_CODE, LICENCE_TYPE_ID, LICENCE_SUBTYPE_ID)),
                     c("SPECIES_CODE", "GEAR_CODE", "LICENCE_TYPE_ID","LICENCE_SUBTYPE_ID", "GEAR","TYPE", "SUBTYPE","cnt")]
# print(thisSpec[thisSpec$LICENCE_SUBTYPE_ID %in% c(24),])
print(thisSpec)
#SPEC/GEAR/TYPE/SUBTYPE
#swordfish - 251/51,81/0,1,11/-99
#swordfish (LL) - 251/51/0,11/-99
#swordfish (harpoon) - 251/81/0,1,11/-99

#urchin - 650/71,76/1,11/-99
#urchin (dive) - 650/76/1,11/-99
#urchin (drag) - 650/71/1,11/-99

#seacuc - 619/71/1,10/-99

#shrimp (fixed) - 702/62/1,11/-99
#shrimp(mobile) - 702/19/0,1,11/-99

#offshoreScallop - 612/71/10/15
#inshorescallop  - 612/71/0,1,11/16,17,18,19
#inshorescallop (INSHORE) 612/71/11/19
#inshorescallop (FULL BAY) 612/71/0,11/18
#inshorescallop (MID BAY) 612/71/1,11/16
#inshorescallop (UPPER BAY) 612/71/1,11/17
