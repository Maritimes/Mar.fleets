#aggHSData
#unit2
test = xlsx::read.xlsx(file = "C:/Users/mcmahonm/Desktop/Summary for Fisheries Management/Observer Coverage/Unit 2 RF by Mesh Size.xlsx", sheetIndex="2018_NAKED", header=TRUE)
test=test[test$MESH_SIZE >=90 & test$MESH_SIZE<=115,]
names(test)
test2 <- stats::aggregate(
  x = list(cnt = test$RED_WT),
  by = list(field1 = test$VR_NUMBER_FISHING,
            field2 = test$YEAR,
            field2 = test$TRIP_ID,
            field2 = test$AREA
  ),
  sum
)
View(test2)
