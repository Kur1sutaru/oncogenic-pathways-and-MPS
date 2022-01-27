# Match the gene and drug list with the cmap data

df4 <- merge( cmapdata, uniquecompounsandgenes, by.x = "Target", by.y = "names" )
write.csv(df3, "cmapgenesamygdala.csv")
