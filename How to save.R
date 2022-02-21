#save1.0
sink("file_name.csv or .txt")
#everything you would like to print on a file
sink(NULL)


#save2.0
list <- list(1, 2, 3, 4, 5, 6, 7)
xx <- lapply(list, function(i){ 
  as.data.frame(HSD_Tr_tr[[i]][["groups"]])
})
write.table(xx, "xxx_test.csv", quote = FALSE, sep = ";")