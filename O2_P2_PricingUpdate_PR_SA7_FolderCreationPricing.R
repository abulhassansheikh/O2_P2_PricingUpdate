############################################################
#' A Folder/File creation function
#'
#' This function allows you to create a set of folders and files depending on the brand name what is required from the update.
#' @param Defaults to TRUE.
#' @keywords File/Folder Creation
#' @export
#' @examples
#' #Folder_File_Creation(BrandName="Test", Final1_Test0=0)
###########################################################

message("--------------------------*File Devision*")

Canada_ER = subset(MergedPricingData, select= c(vendor_code, internal_sku, sku, USA_fedexable,
                                                                       USA_SpecialOrder, usa_MAP_price, usa_price, usa_retail_price,    
                                                                       usa_cost, USA_cost, usa_jobber_price, USA_price, na_usa_shipping,    
                                                                       na_exchange_rate, Current_Exchange_Rate, CND_fedexable, CND_SpecialOrder,    
                                                                       ca_MAP_price, ca_price, ca_retail_price, ca_cost, CND_cost, NEW_ca_cost,    
                                                                       ca_jobber_price, CND_price, na_ca_shipping, CURR_Transaction_Fee,    
                                                                       CURR_Actual_Cost, CURR_CND_Margin, REF_Discount, CURR_Discount, USED_Discount))


USA_ER= subset(MergedPricingData, select= c(vendor_name, vendor_code, internal_sku, sku, USA_fedexable, 
                                                                       USA_SpecialOrder, usa_MAP_price, usa_price, usa_retail_price,    
                                                                       usa_cost, USA_cost, NEW_usa_cost, usa_jobber_price, USA_price,    
                                                                       na_usa_shipping, CURR_Transaction_Fee_usa, CURR_Actual_Cost_usa,    
                                                                       CURR_USA_Margin, REF_Discount, CURR_Discount_usa, USED_Discount_usa))

Auto_CAN_ER= subset(MergedPricingData, select= c(internal_sku, CURR_CND_Margin, USED_CND_Margin, NEW_ca_price, NEW_ca_retail, DECISION))


Auto_USA_ER= subset(MergedPricingData, select= c(internal_sku, CURR_USA_Margin, USED_USA_Margin, NEW_usa_price, NEW_usa_retail, DECISION_usa))


message("--------------------------*Create Worksheet*")

#Load openxlsx package to save excel file with tabs
library("openxlsx")

##Create the workbook for excel and add the following tabs: Canada_ER, USA_ER, Auto_CAN_ER, Auto_USA_ER
CompiledSheet <- createWorkbook()

addWorksheet(CompiledSheet , "Canada_ER")
writeData(CompiledSheet , sheet = "Canada_ER", x = Canada_ER)

addWorksheet(CompiledSheet , "USA_ER")
writeData(CompiledSheet , sheet = "USA_ER", x = USA_ER)

addWorksheet(CompiledSheet , "Auto_CAN_ER")
writeData(CompiledSheet , sheet = "Auto_CAN_ER", x = Auto_CAN_ER)

addWorksheet(CompiledSheet , "Auto_USA_ER")
writeData(CompiledSheet , sheet = "Auto_USA_ER", x = Auto_USA_ER)



message("--------------------------*Starting Exchange Rate Update*")

#Record Time of creation
##Create Folder Name with date and Time
ERfileName_canada = paste("CANADA_KERU",gsub(":", "-", as.character(Sys.time())), sep = "-", collapse = NULL)
FileName_canada = paste(ERfileName_canada, "csv", sep = ".", collapse = NULL)

ERfileName_USA = paste("USA_KERU",gsub(":", "-", as.character(Sys.time())), sep = "-", collapse = NULL)
FileName_USA = paste(ERfileName_USA, "csv", sep = ".", collapse = NULL)

message("Files will be named: ", FileName_canada)
message("Files will be named: ", FileName_USA )

message("--------------------------*Folder_File_Creation*")

#Check if FileOutput is 1
if(Final1_Test0== 1){

	BackUpFolder_CAN = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/1a_Input_Folder_BACKUP", FileName_canada, sep = "/", collapse = NULL)
	#BackUpFolder_USA = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/1a_Input_Folder_BACKUP", FileName_USA, sep = "/", collapse = NULL)

	#write.csv(ExchangeRateFinal_Canada, file = BackUpFolder_CAN, na="")
	#write.csv(ExchangeRateFinal_USA, file = BackUpFolder_USA, na="")

	saveWorkbook(CompiledSheet, BackUpFolder_CAN, overwrite = FALSE)

	OutputFolder_CAN = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/2_Output_Folder", FileName_canada, sep = "/", collapse = NULL)
	#OutputFolder_USA = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/2_Output_Folder", FileName_USA, sep = "/", collapse = NULL)

	#write.csv(ExchangeRateFinal_Canada, file = OutputFolder_CAN, na="")
	#write.csv(ExchangeRateFinal_USA, file = OutputFolder_USA, na="")

	saveWorkbook(CompiledSheet, OutputFolder_CAN, overwrite = FALSE)


} else{

	TEMPFolder_CAN = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/1b_Input_Folder_TEMP", FileName_canada, sep = "/", collapse = NULL)
	#TEMPFolder_USA = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/1b_Input_Folder_TEMP", FileName_USA, sep = "/", collapse = NULL)

	#write.csv(ExchangeRateFinal_Canada, file = TEMPFolder_CAN, na="")
	#write.csv(ExchangeRateFinal_USA, file = TEMPFolder_USA, na="")

	saveWorkbook(CompiledSheet, TEMPFolder_CAN, overwrite = FALSE)

} 

message("File Outputted")



############################################
############################################
############################################
############################################
#TROUBLESHOOT

ExchangeRateFinal = data.frame()

Final1_Test0 = 0

###########################################################
#Future Add Ons





#Create File
message("--------------------------*Folder_File_Creation*")


#Check if FileOutput is 1
if(Final1_Test0== 1){

	BackUpFolder_CAN = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/1a_Input_Folder_BACKUP", FileName_canada, sep = "/", collapse = NULL)
	BackUpFolder_USA = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/1a_Input_Folder_BACKUP", FileName_USA, sep = "/", collapse = NULL)

	write.csv(ExchangeRateFinal_Canada, file = BackUpFolder_CAN, na="")
	write.csv(ExchangeRateFinal_USA, file = BackUpFolder_USA, na="")

	OutputFolder_CAN = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/2_Output_Folder", FileName_canada, sep = "/", collapse = NULL)
	OutputFolder_USA = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/2_Output_Folder", FileName_USA, sep = "/", collapse = NULL)

	write.csv(ExchangeRateFinal_Canada, file = OutputFolder_CAN, na="")
	write.csv(ExchangeRateFinal_USA, file = OutputFolder_USA, na="")

} else{

	TEMPFolder_CAN = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/1b_Input_Folder_TEMP", FileName_canada, sep = "/", collapse = NULL)
	TEMPFolder_USA = paste("//TDOTFS01/Group/Data Team/Brand_Update_Location/1b_Input_Folder_TEMP", FileName_USA, sep = "/", collapse = NULL)

	write.csv(ExchangeRateFinal_Canada, file = TEMPFolder_CAN, na="")
	write.csv(ExchangeRateFinal_USA, file = TEMPFolder_USA, na="")
} 

































