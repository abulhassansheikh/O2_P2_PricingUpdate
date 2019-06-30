############################################################
#' A Process Inventory File Data Function
#'
#' This function allows you to XXX
#' @param Defaults to TRUE.
#' @keywords XXX
#' @export
#' @examples
#' XXX()
###########################################################


message("--------------------------*Processing Inventory File Data*")

message("Subsetting Inventory File to use selected brands")
#Identify which vendor names to subset out and their related prefix data
IF_reference =read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/5_R_Brand_Reference_Files/InventoryFileData.csv", header = TRUE)

Pull_IF_reference_Data = merge(MainFileName, IF_reference, by=c("Brand_Folder_Name"), all=TRUE)
VenderNameData = subset(Pull_IF_reference_Data, Run_Update == 1)

SubsetIF = data.frame()

for(i in 1:nrow(VenderNameData)){

	BrandIFData = subset(Pooled_IF, vendor_name == as.character(VenderNameData$vendor_name[i]))	
	BrandIFData$internal_sku = paste(VenderNameData$Internal_SKU_Prefix[i],as.character(substring(BrandIFData$mpn, 2)), sep = "-", collapse = NULL)

	SubsetIF = rbind(SubsetIF , BrandIFData)

}


###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT


###########################################################
#Future Add Ons




