############################################################
#' A Process Inventory File Function
#'
#' This function allows you to process the 
#' @param Defaults to TRUE.
#' @keywords XXX
#' @export
#' @examples
#' XXX()
###########################################################

Extract_US_CND_InventoryFiles <- function(){

message("--------------------------*Process Inventory File*")

message("Loading necessary files, this may take some time")
#Loading seperate Canadian & US invetory Files & Subsetting them 
IF_USA =read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/9_PRICING_InventoryFiles/Inventory_File_US.csv", header = TRUE)
IF_CND =read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/9_PRICING_InventoryFiles/Inventory_File_CAD.csv", header = TRUE)

IF_USA = subset(IF_USA , select = c("vendor_name","vendor_code","mpn","price", "cost", "fedexable", "SpecialOrderSwt"))
names(IF_USA) = c("vendor_name","vendor_code","mpn","USA_price", "USA_cost", "USA_fedexable", "USA_SpecialOrder")

IF_CND = subset(IF_CND, select = c("VendorName","VenCode","ManufacturerPartNo","JobberPrice", "Cost", "Fedexable","SpecialOrderSwt"))
names(IF_CND) = c("vendor_name","vendor_code","mpn","CND_price", "CND_cost", "CND_fedexable", "CND_SpecialOrder")

Pooled_IF <- merge(IF_USA, IF_CND, by=c("vendor_name","vendor_code", "mpn"), all=TRUE) 



#Return the following data sets
return(Pooled_IF)

}

###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT


###########################################################
#Future Add Ons

