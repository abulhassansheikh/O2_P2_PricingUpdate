############################################################
#' A Merge Pricing Data Function
#'
#' This function allows you to XXX
#' @param Defaults to TRUE.
#' @keywords XXX
#' @export
#' @examples
#' XXX()
###########################################################


message("--------------------------*Merging Inventory File & Main Sheet Data*")


MergedPricingData_IF = merge(PooledMainData , SubsetIF , by=c("internal_sku"), all=TRUE)

CND_MAP =data.frame(read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/9a_PRCING_MAP_File/Formated_MAP_CND.csv", header = TRUE))
USA_MAP =data.frame(read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/9a_PRCING_MAP_File/Formated_MAP_USA.csv", header = TRUE))

MergedPricingData_CAN = merge(MergedPricingData_IF, CND_MAP, by=c("internal_sku"), all=TRUE)
MergedPricingData_USA = merge(MergedPricingData_CAN, USA_MAP, by=c("internal_sku"), all=TRUE)


MergedPricingData = subset(MergedPricingData_USA, sku != "", select= c(vendor_name, vendor_code, internal_sku, sku, attribute_set, 
                                                                       USA_fedexable, USA_SpecialOrder, usa_price, usa_retail_price, usa_cost, 
                                                                       USA_cost, usa_jobber_price, USA_price, na_usa_shipping, na_exchange_rate, 
                                                                       CND_fedexable, CND_SpecialOrder, ca_price, ca_retail_price, ca_cost, CND_cost, 
                                                                       ca_jobber_price, CND_price, na_ca_shipping, ca_MAP_price, usa_MAP_price))

MergedPricingData$ca_MAP_price <-  as.numeric(as.character(MergedPricingData$ca_MAP_price))
MergedPricingData$usa_MAP_price <-  as.numeric(as.character(MergedPricingData$usa_MAP_price))

MergedPricingData  = data.frame(MergedPricingData)

MergedPricingData$Current_Exchange_Rate <- 0
MergedPricingData$REF_Discount <- 0
MergedPricingData$CURR_Discount <- 0
MergedPricingData$USED_Discount <- 0
MergedPricingData$NEW_ca_cost <- 0

MergedPricingData$CURR_Transaction_Fee <- 0
MergedPricingData$CURR_Actual_Cost <- 0
MergedPricingData$CURR_CND_Margin <- 0
MergedPricingData$USED_CND_Margin <- 0
MergedPricingData$NEW_ca_price <- 0
MergedPricingData$NEW_ca_retail <- 0
MergedPricingData$DECISION <- "NONE"


MergedPricingData$REF_Discount_usa <- 0
MergedPricingData$CURR_Discount_usa <- 0
MergedPricingData$USED_Discount_usa <- 0
MergedPricingData$NEW_usa_cost <- 0

MergedPricingData$CURR_Transaction_Fee_usa <- 0
MergedPricingData$CURR_Actual_Cost_usa <- 0
MergedPricingData$CURR_USA_Margin <- 0
MergedPricingData$USED_USA_Margin <- 0
MergedPricingData$NEW_usa_price <- 0
MergedPricingData$NEW_usa_retail <- 0
MergedPricingData$DECISION_usa <- "NONE"



###########################################################
###########################################################
###########################################################
###########################################################
#TROUBLESHOOT


###########################################################
#Future Add Ons




