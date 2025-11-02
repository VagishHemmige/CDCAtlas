#' Internal: temporary dummy payload builder
#' @keywords internal
.build_payload <- function(
    disease,
    geography,
    year,
    stratify_by = NULL
) {

#Initial paylod
id<-NULL

#Add each disease to payload
if ("chlamydia" %in% disease) {id <- c(id, 206)}
if ("gonorrhea" %in% disease) {id <- c(id, 207)}
if ("adult syphilis" %in% disease) {id <- c(id, 208, 213, 290)}
if ("congenital syphilis" %in% disease) {id <- c(id, 214)}
if ("tuberculosis" %in% disease) {id <- c(id, 214)}
if ("hiv" %in% disease) {id <- c(id, 203, 205)}
if ("estimate" %in% disease) {id <- c(id, 285:287)}

#Add geography to payload.  Only allows one geography
#Of note, there is a specific marker () to group by region; otherwise,
id<- switch (geography,
"national" =  c(id,5000),  #500 is national
"region"="",
"state"=c(id,.state_lookup$geo_id),
"county"= c(id, .counties_lookup$geo_id),
"msa"=c(id,.msa_lookup$geo_id)
)

#Add year to payload.  The year id is the current year, subtracting 1478
id<-c(id, year-1478)

#Add stratification variables to id
#Add age stratification variables
if ("age" %in% stratify_by & !("hiv" %in% disease)) {id<-c(id,654,657,660,661,663,664,666,668,669,672,673)}
if (!("age" %in% stratify_by) & !("hiv" %in% disease | "estimate" %in% disease)){id<-c(id, 651)}
if ("age" %in% stratify_by & ("hiv" %in% disease)) {id<-c(id, 656,662,665,668,669,672)}
if (!("age" %in% stratify_by) & ("hiv" %in% disease | "estimate" %in% disease)){id<-c(id, 650)}

#Add race stratification variables
if ("race" %in% stratify_by) {id<-c(id,552,553,554,555,562,556,559,557)}
if (!("race" %in% stratify_by)){id<-c(id, 551)}


#Add sex stratification variables
if ("sex" %in% stratify_by) {id<-c(id,602, 603)}
if (!("sex" %in% stratify_by)){id<-c(id, 601)}

#Transmission category--defaults to all for now
id<-c(id,801)

  list(
    VariableIDs = paste(id,
      collapse = ","
    ))
}
