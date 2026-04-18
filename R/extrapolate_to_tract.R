

.fill_in_censored_count<-function(df){

  df%>%
    mutate(county_cases=if_else(is.na(county_cases),0, county_cases))

}



.extrapolate_to_tract<-function(df){


  #Initialize lists
  Merged_counties<-list()
  Merged_tracts<-list()
  year_list<-list()
  state_list<-list()
  variables_list<-list()
  us_tracts_population<-list()
  us_counties_population<-list()

  county_atlas_data<-df%>%
    rename(county_cases=cases)%>%
    rename(county_fips=fips)%>%
    rename(county_name=geography)%>%
    rename(county_population_atlas=population)%>%
    .fill_in_censored_count()%>%
    mutate(year=as.character(year)) %>%
    nest(data = -year) %>%
    tibble::deframe()


  #Define years and states for analysis
  year_list<-as.list(as.character(unique(df$year)))
  state_vector<-as.list(unique(substr(df$fips, 1, 2)))


  for (year_loop in year_list){
  variables_list[[year_loop]]<-tidycensus::load_variables(as.numeric(year_loop), "acs5")%>%
    filter(label=="Estimate!!Total", concept %in% c("TOTAL POPULATION", "Total Population"))


  us_tracts_population[[year_loop]]<-tidycensus::get_acs(
    geography = "tract",
    variables = variables_list[[year_loop]]$name,
    year = as.numeric(year_loop),
    survey = "acs5",
    state = state_vector
  )%>%
    mutate(county_fips = substr(GEOID, 1, 5))%>%
    #mutate(GEOID=as.numeric(GEOID))%>%
    #mutate(county_fips=as.numeric(county_fips))%>%
    select(-variable, -moe)%>%
    rename(tract_population_acs=estimate)%>%
    rename(tract_fips=GEOID)%>%
    rename(tract_name=NAME)


  us_counties_population[[year_loop]]<-get_acs(
    geography = "county",
    variables = variables_list[[year_loop]]$name,
    year = as.numeric(year_loop),
    survey = "acs5",
    state = state_vector
  )%>%
   # mutate(GEOID=as.numeric(GEOID))%>%
    select(-variable, -moe)%>%
    rename(county_population_acs=estimate)%>%
    rename(county_fips=GEOID)%>%
    select(-NAME)

   Merged_counties[[year_loop]]<-left_join(us_counties_population[[year_loop]],
                                             county_atlas_data[[year_loop]],
                                             by=join_by(county_fips))


   Merged_tracts[[year_loop]]<-left_join(us_tracts_population[[year_loop]],
                                         Merged_counties[[year_loop]],
                                         by=join_by(county_fips))%>%
    # select(-NAME.x, -NAME.y)%>%
     #Estimate census tract HIV population
     mutate(tract_cases=county_cases*tract_population_acs/county_population_acs)%>%
     #Estimate HIV-negative population in census tracts
     mutate(tract_noncases=tract_population_acs-tract_cases)
  }



  bind_rows(Merged_tracts, .id = "year")%>%
    mutate(year=as.numeric(year))



}



#df_extrapolated<-.extrapolate_to_tract(df)


#


#

#
# # Load tract variable labels for age/sex values B01001 from the ACS 5-year 2017 dataset
# vars[[year_loop]] <- load_variables(year_loop, "acs5", cache = TRUE) %>%
#   filter(str_detect(name, "B01001_"))%>%
#   mutate(label = str_replace_all(label, "Estimate!!Total!!", ""))%>%
#   mutate(label = str_replace_all(label, "!!", "_"))%>%
#   mutate(label = str_replace_all(label, " ", "_"))
#
#
#
#
# for (year_loop in year_list) {
#
#   #Join county geographical data with CDC data.
#
# }
