Merged_counties<-list()
Merged_tracts<-list()

us_tracts_population<-list()
us_counties_population<-list()

us_tracts_population[[year_loop]]<-get_acs(
  geography = "tract",
  variables = "B01003_001",
  year = as.numeric(year_loop),
  survey = "acs5",
  geometry = TRUE,
  state = continental_fips
)%>%
  mutate(county_fips = substr(GEOID, 1, 5))%>%
  mutate(GEOID=as.numeric(GEOID))%>%
  mutate(county_fips=as.numeric(county_fips))%>%
  select(-variable, -moe)%>%
  rename(tract_population=estimate)

us_counties_population[[year_loop]]<-get_acs(
  geography = "county",
  variables = "B01003_001",
  year = as.numeric(year_loop),
  survey = "acs5",
  geometry = TRUE,
  state = continental_fips
)%>%
  mutate(GEOID=as.numeric(GEOID))%>%
  select(-variable, -moe)%>%
  rename(county_population=estimate)

# Load tract variable labels for age/sex values B01001 from the ACS 5-year 2017 dataset
vars[[year_loop]] <- load_variables(year_loop, "acs5", cache = TRUE) %>%
  filter(str_detect(name, "B01001_"))%>%
  mutate(label = str_replace_all(label, "Estimate!!Total!!", ""))%>%
  mutate(label = str_replace_all(label, "!!", "_"))%>%
  mutate(label = str_replace_all(label, " ", "_"))

CDCAtlas::get_atlas(disease="hiv",
                    geography="county",
                    year=as.numeric(year_loop))



for (year_loop in year_list) {

  #Join county geographical data with CDC data.
  Merged_Counties[[year_loop]]<-left_join(us_counties_population[[year_loop]],
                                          AtlasPlusTableData_county_totals[[year_loop]],
                                          by=join_by(GEOID==geo_id))

}
