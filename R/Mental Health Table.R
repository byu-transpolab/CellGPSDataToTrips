#' @param accurate_tibble target
#' @return a table including ID, date, sf, algorithm, and the 
#' number of clusters as its own column

addNumTrips <- function(df){
  tibble <- as_tibble(df) %>%
    select(id, date, cleaned, algorithm) %>%
    group_by(id,date) %>%
    rowwise() %>%
    # Calculate the total number of trips someone made in a day
    mutate(numTrips = length(algorithm[[1]]))
  return(tibble)
}

#' @param output from addNumTrips function
#' @return a table including ID, date, sf, algorithm, the
#' number of total clusters as its own column, and how many of those
#'  clusters were at a park, library, or grocery store

addTripType <- function(tibble){
  
  parksSf <- sf::st_read("resources/parks.geojson") %>%
    st_transform(32612)
  grocerySf <- sf::st_read("resources/groceries.geojson") %>%
    st_transform(32612)
  librarySf <- sf::st_read("resources/libraries.geojson") %>%
    st_transform(32612)
  
  tibble %>%
    filter(!is.character(algorithm)) %>%
    select(-c(cleaned)) %>%
    unnest(cols = c(algorithm)) %>%
    st_as_sf() %>%
    st_join(parksSf) %>%
    st_join(grocerySf) %>%
    st_join(librarySf) %>%
    mutate(id = id.x)
    
}

addMentalHealthResponses <- function(tibble){
  morningResponses <- read.csv("C:/Users/griches/Desktop/BigCellGPSDataToTrips/mental_surveys/Morning_Survey_220223.csv") %>%
    rename(id = Study.Id)
  eveningResponses <- read.csv("C:/Users/griches/Desktop/BigCellGPSDataToTrips/mental_surveys/Evening_Survey_220223.csv")
  full_join(tibble,morningResponses, by = 'id')
}

addDemographicData <- function(num_trips) {
  demographic <- readxl::read_excel("C:/Users/griches/Desktop/BigCellGPSDataToTrips/mental_surveys/Demographic_Breakdown.xlsx") %>%
    rename(id = Metricwire_ID)
  full_join(num_trips, demographic, by = 'id')
}

## INITIAL STATISTICAL ANALYSIS BETWEEN GROUPS 

demoTable <- addDemographicData(num_trips = addMentalHealthResponses(tibble = num_trips)) %>%
  select(id, date, Prescribed_Group, numTrips) %>%
  filter(Prescribed_Group != "No Group",
         Prescribed_Group != "NA")

ggplot(demoTable) +
  aes(x = Prescribed_Group, y = numTrips) +
  geom_boxplot() +
  labs(
    x = "Group",
    y = "Number of Activities"
  )

group_by(demoTable, Prescribed_Group) %>%
  summarise(
    median = median(numTrips, na.rm = TRUE),
    sd = sd(numTrips, na.rm = TRUE)
  )

ggplot(demoTable, aes(x = numTrips)) + 
  geom_histogram(bins = 30, color = "white") +
  theme_bw() +
  facet_wrap(~Prescribed_Group) +
  labs(
    x = "Number of Activities",
    y = "Count"
  ) +
  xlim(c(0,30))

res_aov <- aov(numTrips ~ Prescribed_Group,
               data = demoTable)

summary(res_aov)

TukeyHSD(res_aov)

res_lm <- lm(numTrips ~ Prescribed_Group,
               data = demoTable)

summary(res_lm)

## POISSON DISTRIBUTION
demoTable$Prescribed_Group = relevel(factor(demoTable$Prescribed_Group), ref = "Control") ## set control as reference
poisson <- list("Poisson Regression" = glm(numTrips ~  Prescribed_Group, family="poisson", 
                                data=demoTable))
modelsummary(poisson, statistic = 'p.value')

## NUM ACTIVITIES ACROSS WHOLE DATASET
ggplot(num_trips, aes(x = numTrips)) + geom_histogram(bins = 30, color = "white") + theme_bw() +
  labs(x = "Number of Activites", y = "Count") +
  xlim(c(0,30))

## DENSITY OF ACTIVITIES ACROSS WHOLE DATASET
boundary <- st_read("Figures/SchultzBounds.shp")

num_trips %>%
  ungroup() %>%
  sample_n(100) %>%
  select(id, algorithm) %>%
  unnest(cols = c(algorithm)) %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  st_crop(boundary) %>%
  transmute(
    id, cluster, 
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2]
  ) %>%
      ggplot() +
      annotation_map_tile("cartolight", zoom = 12) +
      stat_density2d(aes(x = x, y = y, fill = ..level..), 
                     alpha = 0.3, bins = 45, geom = "polygon")  +
      scale_fill_viridis_c("Activity Density") +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank(),
            legend.key.size = unit(1, 'cm'), #change legend key size
            legend.key.height = unit(1, 'cm'), #change legend key height
            legend.key.width = unit(1, 'cm'), #change legend key width
            legend.title = element_text(size=10), #change legend title font size
            legend.text = element_text(size=8)) #change legend text font size)
