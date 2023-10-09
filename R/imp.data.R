# imp.data.R #
# import data into df

# load packages #
ld_pkgs <- c("tidyverse","readxl", "ggplot2","ggthemes", "lubridate")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

df0 <- readxl::read_excel("data/in/NW_WIMS.xlsx",
                          sheet = "qry_All_Regions_data",
                          col_names = TRUE)

E_N <- rnrfa::osg_parse(df0$SMPT_GRID_REF) #convert grid refs to E & N 
df0$Eastings <- E_N$easting
df0$Northings <- E_N$northing
rm(E_N)

## keep only suspended solids
df0 %>% 
  dplyr::filter(DETE_DESC == "Solids, Suspended at 105 C") %>% ## keep only suspended solids
  # dplyr::filter(grepl("MERSEY",SMPT_LONG_NAME)) %>%
  dplyr::filter(Northings <= 410000) %>% 
  dplyr::filter(MEAS_RESULT <= 300) %>% 
  dplyr::filter(SAMP_PURPOSE_CODE == "MS") %>% 
  ggplot(., aes(x = SAMP_SAMPLE_DATE,
                y = MEAS_RESULT))+
  geom_line(aes(group = SAMP_SMPT_USER_REFERENCE,
                colour=SAMP_SMPT_USER_REFERENCE), show.legend = FALSE)+
  geom_point(aes(group = SAMP_SMPT_USER_REFERENCE,
                 colour=SAMP_SMPT_USER_REFERENCE), show.legend = FALSE)+
  theme_few()+
  #geom_smooth()+
  labs(title = "Suspended sediment concentrations",
       subtitle = "Data for 'Solids, Suspended at 105 C'. Sample Purpose code = 'MS'. Colours represent individual sampling stations
Data filtered to retain only those at <= 410000m Northings (Approx Formby Point) and at measured values <= 300")



names(df0)  

df0 %>% 
  dplyr::filter(DETE_DESC == "Solids, Suspended at 105 C") %>% ## keep only suspended solids
  # dplyr::filter(grepl("MERSEY",SMPT_LONG_NAME)) %>%
  dplyr::filter(SAMP_PURPOSE_CODE == "MS") %>% 
  ggplot(., aes(x = Eastings,
                y = Northings))+
  # geom_line(aes(colour=SAMP_SMPT_USER_REFERENCE), show.legend = FALSE)+
  geom_point(aes(colour=SAMP_SMPT_USER_REFERENCE), show.legend = FALSE)+
  theme_few()+
  coord_fixed(ratio = 1)+
  geom_hline(yintercept = 410000)
