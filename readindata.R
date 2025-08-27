d_T1 <- read.csv('/work/Paternity-leave/01. BarselProject/niels_geospatial/output/dayTime/T1/T1_all_day_metrics.csv')
d_T2 <- read.csv('/work/Paternity-leave/01. BarselProject/niels_geospatial/output/dayTime/T2/T2_all_day_metrics.csv')

d <- rbind(d_T1, d_T2) %>%
  rename("ParticipantID" = participantId,
         "Role" = role,
         "CoupleID" = coupleId,
         "TimePoint" = dataType,
         "StudyDay" = dayOfWeek) %>%
  mutate(TimePoint = str_replace_all(TimePoint, "T1", "1"),
         TimePoint = str_replace_all(TimePoint, "T2", "2")) %>%
  mutate(TimePoint = as.integer(TimePoint)) %>%
  mutate(Role = str_replace_all(Role, "father", "Father"),
         Role = str_replace_all(Role, "mother", "Mother"))

## ... Depression Data

d_T1_epdst <- read.csv('/work/Paternity-leave/01. BarselProject/data/completed/BOTH/Survey/t1/EPDST1.csv')
d_T2_epdst <- read.csv('/work/Paternity-leave/01. BarselProject/data/completed/BOTH/Survey/t2/EPDST2.csv')

d_epdst_join <- rbind(d_T1_epdst, d_T2_epdst) %>%
  group_by(ParticipantID, CoupleID, TimePoint, Role) %>%
  dplyr::summarise(d_epdst_sum = sum(CleanResponse, na.rm = T), .groups = 'drop')


## ... Anxiety Data

d_T1_anx <- read.csv('/work/Paternity-leave/01. BarselProject/data/completed/BOTH/Survey/t1/ANXT1.csv')
d_T2_anx <- read.csv('/work/Paternity-leave/01. BarselProject/data/completed/BOTH/Survey/t2/ANXT2.csv') %>%
  mutate(TimePoint = 2)

d_anx_join <- rbind(d_T1_anx, d_T2_anx) %>%
  group_by(ParticipantID, CoupleID, TimePoint, Role) %>%
  dplyr::summarise(d_anx_sum = sum(CleanResponse, na.rm = T), .groups = 'drop')

## ... Sleepiness Data

### Q about data. Why is there no T2?!
d_T1_epworth <- read.csv('/work/Paternity-leave/01. BarselProject/data/completed/BOTH/Survey/t1/EPWORTHT1.csv')
#d_T2_epworth <- read.csv('/work/Paternity-leave/01. BarselProject/data/completed/BOTH/Survey/t2/EPWORTHT2.csv') # Note that this is the T1 data duplicated.. I found the original data for T2 though!
d_T2_epworth <- read.csv('/work/Paternity-leave/01. BarselProject/data/completed/ONLY/Survey/t2/EPWORTHT2.csv')

d_epworth_join <- rbind(d_T1_epworth, d_T2_epworth) %>% # Add T2 data when you figure out.
  group_by(ParticipantID, CoupleID, TimePoint, Role) %>%
  dplyr::summarise(d_epworth_sum = sum(CleanResponse, na.rm = T), .groups = 'drop')

## ... Morning Questionnaire Data

d_T1_morningquestionnaire <- read.csv('/work/Paternity-leave/01. BarselProject/data/completed/BOTH/ESM/t1/T1_ESM_Morning_Preprocessed.csv')
d_T2_morningquestionnaire <- read.csv('/work/Paternity-leave/01. BarselProject/data/completed/BOTH/ESM/t2/T2_ESM_Morning_Preprocessed.csv')

d_morningquestionnaire_join <- rbind(d_T1_morningquestionnaire, d_T2_morningquestionnaire) %>%
  rename("TimePoint" = ESMTimePoint)

## Join with GPS dataset
d_GPS <- d %>% 
  left_join(d_epdst_join, by = c("ParticipantID", "CoupleID", "TimePoint", "Role")) %>%
  left_join(d_epworth_join, by = c("ParticipantID", "CoupleID", "TimePoint", "Role")) %>%
  left_join(d_anx_join, by = c("ParticipantID", "CoupleID", "TimePoint", "Role")) %>%
  left_join(d_morningquestionnaire_join, by = c("ParticipantID", "CoupleID", "TimePoint", "Role", "StudyDay")) %>%
  mutate(isWeekend = case_when(
    isWeekend == TRUE ~ "Weekend",
    isWeekend == FALSE ~ "Weekday",
    TRUE ~ NA_character_)) %>%
  mutate(Season = case_when(
    month %in% c(12, 1, 2) ~ "Winter",
    month %in% c(3, 4, 5) ~ "Spring",
    month %in% c(6, 7, 8) ~ "Summer",
    month %in% c(9, 10, 11) ~ "Autumn",
    TRUE ~ NA_character_)) %>%
  mutate(Leave = TimePoint) %>%
  mutate(Leave = case_when(
    Leave == 1 ~ "Maternity Leave",
    Leave == 2 ~ "Paternity Leave")) %>%
  mutate(totalHomeTimePercent_01 = 1 - totalOHTimePercent / 100) %>%
  mutate(
    # Convert to ordered factors for ordinal predictors
    d_epdst_sum = as.ordered(as.numeric(d_epdst_sum + 1)),
    d_epdst_sum = as.ordered(d_epdst_sum),
    SleepQuality_int = as.integer(SleepQuality),
    SleepQuality = as.ordered(SleepQuality),
  )

d_GPS_depression <- d_GPS %>%
  mutate(d_epdst_sum = as.numeric(d_epdst_sum)) %>%
  group_by(ParticipantID, Leave, Role) %>%
  summarise(d_epdst_sum = mean(d_epdst_sum), .groups = 'drop') %>%
  mutate(d_epdst_sum = as.integer(d_epdst_sum))

d_Mother_ONLEAVET2 <- read.csv(here("analysis_code", "moms_on_leave_T2_CC.csv")) %>%
  filter(Decision == "On Leave")

length(unique(d_Mother_ONLEAVET2$CoupleID))

d_Dad_NOTONLEAVET2 <- read.csv(here("analysis_code", "dads_NOTon_leave_T2_CC.csv"))

length(unique(d_Dad_NOTONLEAVET2$CoupleID))

length(c(unique(d_Dad_NOTONLEAVET2$CoupleID), unique(d_Mother_ONLEAVET2$CoupleID)))
length(c(unique(d_Dad_NOTONLEAVET2$ParticipantID), unique(d_Mother_ONLEAVET2$ParticipantID)))

d_GPS_nonoverlap <- d_GPS %>%
  filter(Leave == "Maternity Leave" | (Leave == "Paternity Leave" & !CoupleID %in% c(unique(d_Dad_NOTONLEAVET2$CoupleID), unique(d_Mother_ONLEAVET2$CoupleID))))

d_GPS_nonoverlap <- d_GPS %>%
  filter(!CoupleID %in% c(unique(d_Dad_NOTONLEAVET2$CoupleID), unique(d_Mother_ONLEAVET2$CoupleID)))

length(unique((filter(d_GPS_nonoverlap, Leave == "Paternity Leave")$CoupleID)))
length(unique((filter(d_GPS, Leave == "Paternity Leave")$CoupleID)))

length(unique((filter(d_GPS_nonoverlap, Leave == "Paternity Leave")$ParticipantID)))
length(unique((filter(d_GPS, Leave == "Paternity Leave")$ParticipantID)))

length(unique(d_GPS_nonoverlap$CoupleID))
length(unique(d_GPS$ParticipantID))

# Number of Participants at T1
length(unique(filter(d_GPS, Role == "Father", TimePoint == 1)$ParticipantID))
length(unique(filter(d_GPS, Role == "Mother", TimePoint == 1)$ParticipantID))

# Number of Participants at T2
length(unique(filter(d_GPS, Role == "Father", TimePoint == 2)$ParticipantID))
length(unique(filter(d_GPS, Role == "Mother", TimePoint == 2)$ParticipantID))

d_GPS %>%
  group_by(Leave, Role, ParticipantID, isWeekend) %>%
  summarise(n = n(), .groups = 'drop') %>%
  ungroup() %>%
  group_by(Leave, isWeekend, Role) %>%
  summarise(Days_m = mean(n),
            Days_sd = sd(n), .groups = 'drop')

Demo <- read.csv('/work/Paternity-leave/01. BarselProject/data/completed/BOTH/Survey/Other/Demographics/Demographics_Both.csv')

DemoOnly <- read.csv('/work/Paternity-leave/01. BarselProject/data/completed/ONLY/Survey/Other/Demographics/Demographics_Only.csv')

Demo %>%
  filter(Question == "Hvor gammel er du?") %>%
  filter(ParticipantID %in% unique(d_GPS$ParticipantID)) %>%
  group_by(TimePoint, Role) %>%
  summarise(CleanResponse = mean(CleanResponse, na.rm = T), .groups = 'drop')
#CleanResponse_sd = sd(CleanResponse, na.rm = T))
unique(Demo$Question)

try <- Demo %>%
  filter(Question == "Hvad er den højeste uddannelse, du har fuldført?") %>%
  filter(ParticipantID %in% unique(d_GPS$ParticipantID)) %>%
  mutate(University = case_when(
    Response %in% c("Lang videregående uddannelse", "Ph.d. og forskeruddannelse",
                    "Mellemlang videregående uddannelse (fx professionsbachelor)",
                    "Bacheloruddannelse") ~ "UniversityEducation",
    Response %in% c("Gymnasial uddannelse", "Erhvervsfaglig uddannelse",
                    "Grundskole") ~ "NoUniversityEducation"
  ))

d_GPS_t1 <- d_GPS %>% 
  filter(Leave == "Maternity Leave")

d_GPS_t2 <- d_GPS %>% 
  filter(Leave == "Paternity Leave") %>%
  filter(ParticipantID %in% unique(d_GPS_t1$ParticipantID))

participants_both <- intersect(
  unique(d_GPS_t1$ParticipantID), 
  unique(d_GPS_t2$ParticipantID)
)

d_GPS_both_timepoints <- d_GPS %>%
  filter(ParticipantID %in% participants_both)

nrow(d_GPS_nonoverlap)
