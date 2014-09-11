# COUNTRY-YEAR TSCS DATA UTILITIES
# Jay Ulfelder
# 2014-09-09

# This script loads two functions, 'countryyearrackit' and 'pitfcodeit'. The first creates a rectangular
# data set with country-years as its rows that includes ID variables and related metadata. The second adds
# PITF country codes to any data frame (including the one created by 'countryyearrackit') with a column
# of country names.

#######################
# countryyearrackit
#######################

# Country list and birth and death years sourced to:
#   CIA Factbook: https://www.cia.gov/library/publications/the-world-factbook/fields/2088.html
#   Wikipedia: http://en.wikipedia.org/wiki/List_of_sovereign_states_by_date_of_formation

countryyearrackit <- function(startyear, endyear) {

  kuntry <- function(country, birth, death) {
    year <- seq(birth, ifelse(death < endyear, death, endyear), 1)
    country <- rep(country, times = length(year))
    yrborn <- rep(birth, times = length(year))
    yrdied <- rep(death, times = length(year))
    z <- cbind(country, year, yrborn, yrdied)
    return(z)
  }

  # AFRICA
  Algeria <- kuntry("Algeria", 1962, 9999)
  Angola <- kuntry("Angola", 1975, 9999)
  Benin <- kuntry("Benin", 1960, 9999)
  Botswana <- kuntry("Botswana", 1966, 9999)
  BurkinaFaso <- kuntry("Burkina Faso", 1960, 9999)
  Burundi <- kuntry("Burundi", 1962, 9999) 
  Cameroon <- kuntry("Cameroon", 1960, 9999)
  CapeVerde <- kuntry("Cape Verde", 1975, 9999)
  CAR <- kuntry("Central African Republic", 1960, 9999)
  Chad <- kuntry("Chad", 1960, 9999)
  Comoros <- kuntry("Comoros", 1975, 9999)
  DROC <- kuntry("Congo-Kinshasa", 1960, 9999)
  Congo <- kuntry("Congo-Brazzaville", 1960, 9999)
  IvoryCoast <- kuntry("Ivory Coast", 1960, 9999)
  Djibouti <- kuntry("Djibouti", 1977, 9999)
  Egypt <- kuntry("Egypt", 1953, 9999)
  EquatorialGuinea <- kuntry("Equatorial Guinea", 1968, 9999)
  Eritrea <- kuntry("Eritrea", 1993, 9999)
  Ethiopia <- kuntry("Ethiopia", -100, 9999)
  Gabon <- kuntry("Gabon", 1960, 9999)
  Gambia <- kuntry("Gambia", 1965, 9999)
  Ghana <- kuntry("Ghana", 1957, 9999)
  Guinea <- kuntry("Guinea", 1958, 9999)
  GuineaBissau <- kuntry("Guinea-Bissau", 1974, 9999)
  Kenya <- kuntry("Kenya", 1963, 9999)
  Lesotho <- kuntry("Lesotho", 1966, 9999)
  Liberia <- kuntry("Liberia", 1847, 9999)
  Libya <- kuntry("Libya", 1951, 9999)
  Madagascar <- kuntry("Madagascar", 1960, 9999)
  Malawi <- kuntry("Malawi", 1964, 9999)
  Mali <- kuntry("Mali", 1960, 9999)
  Mauritania <- kuntry("Mauritania", 1960, 9999)
  Mauritius <- kuntry("Mauritius", 1968, 9999)
  Morocco <- kuntry("Morocco", 1956, 9999)
  Mozambique <- kuntry("Mozambique", 1975, 9999)
  Namibia <- kuntry("Namibia", 1990, 9999)
  Niger <- kuntry("Niger", 1960, 9999)
  Nigeria <- kuntry("Nigeria", 1960, 9999)
  Rwanda <- kuntry("Rwanda", 1962, 9999)
  SaoTome <- kuntry("Sao Tome and Principe", 1975, 9999)
  Senegal <- kuntry("Senegal", 1960, 9999)
  Seychelles <- kuntry("Seychelles", 1976, 9999)
  SierraLeone <- kuntry("Sierra Leone", 1961, 9999)
  Somalia <- kuntry("Somalia", 1960, 9999)
  SouthAfrica <- kuntry("South Africa", 1910, 9999)
  SouthSudan <- kuntry("South Sudan", 2011, 9999)
  Sudan <- kuntry("Sudan", 1956, 9999)
  Swaziland <- kuntry("Swaziland", 1968, 9999)
  Tanzania <- kuntry("Tanzania", 1961, 9999)
  Togo <- kuntry("Togo", 1960, 9999)
  Tunisia <- kuntry("Tunisia", 1956, 9999)
  Uganda <- kuntry("Uganda", 1962, 9999)
  Zambia <- kuntry("Zambia", 1964, 9999)
  Zimbabwe <- kuntry("Zimbabwe", 1980, 9999)

  africa <- rbind(Algeria, Angola, Benin, Botswana, BurkinaFaso, Burundi, Cameroon, CapeVerde,
    CAR, Chad, Comoros, DROC, Congo, IvoryCoast, Djibouti, Egypt, EquatorialGuinea,
    Eritrea, Ethiopia, Gabon, Gambia, Ghana, Guinea, GuineaBissau, Kenya,
    Lesotho, Liberia, Libya, Madagascar, Malawi, Mali, Mauritania, Mauritius,
    Morocco, Mozambique, Namibia, Niger, Nigeria, Rwanda, Senegal, SierraLeone,
    Somalia, SouthAfrica, SouthSudan, Sudan, Swaziland, Tanzania, Togo, Tunisia,
    Uganda, Zambia, Zimbabwe)

  # AMERICAS
  Antigua <- kuntry("Antigua and Barbuda", 1981, 9999)
  Argentina <- kuntry("Argentina", 1816, 9999)
  Bahamas <- kuntry("Bahamas", 1973, 9999)
  Barbados <- kuntry("Barbados", 1966, 9999)
  Belize <- kuntry("Belize", 1981, 9999)
  Bolivia <- kuntry("Bolivia", 1825, 9999)
  Brazil <- kuntry("Brazil", 1825, 9999)
  Canada <- kuntry("Canada", 1867, 9999)
  Chile <- kuntry("Chile", 1818, 9999)
  Colombia <- kuntry("Colombia", 1819, 9999)
  CostaRica <- kuntry("Costa Rica", 1821, 9999)
  Cuba <- kuntry("Cuba", 1868, 9999)
  Dominica <- kuntry("Dominica", 1978, 9999)
  DominicanRepublic <- kuntry("Dominican Republic", 1865, 9999)
  Ecuador <- kuntry("Ecuador", 1830, 9999)
  ElSalvador <- kuntry("El Salvador", 1841, 9999)
  Grenada <- kuntry("Grenada", 1974, 9999)
  Guatemala <- kuntry("Guatemala", 1839, 9999)
  Guyana <- kuntry("Guyana", 1966, 9999)
  Haiti <- kuntry("Haiti", 1804, 9999)
  Honduras <- kuntry("Honduras", 1838, 9999)
  Jamaica <- kuntry("Jamaica", 1962, 9999)
  Mexico <- kuntry("Mexico", 1821, 9999)
  Nicaragua <- kuntry("Nicaragua", 1838, 9999)
  Panama <- kuntry("Panama", 1903, 9999)
  Paraguay <- kuntry("Paraguay", 1811, 9999)
  Peru <- kuntry("Peru", 1879, 9999)
  StKitts <- kuntry("Saint Kitts and Nevis", 1983, 9999)
  StLucia <- kuntry("Saint Lucia", 1979, 9999)
  StVincent <- kuntry("Saint Vincent and the Grenadines", 1979, 9999)
  Suriname <- kuntry("Suriname", 1975, 9999)
  Trinidad <- kuntry("Trinidad and Tobago", 1962, 9999)
  USA <- kuntry("United States", 1783, 9999)
  Uruguay <- kuntry("Uruguay", 1825, 9999)
  Venezuela <- kuntry("Venezuela", 1830, 9999)

  americas <- rbind(Argentina, Bahamas, Barbados, Belize, Bolivia, Brazil, Canada, Chile,
    Colombia, CostaRica, Cuba, DominicanRepublic, Ecuador, ElSalvador,
    Guatemala, Guyana, Haiti, Honduras, Jamaica, Mexico, Nicaragua,
    Panama, Paraguay, Peru, Trinidad, USA, Uruguay, Venezuela)

  # ASIA
  Afghanistan <- kuntry("Afghanistan", 1919, 9999)
  Bahrain <- kuntry("Bahrain", 1971, 9999)
  Bangladesh <- kuntry("Bangladesh", 1971, 9999)
  Bhutan <- kuntry("Bhutan", 1885, 9999)
  Brunei <- kuntry("Brunei", 1984, 9999)
  Cambodia <- kuntry("Cambodia", 1953, 9999)
  China <- kuntry("China", -221, 9999)
  India <- kuntry("India", 1947, 9999)
  Indonesia <- kuntry("Indonesia", 1949, 9999)
  Iran <- kuntry("Iran", 1501, 9999)
  Iraq <- kuntry("Iraq", 1932, 9999)
  Israel <- kuntry("Israel", 1948, 9999)
  Japan <- kuntry("Japan", 660, 9999)
  Jordan <- kuntry("Jordan", 1946, 9999)
  Kuwait <- kuntry("Kuwait", 1961, 9999)
  Laos <- kuntry("Laos", 1949, 9999)
  Lebanon <- kuntry("Lebanon", 1943, 9999)
  Malaysia <- kuntry("Malaysia", 1957, 9999)
  Maldives <- kuntry("Maldives", 1965, 9999)
  Mongolia <- kuntry("Mongolia", 1911, 9999)
  Myanmar <- kuntry("Myanmar", 1948, 9999)
  Nepal <- kuntry("Nepal", 1768, 9999)
  NorthKorea <- kuntry("North Korea", 1948, 9999)
  Oman <- kuntry("Oman", 1650, 9999)
  Pakistan <- kuntry("Pakistan", 1947, 9999)
  Philippines <- kuntry("Philippines", 1898, 9999)
  Qatar <- kuntry("Qatar", 1971, 9999)
  SaudiArabia <- kuntry("Saudi Arabia", 1932, 9999)
  Singapore <- kuntry("Singapore", 1965, 9999)
  SouthKorea <- kuntry("South Korea", 1948, 9999)
  SriLanka <- kuntry("Sri Lanka", 1972, 9999)
  Syria <- kuntry("Syria", 1946, 9999)
  Taiwan <- kuntry("Taiwan", 1949, 9999)
  Thailand <- kuntry("Thailand", 1350, 9999)
  TimorLeste <- kuntry("Timor Leste", 2002, 9999)
  UAE <- kuntry("United Arab Emirates", 1971, 9999)
  Vietnam <- kuntry("Vietnam", 1976, 9999)
  Yemen <- kuntry("Yemen", 1990, 9999)

  asia <- rbind(Afghanistan, Bahrain, Bangladesh, Bhutan, Cambodia, China, India,
    Indonesia, Iran, Iraq, Israel, Japan, Jordan, Kuwait, Laos, Lebanon,
    Malaysia, Mongolia, Myanmar, Nepal, NorthKorea, Oman, Pakistan,
    Philippines, Qatar, SaudiArabia, Singapore, SouthKorea, SriLanka,
    Syria, Taiwan, Thailand, TimorLeste, UAE, Vietnam, Yemen) 

  # EUROPE
  Albania <- kuntry("Albania", 1912, 9999)
  Andorra <- kuntry("Andorra", 1813, 9999)
  Austria <- kuntry("Austria", 1918, 9999)
  Belarus <- kuntry("Belarus", 1991, 9999)
  Belgium <- kuntry("Belgium", 1830, 9999)
  Bosnia <- kuntry("Bosnia and Herzegovina", 1992, 9999)
  Bulgaria <- kuntry("Bulgaria", 1878, 9999)
  Croatia <- kuntry("Croatia", 1991, 9999)
  Cyprus <- kuntry("Cyprus", 1960, 9999)
  CzechRepublic <- kuntry("Czech Republic", 1993, 9999)
  Denmark <- kuntry("Denmark", 965, 9999)
  Estonia <- kuntry("Estonia", 1991, 9999)
  Finland <- kuntry("Finland", 1918, 9999)
  France <- kuntry("France", 843, 9999)
  Germany <- kuntry("Germany", 1990, 9999)
  Greece <- kuntry("Greece", 1832, 9999)
  Hungary <- kuntry("Hungary", 1849, 9999)
  Iceland <- kuntry("Iceland", 1944, 9999)
  Ireland <- kuntry("Ireland", 1922, 9999)
  Italy <- kuntry("Italy", 1861, 9999)
  Kosovo <- kuntry("Kosovo", 2008, 9999)
  Latvia <- kuntry("Latvia", 1991, 9999)
  Liechtenstein <- kuntry("Liechtenstein", 1813, 9999)
  Lithuania <- kuntry("Lithuania", 1991, 9999)
  Luxembourg <- kuntry("Luxembourg", 1890, 9999)
  Macedonia <- kuntry("Macedonia", 1991, 9999)
  Malta <- kuntry("Malta", 1964, 9999)
  Moldova <- kuntry("Moldova", 1991, 9999)
  Monaco <- kuntry("Monaco", 1861, 9999)
  Montenegro <- kuntry("Montenegro", 2006, 9999)
  Netherlands <- kuntry("Netherlands", 1568, 9999)
  Norway <- kuntry("Norway", 1905, 9999)
  Poland <- kuntry("Poland", 1918, 9999)
  Portugal <- kuntry("Portugal", 1143, 9999)
  Romania <- kuntry("Romania", 1877, 9999)
  SanMarino <- kuntry("San Marino", 301, 9999)
  Serbia <- kuntry("Serbia", 2006, 9999)
  Slovakia <- kuntry("Slovakia", 1993, 9999)
  Slovenia <- kuntry("Slovenia", 1991, 9999)
  Spain <- kuntry("Spain", 1492, 9999)
  Sweden <- kuntry("Sweden", 1523, 9999)
  Switzerland <- kuntry("Switzerland", 1291, 9999)
  Turkey <- kuntry("Turkey", 1923, 9999)
  Ukraine <- kuntry("Ukraine", 1991, 9999)
  UnitedKingdom <- kuntry("United Kingdom", 1536, 9999)
  Armenia <- kuntry("Armenia", 1991, 9999)
  Azerbaijan <- kuntry("Azerbaijan", 1991, 9999)
  Georgia <- kuntry("Georgia", 1991, 9999)
  Kazakhstan <- kuntry("Kazakhstan", 1991, 9999)
  Kyrgyzstan <- kuntry("Kyrgyzstan", 1991, 9999)
  Russia <- kuntry("Russia", 1991, 9999)
  Tajikistan <- kuntry("Tajikistan", 1991, 9999)
  Turkmenistan <- kuntry("Turkmenistan", 1991, 9999)
  Uzbekistan <- kuntry("Uzbekistan", 1991, 9999)

  europe <- rbind(Albania, Austria, Belgium, Bosnia, Bulgaria, Croatia, Cyprus,
    CzechRepublic, Denmark, Finland, France, Germany, Greece, Hungary,
    Ireland, Italy, Macedonia, Montenegro, Netherlands, Norway, Poland,
    Portugal, Romania, Serbia, Slovakia, Slovenia, Spain, Switzerland,
    Sweden, Turkey, UnitedKingdom)

  fsu <- rbind(Armenia, Azerbaijan, Belarus, Estonia, Georgia, Kazakhstan,
    Kyrgyzstan, Latvia, Lithuania, Moldova, Russia, Tajikistan,
    Turkmenistan, Ukraine, Uzbekistan) 

  # OCEANIA
  Australia <- kuntry("Australia", 1901, 9999)
  Fiji <- kuntry("Fiji", 1970, 9999)
  Kiribati <- kuntry("Kiribati", 1979, 9999)
  MarshallIslands <- kuntry("Marshall Islands", 1979, 9999)
  Micronesia <- kuntry("Federated States of Micronesia", 1979, 9999)
  Nauru <- kuntry("Nauru", 1968, 9999)
  NewZealand <- kuntry("New Zealand", 1907, 9999)
  Palau <- kuntry("Palau", 1981, 9999)
  PapuaNewGuinea <- kuntry("Papua New Guinea", 1975, 9999)
  Samoa <- kuntry("Samoa", 1962, 9999)
  SolomonIslands <- kuntry("Solomon Islands", 1978, 9999)
  Tonga <- kuntry("Tonga", 1970, 9999)
  Tuvalu <- kuntry("Tuvalu", 1978, 9999)
  Vanuatu <- kuntry("Vanuatu", 1980, 9999)

  oceania <- rbind(Australia, Fiji, NewZealand, PapuaNewGuinea, SolomonIslands)

  # DEFUNCT
  Czechoslovakia <- kuntry("Czechoslovakia", 1918, 1992)
  Yugoslavia <- kuntry("Yugoslavia", 1918, 1991)
  FedRepYugoslavia <- kuntry("Federal Republic of Yugoslavia", 1992, 2002)
  SerbiaMontenegro <- kuntry("Serbia and Montenegro", 2003, 2006)
  WestGermany <- kuntry("West Germany", 1945, 1989)
  EastGermany <- kuntry("East Germany", 1945, 1989)
  USSR <- kuntry("Soviet Union", 1922, 1991)
  NorthYemen <- kuntry("North Yemen", 1918, 1989)
  SouthYemen <- kuntry("South Yemen", 1967, 1989)
  NorthVietnam <- kuntry("North Vietnam", 1954, 1975)
  SouthVietnam <- kuntry("South Vietnam", 1954, 1975)

  defunct <- rbind(Czechoslovakia, Yugoslavia, FedRepYugoslavia, SerbiaMontenegro,
    WestGermany, EastGermany, USSR, NorthYemen, SouthYemen,
    NorthVietnam, SouthVietnam)

  # Aggregate and fix types
  rack <- data.frame(rbind(africa, americas, asia, europe, fsu, oceania, defunct))
  rack$country <- as.character(rack$country)
  rack$year <- as.numeric(as.character(rack$year))
  rack$yrborn <- as.numeric(as.character(rack$yrborn))
  rack$yrdied <- as.numeric(as.character(rack$yrdied))

  # Trim at specified start year
  rack <- rack[which(rack$year >= startyear),]

  ### STATE DEPT REGIONS ###

  rack$reg.eap <- ifelse(rack$country=="Australia" | rack$country=="Brunei" | rack$country=="Burma" | rack$country=="Cambodia" |
    rack$country=="China" | rack$country=="East Timor" | rack$country=="Fiji" | rack$country=="Indonesia" |
    rack$country=="Japan" | rack$country=="Kiribati" | rack$country=="Laos" | rack$country=="Malaysia" |
    rack$country=="Marshall Islands" | rack$country=="Micronesia" | rack$country=="Mongolia" | rack$country=="Nauru" |
    rack$country=="New Zealand" | rack$country=="North Korea" | rack$country=="Palau" | rack$country=="Papua New Guinea" |
    rack$country=="Philippines" | rack$country=="Samoa" | rack$country=="Singapore" | rack$country=="Solomon Islands" |
    rack$country=="South Korea" | rack$country=="Taiwan" | rack$country=="Thailand" | rack$country=="Tonga" |
    rack$country=="Tuvalu" | rack$country=="Vanuatu" | rack$country=="Vietnam" |
    rack$country=="South Vietnam" | rack$country=="North Vietnam" | rack$country=="Myanmar" | rack$country=="Timor Leste",
    1, 0)

  rack$reg.afr <- ifelse(rack$country=="Angola" | rack$country=="Benin" | rack$country=="Botswana" | rack$country=="Burkina Faso" |
    rack$country=="Burundi" | rack$country=="Cameroon" | rack$country=="Cape Verde" | rack$country=="Central African Republic" |
    rack$country=="Chad" | rack$country=="Comoros" | rack$country=="Congo-Kinshasa" | rack$country=="Congo-Brazzaville" |
    rack$country=="Ivory Coast" | rack$country=="Djibouti" | rack$country=="Equatorial Guinea" | rack$country=="Eritrea" |
    rack$country=="Ethiopia" | rack$country=="Gabon" | rack$country=="Gambia" | rack$country=="Ghana" |
    rack$country=="Guinea" | rack$country=="Guinea-Bissau" | rack$country=="Kenya" | rack$country=="Lesotho" |
    rack$country=="Liberia" | rack$country=="Madagascar" | rack$country=="Malawi" | rack$country=="Mali" |
    rack$country=="Mauritania" | rack$country=="Mauritius" | rack$country=="Mozambique" | rack$country=="Namibia" |
    rack$country=="Niger" | rack$country=="Nigeria" | rack$country=="Rwanda" | rack$country=="Sao Tome and Principe" |
    rack$country=="Senegal" | rack$country=="Seychelles" | rack$country=="Sierra Leone" | rack$country=="Somalia" |
    rack$country=="South Africa" | rack$country=="Sudan" | rack$country=="South Sudan" | rack$country=="Swaziland" |
    rack$country=="Tanzania" | rack$country=="Togo" | rack$country=="Uganda" | rack$country=="Zambia" |
    rack$country=="Zimbabwe",
    1, 0)

  rack$reg.eur <- ifelse(rack$country=="Albania" | rack$country=="Armenia" | rack$country=="Austria" | rack$country=="Azerbaijan" |
    rack$country=="Belarus" | rack$country=="Belgium" | rack$country=="Bosnia and Herzegovina" | rack$country=="Bulgaria" |
    rack$country=="Croatia" | rack$country=="Czech Republic" | rack$country=="Cyprus" | rack$country=="Denmark" |
    rack$country=="Estonia" | rack$country=="Finland" | rack$country=="France" | rack$country=="Georgia" |
    rack$country=="Germany" | rack$country=="Greece" | rack$country=="Hungary" | rack$country=="Iceland" |
    rack$country=="Ireland" | rack$country=="Italy" | rack$country=="Kosovo" | rack$country=="Latvia" |
    rack$country=="Lithuania" | rack$country=="Liechtenstein" | rack$country=="Macedonia" | rack$country=="Malta" |
    rack$country=="Moldova" | rack$country=="Monaco" | rack$country=="Montenegro" | rack$country=="Netherlands" |
    rack$country=="Norway" | rack$country=="Poland" | rack$country=="Portugal" | rack$country=="Romania" |
    rack$country=="Russia" | rack$country=="San Marino" | rack$country=="Serbia" | rack$country=="Slovakia" |
    rack$country=="Slovenia" | rack$country=="Spain" | rack$country=="Sweden" | rack$country=="Switzerland" |
    rack$country=="Turkey" | rack$country=="Ukraine" | rack$country=="United Kingdom" |
    rack$country=="East Germany" | rack$country=="West Germany" | rack$country=="Soviet Union" | rack$country=="Yugoslavia" |
    rack$country=="Federal Republic of Yugoslavia" | rack$country=="Serbia and Montenegro" | rack$country=="Czechoslovakia",
    1, 0)

  rack$reg.mna <- ifelse(rack$country=="Algeria" | rack$country=="Bahrain" | rack$country=="Egypt" | rack$country=="Iran" |
    rack$country=="Iraq" | rack$country=="Israel" | rack$country=="Jordan" | rack$country=="Kuwait" |
    rack$country=="Lebanon" | rack$country=="Libya" | rack$country=="Morocco" | rack$country=="Oman" |
    rack$country=="Palestinian Territories" | rack$country=="Qatar" | rack$country=="Saudi Arabia" | rack$country=="Syria" |
    rack$country=="Tunisia" | rack$country=="United Arab Emirates" | rack$country=="Yemen" |
    rack$country=="North Yemen" | rack$country=="South Yemen",
    1, 0)

  rack$reg.sca <- ifelse(rack$country=="Afghanistan" | rack$country=="Bangladesh" | rack$country=="Bhutan" | rack$country=="India" |
    rack$country=="Kazakhstan" | rack$country=="Kyrgyzstan" | rack$country=="Maldives" | rack$country=="Nepal" |
    rack$country=="Pakistan" | rack$country=="Sri Lanka" | rack$country=="Tajikistan" | rack$country=="Turkmenistan" |
    rack$country=="Uzbekistan",
    1, 0)

  rack$reg.amr <- ifelse(rack$country=="Antigua and Barbuda" | rack$country=="Argentina" | rack$country=="Bahamas" |
    rack$country=="Barbados" | rack$country=="Belize" | rack$country=="Bolivia" | rack$country=="Brazil" | rack$country=="Canada" |
    rack$country=="Cayman Islands" | rack$country=="Chile" | rack$country=="Colombia" | rack$country=="Costa Rica" |
    rack$country=="Cuba" | rack$country=="Dominica" | rack$country=="Dominican Republic" | rack$country=="Ecuador" |
    rack$country=="El Salvador" | rack$country=="Grenada" | rack$country=="Guatemala" | rack$country=="Guyana" |
    rack$country=="Haiti" | rack$country=="Honduras" | rack$country=="Jamaica" | rack$country=="Mexico" |
    rack$country=="Nicaragua" | rack$country=="Panama" | rack$country=="Paraguay" | rack$country=="Peru" |
    rack$country=="Saint Kitts and Nevis" | rack$country=="Saint Lucia" | rack$country=="Saint Vincent and the Grenadines" |
    rack$country=="Suriname" | rack$country=="Trinidad and Tobago" | rack$country=="United States" | rack$country=="Uruguay" |
    rack$country=="Venezuela",
    1, 0)

  rack$dosreg <- NA
  rack$dosreg[rack$reg.afr==1] <- "Africa"
  rack$dosreg[rack$reg.eap==1] <- "East Asia & Pacific"
  rack$dosreg[rack$reg.eur==1] <- "Europe & Eurasia"
  rack$dosreg[rack$reg.mna==1] <- "Middle East & North Africa"
  rack$dosreg[rack$reg.sca==1] <- "South & Central Asia"
  rack$dosreg[rack$reg.amr==1] <- "Americas"

  # Spit out the TSCS frame
  return(rack)
}

##########################
# pitfcodeit
##########################

# Function to create 'sftgcode' variable to match SFTGCODE in PITF Merge files.

# NOTE:
# 1. Namevar needs to be in quotes, e.g., use pitfcodeit(data, "country"), not pitfcodeit(data, country), so
#      it isn't confused with an object.
# 2. Give the function a target so you don't dump new df to terminal, e.g., > z <- pitfcodeit(z, "name")
#      and not just > pitfcodeit(z, "name")

pitfcodeit <- function(df, namevar) {
  data <- df
  data$sftgcode <- NA
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Afghanistan", 'AFG')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Albania", 'ALB')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Algeria", 'ALG')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Angola", 'ANG')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Argentina", 'ARG')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Armenia", 'ARM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Australia", 'AUL')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Austria", 'AUS')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Azerbaijan", 'AZE')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Bahamas" |
    data[,namevar]=="Bahamas, The" |
    data[,namevar]=="The Bahamas"), 'BHM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Bahrain", 'BAH')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Bangladesh", 'BNG')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Barbados", 'BAR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Belarus", 'BLR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Belgium", 'BEL')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Belize", 'BLZ')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Benin", 'BEN')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Bhutan", 'BHU')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Bolivia", 'BOL')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Bosnia" |
    data[,namevar]=="Bosnia and Herzegovina" |
    data[,namevar]=="Bosnia Herzegovenia" |
    data[,namevar]=="Bosnia-Herzegovina"), 'BOS')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Botswana", 'BOT')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Brazil", 'BRA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Brunei", 'BRU')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Bulgaria", 'BUL')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Burkina Faso", 'BFO')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Burma" |
    data[,namevar]=="Myanmar" |
    data[,namevar]=="Myanmar (Burma)"), 'MYA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Burundi", 'BUI')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Bangladesh", 'BNG')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Cambodia", 'CAM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Cape Verde", 'CAP')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Cameroon", 'CAO')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Canada", 'CAN')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Central African Republic" |
    data[,namevar]=="Cen African Rep" |
    data[,namevar]=="CAR"), 'CEN')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Chad", 'CHA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Chile", 'CHL')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="China", 'CHN')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Colombia", 'COL')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Comoros", 'COM')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Congo-Brazzaville" |
    data[,namevar]=="Republic of Congo" |
    data[,namevar]=="Congo, Republic of" |
    data[,namevar]=="Congo (Brazzaville)" |
    data[,namevar]=="Republic of the Congo" |
    data[,namevar]=="Congo Brazzaville" |
    data[,namevar]=="Congo" |
    data[,namevar]=="Congo, Rep." |
    data[,namevar]=="Congo, Rep.(Brazzaville)" |
    data[,namevar]=="Congo-Brz" |
    data[,namevar]=="Congo (Brazzaville, Republic of Congo)"), 'CON')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Congo-Kinshasa" |
    data[,namevar]=="Zaire" |
    data[,namevar]=="Zaire/DRC" |
    data[,namevar]=="Democratic Republic of Congo" |
    data[,namevar]=="Democratic Republic of the Congo" |
    data[,namevar]=="Congo, Democratic Republic of" |
    data[,namevar]=="Congo, Dem. Rep." |
    data[,namevar]=="Congo (Kinshasa)" |
    data[,namevar]=="Congo Kinshasa" |
    data[,namevar]=="DROC" |
    data[,namevar]=="Congo, Dem. Rep. (Zaire, Kinshasa)" |
    data[,namevar]=="Congo/Zaire" |
    data[,namevar]=="Democratic Republic of the Congo (Zaire, Congo-Kinshasha)"), 'ZAI')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Costa Rica", 'COS')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Cote d'Ivoire" |
    data[,namevar]=="Ivory Coast" |
    data[,namevar]=="Cote d Ivoire" |
    data[,namevar]=="Ivory Coast (Cote d'Ivoire)" |
    data[,namevar]=="Côte d'Ivoire"), 'IVO')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Croatia", 'CRO')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Cuba", 'CUB')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Cyprus", 'CYP')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Czech Republic", 'CZR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Czechoslovakia", 'CZE')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Denmark", 'DEN')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Djibouti", 'DJI')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Dominican Republic" |
    data[,namevar]=="Dominican Rep"), 'DOM')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="East Timor" |
    data[,namevar]=="Timor" |
    data[,namevar]=="Timor Leste" |
    data[,namevar]=="East Timor (Timor L'este)" |
    data[,namevar]=="Timor-Leste"), 'ETM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Ecuador", 'ECU')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Egypt" |
    data[,namevar]=="Egypt, Arab Rep."), 'EGY')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="El Salvador", 'SAL')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Equatorial Guinea", 'EQG')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Eritrea", 'ERI')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Estonia", 'EST')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Ethiopia" & data$year < 1993), 'ETH')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Ethiopia" & data$year >= 1993), 'ETI')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Fiji", 'FJI')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Finland" |
    data[,namevar]=="Finland "), 'FIN')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="France", 'FRN')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Estonia", 'EST')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Gabon", 'GAB')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Gambia" |
    data[,namevar]=="The Gambia" |
    data[,namevar]=="Gambia, the" |
    data[,namevar]=="Gambia, The"), 'GAM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Georgia", 'GRG')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Germany" & data$year >= 1990), 'GER')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Germany" & data$year < 1990), 'GFR')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="East Germany" |
    data[,namevar]=="Germany, East" |
    data[,namevar]=="German Democratic Republic" |
    data[,namevar]=="Germany East" |
    data[,namevar]=="Germany, E. "), 'GDR')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="West Germany" |
    data[,namevar]=="Germany, West" |
    data[,namevar]=="Federal Republic of Germany" |
    data[,namevar]=="German Federal Republic" |
    data[,namevar]=="Germany West" |
    data[,namevar]=="Germany, W. "), 'GFR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Ghana", 'GHA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Greece", 'GRC')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Guatemala", 'GUA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Guinea", 'GUI')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Guinea-Bissau" |
    data[,namevar]=="Guinea Bissau"), 'GNB')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Guyana", 'GUY')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Haiti", 'HAI')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Honduras", 'HON')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Hungary", 'HUN')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Iceland", 'ICE')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="India", 'IND')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Indonesia", 'INS')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Guyana", 'GUY')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Iran" |
    data[,namevar]=="Iran, Islamic Rep." |
    data[,namevar]=="Islamic Republic of Iran"), 'IRN')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Iraq", 'IRQ')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Ireland", 'IRE')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Israel" |
    data[,namevar]=="Israel and Occupied Territories**" |
    data[,namevar]=="Israel, pre-1967 borders"), 'ISR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Italy", 'ITA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Jamaica", 'JAM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Japan", 'JPN')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Jordan", 'JOR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Kazakhstan", 'KZK')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Kenya", 'KEN')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Korea, North" |
    data[,namevar]=="North Korea" |
    data[,namevar]=="People's Republic of Korea" |
    data[,namevar]=="Korea North" |
    data[,namevar]=="Korea, Dem. Rep." |
    data[,namevar]=="North Korea (Democratc People's Republic of Korea)" |
    data[,namevar]=="Korea, Dem. Rep. (N)" |
    data[,namevar]=="Korea, Democratic People's Republic of"), 'PRK')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Korea, South" |
    data[,namevar]=="South Korea" |
    data[,namevar]=="Korea South" |
    data[,namevar]=="Republic of Korea" |
    data[,namevar]=="Korea, Rep." |
    data[,namevar]=="South Korea (Republic of Korea)" |
    data[,namevar]=="Korea" |
    data[,namevar]=="Korea, Rep. (S)" |
    data[,namevar]=="Korea, Republic of"), 'ROK')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Kuwait", 'KUW')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Kyrgyzstan" |
    data[,namevar]=="Kyrgyz Republic"), 'KYR')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Laos" |
    data[,namevar]=="Lao PDR" |
    data[,namevar]=="Lao P.D.R." |
    data[,namevar]=="Lao, PDR"), 'LAO')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Latvia", 'LAT')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Lebanon", 'LEB')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Lesotho", 'LES')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Liberia", 'LBR')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Libya" |
    data[,namevar]=="Libyan Arab Jamahiriya"), 'LIB')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Lithuania", 'LIT')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Luxembourg", 'LUX')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Macedonia" |
    data[,namevar]=="Macedonia, FYR" |
    data[,namevar]=="FYR Macedonia"), 'MAC')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Madagascar", 'MAG')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Malawi", 'MAW')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Malaysia", 'MAL')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Maldives", 'MAD')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Mali", 'MLI')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Malta", 'MLT')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Mauritania", 'MAA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Mauritius", 'MAS')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Mexico", 'MEX')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Moldova", 'MLD')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Mongolia", 'MON')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Montenegro", 'MNE')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Morocco", 'MOR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Mozambique", 'MZM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Namibia", 'NAM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Nepal", 'NEP')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Netherlands", 'NTH')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="New Zealand", 'NEW')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Nicaragua", 'NIC')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Nigeria", 'NIG')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Niger", 'NIR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Norway", 'NOR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Oman", 'OMA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Panama", 'PAN')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Pakistan" & data$year < 1972), 'PKS')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Pakistan" & data$year >= 1972), 'PAK')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Papua New Guinea", 'PNG')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Paraguay", 'PAR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Peru", 'PER')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Philippines", 'PHI')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Poland", 'POL')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Portugal", 'POR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Qatar", 'QAT')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Romania", 'RUM')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Russia" |
    data[,namevar]=="Russian Federation"), 'RUS')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Soviet Union" |
    data[,namevar]=="USSR" |
    data[,namevar]=="USSR(Soviet Union)" |
    data[,namevar]=="U.S.S.R."), 'USS')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Rwanda", 'RWA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Sao Tome and Principe", 'STP')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Saudi Arabia", 'SAU')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Senegal", 'SEN')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Serbia" |
    (data[,namevar]=="Yugoslavia" & data$year > 2005)), 'SRB')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Sierra Leone", 'SIE')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Singapore", 'SIN')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Slovakia" |
    data[,namevar]=="Slovak Republic"), 'SLO')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Slovenia", 'SLV')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Solomon Islands", 'SOL')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Somalia", 'SOM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="South Africa", 'SAF')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Spain", 'SPN')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Sri Lanka", 'SRI')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Sudan", 'SUD')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="South Sudan", 'SSD')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Suriname", 'SUR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Swaziland", 'SWA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Sweden", 'SWD')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Switzerland", 'SWZ')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Syria" |
    data[,namevar]=="Syrian Arab Republic"), 'SYR')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Taiwan" |
    data[,namevar]=="Taiwan Province of China" |
    data[,namevar]=="Taiwan, China"), 'TAW')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Tajikistan", 'TAJ')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Tanzania", 'TAZ')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Thailand", 'THI')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Togo", 'TOG')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Trinidad" |
    data[,namevar]=="Trinidad and Tobago" |
    data[,namevar]=="Trinidad & Tobago"), 'TRI')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Tunisia", 'TUN')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Turkey", 'TUR')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Turkmenistan", 'TKM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Uganda", 'UGA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Ukraine", 'UKR')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="United Arab Emirates" |
    data[,namevar]=="UAE"), 'UAE')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="United Kingdom", 'UKG')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="United States" |
    data[,namevar]=="United States of America"), 'USA')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Uruguay", 'URU')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Uzbekistan", 'UZB')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Venezuela" |
    data[,namevar]=="Venezuela, RB"), 'VEN')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Vietnam" |
    data[,namevar]=="Viet Nam" |
    data[,namevar]=="Vietnam, Socialist Republic of"), 'VIE')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="North Vietnam" |
    data[,namevar]=="Democratic Republic of Vietnam" |
    data[,namevar]=="DRVN" |
    data[,namevar]=="Vietnam, North" |
    data[,namevar]=="Vietnam North" |
    data[,namevar]=="Vietnam, N."), 'DRV')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="South Vietnam" |
    data[,namevar]=="Republic of Vietnam" |
    data[,namevar]=="Vietnam, South" |
    data[,namevar]=="Vietnam South" |
    data[,namevar]=="Vietnam, S."), 'RVN')
  data$sftgcode <- replace(data$sftgcode, which((data[,namevar]=="Yemen Arab Republic" & data$year < 1990) |
    data[,namevar]=="North Yemen" |
    data[,namevar]=="Yemen, North" |
    data[,namevar]=="Yemen North" |
    data[,namevar]=="Yemen, N." |
    data[,namevar]=="Yemen Arab Republic; N. Yemen"), 'YAR')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Yemen" |
    data[,namevar]=="Yemen, Rep." |
    (data[,namevar]=="Yemen Arab Republic" & data$year >= 1990)), 'YEM')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="South Yemen" |
    data[,namevar]=="Yemen, South" |
    data[,namevar]=="Yemen South" |
    data[,namevar]=="Yemen People's Republic" |
    data[,namevar]=="SouthYemen" |
    data[,namevar]=="Yemen, S." |
    data[,namevar]=="Yemen People's Republic; S. Yemen" |
    data[,namevar]=="Yemen PDR (South)"), 'YPR')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Yugoslavia, FPR" |
    (data[,namevar]=="Yugoslavia" & data$year < 1992) |
    (data[,namevar]=="Yugoslavia, Fed. Rep." & data$year < 1992) |
    (data[,namevar]=="Federal Republic of Yugoslavia" & data$year < 1992) |
    (data[,namevar]=="Serbia and Montenegro" & data$year < 1992) |
    (data[,namevar]=="Former Yugoslavia" & data$year < 1992)), 'YUG')
  data$sftgcode <- replace(data$sftgcode, which(data[,namevar]=="Yugoslavia, Federal Republic of" |
    data[,namevar]=="Yugoslavia (Serbia & Montenegro)" |
    data[,namevar]=="Yugoslavia, FR (Serbia/Montenegro)" |
    (data[,namevar]=="Yugoslavia" & data$year >= 1992 & data$year <= 2005) |
    (data[,namevar]=="Yugoslavia, Fed. Rep." & data$year >= 1992) |
    (data[,namevar]=="Federal Republic of Yugoslavia" & data$year >= 1992) |
    (data[,namevar]=="Serbia and Montenegro" & data$year >= 1992) |
    (data[,namevar]=="Former Yugoslavia" & data$year >= 1992)), 'YGS')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Zambia", 'ZAM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Zimbabwe", 'ZIM')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Kosovo", 'KOS')
  data$sftgcode <- replace(data$sftgcode, data[,namevar]=="Palestinian Authority" |
    data[,namevar]=="Israel, occupied territories only", 'PAL')
  return(data)
}

