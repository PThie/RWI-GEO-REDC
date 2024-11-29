cleaning_org_data <- function(
    housing_data = NA
) {
    #' @title Cleaning original data
    #' 
    #' @description This function performs first cleaning steps. 
    #' Many steps are similar to the parent data set RWI-GEO-RED.
    #' 
    #' @param housing_data Raw housing data
    #' 
    #' @return DataFrame
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # cleaning
    
    # make all names lowercase
    names(housing_data) <- tolower(names(housing_data))

    # define REDC delivery as variable
    # NEEDED? with current_delivery_counter
    # if (config_globals()[["current_delivery"]] == "Lieferung_23121") {
    #     # NOTE: Lieferung_23121 was delivered later than Lieferung_2312 but
    #     # still belongs to the same wave
    #     del <- "Lieferung_2312"
    # } else {
    #     del <- config_globals()[["current_delivery"]]
    # }

    # apply cleaning steps
    housing_data_prep <- housing_data |>
        dplyr::mutate(
            #----------------------------------------------
            # add current version and delivery (more for internal documentation)
            redc_version = config_globals()[["current_version"]],
            redc_delivery = config_globals()[["current_delivery_counter"]],
            #----------------------------------------------
            # split date variable
            # starting year and month
            ajahr = as.numeric(substring(zeitraum, first = 1, last = 4)),
            amonat = as.integer(substring(zeitraum, first = 5, last = 7)),
            # end year and month
            ejahr = as.numeric(substring(zeitraum, first = 12, last = 15)),
            emonat = as.integer(substring(zeitraum, first = 16, last = 17)),
            #----------------------------------------------
            # delivery 2312 has two variables referencing the energy class
            # merge both
            energieeffizienzklasse = dplyr::case_when(
                energieeffizienzklasse == "" ~ NA_character_,
                TRUE ~ energieeffizienzklasse
            ),
            # energieeffizienz_klasse = dplyr::case_when(
            #     energieeffizienz_klasse == "" ~ NA_character_,
            #     TRUE ~ energieeffizienz_klasse
            # ),
            # energieeffizienzklasse := data.table::fcoalesce(
            #     energieeffizienzklasse,
            #     energieeffizienz_klasse
            # ),
            # energieeffizienz_klasse = NULL,
            #----------------------------------------------
            # fix housing type (remove Umlaute)
            immobilientyp = stringi::stri_trans_general(
                immobilientyp,
                "de-ASCII; Latin-ASCII"
            ),
            # add separator for two-word types
            immobilientyp = stringr::str_replace_all(
                immobilientyp,
                stringr::fixed(" "),
                "_"
            ),
            #----------------------------------------------
            # fix anbietertyp (remove Umlaute)
            anbietertyp = stringi::stri_trans_general(
                anbietertyp,
                "de-ASCII; Latin-ASCII"
            ),
            # redefine anbieter
            anbieter = dplyr::case_when(
                anbietertyp == "Privatanbieter" ~ 1,
                anbietertyp == "Makler" ~ 2,
                anbietertyp == "Wohnungswirtschaft" ~ 3,
                anbietertyp == "Bautraeger" ~ 4,
                anbietertyp == "Finanzsektor" ~ 5,
                anbietertyp == "Gewerbeanbieter" ~ 6,
                anbietertyp == "Hausbau" ~ 7,
                anbietertyp == "Umzug" ~ 8,
                anbietertyp == "unbekannt" ~ 9
            ),
            anbieter = as.integer(anbieter),
            #----------------------------------------------
            # fix objektkategorie2 (remove Umlaute)
            objektkategorie2 = stringi::stri_trans_general(
                objektkategorie2,
                "de-ASCII; Latin-ASCII"
            ),
            #----------------------------------------------
            # replace NAs in befeuerungsarten
            befeuerungsarten = dplyr::case_when(
                befeuerungsarten == "" ~ NA_character_,
                TRUE ~ befeuerungsarten
            ),
            # generate bef1 which contains the firing type if there is only one
            # given
            # NOTE: cannot be done in one step because of the warning issue
            # "insert NAs because of coercion to numeric" which crashes the
            # pipeline
            bef1 = dplyr::case_when(
                stringr::str_detect(befeuerungsarten, "|") ~ NA_character_,
                TRUE ~ befeuerungsarten
            ),
            bef1 = as.numeric(bef1),
            # generate help variable which is missing when bef1 has a value
            bef_help = dplyr::case_when(
                !is.na(bef1) ~ NA,
                TRUE ~ befeuerungsarten
            ),
            # replace help variable with splitted values (by "|")
            # output column will be of type list
            bef_help = stringi::stri_split_fixed(bef_help, "|"),
            # get the number of times "|" occured
            bef_count = stringi::stri_count_fixed(befeuerungsarten, "|"),
            #----------------------------------------------
            # bauphase
            bauphase = dplyr::case_when(
                bauphase == "Keine Angabe" ~ as.character(
                    helpers_missing_values()[["not_specified"]]
                ),
                TRUE ~ bauphase
            ),
            #----------------------------------------------
            # pets
            haustier_erlaubt = dplyr::case_when(
                haustier_erlaubt == "Keine Angabe" ~ as.character(
                    helpers_missing_values()[["not_specified"]]
                ),
                TRUE ~ haustier_erlaubt
            ),
            #----------------------------------------------
            # heating costs in rent
            heizkosten_in_wm_enthalten = dplyr::case_when(
                heizkosten_in_wm_enthalten == "null" ~ as.character(
                    helpers_missing_values()[["not_specified"]]
                ),
                heizkosten_in_wm_enthalten == "true" ~ "1",
                heizkosten_in_wm_enthalten == "false" ~ "0",
                TRUE ~ heizkosten_in_wm_enthalten
            )
        ) |>
        # split bef_help column into separate columns and generate new names
        tidyr::unnest_wider(bef_help, names_sep = "_") |>
        # set as dataframe
        as.data.frame()

    #--------------------------------------------------
    # check that anbieter is correctly recoded, i.e. has not more categories
    # than defined in recoding

    targets::tar_assert_true(
        length(unique(housing_data_prep$anbieter)) == 9,
        msg = glue::glue(
            "!!! WARNING:
            Variable anbieter has more categories than defined in recoding.",
            "(Error code: cod#1)"
        )
    )

    #----------------------------------------------
    # rename columns "bef" (befeuerungsarten)

    # get the max amount of potential splits based on delimiter "|"
    max_split <- max(housing_data_prep$bef_count, na.rm = TRUE)

    # extract original names (names to leave unchanged)
    # exception of bef1
    org_names <- housing_data_prep |>
        dplyr::select(!contains("bef_help") & !contains("bef_count")) |>
        names()

    # generate new names
    # why plus 2: because we start at 2 (bef1 already exists) and the first
    # split breaks into two pieces
    new_names <- c()
    for (i in seq(2, max_split + 2)) {
        nam <- paste0("bef", i)
        # update empty list of names
        new_names <- c(new_names, nam)
    }

    # assign new names
    names(housing_data_prep) <- c(org_names, new_names, "bef_count")

    #----------------------------------------------
    # drop unnecessary columns

    housing_data_prep <- housing_data_prep |>
        dplyr::select(-c(
            "bef_count", # auxiliary variable from previous step
            "befeuerungsarten", # transformed to bef1, bef2, ...
            "anbietertyp", # tranformed to anbieter
            "zeitraum", # transformed to ajahr, amonat, ejahr, emonat
            #----------------------------------------------
            # other
            "objektkategorie2id", # removed because recode objektkategorie2
        ))

    #----------------------------------------------
    # variables that are potentially in the data set because the data were
    # delivered with the residential data together
    # remove those variables

    # check that these variables are "empty", i.e. only one value like NA or
    # "not used anymore"
    for (var in helpers_deleted_variables()) {
        # NOTE: these variables are dropped for other reasons (see
        # helpers_deleted_variables) and are, therefore, excluded from checking
        if (!var %in% c(
            "is24_stadt_kreis", "kreis", "is24_bezirk_gemeinde",
            "is24_bundesland", "skid", "bgid", "objekt_beschreibung",
            "einstelldatum"
        )) {
            targets::tar_assert_true(
                # length(unique(housing_data_prep[[var]])) == 1,
                all(unique(housing_data_prep[[var]]) %in% c(
                    NA, "nicht mehr existent"
                )),
                msg = glue::glue(
                    "!!! WARNING:
                    Variable {var} has more than one value.",
                    "(Error code: cod#2)"
                )
            )
        }
    }

    # remove variables
    for (var in helpers_deleted_variables()) {
        if (var %in% names(housing_data_prep)) {
            housing_data_prep[[var]] <- NULL
        }
    }

    #----------------------------------------------
    # recode Immo's missing observations (-1) to our missings (-9)

    housing_data_prep[
        housing_data_prep == helpers_missing_values()[["immo_missing"]]
    ] <- helpers_missing_values()[["other"]]

    #----------------------------------------------
    # fix dummy variables (Yes or No variables)

    # retrieve logical variables
    log_col <- c()
    for (col in names(housing_data_prep)) {
        if(typeof(housing_data_prep[[col]]) == "logical") {
            log_col <- c(log_col, col)
        }
    }

    # check that all possible values are considered in the recoding
    for (col in log_col) {
        # get unique values
        unique_values <- unique(housing_data_prep[[col]])

        targets::tar_assert_true(
            all(unique_values %in% c(
                TRUE, FALSE, NA, "Keine Angabe", "keine Angabe", "keine Angaben",
                "nicht mehr existent"
            )),
            msg = glue::glue(
                "!!! WARNING:
                Variable {col} has values that are not considered in the recoding.",
                "(Error code: cod#3)"
            )
        )
    }

    # recode logical variables
    housing_data_prep <- housing_data_prep |>
        dplyr::mutate(
            dplyr::across(
                .cols = all_of(log_col),
                ~ dplyr::case_when(
                    .x == TRUE ~ 1,
                    .x == FALSE ~ 0,
                    .x == "nicht mehr existent" ~ helpers_missing_values()[["not_used_anymore"]],
                    (.x == "Keine Angabe" | 
                        .x == "keine Angabe" | 
                        .x == "keine Angaben"
                        ) ~ helpers_missing_values()[["not_specified"]],
                    TRUE ~ helpers_missing_values()[["other"]]
                )
            )
        )

    #--------------------------------------------------
    # rename ausstattungsqualitaet if exists

    if ("ausstattungsqualitaet" %in% names(housing_data_prep)) {
        housing_data_prep <- housing_data_prep |>
            dplyr::rename(
                ausstattung = ausstattungsqualitaet
            )
    }

    #----------------------------------------------
    # recode categorical variables

    housing_data_prep <- housing_data_prep |>
        dplyr::mutate(
            ausstattung = dplyr::case_when(
                ausstattung == "Einfach" ~ 1,
                ausstattung == "Normal" ~ 2,
                ausstattung == "Gehoben" ~ 3,
                ausstattung == "Luxus" ~ 4,
                (ausstattung == "Keine Angabe" |
                    ausstattung == "keine Angabe" |
                    ausstattung == "keine Angaben"
                    ) ~ helpers_missing_values()[["not_specified"]],
                TRUE ~ helpers_missing_values()[["other"]]
            ),
            energieeffizienzklasse = dplyr::case_when(
                energieeffizienzklasse == "A_PLUS" ~ 1,
                energieeffizienzklasse == "A" ~ 2,
                energieeffizienzklasse == "B" ~ 3,
                energieeffizienzklasse == "C" ~ 4,
                energieeffizienzklasse == "D" ~ 5,
                energieeffizienzklasse == "E" ~ 6,
                energieeffizienzklasse == "F" ~ 7,
                energieeffizienzklasse == "G" ~ 8,
                energieeffizienzklasse == "H" ~ 9,
                (energieeffizienzklasse == "Keine Angabe" |
                    energieeffizienzklasse == "keine Angabe" |
                    energieeffizienzklasse == "keine Angaben"
                    ) ~ helpers_missing_values()[["not_specified"]],
                TRUE ~ helpers_missing_values()[["other"]]
            ),
            energieausweistyp = dplyr::case_when(
                energieausweistyp == "Endenergiebedarf" ~ 1,
                energieausweistyp == "Energieverbrauchskennwert" ~ 2,
                (energieausweistyp == "Keine Angabe" |
                    energieausweistyp == "keine Angabe" |
                    energieausweistyp == "keine Angaben"
                    ) ~ helpers_missing_values()[["not_specified"]],
                TRUE ~ helpers_missing_values()[["other"]]
            ),
            heizungsart = dplyr::case_when(
                heizungsart == "Blockheizkraftwerke" ~ 1,
                heizungsart == "Elektro-Heizung" ~ 2,
                heizungsart == "Etagenheizung" ~ 3,
                heizungsart == "Fernwaerme" ~ 4,
                heizungsart == "Fussbodenheizung" ~ 5,
                heizungsart == "Gas-Heizung" ~ 6,
                heizungsart == "Holz-Pelletheizung" ~ 7,
                heizungsart == "Nachtspeicheroefen" ~ 8,
                heizungsart == "Ofenheizung" ~ 9,
                heizungsart == "Oel-Heizung" ~ 10,
                heizungsart == "Solar-Heizung" ~ 11,
                heizungsart == "Waermepumpe" ~ 12,
                heizungsart == "Zentralheizung" ~ 13,
                (heizungsart == "Keine Angabe" |
                    heizungsart == "keine Angabe" |
                    heizungsart == "keine Angaben"
                    ) ~ helpers_missing_values()[["not_specified"]],
                TRUE ~ helpers_missing_values()[["other"]]
            ),
            kategorie_business = dplyr::case_when(
                objektkategorie2 == "Buero- und Geschaeftsgebaeude" ~ 1,    
                objektkategorie2 == "Laden" ~ 2,                         
                objektkategorie2 == "Wohn- und Geschaeftsgebaeude" ~ 3,  
                objektkategorie2 == "Verkaufsflaeche" ~ 4,
                objektkategorie2 == "Buerohaus" ~ 5,           
                objektkategorie2 == "Bueroetage" ~ 6,                 
                objektkategorie2 == "Spezialobjekt" ~ 7,                
                objektkategorie2 == "Buerozentrum" ~ 8,                
                objektkategorie2 == "Ausstellungsflaeche" ~ 9,            
                objektkategorie2 == "Restaurant" ~ 10,       
                objektkategorie2 == "Anwesen" ~ 11,              
                objektkategorie2 == "Einkaufszentrum" ~ 12,              
                objektkategorie2 == "Café" ~ 13,         
                objektkategorie2 == "Gaestehaus" ~ 14,                  
                objektkategorie2 == "Freizeitanlage" ~ 15,                
                objektkategorie2 == "Gewerbezentrum" ~ 16,                
                objektkategorie2 == "Hotel" ~ 17,                
                objektkategorie2 == "Barbetrieb/Lounge" ~ 18,            
                objektkategorie2 == "Reiterhof" ~ 19,                     
                objektkategorie2 == "Lager mit Freiflaeche" ~ 20,          
                objektkategorie2 == "Speditionslager" ~ 21,               
                objektkategorie2 == "Hochregallager" ~ 22,                
                objektkategorie2 == "Club/Diskothek" ~ 23,                
                objektkategorie2 == "Kuehlhaus" ~ 24,                      
                objektkategorie2 == "Kaufhaus" ~ 25,                      
                objektkategorie2 == "Factory Outlet" ~ 26,                
                objektkategorie2 == "Ferienbungalows" ~ 27,               
                objektkategorie2 == "Praxis" ~ 28,
                objektkategorie2 == "Buero" ~ 29,
                objektkategorie2 == "Halle" ~ 30,
                objektkategorie2 == "Industriehalle" ~ 31,
                objektkategorie2 == "Buero-/ Lagergebaeude" ~ 32,
                objektkategorie2 == "Gewerbeflaeche" ~ 33,
                objektkategorie2 == "Lagerflaeche" ~ 34,
                objektkategorie2 == "Lagerhalle" ~ 35,
                objektkategorie2 == "Werkstatt" ~ 36,
                objektkategorie2 == "Serviceflaeche" ~ 37,
                objektkategorie2 == "Gaststaette" ~ 38,
                objektkategorie2 == "Industriehalle mit Freiflaeche" ~ 39,
                objektkategorie2 == "Loft" ~ 40,
                objektkategorie2 == "SB-Markt" ~ 41,
                objektkategorie2 == "Praxisetage" ~ 42,
                objektkategorie2 == "Pension" ~ 43,
                objektkategorie2 == "Atelier" ~ 44,
                objektkategorie2 == "Verkaufshalle" ~ 45,
                objektkategorie2 == "Praxishaus" ~ 46,
                objektkategorie2 == "Kiosk" ~ 47,
                objektkategorie2 == "Hotelanwesen" ~ 48,
                objektkategorie2 == "Gewerbepark" ~ 49,
                objektkategorie2 == "Bauernhof" ~ 50,
                objektkategorie2 == "Hotel garni" ~ 51,
                objektkategorie2 == "Kuehlregallager" ~ 52,
                objektkategorie2 == "Weingut" ~ 53,
                (objektkategorie2 == "Keine Angabe" |
                    objektkategorie2 == "keine Angabe" |
                    objektkategorie2 == "keine Angaben"
                    ) ~ helpers_missing_values()[["not_specified"]],
                TRUE ~ helpers_missing_values()[["other"]]
            ),
            objektzustand = dplyr::case_when(
                objektzustand == "Erstbezug" ~ 1,
                objektzustand == "Erstbezug nach Sanierung" ~ 2,
                objektzustand == "Neuwertig" ~ 3,
                objektzustand == "Saniert" ~ 4,
                objektzustand == "Modernisiert" ~ 5,
                objektzustand == "Vollstaendig Renoviert" ~ 6,
                objektzustand == "Gepflegt" ~ 7,
                objektzustand == "Renovierungsbeduerftig" ~ 8,
                objektzustand == "Nach Vereinbarung" ~ 9,
                objektzustand == "Abbruchreif" ~ 10,
                (objektzustand == "Keine Angabe" |
                    objektzustand == "keine Angabe" |
                    objektzustand == "keine Angaben"
                    ) ~ helpers_missing_values()[["not_specified"]],
                TRUE ~ helpers_missing_values()[["other"]]
            ),
            immobilientyp = dplyr::case_when(
                immobilientyp == "Buero_Praxis" ~ 1,
                immobilientyp == "Einzelhandel" ~ 2,
                immobilientyp == "Hallen_Produktion" ~ 3,
                immobilientyp == "Spezialgewerbe" ~ 4,
                immobilientyp == "Gastronomie_Hotel" ~ 5,
                (immobilientyp == "Keine Angabe" |
                    immobilientyp == "keine Angabe" |
                    immobilientyp == "keine Angaben"
                    ) ~ helpers_missing_values()[["not_specified"]],
                TRUE ~ helpers_missing_values()[["other"]]
            )
        ) |>
        # remove objektkategorie because its an integer in kategorie_business now
        dplyr::select(-objektkategorie2)

    #--------------------------------------------------
    # checks for recoding
    # if variables have the same values, they are recoded
    # NOTE: use original data for this check (i.e. without recoding)

    # function to avoid repetition
    recoding_length_check <- function(var, expected_unique_values) {
        #' @description Function to check if a variable has the expected number of
        #' unique values. Otherwise, recoding needs to be adjusted.
        #' 
        #' @param var Name of the variable to be checked.
        #' @param expected_unique_values Expected number of unique values.
        
        #--------------------------------------------------
        # rename some variables to align with naming above

        dta <- housing_data |>
            dplyr::rename(
                ausstattung = ausstattungsqualitaet,
                kategorie_business = objektkategorie2
            )
        #--------------------------------------------------
        targets::tar_assert_true(
            length(unique(dta[[var]])) == expected_unique_values,
            msg = glue::glue(
                "!!! WARNING: ",
                "Variable {var} contains unexpected values. ",
                "Please check the data and recode if necessary.",
                "(Error code: cod#4)"
            )
        )
    }

    # check recoding
    recoding_length_check("ausstattung", 5)
    recoding_length_check("energieeffizienzklasse", 10)
    recoding_length_check("energieausweistyp", 3)
    recoding_length_check("heizungsart", 14)
    recoding_length_check("kategorie_business", 54)
    recoding_length_check("objektzustand", 11)
    recoding_length_check("immobilientyp", 5)

    #----------------------------------------------
    # issue of rent
    # explanation: for commercial data its possible to provide information
    # as rent (mietekalt) + additional expenses (nebenkosten) or to provide
    # rent per square meter (mieteproqm)
    # if mieteproqm was select nebenkosten are also per square meter
    # Potential source of error: User select the mieteproqm option but provide
    # a monthly value
    # See also Email Kehlert 01.08.2023 (store in documentation/infos/Lieferung_2023)

    if ("mieteproqm" %in% names(housing_data_prep)) {
        housing_data_prep <- housing_data_prep |>
            dplyr::rename(
                miete_proqm = mieteproqm
            )
    }

    housing_data_prep <- housing_data_prep |>
        dplyr::mutate(
            # separate nebenkosten from nebenkosten per square meter if "per
            # square meter" option is selected
            nebenkosten_proqm = dplyr::case_when(
                !is.na(miete_proqm) ~ nebenkosten,
                TRUE ~ helpers_missing_values()[["other"]]
            ),
            nebenkosten = dplyr::case_when(
                !is.na(miete_proqm) ~ helpers_missing_values()[["other"]],
                TRUE ~ nebenkosten
            ), 
            # censor implausible values for square meter variables
            # different thresholds compared to below because there are some
            # large outliers that are clearly in "total-terms" and not "per
            # square meter"
            nebenkosten_proqm = dplyr::case_when(
                nebenkosten_proqm <= as.numeric(quantile(
                    nebenkosten_proqm[(nebenkosten_proqm != helpers_missing_values()[["other"]])],
                    prob = 0.1,
                    na.rm = TRUE
                )) ~ helpers_missing_values()[["implausible"]],
                nebenkosten_proqm >= as.numeric(quantile(
                    nebenkosten_proqm[(nebenkosten_proqm != helpers_missing_values()[["other"]])],
                    prob = 0.95,
                    na.rm = TRUE
                )) ~ helpers_missing_values()[["implausible"]],
                TRUE ~ nebenkosten_proqm
            ),
            miete_proqm = dplyr::case_when(
                miete_proqm <= as.numeric(quantile(
                    miete_proqm[(miete_proqm != helpers_missing_values()[["other"]])],
                    prob = 0.1,
                    na.rm = TRUE
                )) ~ helpers_missing_values()[["implausible"]],
                miete_proqm >= as.numeric(quantile(
                    miete_proqm[(miete_proqm != helpers_missing_values()[["other"]])],
                    prob = 0.99,
                    na.rm = TRUE
                )) ~ helpers_missing_values()[["implausible"]],
                TRUE ~ miete_proqm
            )
        )

    #----------------------------------------------
    # censor implausible values

    cols <- c(
        "kaufpreis",
        "grundstuecksflaeche",
        "nutzflaeche",
        "ev_kennwert",
        "mietekalt",
        "nebenkosten",
        "teilbar_ab",
        "nebenkosten"
    )

    housing_data_prep <- housing_data_prep |>
        dplyr::mutate(
            # make sure that the variables are numeric
            dplyr::across(
                .cols = cols,
                ~ as.numeric(.x)
            ),
            dplyr::across(
                .cols = cols,
                # dropping values below the 0.1 percentile and above the 99.9 percentile
                ~ dplyr::case_when(
                    .x <= as.numeric(quantile(
                        .x[(.x != helpers_missing_values()[["other"]])],
                        prob = 0.001,
                        na.rm = TRUE
                    )) ~ helpers_missing_values()[["implausible"]],
                    .x >= as.numeric(quantile(
                        .x[(.x != helpers_missing_values()[["other"]])],
                        prob = 0.999,
                        na.rm = TRUE
                    )) ~ helpers_missing_values()[["implausible"]],
                    TRUE ~ .x
                )
            ),
            letzte_modernisierung = dplyr::case_when(
                # censor if last renovation is in the future
                letzte_modernisierung > config_globals()[["max_year"]] ~ helpers_missing_values()[["implausible"]],
                # censor if last renovation is far back
                letzte_modernisierung >= 0 & letzte_modernisierung < helpers_implausible_values()[["last_renovation_max_value"]] ~ helpers_missing_values()[["implausible"]],
                is.na(letzte_modernisierung) ~ helpers_missing_values()[["other"]],
                TRUE ~ letzte_modernisierung
            ),
            baujahr = dplyr::case_when(
                # censor if construction year is far back (before 1000)
                baujahr >= 0 & baujahr < helpers_implausible_values()[["construction_year_max_value"]] ~ helpers_missing_values()[["implausible"]],
                # censor if construction year is in the future
                baujahr > config_globals()[["max_year"]] ~ helpers_missing_values()[["implausible"]],
                is.na(baujahr) ~ helpers_missing_values()[["other"]],
                TRUE ~ baujahr
            ),
            plz = dplyr::case_when(
                # censor if zip code is not complete
                nchar(plz) == 4 ~ as.character(
                    helpers_missing_values()[["implausible"]]
                ),
                TRUE ~ plz
            )
        )

    #--------------------------------------------------
    # checks

    # check that plz only has three different character lengths
    # otherwise the recoding is incomplete
    # NOTE: check based on raw data since the recoding is already applied for
    # prepared data
    targets::tar_assert_true(
        length(unique(nchar(housing_data$plz))) == 3,
        msg = glue::glue(
            "!!! WARNING:
            Variable plz contains unexpected values. ",
            "Please check the data and recode if necessary.",
            "(Error code: cod#5)"
        )
    )
    
    #--------------------------------------------------
    # fix other things

    housing_data_prep <- housing_data_prep |>
        dplyr::mutate(
            # in few cases there is a rent and a price given
            # if they are equal its clearly a mistake (set to missing)
            kaufpreis = dplyr::case_when(
                mietekalt == kaufpreis ~ helpers_missing_values()[["other"]],
                TRUE ~ kaufpreis
            ),
            # fix zip code
            # for a few observations there is a "D-" in front of the zip code
            # e.g. "D-58095"
            plz = dplyr::case_when(
                stringr::str_detect(plz, "D-") ~ stringr::str_replace_all(plz, "D-", ""),
                TRUE ~ plz
            ),
            # remove Umlaute
            dplyr::across(
                .cols = c("freiab", "strasse", "courtage", "mietekaution"),
                ~ stringi::stri_trans_general(.x, "de-ASCII; Latin-ASCII")
            )
        ) 

    #----------------------------------------------
    # fix etage
    # problem: sometimes there is a specific number given but sometimes
    # there is also a range given
    # fix the specific numbers (i.e. where it is clear which floor) but
    # keep the ranges (researcher has to decide what this should be)
    housing_data_prep <- housing_data_prep |>
        dplyr::mutate(
            # replace some common strings
            # this helps with the next step to turn them into numbers
            etage = dplyr::case_when(
                stringr::str_detect(etage, ". UG") ~ 
                    stringr::str_replace_all(etage, ". UG", ""),
                stringr::str_detect(etage,".OG") ~
                    stringr::str_replace_all(etage, ".OG", ""),
                stringr::str_detect(etage, ". OG") ~
                    stringr::str_replace_all(etage, ". OG", ""),
                stringr::str_detect(etage, " OG") ~
                    stringr::str_replace_all(etage, " OG", ""),
                stringr::str_detect(etage, ". Obergeschoss") ~
                    stringr::str_replace_all(etage, ". Obergeschoss", ""),
                stringr::str_detect(etage, ". Obergeschoss") ~
                    stringr::str_replace_all(etage, ".Obergeschoss", ""),
                stringr::str_detect(etage, ",00") ~
                    stringr::str_replace_all(etage, ",00", ""),
                stringr::str_detect(etage, ". links") ~
                    stringr::str_replace_all(etage, ". links", ""),
                stringr::str_detect(etage, ". rechts") ~
                    stringr::str_replace_all(etage, ". rechts", ""),
                stringr::str_detect(etage, ". Ebene") ~
                    stringr::str_replace_all(etage, ". Ebene", ""),
                stringr::str_detect(etage, " Etage") ~
                    stringr::str_replace_all(etage, " Etage", ""),
                stringr::str_detect(etage, " rechts") ~
                    stringr::str_replace_all(etage, " rechts", ""),
                stringr::str_detect(etage, " Stock") ~
                    stringr::str_replace_all(etage, " Stock", ""),
                stringr::str_detect(etage, " links") ~
                    stringr::str_replace_all(etage, " links", ""),
                etage == "*" ~ NA,
                TRUE ~ etage
            ),
            # turn into numbers
            etage_clean = as.numeric(etage),
            # fix some special cases
            etage_clean = dplyr::case_when(
                (etage == "UG" |
                    etage == "Keller" |
                    etage == "Untergeschoss"
                    ) ~ -1,
                (etage == "Erdgeschoss" | 
                    etage == "EG" |
                    etage == "EG " | 
                    etage == "Erdgeschoß" |
                    etage == "EG." |
                    etage == "ebenerdig" |
                    etage == "Erdgeschos" |
                    etage == "Erdgeschossß" |
                    etage == "Ebenerdig" |
                    etage == "Erd"
                    ) ~ 0,
                (etage == "1. Etage" |
                    etage == "OG"
                    ) ~ 1,
                (etage == "II." |
                    etage == "II"
                    ) ~ 2,
                TRUE ~ etage_clean
            ),
            # replace etage with NA with clean value could have been identified
            etage = dplyr::case_when(
                !is.na(etage_clean) ~ NA,
                etage == "" ~ NA,
                TRUE ~ etage
            ),
            # bring both columns together
            # assuming that the rest could not have been cleaned (since
            # mostly there is a range given; the researcher has to decide
            # what to do with that)
            etage = dplyr::case_when(
                !is.na(etage_clean) ~ as.character(etage_clean),
                TRUE ~ etage
            )
        ) |>
        # remove helper variable
        dplyr::select(-etage_clean)

    #----------------------------------------------
    # setting correct types
    # sometimes it is already correct by forcing it here again

    # integer columns
    bef_cols <- housing_data_prep |>
        dplyr::select(dplyr::starts_with("bef")) |>
        names()
    
    int_cols <- c(
        "obid", "version", "koid", "laid", "skid_id", "sc_id",
        "anbieter", "duplicateid", "letzte_modernisierung",
        "baujahr", "blid", "immobilientyp", "objektzustand", "ausstattung",
        "heizungsart", "energieausweistyp", "energieeffizienzklasse",
        "ejahr", "emonat", "ajahr", "amonat", "kategorie_business", 
        "laufzeittage", "gkz", "heizkosten_in_wm_enthalten",
        "bauphase", "kaufvermietet", "haustier_erlaubt", bef_cols
    )

    # numeric columns
    num_cols <- c(
        "grundstuecksflaeche", "nutzflaeche", "wohnflaeche", "zimmeranzahl",
        "kaufpreis", "mietekalt", "nebenkosten", "geox", "geoy", "miete_proqm",                                 
        "teilbar_ab", "nebenkosten_proqm", "ev_kennwert", "heizkosten",
        "mietewarm", "hits", "click_schnellkontakte", "liste_show", "liste_match",
        "click_weitersagen", "click_url"  
    )

    # character columns
    char_cols <- c(
        "freiab", "courtage", "mietekaution", "plz", "ort",
        "strasse", "hausnr", "redc_version", "redc_delivery", "etage"
    )

    for (col in int_cols) {
        if (col %in% names(housing_data_prep)) {
            housing_data_prep[[col]] <- as.integer(housing_data_prep[[col]])
        } else {
            housing_data_prep[[col]] <- helpers_missing_values()[["other"]]
        }
    }

    for (col in num_cols) {
        if (col %in% names(housing_data_prep)) {
            housing_data_prep[[col]] <- as.numeric(housing_data_prep[[col]])
        } else {
            housing_data_prep[[col]] <- helpers_missing_values()[["other"]]
        }
    }

    for (col in char_cols) {
        if (col %in% names(housing_data_prep)) {
            housing_data_prep[[col]] <- as.character(housing_data_prep[[col]])
        } else {
            housing_data_prep[[col]] <- as.character(helpers_missing_values()[["other"]])
        }
    }

    #----------------------------------------------
    # replace missings according to type
    # NOTE: serves as check if not already recoded befor

    housing_data_prep <- housing_data_prep |>
        dplyr::mutate_if(
            is.integer,
            replace_na,
            replace = helpers_missing_values()[["other"]]
        ) |>
        dplyr::mutate_if(
            is.numeric,
            replace_na,
            replace = helpers_missing_values()[["other"]]
        )  |>
        dplyr::mutate_if(
            is.character,
            replace_na,
            replace = as.character(
                helpers_missing_values()[["other"]]
            )
        )

    #----------------------------------------------
    # export

    fst::write.fst(
        housing_data_prep,
        file.path(
            config_paths()[["data_path"]],
            "processed",
            config_globals()[["current_delivery"]],
            "clean_data.fst"
        )
    )

    #----------------------------------------------
    # return

    return(housing_data_prep)
}