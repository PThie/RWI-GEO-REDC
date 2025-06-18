helpers_original_categories <- function() {
    #' @title Original Categories
    #' 
    #' @description This function lists the original categories of different
    #' categorical variables used in the dataset. It is used different checks to
    #' guarantee that the recoding of the variables is correct and no category
    #' # is lost.
    #' 
    #' @return List of vectors, each vector contains the original categories
    #' for a specific variable.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # list categories
    # NOTE: If the categories are not exactly written as in the original
    # dataset, the checks will fail. This includes Umlaute.

    categories <- list(
        "anbieter" = c(
            "",
            "Privatanbieter",
            "Makler",
            "Wohnungswirtschaft",
            "Bautraeger",
            "Finanzsektor",
            "Gewerbeanbieter",
            "Hausbau",
            "Umzug",
            "unbekannt"
        ),
        "ausstattung" = c(
            "Einfach",
            "Normal",
            "Gehoben",
            "Luxus",
            helpers_missing_values()[["not_specified_variants"]]
        ),
        "energieeffizienzklasse" = c(
            "A_PLUS",
            "A",
            "B",
            "C",
            "D",
            "E",
            "F",
            "G",
            "H",
            helpers_missing_values()[["not_specified_variants"]]
        ),
        "energieausweistyp" = c(
            "Endenergiebedarf",
            "Energieverbrauchskennwert",
            helpers_missing_values()[["not_specified_variants"]]
        ),
        "heizungsart" = c(
            "Blockheizkraftwerke",
            "Elektro-Heizung",
            "Etagenheizung",
            "Fernwaerme",
            "Fussbodenheizung",
            "Gas-Heizung",
            "Holz-Pelletheizung",
            "Nachtspeicheroefen",
            "Ofenheizung",
            "Oel-Heizung",
            "Solar-Heizung",
            "Waermepumpe",
            "Zentralheizung",
            helpers_missing_values()[["not_specified_variants"]]
        ),
        "kategorie_business" = c(
            "Buero- und Geschaeftsgebaeude",
            "Laden",
            "Wohn- und Geschaeftsgebaeude",
            "Verkaufsflaeche",
            "Buerohaus",
            "Bueroetage",
            "Spezialobjekt",
            "Buerozentrum",
            "Ausstellungsflaeche",
            "Restaurant",
            "Anwesen",
            "Einkaufszentrum",
            "Café",
            "Cafe",
            "Gaestehaus",
            "Freizeitanlage",
            "Gewerbezentrum",
            "Hotel",
            "Barbetrieb/Lounge",
            "Reiterhof",
            "Lager mit Freiflaeche",
            "Speditionslager",
            "Hochregallager",
            "Club/Diskothek",
            "Kuehlhaus",
            "Kaufhaus",
            "Factory Outlet",
            "Ferienbungalows",
            "Praxis",
            "Buero",
            "Halle",
            "Industriehalle",
            "Buero-/ Lagergebaeude",
            "Gewerbeflaeche",
            "Lagerflaeche",
            "Lagerhalle",
            "Werkstatt",
            "Serviceflaeche",
            "Gaststaette",
            "Industriehalle mit Freiflaeche",
            "Loft",
            "SB-Markt",
            "Praxisetage",
            "Pension",
            "Atelier",
            "Verkaufshalle",
            "Praxishaus",
            "Kiosk",
            "Hotelanwesen",
            "Gewerbepark",
            "Bauernhof",
            "Hotel garni",
            "Kuehlregallager",
            "Weingut",
            helpers_missing_values()[["not_specified_variants"]]
        ),
        objektzustand = c(
            "Erstbezug",
            "Erstbezug nach Sanierung",
            "Neuwertig",
            "Saniert",
            "Modernisiert",
            "Vollstaendig Renoviert",
            "Gepflegt",
            "Renovierungsbeduerftig",
            "Nach Vereinbarung",
            "Abbruchreif",
            helpers_missing_values()[["not_specified_variants"]]
        ),
        immobilientyp = c(
            "Buero_Praxis",
            "Buero Praxis",
            "Einzelhandel",
            "Hallen_Produktion",
            "Hallen Produktion",
            "Spezialgewerbe",
            "Gastronomie_Hotel",
            "Gastronomie Hotel",
            helpers_missing_values()[["not_specified_variants"]]
        )
    )

    #--------------------------------------------------
    # return

    return(categories)
}
