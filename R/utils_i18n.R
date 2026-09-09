# Internationalisation (i18n) of user-facing strings ---------------------------
#
# Default language is English ("en"); set options(fe_lang = "de") for German.
# FeNEUShiny sets this option from its language switch; no function signatures
# change. German source strings use \u escapes for umlauts to keep R files
# ASCII-clean (R CMD check "non-ASCII characters").


#' Internal lookup table of translated user-facing strings
#'
#' One named list per supported language; the names (keys) are identical across
#' languages. Values may contain \code{sprintf} placeholders (\code{\%s}).
#'
#' @keywords internal
#' @noRd
.fe_strings <- list(

  de = list(

    # --- progress phase labels (progressr) ------------------------------------
    progress_read     = "Lese Rohdaten",
    progress_validate = "Validiere Daten",
    progress_build    = "Erzeuge Ausgabedateien",
    progress_write    = "Schreibe Dateien",
    pre_write_ok      = "Vorprozessierte Daten erfolgreich geschrieben: %d Datei(en) nach %s.",
    pre_write_fail    = "Vorprozessierte Daten wurden NICHT geschrieben (Import fehlgeschlagen; siehe report$errors).",
    progress_process  = "Verarbeite Daten",

    # --- progress phase labels: repeated-survey increment ----------------------
    progress_inc_match       = "Verkn\u00fcpfe die beiden Inventuren",
    progress_inc_fill        = "Sch\u00e4tze Zuw\u00e4chse zur L\u00fcckenf\u00fcllung (BWI 3)",
    progress_inc_consolidate = "Konsolidiere die Baumzuw\u00e4chse",
    progress_inc_extend      = "Erg\u00e4nze Merkmale f\u00fcr die Aggregation",
    progress_inc_second_only = "Erg\u00e4nze die Punkte der Zweitaufnahme",
    progress_inc_summaric    = "Berechne den summarischen Zuwachs",
    progress_inc_overview    = "Aggregiere auf Betriebsebene",

    # --- import: circle / radius arguments ------------------------------------
    circle_args_len_mismatch = "Die BHD-Untergrenzen und die Kreisradien m\u00fcssen gleich lang sein",
    cols_missing_in_file     = "Spalte(n) %s fehlen in %s",
    plot_id_duplicate        = "Der Plot-Identifikator in %s ist mehrfach vergeben (%s). Jeder Inventurpunkt braucht einen eindeutigen Identifikator; bitte die Dubletten in den Rohdaten beheben.",
    plot_coords_missing      = "In %s fehlen die Mittelpunkt-Koordinaten f\u00fcr Inventurpunkt(e): %s. Jeder Probekreis braucht einen Mittelpunkt; bitte die leeren Koordinaten in den Rohdaten erg\u00e4nzen.",
    file_missing             = "Datei nicht gefunden: %s",
    output_dir_missing       = "%s: Das Argument 'output_dir' muss angegeben werden -- es bestimmt, wohin die vorverarbeiteten Dateien geschrieben werden. Einen Vorgabepfad gibt es bewusst nicht; frueher wurde in das Verzeichnis der Rohdaten geschrieben, was nicht zulaessig ist. Fuer einen Wegwerf-Lauf eignet sich tempdir().",
    pdf_class_type_missing   = "%s: Das Eingabeobjekt hat kein 'class_type'-Attribut. Bitte die Tabelle mit output_base_table() bzw. output_structure_table() erzeugen (diese setzen die Klassenachse age/dq selbst).",
    inventory_meta_multiyear = "inventory_meta(): Die Inventurpunkte tragen mehr als ein Jahr (%s); es wird das groesste (juengste) verwendet.",
    structure_table_not_sample = "output_structure_table_pdf(): Strukturtabellen sind nur fuer Stichprobeninventuren (konzentrische Probekreise) sinnvoll; die uebergebene Inventur ist keine reine Stichprobeninventur.",
    increment_bundle_without_trees = "%s(): das uebergebene Objekt der Klasse %s enthaelt keine Einzelbaumzuwaechse. Ertragstafel-Zuwaechse werden auf Bestandesebene geschaetzt; bitte increment_ytables_base_table() verwenden.",
    ytables_multiple_ages = "Ertragstafel-Zuwachs: bei %d von %d Kollektiven (Inventurpunkt x Baumart im Hauptbestand) fuehren die Daten mehr als ein Alter. Je Kollektiv wird ein Ertragstafelzuwachs geschaetzt, das Alter wird dafuer standflaechengewichtet gemittelt. Bitte pruefen, ob die Altersangaben stimmen.",
    ytables_selection_required = "increment_ytables_base_table(): ohne ein Ergebnis von inv_increment_ytables() wird das Argument ytable_selection benoetigt.",
    increment_overall_needs_repsurv = "output_increment_overall(): diese Uebersicht stellt die in beiden Aufnahmen gemessenen Inventurpunkte denen gegenueber, die nur die zweite Aufnahme abdeckt; das gibt es nur bei einer Wiederholungsinventur. Das uebergebene Objekt der Klasse %s stammt aus einer Einzelinventur. Fuer diese bitte increment_base_table() und output_increment_overview_gnfi3() verwenden.",
    structure_table_maybe_standwise = "%s: Die uebergebenen Baumdaten tragen keine Probekreis-Information (dbh_trshld ist durchgehend leer) - offenbar stammen sie aus einer bestandesweisen Inventur. Die Strukturtabelle wird erzeugt und ist technisch korrekt, ihre Aussagekraft ist fuer solche Daten aber begrenzt.",
    reader_experimental_f3   = "Experimenteller Reader fuer ein drittes, BaySF-benanntes Rohformat (Format 3). Noch keine Funktions- oder Stabilitaetsgarantie; die Schnittstelle kann sich aendern. Verwendung auf eigene Gefahr.",
    reader_bw_construction   = "Der BW-Import (Baden-Wuerttemberg) ist noch im Aufbau (\"under construction\"). Von seiner Verwendung wird ausdruecklich abgeraten.",
    circle_class_undefined   = "Inventurpunkte verweisen auf Kreisklasse(n) %s, f\u00fcr die keine Definition in den Kreisdefinitionen gefunden wurde.",
    radiuses_gt_zero         = "radiuses m\u00fcssen gr\u00f6\u00dfer als 0 sein",
    mortal_col_empty         = "Die Mortalit\u00e4ts-Spalte in %s enth\u00e4lt leere Werte; nur 0, 1, 2 sind erlaubt",
    height_gt_50             = "-------------------------------------------------------\n Warnung: mindestens eine Baumh\u00f6he ist gr\u00f6\u00dfer als 50 m\n-------------------------------------------------------",
    dbh_col_nonnumeric       = "Die BHD-Spalte in %s enth\u00e4lt nicht-numerische oder leere Werte f\u00fcr lebende B\u00e4ume",
    fe_ikl_mismatch          = "fe_ikl in den Daten stimmt nicht mit den angegebenen radiuses \u00fcberein; bitte die Kreisdefinition anpassen",

    # --- import: missing-value plausis ----------------------------------------
    plot_id_missing          = "Die Spalte plot_id enth\u00e4lt fehlende Werte",
    tree_nr_missing          = "Die Spalte tree_nr enth\u00e4lt fehlende Werte in Punkt(en): %s",
    dbh_missing              = "Die Spalte dbh enth\u00e4lt fehlende Werte in Punkt(en): %s",
    tree_no_size             = "Baum/B\u00e4ume ohne jede Gr\u00f6\u00dfenangabe (weder BHD > 0 noch H\u00f6he) in Punkt(en): %s",
    tree_dbh_required        = "Baum/B\u00e4ume in der Baumdatei ohne g\u00fcltigen BHD (fehlt oder 0; H\u00f6he ersetzt ihn nicht) in Punkt(en): %s",
    angle_missing            = "Die Spalte angle enth\u00e4lt fehlende Werte in Punkt(en): %s",
    distance_missing         = "Die Spalte distance enth\u00e4lt fehlende Werte in Punkt(en): %s",
    mortality_missing        = "Die Spalte mortality enth\u00e4lt fehlende Werte in Punkt(en): %s",
    dup_plot_ids             = "Die Inventurpunkt-Liste enth\u00e4lt doppelte plot_ids",
    points_mismatch          = "Punkte aus Baumliste und Inventurpunkt-Liste stimmen nicht \u00fcberein",
    points_count             = "Anzahl Punkte in der Baumliste: %s\nAnzahl Punkte in der Inventurpunkt-Liste: %s",
    missing_points_saved     = "Die fehlenden Punkte werden im Ausgabeordner (%s) gespeichert als:\ntree_not_in_inv.txt\ninv_not_in_tree.txt\n",
    points_match             = "Punkte aus Baumliste und Inventurpunkt-Liste stimmen \u00fcberein",
    plots_trees_no_point     = "%d Plot(s) haben B\u00e4ume, aber keinen Inventurpunkt; deren B\u00e4ume werden im weiteren Aufbau verworfen (siehe tree_not_in_inv.txt).",

    # --- import: BaySF tables / columns / dates --------------------------------
    tables_missing           = "fehlende Tabelle(n) %s",
    cols_missing             = "fehlende Spalte(n) %s",
    date_format_undetected   = "Es konnte kein g\u00fcltiges Datumsformat f\u00fcr `erfdat` erkannt werden.",

    # --- import: height estimation --------------------------------------------
    height_method_invalid    = "H\u00f6hensch\u00e4tzmethode muss `NFI` oder `Bavaria` sein.",
    age_missing_bavaria_plot = "Baumalter fehlen f\u00fcr Punkt %s (H\u00f6hensch\u00e4tzmethode `Bavaria`).",
    age_missing_bavaria      = "H\u00f6hensch\u00e4tzmethode `Bavaria` kann bei fehlenden Altersangaben nicht angewendet werden",
    diagnostic_warning       = "Sie haben height_complete_inventory() mit diagnostic = TRUE aufgerufen. Die Ausgabe ist NICHT mit der Standardverwendung kompatibel.",
    nan_group_heights        = "NaN-Gruppenmittelh\u00f6hen werden mit ForestElementsR::h_q_from_d_q() gesch\u00e4tzt",

    # --- import: tree positions -----------------------------------------------
    trees_no_position        = "ACHTUNG: %s Baum/B\u00e4ume mit BHD >= %s cm (also au\u00dferhalb des innersten Probekreises) haben keine verwertbare Position -- Azimut und/oder Entfernung fehlen.\nDiese B\u00e4ume werden aus den vorprozessierten Daten AUSGESCHLOSSEN und sind damit in KEINER Auswertung enthalten (Vorrat, Grundfl\u00e4che, Zuwachs).\nSie k\u00f6nnen mit den Daten vorerst weiterarbeiten, sollten die fehlenden Koordinaten aber in den Rohdaten erg\u00e4nzen und den Import wiederholen.\nBetroffene B\u00e4ume (Punkt: BHD in cm, ggf. #Baumnummer): %s",
    trees_partial_position   = "Hinweis: %s Baum/B\u00e4ume im innersten Probekreis tragen nur eine unvollst\u00e4ndige Position (Entfernung ODER Azimut fehlt). Innenkreisb\u00e4ume brauchen keine Position; die unvollst\u00e4ndige Angabe wird verworfen, die B\u00e4ume bleiben in der Auswertung (Anzahl/Vorrat), werden aber nicht r\u00e4umlich verortet.\nBetroffene B\u00e4ume (Punkt: BHD in cm, ggf. #Baumnummer): %s",
    trees_no_layer           = "ACHTUNG: %s regul\u00e4re(r) Baum/B\u00e4ume haben keinen Bestandesschicht-Code (Schicht/bestku fehlt oder ist leer) und k\u00f6nnen keiner Bestandesschicht zugeordnet werden.\nDiese B\u00e4ume werden aus den vorprozessierten Daten AUSGESCHLOSSEN und sind damit in KEINER Auswertung enthalten.\nSie k\u00f6nnen mit den Daten vorerst weiterarbeiten, sollten den fehlenden Schicht-Code aber in den Rohdaten erg\u00e4nzen und den Import wiederholen.\nBetroffene B\u00e4ume (Punkt: BHD in cm, ggf. #Baumnummer): %s",
    small_trees_no_layer     = "ACHTUNG: %s Kleinbaum/Kleinb\u00e4ume aus der Kleinbaumdatei haben keinen Bestandesschicht-Code (Schicht fehlt oder ist leer) und k\u00f6nnen keiner Bestandesschicht zugeordnet werden.\nDiese Kleinb\u00e4ume werden aus den vorprozessierten Daten AUSGESCHLOSSEN und sind damit in KEINER Auswertung enthalten.\nSie k\u00f6nnen mit den Daten vorerst weiterarbeiten, sollten den fehlenden Schicht-Code aber in der Kleinbaumdatei erg\u00e4nzen und den Import wiederholen.\nBetroffene Kleinb\u00e4ume (Punkt: BHD in cm, ggf. #Baumnummer): %s",
    format2_no_slope         = "Die Inventurpunktdatei enth\u00e4lt keine Hangneigungsspalte (neigung/slope). Die Hangneigung wird auf 0 gesetzt (ebenes Gel\u00e4nde, keine Fl\u00e4chenkorrektur der Probekreise). Falls tats\u00e4chlich Hangneigung vorliegt, eine Spalte neigung (in Prozent) erg\u00e4nzen.",

    # --- import: species guessing ---------------------------------------------
    commas_in_numbers        = "Zahlen mit Kommas gefunden. Werden in Punkt-Format umgewandelt.",
    numeric_parse_failed     = "Spalte %s enth\u00e4lt Werte, die sich nicht als Zahl lesen lassen (z. B. %s); sie werden auf NA gesetzt. Bitte die Quelldaten pr\u00fcfen.",
    species_col_has_na       = "Die Artenspalte enth\u00e4lt fehlende Werte",
    species_not_in_table_warn = "Folgende Arten sind nicht in der Standard-Artentabelle: %s\n  --> Neuabgleich wird versucht",
    species_not_in_table     = "Folgende Arten befinden sich noch nicht in der Standard-Artentabelle: %s",
    species_all_matched      = "Alle Arten wurden erfolgreich zugeordnet, manuelle Pr\u00fcfung empfohlen",
    species_col_nonnumeric   = "Die Artenspalte in %s enth\u00e4lt nicht-numerische Werte (z. B. %s). Falls es sich um Arten-K\u00fcrzel handelt, rufen Sie die Funktion mit species_guess = TRUE auf.",
    species_col_missing      = "Die Artenspalte in %s enth\u00e4lt fehlende/leere Werte in Punkt(en): %s. Jeder Baum braucht einen Artcode.",
    species_needs_review     = "%d Arten-K\u00fcrzel wurden nur \u00fcber unscharfe/ann\u00e4hernde Regeln zugeordnet und sollten manuell gepr\u00fcft werden (siehe report$species_mapping, tier == 'review').",
    date_no_valid_string     = "Keine g\u00fcltigen Datumszeichenketten f\u00fcr die Formaterkennung gefunden.",
    date_format_undetectable = "F\u00fcr '%s' konnte kein g\u00fcltiges Datumsformat erkannt werden. Bitte stellen Sie sicher, dass erfdat ein einheitliches Format verwendet.",
    date_format_ambiguous    = "Mehrdeutiges Datumsformat. Eingabewert '%s' passt zu mehreren Formaten: %s. Der Datensatz muss ein einheitliches Format verwenden.",
    inventory_year_invalid   = "`inventory_year` muss eine einzelne Jahreszahl sein (erhalten: '%s').",
    inventory_year_required  = "Die Datei liefert keine verwertbare Zeitangabe. Bitte `inventory_year` (Inventurjahr) angeben.",
    inventory_year_undetectable = "Das Datumsformat der Zeitangabe in der Datei konnte nicht eindeutig erkannt werden. Bitte `inventory_year` (Inventurjahr) angeben.",
    age_missing_incomplete   = "Altersangaben fehlen oder sind unvollstaendig. Auswertungen, die das (Bestandes-)Alter benoetigen, sind daher nicht moeglich (siehe Funktionsdokumentation).",

    # --- import: relascope checks ---------------------------------------------
    bhd_negative             = "bhd-Spalte enth\u00e4lt negative Werte",
    schicht_invalid          = "schicht muss 0, 1, 2 oder 3 sein",
    stichtag_format_dmY      = "stichtag konnte nicht im Format TT.MM.JJJJ gelesen werden",
    stichtag_format_dmy      = "stichtag konnte nicht im Format TT-MM-JJ gelesen werden",
    stand_id_duplicate       = "Doppelte Bestandskennungen in den Inventuraufnahmen",
    bestflaeche_not_positive = "bestflaeche muss > 0 sein",
    aufnahmeart_invalid      = "aufnahmeart muss 1, 2, 4 oder 5 sein",

    # --- import: BW duplicates ------------------------------------------------
    dup_tree_rows_bw         = "%d doppelte Zeile(n) in BW-Daten gefunden und entfernt.",

    # --- import helpers: circle map / source ----------------------------------
    circle_map_col_missing   = "Spalte circle_map$column '%s' nicht in Inventurpunkten gefunden",
    circle_map_entry_missing = "circle_map$mapping hat keinen Eintrag f\u00fcr: %s",
    circle_def_cols_missing  = "circle_definitions muss die Spalten enthalten: %s",
    circle_def_file_cols_missing = "Die Probekreisdefinitionsdatei muss die Spalten enthalten: %s",
    circle_def_file_unmatched = "Die Probekreisdefinitionsdatei deckt nicht alle Inventurpunkte ab. Ohne Definition: %s",
    circle_def_gen_args      = "dbh_cm_from und radiuses_m: gleiche Anzahl numerischer Werte (mindestens 1) erforderlich",
    circle_def_missing       = "Keine Probekreisdefinition angegeben. Bitte eine Probekreisdefinitionsdatei (circle_def_filename) oder dbh_cm_from/radiuses_m angeben, oder mit generate_circle_definition() erzeugen.",
    circle_def_conflict      = "Kreisdefinition doppelt angegeben: entweder circle_def_filename ODER dbh_cm_from/radiuses_m, nicht beides.",
    circle_def_all_mixed     = "fe_ikl = \"ALL\" muss die einzige Definition in der Probekreisdefinitionsdatei sein (keine Mischung mit Klassen-Definitionen).",
    circle_def_ikl_noninteger = "fe_ikl in der Probekreisdefinitionsdatei muss ganzzahlig sein (oder \"ALL\"); ung00fcltig: %s",
    circle_ikl_col_missing   = "Die Kreisdefinition enth00e4lt mehrere Klassen, aber die Inventurpunkt-Datei hat keine fe_ikl-Spalte zum Verkn00fcpfen.",
    circle_uniform_multi_ikl = "Es wurde eine einzige (pauschale) Kreisdefinition angegeben, aber die Inventurpunkte tragen mehrere verschiedene fe_ikl; die eine Definition wird auf alle angewandt.",
    source_invalid           = "`source` muss \"inputmask\" oder \"concentric\" sein",

    # --- coordinates ----------------------------------------------------------
    coord_not_dataframe      = "coordinate muss ein data frame sein.",
    coord_cols_missing       = "Fehlende Koordinatenspalten: %s, %s",
    coord_epsg_invalid       = "coord_sys = 'epsg' erfordert einen einzelnen numerischen EPSG-Wert.",
    coord_sys_unknown        = "Unbekanntes coord_sys: %s. Erlaubte Werte sind: %s oder 'epsg'.",
    coord_gk_prefix_missing  = "Rechtswerte ohne Gau\u00df-Kr\u00fcger-Zonenpr\u00e4fix erkannt (Median x = %s) und passend zum angegebenen coord_sys = '%s' um %s erg\u00e4nzt. Das ist die \u00fcbliche BaySF-Speicherform: ein sechsstelliger Rechtswert ist f\u00fcr sich genommen kein g\u00fcltiges Gau\u00df-Kr\u00fcger. Sollte eine Envelope-Warnung folgen, liegen die Daten vermutlich doch in einem anderen System.",
    coord_sys_undeclared     = "Das Koordinatensystem wurde nicht angegeben (coord_sys = NULL), und die Rechtswerte liegen offenbar ohne Zonenpr\u00e4fix vor (Median x = %s). Um sie als '%s' zu lesen, m\u00fcssten %s addiert werden \u2013 eine Ver\u00e4nderung der Daten, die hier auf einer blossen Vermutung beruhte.\nIn modernen Exporten im BaySF-Stil ist meist 'utm32' zutreffend; ohne explizite Angabe wird es aber nicht angenommen. Bitte coord_sys explizit angeben. Zur Auswahl: 'gk2'\u2013'gk5' (BaySF-Rechtswerte ohne Pr\u00e4fix sind \u00fcblich, es wird dann passend erg\u00e4nzt), 'utm32'/'utm33' (Ostwerte ca. 300.000\u2013900.000), 'lonlat'/'etrs89' (geographisch, Werte < 90).",
    coord_outside_envelope   = "Koordinaten liegen au\u00dferhalb des erwarteten deutschen Sprachraums (lon %s\u2013%s, lat %s\u2013%s). Nach Interpretation als coord_sys = '%s' ergibt sich folgende Ausdehnung in WGS84: %s Das coord_sys stimmt wahrscheinlich nicht mit den tats\u00e4chlichen Koordinaten \u00fcberein (z. B. UTM-Daten als GK beschriftet oder geographische Koordinaten als projiziertes KRS). Falls die Inventur tats\u00e4chlich au\u00dferhalb des deutschen Sprachraums liegt, ist dieser Hinweis unerheblich und kann mit check_envelope = FALSE abgeschaltet werden.",
    coord_sys_conflict       = "Widerspruch beim Koordinatensystem: die Spalte %s der Rohdaten deklariert '%s', als Argument wurde aber coord_sys = '%s' 00fcbergeben. FeNEU entscheidet das nicht still. Bitte kl00e4ren, welches System zutrifft, und entweder das Argument weglassen (dann gilt die Spalte) oder die Spalte korrigieren.",
    coord_sys_needed         = "Das Koordinatensystem der Probekreismittelpunkte ist unbekannt: die Rohdaten enthalten keine verwertbare Deklaration des Koordinatensystems, und coord_sys wurde nicht angegeben. Bitte coord_sys angeben. Zur Auswahl: %s.",
    coord_sys_implausible    = "Das angegebene Koordinatensystem '%s' passt nicht zu den Werten: %s\nFeNEU r00e4t hier nicht. Bitte das zutreffende System angeben 2013 in Frage kommt am ehesten: %s. (Pr00fcfen Sie zuvor, ob die Spalten wirklich das enthalten, was ihre Namen sagen.)",
    coord_looks_geographic   = "die Koordinaten sehen geographisch aus (Werte innerhalb |L00e4nge| <= 180 und |Breite| <= 90), das angegebene System ist aber ein projiziertes.",
    coord_looks_projected    = "die Koordinaten sehen projiziert aus (Werte weit au00dferhalb des Grad-Bereichs), das angegebene System ist aber ein geographisches.",
    coord_sys_from_file      = "Koordinatensystem aus den Rohdaten 00fcbernommen (Spalte %s): '%s'. Mit dem Argument coord_sys l00e4sst es sich 00fcberschreiben.",
    coord_reprojected        = "Geographische Koordinaten (coord_sys = '%s') werden f\u00fcr die weitere Verarbeitung automatisch nach UTM Zone %s (EPSG:%s) reprojiziert. Das ist notwendig, weil Baumpositionen metrisch aus Entfernung und Azimut um den Probekreismittelpunkt gebildet werden; in Grad w\u00e4re das nicht m\u00f6glich. Die Probekreiskoordinaten im Ergebnisobjekt liegen daher in UTM vor.",

    # --- object construction --------------------------------------------------
    dup_plot_id_stand        = "Mehr als ein Punkt hat dieselbe plot_id: %s",
    no_area_in_plot          = "Keine Fl\u00e4chenangaben in Punkt %s",
    dbh_lower_not_unique     = "dbh_lower ist nicht eindeutig in Punkt %s",
    no_crs_info              = "Zentrierkoordinaten haben keine KRS-Informationen (Attribut 'fe_crs' oder Spalte 'CRS').",
    no_trees_in_plot         = "Keine B\u00e4ume > 0 cm in Punkt %s",
    no_coords_in_plot        = "Keine Koordinatenwerte in Punkt %s",
    height_method_used       = "Fehlende Baumh\u00f6hen werden mit Methode %s gesch\u00e4tzt",
    non_tree_removed         = "Punkt %s: %d Nicht-Baum-Datens\u00e4tze entfernt (z. B. Str\u00e4ucher, Code 99)",

    # --- fe_inventory ---------------------------------------------------------
    inv_year_not_uniform     = "Es darf nur ein Inventurjahr vorhanden sein",

    # --- inventory matching ---------------------------------------------------
    multiple_matches_coord   = "Mehrfache Punktzuordnungen \u00fcber Mittelpunktskoordinaten. Bitte coord_tolerance reduzieren. Ausgabe pr\u00fcfen.",
    no_matching_plots        = "Keine \u00fcbereinstimmenden Inventurpunkte. Ausgabe pr\u00fcfen.",
    multiple_matches_matches = "Mehrfache Zuordnungen in `matches`. Ausgabelistenelement `multiple_matches` pr\u00fcfen.",
    incompat_circle_defs     = "%d zugeordnetes/zugeordnete Punktepaar(e) wegen inkompatibler Kreisdefinitionen ausgeschlossen.",
    slope_harmonized         = "Neigung f\u00fcr %d Punkt(e) in der ersten Inventur angeglichen (auf Wert der zweiten Inventur gesetzt).",

    # --- inventory period -----------------------------------------------------
    inv_year_same            = "Beide Inventuren haben denselben Jahrgang.",
    inv_year_order           = "Die erste Inventur kann nicht j\u00fcnger als die zweite sein.",
    inv_period_too_short     = "Der Zeitabstand zwischen zwei Inventuren muss mindestens ein Jahr betragen.",

    # --- increment / aggregation ----------------------------------------------
    fill_all_estimates_has_na = "F\u00fclloption 'all_estimates' akzeptiert keine NA-Sch\u00e4tzungen.",
    matched_trees_empty      = "matched_trees ist leer \u2013 Inventurperiode kann nicht bestimmt werden.",
    inv_period_invalid       = "Aus den Daten konnte keine g\u00fcltige Inventurperiode bestimmt werden.",
    tree_status_other        = "Unerwartete 'other'-Eintr\u00e4ge in bygroups$tree_status gefunden.",
    checksum_mismatch        = "Pr\u00fcfsummenfehler (%s != %s) in inv_inc_fill_gaps_gnfi3(). Wahrscheinlich doppelte Baumkoordinaten auf einigen Probefl\u00e4chen.",
    inv_matching_problematic = "Problematische Ergebnisse beim Abgleich der Inventuren. Ausgabe pr\u00fcfen. %s",
    na_increments_found      = "%d NA-Zuw\u00e4chse wurden berechnet.",
    increment_age_required   = "%s: Diese Zuwachssch\u00e4tzung beruht auf dem Bestandesalter, bei %d von %d B\u00e4umen des ausgewerteten Kollektivs fehlt die Altersangabe. Ohne Alter lassen sich f\u00fcr diese B\u00e4ume keine Zuw\u00e4chse berechnen; das Ergebnis w\u00e4re unvollst\u00e4ndig, ohne dass man es der Tabelle ansieht. Bitte die Altersangaben erg\u00e4nzen oder die Auswertung auf ein Kollektiv mit vollst\u00e4ndigen Altern einschr\u00e4nken. Liegen die Alter nur f\u00fcr den Hauptbestand vor, kann die Baumliste vorab darauf eingeschr\u00e4nkt werden: dplyr::filter(baumliste, layer_key == 1). Die Bezugsfl\u00e4che bleibt dabei die volle Inventurfl\u00e4che, das Ergebnis ist also eine Hauptbestandsauswertung des ganzen Betriebs.",
    increment_age_col_missing = "%s: Diese Zuwachssch\u00e4tzung beruht auf dem Bestandesalter, die Daten enthalten aber keine Spalte age_yr.",
    tree_position_ambiguous  = "Inventurpunkt %s, Inventur %s: mehrere B\u00e4ume stehen auf derselben Position (Distanz und Azimut identisch). Eine geteilte Position kann keinen dieser B\u00e4ume identifizieren, sie werden daher wie B\u00e4ume ohne Koordinaten als nicht zuordenbar behandelt und vom Baumabgleich ausgeschlossen. Ihre gemessenen Volumina bleiben in den Vorratssummen enthalten. Betroffene B\u00e4ume: %s",

    # --- pulled methods -------------------------------------------------------
    height_m_missing         = "Die Funktion trees_add_essentials() akzeptiert keine fehlenden height_m-Werte. Bitte die Werte vorher mit height_complete_inventory() und fill_heights_back() komplettieren.",
    height_m_col_missing     = "Die Funktion trees_add_essentials() braucht eine Spalte height_m. Bitte die Werte vorher mit height_complete_inventory() und fill_heights_back() komplettieren.",
    wood_densities_wwk_only  = "Holzdichten sind nur f\u00fcr die wwk-Methode verf\u00fcgbar",

    # --- PDF toolchain --------------------------------------------------------
    pdf_toolchain_ok         = "PDF-Toolkette OK: pandoc und LaTeX sind verf\u00fcgbar.",
    pdf_pandoc_missing       = "pandoc nicht gefunden. Installieren Sie RStudio (enth\u00e4lt pandoc) oder installieren Sie pandoc von https://pandoc.org/installing.html",
    pdf_latex_missing        = "Keine LaTeX-Distribution gefunden. Installieren Sie TinyTeX mit: tinytex::install_tinytex()",
    pdf_tinytex_installing   = "TinyTeX wird installiert ...",
    pdf_latex_available      = "LaTeX bereits verf\u00fcgbar, nichts zu installieren."
  ),

  en = list(

    # --- progress phase labels (progressr) ------------------------------------
    progress_read     = "Reading raw data",
    progress_validate = "Validating data",
    progress_build    = "Building output files",
    progress_write    = "Writing files",
    pre_write_ok      = "Preprocessed data written successfully: %d file(s) to %s.",
    pre_write_fail    = "Preprocessed data was NOT written (import failed; see report$errors).",
    progress_process  = "Processing data",

    # --- progress phase labels: repeated-survey increment ----------------------
    progress_inc_match       = "Matching the two inventories",
    progress_inc_fill        = "Estimating increments for gap filling (NFI 3)",
    progress_inc_consolidate = "Consolidating the tree increments",
    progress_inc_extend      = "Adding attributes for aggregation",
    progress_inc_second_only = "Folding in the second-only plots",
    progress_inc_summaric    = "Calculating the summaric increment",
    progress_inc_overview    = "Aggregating to enterprise level",

    # --- import: circle / radius arguments ------------------------------------
    circle_args_len_mismatch = "The lower DBH limits and the circle radii must have the same length",
    cols_missing_in_file     = "column(s) %s missing in %s",
    plot_id_duplicate        = "The plot identifier in %s is used more than once (%s). Each inventory point needs a unique identifier; please resolve the duplicates in the raw data.",
    plot_coords_missing      = "In %s the plot-centre coordinates are missing for inventory point(s): %s. Every sample circle needs a centre; please fill in the empty coordinates in the raw data.",
    file_missing             = "File not found: %s",
    output_dir_missing       = "%s: argument 'output_dir' must be supplied -- it determines where the preprocessed files are written. There is deliberately no default; earlier versions wrote into the raw data directory, which is not permitted. For a throw-away run, tempdir() is a good choice.",
    pdf_class_type_missing   = "%s: the input object carries no 'class_type' attribute. Please build the table with output_base_table() or output_structure_table() (they set the age/dq class axis themselves).",
    inventory_meta_multiyear = "inventory_meta(): the inventory points carry more than one year (%s); using the most recent one.",
    structure_table_not_sample = "output_structure_table_pdf(): structure tables are only meaningful for sample inventories (concentric circle plots); the supplied inventory is not a pure sample inventory.",
    increment_bundle_without_trees = "%s(): the supplied object of class %s holds no single-tree increments. Yield-table increments are estimated at stand level; please use increment_ytables_base_table().",
    ytables_multiple_ages = "Yield-table increment: for %d of %d cohorts (inventory point x species in the main stand) the data carry more than one age. One yield-table increment is estimated per cohort, for which the age is averaged weighted by standing area. Please check whether the ages are correct.",
    ytables_selection_required = "increment_ytables_base_table(): without a result of inv_increment_ytables(), the argument ytable_selection is required.",
    increment_overall_needs_repsurv = "output_increment_overall(): this overview contrasts the inventory points measured in both surveys with those covered by the second survey only, a distinction that exists in a repeated survey alone. The supplied object of class %s comes from a single inventory. For that, please use increment_base_table() and output_increment_overview_gnfi3().",
    structure_table_maybe_standwise = "%s: the supplied tree data carry no concentric-circle information (dbh_trshld is empty throughout) - seemingly they come from a standwise inventory. The structure table will be produced and is technically correct, but the relevance of the outcome is limited for such data.",
    reader_experimental_f3   = "Experimental reader for a third, BaySF-named raw format (format 3). No correctness or stability guarantee yet; the interface may change. Use at your own risk.",
    reader_bw_construction   = "BW-style import (Baden-Wuerttemberg) is under construction. Its use is explicitly discouraged.",
    circle_class_undefined   = "Inventory points reference circle class(es) %s but no definition was found in the circle definitions.",
    radiuses_gt_zero         = "radiuses must be larger than 0",
    mortal_col_empty         = "mortality column in %s contains empty values; only 0, 1, 2 are allowed",
    height_gt_50             = "-------------------------------------------------------\n Warning: at least one tree height is larger than 50 m\n-------------------------------------------------------",
    dbh_col_nonnumeric       = "The dbh column in %s contains non-numeric or empty values for alive trees",
    fe_ikl_mismatch          = "fe_ikl in data does not match the given radiuses, consider adapting circle definition",

    # --- import: missing-value plausis ----------------------------------------
    plot_id_missing          = "The plot_id column contains missing values",
    tree_nr_missing          = "The tree_nr column contains missing values in plot(s): %s",
    dbh_missing              = "The dbh column contains missing values in plot(s): %s",
    tree_no_size             = "Tree(s) with no size at all (neither dbh > 0 nor height) in plot(s): %s",
    tree_dbh_required        = "Tree(s) in the tree-list file without a valid dbh (missing or zero; height is no substitute) in plot(s): %s",
    angle_missing            = "The angle column contains missing values in plot(s): %s",
    distance_missing         = "The distance column contains missing values in plot(s): %s",
    mortality_missing        = "The mortality column contains missing values in plot(s): %s",
    dup_plot_ids             = "Inventory points list contains duplicated plot_ids",
    points_mismatch          = "Points from tree list and inventory points list do not match",
    points_count             = "Number of points in tree list: %s\nNumber of points in inventory points list: %s",
    missing_points_saved     = "The missing points are saved in the output folder (%s) as:\ntree_not_in_inv.txt\ninv_not_in_tree.txt\n",
    points_match             = "Points from tree list and inventory points list match",
    plots_trees_no_point     = "%d plot(s) have trees but no inventory point; their trees are dropped further down the build (see tree_not_in_inv.txt).",

    # --- import: BaySF tables / columns / dates --------------------------------
    tables_missing           = "missing table(s) %s",
    cols_missing             = "missing column(s) %s",
    date_format_undetected   = "Could not detect a valid date format for `erfdat`.",

    # --- import: height estimation --------------------------------------------
    height_method_invalid    = "Height estimation method must be `NFI` or `Bavaria`.",
    age_missing_bavaria_plot = "Tree ages missing for plot %s (height estimation method `Bavaria`).",
    age_missing_bavaria      = "Height estimation method `Bavaria` cannot be applied with missing age information",
    diagnostic_warning       = "You have called height_complete_inventory() with diagnostic = TRUE. Its output will NOT be compatible with the standard use.",
    nan_group_heights        = "NaN group mean heights are estimated with ForestElementsR::h_q_from_d_q()",

    # --- import: tree positions -----------------------------------------------
    trees_no_position        = "ATTENTION: %s tree(s) with dbh >= %s cm (i.e. outside the innermost sample circle) have no usable position -- azimuth and/or distance are missing.\nThese trees are EXCLUDED from the preprocessed data and are therefore part of NO evaluation (standing volume, basal area, increment).\nYou can carry on with the data for now, but you should add the missing coordinates to the raw data and re-run the import.\nAffected trees (plot: dbh in cm, #tree-number if present): %s",
    trees_partial_position   = "Note: %s tree(s) in the innermost circle carry only a partial position (distance OR azimuth missing). Innermost-circle trees need no position, so the incomplete entry is discarded; the trees remain in the evaluation (count/volume) but are not placed spatially.\nAffected trees (plot: dbh in cm, #tree-number if present): %s",
    trees_no_layer           = "ATTENTION: %s regular tree(s) have no stand-layer code (layer/bestku missing or empty) and cannot be assigned to a stand layer.\nThese trees are EXCLUDED from the preprocessed data and are therefore part of NO evaluation.\nYou can carry on with the data for now, but you should add the missing stand-layer code to the raw data and re-run the import.\nAffected trees (plot: dbh in cm, #tree-number if present): %s",
    small_trees_no_layer     = "ATTENTION: %s small tree(s) from the small-tree file have no stand-layer code (layer missing or empty) and cannot be assigned to a stand layer.\nThese small trees are EXCLUDED from the preprocessed data and are therefore part of NO evaluation.\nYou can carry on with the data for now, but you should add the missing stand-layer code to the small-tree file and re-run the import.\nAffected small trees (plot: dbh in cm, #tree-number if present): %s",
    format2_no_slope         = "The inventory-point file has no terrain-slope column (neigung/slope). Slope is set to 0 (level ground, no area correction of the sample circles). If the plots actually lie on a slope, add a neigung column (in percent).",

    # --- import: species guessing ---------------------------------------------
    commas_in_numbers        = "Detected numbers with commas. Converting them to point format.",
    numeric_parse_failed     = "column %s contains values that cannot be read as a number (e.g. %s); they are set to NA. Please check the source data.",
    species_col_has_na       = "The species column contains missing values",
    species_not_in_table_warn = "The following species are not in the standard species table: %s\n  --> attempting rematch",
    species_not_in_table     = "The following species are still not in the standard species table: %s",
    species_all_matched      = "All species have been matched successfully, manual check is recommended",
    species_col_nonnumeric   = "The species column in %s contains non-numeric values (e.g. %s). If these are species abbreviations, call the function with species_guess = TRUE.",
    species_col_missing      = "The species column in %s has missing/empty values in plot(s): %s. Every tree needs a species code.",
    species_needs_review     = "%d species abbreviation(s) were matched only by fuzzy/approximate rules and should be checked manually (see report$species_mapping, tier == 'review').",
    date_no_valid_string     = "No valid date strings found for format detection.",
    date_format_undetectable = "Unable to detect a valid date format for '%s'. Please ensure that erfdat uses a uniform date format.",
    date_format_ambiguous    = "Ambiguous date format detected. Input value '%s' matches multiple formats: %s. The dataset must use one consistent date format.",
    inventory_year_invalid   = "`inventory_year` must be a single year (got: '%s').",
    inventory_year_required  = "The file provides no usable date. Please supply `inventory_year`.",
    inventory_year_undetectable = "The date format of the file's date column could not be detected unambiguously. Please supply `inventory_year`.",
    age_missing_incomplete   = "Stand-age data are missing or incomplete. Analyses that need the (stand) age are therefore not possible (see the function documentation).",

    # --- import: relascope checks ---------------------------------------------
    bhd_negative             = "bhd column contains negative values",
    schicht_invalid          = "schicht must be one of 0, 1, 2, 3",
    stichtag_format_dmY      = "stichtag could not be parsed as DD.MM.YYYY",
    stichtag_format_dmy      = "stichtag could not be parsed as DD-MM-YY",
    stand_id_duplicate       = "Duplicate stand identifiers in Inventuraufnahmen",
    bestflaeche_not_positive = "bestflaeche must be > 0",
    aufnahmeart_invalid      = "aufnahmeart must be 1, 2, 4 or 5",

    # --- import: BW duplicates ------------------------------------------------
    dup_tree_rows_bw         = "%d duplicated tree row(s) found in BW data and removed.",

    # --- import helpers: circle map / source ----------------------------------
    circle_map_col_missing   = "circle_map$column '%s' not found in inventory points",
    circle_map_entry_missing = "circle_map$mapping has no entry for: %s",
    circle_def_cols_missing  = "circle_definitions must have columns: %s",
    circle_def_file_cols_missing = "The circle-definition file must contain the columns: %s",
    circle_def_file_unmatched = "The circle-definition file does not cover all inventory points. Without a definition: %s",
    circle_def_gen_args      = "dbh_cm_from and radiuses_m must be numeric vectors of equal length (>= 1)",
    circle_def_missing       = "No circle definition provided. Supply a circle-definition file (circle_def_filename) or dbh_cm_from/radiuses_m, or create one with generate_circle_definition().",
    circle_def_conflict      = "Circle definition given twice: supply either circle_def_filename OR dbh_cm_from/radiuses_m, not both.",
    circle_def_all_mixed     = "fe_ikl = \"ALL\" must be the only definition in the circle-definition file (do not mix it with per-class definitions).",
    circle_def_ikl_noninteger = "fe_ikl in the circle-definition file must be integer (or \"ALL\"); invalid: %s",
    circle_ikl_col_missing   = "The circle definition has multiple classes, but the inventory-point file has no fe_ikl column to link them.",
    circle_uniform_multi_ikl = "A single (uniform) circle definition was given, but the inventory points carry several distinct fe_ikl; the one definition is applied to all.",
    source_invalid           = "`source` must be \"inputmask\" or \"concentric\"",

    # --- coordinates ----------------------------------------------------------
    coord_not_dataframe      = "coordinate must be a data frame.",
    coord_cols_missing       = "Missing coordinate columns: %s, %s",
    coord_epsg_invalid       = "coord_sys = 'epsg' requires a single numeric epsg value.",
    coord_sys_unknown        = "Unknown coord_sys: %s. Allowed values are: %s, or 'epsg'.",
    coord_gk_prefix_missing  = "Eastings without a Gauss-Krueger zone prefix detected (median x = %s); to match the coord_sys you declared ('%s'), %s was added. This is the usual BaySF storage form: a six-digit easting on its own is not valid Gauss-Krueger. If an envelope warning follows, the data are probably in a different system after all.",
    coord_sys_undeclared     = "The coordinate reference system was not declared (coord_sys = NULL), and the eastings appear to lack the zone prefix (median x = %s). Reading them as '%s' would require adding %s - an alteration of your data resting on nothing but a guess.\nFor modern exports in BaySF style the answer is usually 'utm32', but FeNEU will not assume it without your confirmation. Please state coord_sys explicitly. Options: 'gk2'-'gk5' (BaySF eastings without the prefix are the norm; it is then added to match), 'utm32'/'utm33' (eastings around 300,000-900,000), 'lonlat'/'etrs89' (geographic, values < 90).",
    coord_outside_envelope   = "Coordinates fall outside the expected German-speaking Europe envelope (lon %s-%s, lat %s-%s). After interpretation as coord_sys = '%s', the bbox in WGS84 is: %s Your coord_sys probably does not match the actual coordinates (e.g. UTM data labelled as GK, or lonlat data labelled as a projected CRS). If the inventory genuinely lies outside German-speaking Europe this hint is irrelevant and can be switched off with check_envelope = FALSE.",
    coord_sys_conflict       = "Conflicting coordinate reference systems: column %s of the raw data declares '%s', but coord_sys = '%s' was passed as an argument. FeNEU does not settle this silently. Please establish which one is right and either drop the argument (the column then applies) or fix the column.",
    coord_sys_needed         = "The coordinate reference system of the plot centres is unknown: the raw data hold no usable declaration of it, and coord_sys was not supplied. Please state coord_sys. Options: %s.",
    coord_sys_implausible    = "The coordinate reference system you stated ('%s') does not fit the values: %s\nFeNEU does not guess here. Please state the system that applies - the likeliest candidates are: %s. (Check first whether the columns really hold what their names claim.)",
    coord_looks_geographic   = "the coordinates look geographic (all within |lon| <= 180 and |lat| <= 90), but the system you stated is a projected one.",
    coord_looks_projected    = "the coordinates look projected (values far outside the range of degrees), but the system you stated is a geographic one.",
    coord_sys_from_file      = "Coordinate reference system taken from the raw data (column %s): '%s'. Pass the coord_sys argument to override it.",
    coord_reprojected        = "Geographic coordinates (coord_sys = '%s') are reprojected automatically to UTM zone %s (EPSG:%s) for further processing. This is necessary because tree positions are built metrically from distance and azimuth around the plot centre, which is not possible in degrees. The plot coordinates in the result object are therefore in UTM.",

    # --- object construction --------------------------------------------------
    dup_plot_id_stand        = "more than one point has the same plot_id: %s",
    no_area_in_plot          = "no area values in plot %s",
    dbh_lower_not_unique     = "dbh_lower is not unique in Plot %s",
    no_crs_info              = "Center coordinates have no CRS information (attr 'fe_crs' or column 'CRS').",
    no_trees_in_plot         = "no trees > 0cm in plot %s",
    no_coords_in_plot        = "no coordinate values in plot %s",
    height_method_used       = "missing tree heights will be estimated with method %s",
    non_tree_removed         = "plot %s: removed %d non-tree species record(s) (e.g. shrubs, code 99)",

    # --- fe_inventory ---------------------------------------------------------
    inv_year_not_uniform     = "only one inventory year is allowed",

    # --- inventory matching ---------------------------------------------------
    multiple_matches_coord   = "Multiple inventory point matches by center coordinates. Consider reducing coord_tolerance. Check output for details.",
    no_matching_plots        = "No matching inventory points. Check output for details.",
    multiple_matches_matches = "Multiple matches in `matches`. Check output list element `multiple_matches`.",
    incompat_circle_defs     = "%d matched plot pair(s) excluded due to incompatible circle definitions.",
    slope_harmonized         = "Slope harmonized for %d plot(s) in the first inventory (set to second inventory's slope).",

    # --- inventory period -----------------------------------------------------
    inv_year_same            = "Both inventories have the same year entry.",
    inv_year_order           = "First inventory cannot be later than second.",
    inv_period_too_short     = "Time interval between two surveys must be at least one year.",

    # --- increment / aggregation ----------------------------------------------
    fill_all_estimates_has_na = "Fill option 'all_estimates' cannot accept NA estimates.",
    matched_trees_empty      = "matched_trees is empty - cannot derive an inventory period.",
    inv_period_invalid       = "Could not determine a valid inventory period from the data.",
    tree_status_other        = "Found unexpected 'other' entries in bygroups$tree_status.",
    checksum_mismatch        = "Checksum mismatch (%s != %s) in inv_inc_fill_gaps_gnfi3(). Probably duplicated tree coordinates on some plots.",
    inv_matching_problematic = "Problematic results when matching inventories. Check output. %s",
    na_increments_found      = "There were %d NA increments obtained.",
    increment_age_required   = "%s: This increment estimate is based on stand age, but %d of %d trees in the evaluated collective have no age. Without an age no increment can be computed for those trees, so the result would be incomplete without the table showing it. Please add the missing ages, or restrict the evaluation to a collective with complete ages. If ages are available for the main stand only, the tree list can be restricted beforehand: dplyr::filter(tree_list, layer_key == 1). The reference area stays the full inventory area, so the result is a main-stand evaluation of the whole enterprise.",
    increment_age_col_missing = "%s: This increment estimate is based on stand age, but the data have no age_yr column.",
    tree_position_ambiguous  = "Plot %s, survey %s: several trees share the same position (identical distance and azimuth). A shared position cannot identify any of them, so they are treated like trees recorded without coordinates: excluded from the tree matching as unmatchable. Their measured volumes remain part of the standing-volume sums. Affected trees: %s",

    # --- pulled methods -------------------------------------------------------
    height_m_missing         = "Function trees_add_essentials() does not accept missing height_m values. Please complete them first, e.g. with height_complete_inventory() and fill_heights_back().",
    height_m_col_missing     = "Function trees_add_essentials() requires a height_m column. Please complete the heights first, e.g. with height_complete_inventory() and fill_heights_back().",
    wood_densities_wwk_only  = "wood densities are only available for the wwk method",

    # --- PDF toolchain --------------------------------------------------------
    pdf_toolchain_ok         = "PDF toolchain OK: pandoc and LaTeX are available.",
    pdf_pandoc_missing       = "pandoc not found. Install RStudio (which bundles pandoc) or install pandoc from https://pandoc.org/installing.html",
    pdf_latex_missing        = "No LaTeX distribution found. Install TinyTeX by running: tinytex::install_tinytex()",
    pdf_tinytex_installing   = "Installing TinyTeX ...",
    pdf_latex_available      = "LaTeX already available, nothing to install."
  )
)


#' Fetch a translated, interpolated user-facing string
#'
#' Looks up \code{key} in \code{.fe_strings} for the language given by
#' \code{getOption("fe_lang", "en")} and, when extra arguments are supplied,
#' interpolates them into the string via \code{\link[base]{sprintf}}.
#'
#' Falls back to English if the option names an unknown language, and to the raw
#' \code{key} if the key itself is unknown (so a missing translation degrades
#' gracefully rather than erroring).
#'
#' @param key Character scalar; a key present in \code{.fe_strings}.
#' @param ... Optional values for \code{sprintf} placeholders in the string.
#'
#' @return A character scalar, ready to pass to \code{stop()}, \code{warning()}
#'   or \code{message()}.
#'
#' @keywords internal
#' @noRd
fe_msg <- function(key, ...) {
  lang    <- getOption("fe_lang", "en")
  strings <- .fe_strings[[lang]]
  if (is.null(strings)) strings <- .fe_strings[["en"]]

  s <- strings[[key]]
  if (is.null(s)) s <- .fe_strings[["en"]][[key]]
  if (is.null(s)) s <- key

  dots <- list(...)
  if (length(dots) > 0L) do.call(sprintf, c(list(s), dots)) else s
}
