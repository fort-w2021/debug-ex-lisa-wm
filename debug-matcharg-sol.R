# SOURCE CODE ------------------------------------------------------------------

match_arg_src <- function (arg, choices, several.ok = FALSE) {
  
  if (missing(choices)) {
    
    # Falls keine choices spezifiziert wurden, greift match.arg auf die
    # Funktionsargumente eine Ebene höher zu, i. e., auf die formals der
    # Funktion, die match.arg aufruft
    
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    
    # Hier wird das Funktionsargument ausgewählt, das als Argument in match.arg 
    # gesteckt wird
    
    choices <- eval(formal.args[[as.character(substitute(arg))]], 
                    envir = sys.frame(sysP))
  }
  
  # Wenn arg nicht übergeben wird, gebe ersten Eintrag in choices zurück
  
  if (is.null(arg)) return(choices[1L])
  
  # Wenn arg übergeben wurde, aber kein character ist, stoppe mit Warnmeldung
  
  else if (!is.character(arg)) stop("'arg' must be NULL or a character vector")
  
  # Falls arg nur einelementig sein darf
  
  if (!several.ok) {
    
    # Wenn arg genau einer der choices entspricht, gebe erstes Element zurück
    # (sollte zwar eh einelementig sein, aber falls arg mehr als ein Element
    # hat, soll die informativere nachfolgende Fehlermeldung ausgegeben werden)
    
    if (identical(arg, choices)) return(arg[1L])
    
    # Wenn arg mehr als ein Element hat, stoppe mit Warnmeldung
    
    if (length(arg) > 1L) stop("'arg' must be of length 1") }
  
  # Wenn arg zwar übergeben wurde (denke ich, sonst hätte ja schon is.null
  # greifen müssen), aber Länge 0 hat (z. B. leerer Vektor), stoppe mit 
  # Warnmeldung
  
  else if (length(arg) == 0L) stop("'arg' must be of length >= 1")
  
  # Finde Index von choices, zu dem arg (am besten) passt, via partial string
  # matching. Setze Index auf 0, falls kein Match.
  
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  
  # Prüfe, ob alle i 0 sind ("all" notwendig, da multiple Matches erlaubt 
  # wurden). Wenn ja, kein Match möglich und Abbruch
  
  if (all(i == 0L)) {
    
    stop(gettextf(
      "'arg' should be one of %s", 
      paste(dQuote(choices), collapse = ", ")), domain = NA)
    
  }
  
  # Reduziere Indizes auf die, für die ein partial match gefunden wurde
  
  i <- i[i > 0L]
  
  # Falls das mehrere sind, several.ok aber FALSE ist, Abbruch
  
  if (!several.ok && length(i) > 1) {
    
    stop("there is more than one match in 'match.arg'")
    
  }
  
  # Falls mehrere Indizes matchen und several.ok TRUE ist, oder falls ohnehin
  # nur ein Index mit Match existiert, gebe diejenigen Elemente von choices
  # zurück, für die ein match gefunden werden konnte
  
  choices[i]
  
}



