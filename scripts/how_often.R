how_often <- function(scripts, regex, min_texts = 10, min_said = 1) {
  # jak czesto na jedna swoja wypowiedz postac mowi tekst spelniajacy regex
  scripts <- as.data.table(scripts)
  counted <- scripts[,.(char, temp = stri_count_regex(stri_trans_tolower(dialog), regex))]
  
  counted <- counted[,.(summed = sum(temp), texts = .N), by = char]
  
  counted <- counted[summed>min_said]
  
  counted <- counted[texts>min_texts]
  
  counted <- counted[,.(char, average = summed/texts)]
  
  setorder(counted, -average)
  
  counted
}