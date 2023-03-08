
cand_no_rescue <- function(filepath){
  lines <- data.frame(lines = readLines(filepath))

  cand_no_line <- lines$lines[which(stringr::str_detect(lines$lines, "Enter your candidate number after the arrow as a number like this: cand_no"))+1]
  cand_no_line <- stringr::str_remove_all(cand_no_line, "<span id=\"cb1-2\"><a href=\"#cb1-2\" aria-hidden=\"true\" tabindex=\"-1\"></a>cand_no <span class=\"ot\">&lt;-</span> <span class=\"dv\">")
  cand_no <- stringr::str_remove_all(cand_no_line, "</span></span>")
}
