scientific_10_simple <- function(x) {
  parse(text=gsub("[+]", "", gsub("1e", "10^", scales::scientific_format()(x))))
}
