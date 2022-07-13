#' Fix shiny code.
#'
#' Call this function as an addin move the cursor to the first warning.
#'
#' @export
shinyFix <- function () {
  activeDocument <- rstudioapi::getActiveDocumentContext()
  if (rstudioapi::getActiveDocumentContext()$id=='#console') {
    rstudioapi::showDialog("Error", "Your cursor needs to be in the file you want to fix (not the console)")
    return(FALSE)
  }
  text <- paste0(activeDocument$contents,collapse = '\n')
  tokens <- sourcetools::tokenize_string(text)
  find_scopes <- function(tokens) {
    tokens <- tokens[!(tokens$type %in% c("whitespace", 
                                          "comment")), ]
    tokens$type[tokens$type %in% c("string", "number", 
                                   "symbol", "keyword")] <- "value"
    brace_idx <- tokens$value %in% c("(", ")", 
                                     "{", "}", "[", "]", ",")
    tokens$type[brace_idx] <- tokens$value[brace_idx]
    stack <- "{"
    push <- function(x) {
      stack <<- c(stack, x)
    }
    pop <- function() {
      if (length(stack) == 1) {
        return(NA_character_)
      }
      res <- stack[length(stack)]
      stack <<- stack[-length(stack)]
      res
    }
    peek <- function() {
      stack[length(stack)]
    }
    for (i in seq_len(nrow(tokens))) {
      value <- tokens$value[i]
      tokens$scope[i] <- peek()
      if (value %in% c("{", "(", "[")) {
        push(value)
      }
      else if (value == "}") {
        if (!identical(pop(), "{")) 
          tokens$err[i] <- "unmatched_brace"
        tokens$scope[i] <- peek()
      }
      else if (value == ")") {
        if (!identical(pop(), "(")) 
          tokens$err[i] <- "unmatched_paren"
        tokens$scope[i] <- peek()
      }
      else if (value == "]") {
        if (!identical(pop(), "[")) 
          tokens$err[i] <- "unmatched_bracket"
        tokens$scope[i] <- peek()
      }
    }
    tokens
  }
  check_commas <- function(tokens) {
    tokens$err <- mapply(tokens$type, c("", tokens$type[-length(tokens$type)]), 
                         c(tokens$type[-1], ""), tokens$scope, tokens$err, 
                         SIMPLIFY = FALSE, FUN = function(type, prevType, 
                                                          nextType, scope, err) {
                           if (!is.na(err)) {
                             return(err)
                           }
                           if (scope == "(") {
                             if (type == "," && (prevType == "(" || 
                                                 prevType == "," || nextType == ")")) {
                               return("extra_comma")
                             }
                             if ((prevType == ")" && type == "value") || 
                                 (prevType == "value" && type == "value")) {
                               return("missing_comma")
                             }
                           }
                           NA_character_
                         })
    tokens
  }
  tokens$err <- NA_character_
  tokens <- find_scopes(tokens)
  tokens <- check_commas(tokens)
  if (all(is.na(tokens$err))) {
    rstudioapi::showDialog("Complete", "No issues found")
    return(TRUE)
  }
  lines <- strsplit(text, "\n")[[1]]
  show_code_error <- function(msg, lines, row, col) {
    message(paste0(msg, "\n", row, ":", lines[row], 
                   "\n", paste0(rep.int(" ", nchar(as.character(row)) + 
                                          1), collapse = ""), gsub(perl = TRUE, "[^\\s]", 
                                                                   " ", substr(lines[row], 1, col - 1)), "^"))
    rstudioapi::setCursorPosition(rstudioapi::document_position(row,col), id=activeDocument$id)
    # If there's a comma that's not meant to be here, select it
    if (grepl('extra comma', msg)) {
      FROM <- rstudioapi::document_position(row,col)
      TO <- rstudioapi::document_position(row,col+1)
      rstudioapi::setSelectionRanges(rstudioapi::document_range(FROM,TO),
                                     id=activeDocument$id)
    }
  }
  err_idx <- which(!is.na(tokens$err))
  msg <- ""
  i <- err_idx[[1]]  # TODO: Consider allowing user to view more than just the first error
  row <- tokens$row[i]
  col <- tokens$column[i]
  err <- tokens$err[i]
  if (err == "missing_comma") {
    show_code_error("Possible missing comma at:", 
                    lines, row, col)
  }
  else if (err == "extra_comma") {
    show_code_error("Possible extra comma at:", 
                    lines, row, col)
  }
  else if (err == "unmatched_brace") {
    show_code_error("Possible unmatched '}' at:", 
                    lines, row, col)
  }
  else if (err == "unmatched_paren") {
    show_code_error("Possible unmatched ')' at:", 
                    lines, row, col)
  }
  else if (err == "unmatched_bracket") {
    show_code_error("Possible unmatched ']' at:", 
                    lines, row, col)
  }
  return(FALSE)
}
