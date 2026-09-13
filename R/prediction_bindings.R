# codetools treats a partial assignment as a local binding and can omit the
# lexical object read before that binding exists. An environment modified via
# state$field <- value is a consequential example: its contents remain shared.
# Supplement only roots involved in partial assignment, preserving the existing
# codetools treatment of other names and function dependencies.
prediction_binding_call_name <- function(code) {
  if (!is.call(code)) return("")
  head <- code[[1L]]
  if (is.symbol(head)) return(as.character(head))
  if (is.call(head) && is.symbol(head[[1L]]) && as.character(head[[1L]]) %in% c("::", ":::")) {
    return(as.character(head[[3L]]))
  }
  ""
}

# Local removal has a statically bounded scope. Searching parents or choosing
# an environment dynamically can invalidate a binding in a different frame.
# That behavior belongs to the existing unsupported dynamic-context boundary.
prediction_unsupported_binding_removal <- function(fun) {
  if (!is.function(fun) || is.primitive(fun)) return(character())
  inspect <- function(code) {
    if (missing(code)) return(character())
    if (is.symbol(code)) {
      removal <- as.character(code) %in% c("rm", "remove")
      return(if (removal) "a binding-removal function used as a value" else character())
    }
    if (is.pairlist(code)) return(unlist(lapply(code, inspect), use.names = FALSE))
    if (!is.call(code)) return(character())
    name <- prediction_binding_call_name(code)
    parts <- as.list(code)[-1L]
    if (name %in% c("quote", "expression", "alist")) return(character())
    if (name %in% c("$", "@")) return(inspect(parts[[1L]]))
    if (name %in% c("::", ":::") && as.character(parts[[2L]]) %in% c("rm", "remove")) {
      return("a binding-removal function used as a value")
    }
    if (name %in% c("rm", "remove")) {
      labels <- names(parts) %||% rep("", length(parts))
      inheritance <- which(labels == "inherits")
      forwarded <- any(vapply(parts, function(value) identical(value, quote(...)), logical(1)))
      if (any(labels %in% c("pos", "envir")) || forwarded ||
            (length(inheritance) && !identical(parts[[inheritance[[1L]]]], FALSE))) {
        return(paste0("non-local or dynamic binding removal in `", name, "()`"))
      }
    }
    unlist(lapply(parts, inspect), use.names = FALSE)
  }
  unique(c(inspect(body(fun)), inspect(formals(fun))))
}

prediction_partial_assignment_bindings <- function(fun) {
  if (!is.function(fun) || is.primitive(fun)) {
    return(character())
  }
  head_name <- prediction_binding_call_name
  assignment <- function(code) head_name(code) %in% c("<-", "=", "<<-", "->", "->>")
  left_side <- function(code) code[[if (head_name(code) %in% c("->", "->>")) 3L else 2L]]
  root_name <- function(code) {
    while (is.call(code) && length(code) >= 2L) code <- code[[2L]]
    if (is.symbol(code)) as.character(code) else character()
  }
  collect <- function(code) {
    if (missing(code)) {
      return(character())
    }
    if (is.pairlist(code)) {
      return(unique(unlist(lapply(code, collect), use.names = FALSE)))
    }
    if (!is.call(code)) {
      return(character())
    }
    if (head_name(code) %in% c("quote", "expression", "alist")) {
      return(character())
    }
    root <- if (assignment(code) && is.call(left_side(code))) root_name(left_side(code)) else character()
    unique(c(root, unlist(lapply(as.list(code)[-1L], collect), use.names = FALSE)))
  }
  roots <- unique(c(collect(body(fun)), collect(formals(fun))))
  if (!length(roots)) {
    return(character())
  }
  found <- new.env(parent = emptyenv())
  found$names <- character()
  read_root <- function(name, bound) {
    if (name %in% roots && !name %in% bound) found$names <- union(found$names, name)
  }
  scan <- function(code, bound, enclosing = character()) {
    if (missing(code)) {
      return(bound)
    }
    if (is.symbol(code)) {
      read_root(as.character(code), bound)
      return(bound)
    }
    if (!is.call(code)) {
      return(bound)
    }
    name <- head_name(code)
    parts <- as.list(code)[-1L]
    if (name %in% c("quote", "expression", "alist")) {
      return(bound)
    }
    if (name == "{") {
      for (part in parts) bound <- scan(part, bound, enclosing)
      return(bound)
    }
    if (assignment(code)) {
      left <- left_side(code)
      right <- code[[if (name %in% c("->", "->>")) 2L else 3L]]
      bound <- scan(right, bound, enclosing)
      root <- root_name(left)
      if (is.call(left)) {
        if (name %in% c("<<-", "->>")) read_root(root, enclosing)
        bound <- scan(left, bound, enclosing)
      }
      return(if (name %in% c("<<-", "->>")) bound else union(bound, root))
    }
    if (name == "if") {
      bound <- scan(parts[[1L]], bound, enclosing)
      yes <- scan(parts[[2L]], bound, enclosing)
      no <- if (length(parts) >= 3L) scan(parts[[3L]], bound, enclosing) else bound
      return(intersect(yes, no))
    }
    if (name == "function") {
      nested_bound <- union(bound, names(parts[[1L]]))
      for (default in as.list(parts[[1L]])) scan(default, nested_bound, bound)
      scan(parts[[2L]], nested_bound, bound)
      return(bound)
    }
    if (name == "for") {
      bound <- scan(parts[[2L]], bound, enclosing)
      loop_bound <- union(bound, as.character(parts[[1L]]))
      scan(parts[[3L]], loop_bound, enclosing)
      return(loop_bound)
    }
    if (name %in% c("while", "repeat")) {
      if (name == "while") {
        bound <- scan(parts[[1L]], bound, enclosing)
        scan(parts[[2L]], bound, enclosing)
      } else {
        scan(parts[[1L]], bound, enclosing)
      }
      return(bound)
    }
    # Field/slot names are literal; indexed arguments can themselves read roots.
    if (name %in% c("$", "@")) {
      scan(parts[[1L]], bound, enclosing)
      return(bound)
    }
    if (name %in% c("rm", "remove")) {
      labels <- names(parts) %||% rep("", length(parts))
      removed <- character()
      for (index in seq_along(parts)) {
        value <- parts[[index]]
        if (labels[[index]] == "list") {
          literal_names <- if (is.character(value)) {
            value
          } else if (head_name(value) == "c" && all(vapply(as.list(value)[-1L], is.character, logical(1)))) {
            unlist(as.list(value)[-1L], use.names = FALSE)
          } else {
            roots
          }
          removed <- union(removed, literal_names)
          scan(value, bound, enclosing)
        } else if (!nzchar(labels[[index]])) {
          literal_names <- if (is.symbol(value)) {
            as.character(value)
          } else if (is.character(value)) {
            value
          } else {
            roots
          }
          removed <- union(removed, literal_names)
        } else {
          scan(value, bound, enclosing)
        }
      }
      # Removing a local binding exposes a known enclosing local, or the
      # external lexical binding if no such enclosing local is established.
      return(union(setdiff(bound, removed), intersect(enclosing, removed)))
    }
    read_root(name, bound)
    # Arbitrary functions choose when to force arguments. An assignment in one
    # argument does not establish a definite binding before another is forced.
    for (part in parts) scan(part, bound, enclosing)
    bound
  }
  # Defaults can be forced at different points or replaced by explicit values.
  # Inspect their reads, but do not carry default assignments into the body.
  for (default in as.list(formals(fun))) scan(default, names(formals(fun)))
  scan(body(fun), names(formals(fun)))
  sort(found$names)
}
