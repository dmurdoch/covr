#' @useDynLib covr, .registration = TRUE
replacement <- function(name, env = as.environment(-1), target_value = get(name, envir = env)) {
  if (is.function(target_value) && !is.primitive(target_value)) {
    if (is_vectorized(target_value)) {
      new_value <- target_value
      environment(new_value)$FUN <- trace_calls(environment(new_value)$FUN, name)
    } else if (is.function(target_value) && inherits(target_value, "memoised")) {
      new_value <- target_value
      environment(new_value)$`_f` <- trace_calls(environment(new_value)$`_f`, name)
    } else {
      new_value <- trace_calls(target_value, name)
      attributes(body(new_value)) <- attributes(body(target_value))
    }
    attributes(new_value) <- attributes(target_value)

    if (isS4(target_value)) {
      new_value <- asS4(new_value)
    }

    list(
      env = env,
      name = as.name(name),
      orig_value = .Call(covr_duplicate_, target_value),
      target_value = target_value,
      new_value = new_value
    )
  } else if (is.list(target_value)) {
    replacements_from_list(name, env, target_value)
  }
}

replace <- function(replacement) {
  if (inherits(replacement, "covr_list")) {
    replace_or_reset_list(replace, replacement)
  } else
    .Call(covr_reassign_function, replacement$name, replacement$env, replacement$target_value, replacement$new_value)
}

reset <- function(replacement) {
  if (inherits(replacement, "covr_list")) {
    replace_or_reset_list(reset, replacement)
  } else
    .Call(covr_reassign_function, replacement$name, replacement$env, replacement$target_value, replacement$orig_value)
}

replace_or_reset_list <- function(theupdate, replacement) {
  lapply(replacement$replacements, theupdate)
  env <- replacement$env
  name <- replacement$name
  tempenv <- replacement$tempenv
  for (i in ls(tempenv))
    eval(substitute(thelist[[i]] <- value,
                    list(thelist = as.name(name),
                         i = as.numeric(i),
                         value = get(i, envir = tempenv))), envir = env)
}
