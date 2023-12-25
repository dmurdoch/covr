replacements_from_list <- function(name, env, value = get(name, envir = env)) {
  names <- as.character(seq_along(value))
  tempenv <- new.env(parent = emptyenv())
  replacements <- lapply(seq_along(value),
    function(i) {
      obj <- value[[i]]
      assign(names[i], obj, envir = tempenv)
      if (is.function(obj) && !is.primitive(obj)) {
        replacement(names[i], env = tempenv, target_value = obj)
      } else if (is.list(obj))
        replacements_from_list(names[i], tempenv, obj)
    })
  replacements <- compact(replacements)
  if (length(replacements))
    structure(list(env = env,
                   name = name,
                   tempenv = tempenv,
                   replacements = replacements),
              class = "covr_list")
}
