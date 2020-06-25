.dt_overflow_key <- "_overflow"

dt_overflow_get <- function(data) {

  dt__get(data, .dt_overflow_key)
}

dt_overflow_set <- function(data, overflow) {

  dt__set(data, .dt_overflow_key, overflow)
}

dt_overflow_init <- function(data) {

  list(
    message = NULL,
    message.align = 'right',
    message.style = 'italic',
    repeat_column_labels = FALSE
  ) %>%
    dt_overflow_set(overflow = ., data = data)
}

dt_overflow <- function(data, overflow) {

  dt_overflow_set(data = data, overflow = overflow)
}

dt_overflow_build <- function(data, context) {

  overflow <- dt_overflow_get(data = data)

  overflow <- lapply(overflow, function(val) process_text(val, context = context))

  dt_overflow_set(data = data, overflow = overflow)
}

dt_has_overflow <- function(data, context='latex') {

  overflow <- dt_overflow_get(data = data)

  if(context == 'latex'){
    (is.null(overflow$message) | overflow$repeat_column_labels)
  }
}
