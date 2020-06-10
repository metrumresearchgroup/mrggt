.dt_caption_key <- "_caption"

dt_caption_get <- function(data) {

  dt__get(data, .dt_caption_key)
}

dt_caption_set <- function(data, caption) {

  dt__set(data, .dt_caption_key, caption)
}

dt_caption_init <- function(data) {

  list(
    caption = NULL
  ) %>%
    dt_caption_set(caption = ., data = data)
}

dt_caption <- function(data, caption) {
  cap <- dt_caption_get(data = data)

  cap$caption <- caption

  dt_caption_set(data = data, caption = cap)
}

dt_caption_build <- function(data, context) {

  caption <- dt_caption_get(data = data)

  caption <- lapply(caption, function(val) process_text(val, context = context))

  dt_caption_set(data = data, caption = caption)
}

dt_has_caption <- function(data, context='latex') {

  caption <- dt_caption_get(data = data)

  if(context == 'latex'){
    !is.null(caption$caption)
  }
}
