clean_name <- function(dirty_name) {
  gsub("\\s\\(Unlicensed\\)", "", dirty_name)
}

# count_weekdays <- function(start_date, end_date) {
#   date_seq <- mapply(function(start, end) {
#     seq(ymd(start), ymd(end), by = "days")
#   }, start_date, end_date, SIMPLIFY = FALSE) %>%
#     do.call(c, .) %>%
#     unique()
#
#   weekdays_only <- date_seq[!wday(date_seq, week_start = 1) %in% c(6, 7)]
#
#   length(weekdays_only)
# }

count_weekdays <- function(start_date, end_date, half_day) {

  full_days_list <- purrr::map2(ymd(start_date[!half_day]),
                                ymd(end_date[!half_day]),
                                ~ seq(.x, .y, by = "day")) %>%
    do.call(c, .) %>%
    unique()
  full_days_list <- full_days_list[!wday(full_days_list, week_start = 1) %in% c(6, 7)]

  half_days_list <- purrr::map2(ymd(start_date[half_day]),
                                ymd(end_date[half_day]),
                                ~ seq(.x, .y, by = "day")) %>%
    do.call(c, .) %>%
    unique()
  half_days_list <- half_days_list[!wday(half_days_list, week_start = 1) %in% c(6, 7)]

  non_overlapping_half_days <- setdiff(half_days_list,
                                       full_days_list)

  length(full_days_list) + 0.5 * length(half_days_list)
}

split_row <- function(row) {
  start_date <- ymd(row$date_start)
  end_date <- ymd(row$date_end)

  month_seq <- seq(floor_date(start_date, "month"),
                   floor_date(end_date, "month"),
                   by = "month")

  split_rows <- map_df(month_seq, function(month_start) {
    month_end <- min(end_date, ceiling_date(month_start, "month") - days(1))

    if (month_start == floor_date(start_date, "month")) {
      actual_start <- start_date
    } else {
      actual_start <- month_start
    }

    if (month_end == ceiling_date(end_date, "month") - days(1)) {
      actual_end <- end_date
    } else {
      actual_end <- month_end
    }

    data.frame(date_start = actual_start, date_end = actual_end, row[-c(1, 2)])
  })

  split_rows
}

expand_absences_over_months <- function(events_df) {
  events_df %>%
    rowwise() %>%
    do(split_row(.)) %>%
    ungroup()
}

parse_date <- function(date_string, date_type = "start") {
  if (length(grep("^VALUE=DATE:\\d*", date_string)) > 0) {
    date <- ymd(gsub("^VALUE=DATE:(\\d8)", "\\1", date_string))
    if (date_type == "end")
      date <- date - 1
  }
  if (length(grep("^TZID=\\w*\\/\\w*:\\d{8}T\\d{6}", date_string)) > 0) {
    date <- ymd(gsub("^TZID=\\w*\\/\\w*:(\\d{8})T\\d{6}", "\\1", date_string))
  }
  date
}

parse_ics <- function(ics_file,
                      names_mapping_file = NULL,
                      filter_year = 2024) {
  x <<- readr::read_lines(ics_file)

  organiser_indices <- which(grepl("^ORGANIZER", x))
  lines_removed <- 0
  for (index in organiser_indices) {
    target_line <- index - lines_removed
    num_lines_starting_with_spaces <- min(which(grepl("^\\S", tail(
      x, length(x) - target_line
    )))) - 1
    organiser_lines <- x[target_line:(target_line + num_lines_starting_with_spaces)]
    concatenated_line <- paste0(c(organiser_lines[1], str_sub(organiser_lines[-1], 2)), collapse = "")
    organizer <- gsub(".*CN=(.*);.*", "\\1", concatenated_line)
    replacement_line <- paste0("ORGANIZER=", organizer)
    x[target_line] <- replacement_line
    x <- x[-(seq(num_lines_starting_with_spaces) + target_line)]
    lines_removed <- lines_removed + num_lines_starting_with_spaces
  }

  attendee_indices <- which(grepl("^ATTENDEE", x))
  lines_removed <- 0
  for (index in attendee_indices) {
    target_line <- index - lines_removed
    num_lines_starting_with_spaces <- min(which(grepl("^\\S", tail(
      x, length(x) - target_line
    )))) - 1
    attendee_lines <- x[target_line:(target_line + num_lines_starting_with_spaces)]
    concatenated_line <- paste0(c(attendee_lines[1], str_sub(attendee_lines[-1], 2)), collapse = "")
    attendee <- gsub(".*CN=(.*);.*", "\\1", concatenated_line)
    replacement_line <- paste0("ATTENDEE=", attendee)
    x[target_line] <- replacement_line
    x <- x[-(seq(num_lines_starting_with_spaces) + target_line)]
    lines_removed <- lines_removed + num_lines_starting_with_spaces
  }

  event_indices <- which(grepl("^BEGIN:VEVENT", x))
  events <- data.frame()
  for (index in event_indices) {
    event_end <- min(which(grepl(
      "^END:VEVENT", tail(x, length(x) - index)
    ))) + index
    this_event <- x[index:event_end]
    created <- gsub(".*CREATED:(.*)", "\\1", this_event[which(grepl("^CREATED", this_event))])
    date_start <- gsub(".*DTSTART;(.*)", "\\1", this_event[which(grepl("^DTSTART", this_event))]) %>%
      parse_date()
    date_end <- gsub(".*DTEND;(.*)", "\\1", this_event[which(grepl("^DTEND", this_event))]) %>%
      parse_date("end")
    description <- gsub(".*DESCRIPTION:(.*)", "\\1", this_event[which(grepl("^DESCRIPTION", this_event))])
    summary <- gsub(".*SUMMARY:(.*)", "\\1", this_event[which(grepl("^SUMMARY", this_event))])
    organizer <- gsub(".*ORGANIZER=(.*)", "\\1", this_event[which(grepl("^ORGANIZER", this_event))])
    organizer <- clean_name(organizer)
    attendees <- gsub(".*ATTENDEE=(.*)", "\\1", this_event[which(grepl("^ATTENDEE", this_event))])
    attendees <- clean_name(attendees)
    category <- gsub(".*CATEGORIES:(.*)", "\\1", this_event[which(grepl("^CATEGORIES", this_event))])
    if (length(attendees) == 0)
      attendees <- organizer
    for (attendee in attendees) {
      events <- events %>%
        bind_rows(
          data.frame(
            date_start = date_start,
            date_end = date_end,
            description = description,
            summary = summary,
            organizer = ifelse(length(organizer) == 0,
                           "",
                           organizer),
            attendee = attendee,
            category = category,
            half_day = any(str_detect(
              tolower(summary),
              c("half", "1/2", "afternoon", "morning")
            ), str_detect(
              tolower(description),
              c("half", "1/2", "afternoon", "morning")
            )),
            created = created
          )
        )
    }
  }

  events <- expand_absences_over_months(events)

  monthly_aggregation <- events %>%
    group_by(
      attendee,
      year = as.integer(year(date_start)),
      month_num = month(date_start),
      month_name = month(date_start, label = TRUE),
      created
    ) %>%
    summarise(
      num_vacation_days = count_weekdays(date_start, date_end, half_day),
      .groups = "drop_last"
    ) %>%
    summarise(num_vacation_days = sum(num_vacation_days)) %>%
    ungroup()

  wide_format <- monthly_aggregation %>%
    complete(attendee,
             year,
             month_num = 1:12,
             fill = list(num_vacation_days = 0)) %>%
    mutate(month_name = month(month_num, label = TRUE, abbr = TRUE)) %>%
    arrange(year, month_num, attendee) %>%
    select(attendee, year, month_name, num_vacation_days) %>%
    pivot_wider(
      names_from = month_name,
      values_from = num_vacation_days,
      values_fill = 0
    ) %>%
    arrange(attendee, year)

  print(filter_year)

  if (!is.null(names_mapping_file)) {
    names_mapping <<- read_csv2(names_mapping_file)
    excel_export <<- wide_format %>%
      select(attendee, year, all_of(seq(ymd(
        paste0(filter_year, '-01-01')
      ), ymd(
        paste0(filter_year, '-12-31')
      ), by = '1 month') %>% month(label = TRUE))) %>%
      left_join(names_mapping, ., by = join_by(ical_name == attendee)) %>%
      mutate(year = replace_na(year, filter_year)) %>%
      replace(is.na(.), 0) %>%
      filter(year == filter_year | is.na(year))
  } else {
    excel_export <- wide_format %>%
      filter(year == filter_year) %>%
      select(name = attendee, year, all_of(seq(ymd(
        paste0(filter_year, '-01-01')
      ), ymd(
        paste0(filter_year, '-12-31')
      ), by = '1 month') %>% month(label = TRUE)))
  }

  excel_export
}
