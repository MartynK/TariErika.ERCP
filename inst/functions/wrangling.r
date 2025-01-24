# Data wrangling

fil <- here::here("inst","extdata","Iris.xls")

descriptor <-
  here::here("inst","extdata","description.xlsx") %>%
  file.path() %>%
  readxl::read_excel(skip = 0)

sheets <- readxl::excel_sheets(fil) %>%
  # Filter so only those sheets mentioned in the description are used
  intersect(unique(descriptor$Sheet))

# For each sheet mentioned in the description, make a
# data.frame with its unique name
for (act_sheet in sheets) {

  act_descriptor <- descriptor %>%
    filter( Sheet == act_sheet)

  # make a list to be used as labels for 'labelled::'
  act_labs <-
    # take a 1xN matrix
    matrix( act_descriptor$description,
            ncol = act_descriptor$description %>%
              length()
    ) %>%
    as.list() %>%
    # add column names as 'names'
    `names<-`(act_descriptor$name_new)

  datachunk <- fil |>
    readxl::read_xls(sheet = act_sheet) |> # or read_xls() as appropriate
    # handle duplicate colnames
    janitor::clean_names() |>
    mutate( across( .cols = which( act_descriptor$trf == "factor"),
                    .fns = as.factor
    ),
    across( .cols = which( act_descriptor$trf == "numeric"),
            .fns = as.numeric # removing potential '?', 'NA', '.' etc.
    ),
    across( .cols = which( act_descriptor$trf == "date"),
            .fns = lubridate::as_datetime
    )) %>%
    `colnames<-`( act_descriptor$name_new) %>%
    # select only columns whose name isnt NA due to duplicates or whatnot
    select( act_descriptor$name_new[!is.na(act_descriptor$name_new)])%>%
    labelled::`var_label<-`(   act_labs  )

  # deselect the "not_relevant" columns in datachunk
  try( {
    datachunk <- datachunk %>%
      select( -not_relevant)
  }, silent = TRUE)

  ## NOT IMPLEMENTED, alternative to making a list of dataframes
  # # make act_sheet a valid name then
  # # rename datachunk to the sheet name
  # act_sheet %>% make.names() %>% assign( ., datachunk, envir = .GlobalEnv)

  # make a 'database' object which should be a list with the sheet name as the key
  # and the datachunk as the value
  if (!exists("database")) {
    database <- list()
  }
  database[[act_sheet]] <- datachunk

}


