
#####################################################################################################################

my_read.gadget.likelihood <- 
function (files = "likelihood") 
{
  lik <- NULL
  for (file in files) {
    lik <- c(lik, sub(" +$", "", gsub("\t", " ", readLines(file))))
  }
  lik <- stringr::str_trim(lik)
  lik <- lik[lik != ""]
  lik <- lik[!grepl(";", substring(lik, 1, 1))]
  lik <- sapply(strsplit(lik, ";"), function(x) sub(" +$", 
                                                    "", x[1]))
  comp.loc <- grep("component", lik)
  name.loc <- comp.loc + 3
  weights <- NULL
  common <- c("name", "weight", "type", "datafile", "areaaggfile", 
              "lenaggfile", "ageaggfile", "sitype", "function", "preyaggfile")
  tmp.func <- function(comp) {
    loc <- grep(paste("[ \t]", tolower(comp), sep = ""), 
                tolower(lik[name.loc]))
    if (sum(loc) == 0) {
      return(NULL)
    }
    else {
      dat <- plyr::ldply(loc, function(dd) {
        if (dd < length(comp.loc)) {
          restr <- (comp.loc[dd] + 1):(comp.loc[dd + 
                                                  1] - 1)
        }
        else {
          restr <- 1:length(lik) > comp.loc[dd]
        }
        tmp <- sapply(strsplit(sapply(strsplit(lik[restr], 
                                               "[ \t]"), function(x) {
                                                 paste(x[!(x == "" | x == "\t")], collapse = " ")
                                               }), " "), function(x) as.character(x))

        if (class(tmp)[1] != "list") {
          names.tmp <- utils::head(tmp, 1)
          tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)[2, 
          ]
          names(tmp) <- names.tmp
          row.names(tmp) <- tmp$name
          tmp$type <- tolower(tmp$type)
        }
        else {
          names.tmp <- sapply(tmp, function(x) x[1])
          tmp <- sapply(tmp, function(x) paste(x[-1], 
                                               collapse = "\t"))
          names(tmp) <- names.tmp
          tmp <- as.data.frame(t(tmp), stringsAsFactors = FALSE)
          tmp$type <- tolower(tmp$type)
        }
        return(tmp)
      })
      if (is.null(weights)) {
        weights <<- dat[intersect(common, unique(c(names(weights), 
                                                   names(dat))))]
      }
      else {
        weights <<- dplyr::bind_rows(dat, weights)[intersect(common, 
                                                             unique(c(names(weights), names(dat))))]
      }
      dat$weight <- NULL
      return(dat)
    }
  }
  likelihood <- list(penalty = tmp.func("penalty"), understocking = tmp.func("understocking"), 
                     migrationpenalty = tmp.func("migrationpenalty"), surveyindices = tmp.func("surveyindices"), 
                     catchdistribution = tmp.func("catchdistribution"), catchstatistics = tmp.func("catchstatistics"), 
                     surveydistribution = tmp.func("surveydistribution"), 
                     stockdistribution = tmp.func("stockdistribution"), stomachcontent = tmp.func("stomachcontent"), 
                     recaptures = tmp.func("recaptures"), recstatistics = tmp.func("recstatistics"), 
                     catchinkilos = tmp.func("catchinkilos"))
  likelihood$weights <- weights
  row.names(likelihood$weights) <- weights$name
  likelihood$weights$weight <- as.numeric(weights$weight)
  likelihood <- likelihood[c("weights", unique(likelihood$weights$type))]
  class(likelihood) <- c(class(likelihood), "gadget.likelihood")
  return(likelihood)
}



####################################################################################################################

my_gadget.fit <- 
  function (wgts = "WGTS", main.file = NULL, fleet.predict = NULL, 
            mat.par = NULL, params.file = NULL, f.age.range = NULL, fit.folder = "FIT", 
            printatstart = 1, steps = 1, recruitment_step_age = NULL, 
            gd = NULL) 
  {
    warning("gadget.fit is deprecated, please use gadget_fit")
    old.dir <- getwd()
    on.exit(setwd(old.dir))
    if (!is.null(gd)) {
      setwd(gd)
    }
    else {
      gd <- gadget.variant.dir(".")
    }
    if (!is.null(f.age.range) & !("data.frame" %in% class(f.age.range))) {
      stop("F age range should be specified as a data.frame with columns stock, age.min and age.max")
    }
    if (is.null(main.file)) {
      if (is.null(wgts)) {
        main.file <- "main"
      }
      else {
        main.file <- sprintf("%s/main.final", wgts)
      }
    }
    main <- Rgadget:::read.gadget.main(main.file)
    if (!is.null(wgts)) {
      resTable <- Rgadget:::read.gadget.results(wgts = wgts)
      nesTable <- Rgadget:::read.gadget.results(wgts = wgts, normalize = TRUE)
      if (is.null(params.file)) {
        params.file <- sprintf("%s/params.final", wgts)
      }
      params <- Rgadget:::read.gadget.parameters(params.file)
      lik <- my_read.gadget.likelihood(sprintf("%s/likelihood.final", 
                                               wgts))
    }
    else {
      resTable <- list()
      nesTable <- list()
      wgts <- fit.folder
      dir.create(fit.folder, showWarnings = FALSE)
      params <- if (!is.null(params.file)) 
        Rgadget:::read.gadget.parameters(params.file)
      else NULL
      lik <- my_read.gadget.likelihood(main$likelihoodfiles)
    }
    print("Reading input data")
    lik.dat <- Rgadget:::read.gadget.data(lik)
    stocks <- main$stockfiles %>% purrr::map(~Rgadget:::read.gadget.file(path = ".", 
                                                               file_name = ., file_type = "stock", recursive = FALSE))
    names(stocks) <- stocks %>% purrr::map(1) %>% purrr::map("stockname") %>% 
      unlist()
    make.gadget.printfile(main.file = main.file, file = sprintf("%s/printfile.fit", 
                                                                wgts), gd = list(dir = ".", output = sprintf("%s/out.fit", 
                                                                                                             wgts), aggfiles = sprintf("%s/print.aggfiles", wgts)), 
                          recruitment_step_age = recruitment_step_age, printatstart = printatstart, 
                          steps = steps)
    on.exit(unlink(sprintf("%s/out.fit", wgts), recursive = TRUE))
    main$printfiles <- sprintf("%s/printfile.fit", wgts)
    write.gadget.main(main, file = sprintf("%s/main.print", wgts))
    print("Running Gadget")
    callGadget(s = 1, i = params.file, main = sprintf("%s/main.print", 
                                                      wgts), o = sprintf("%s/SS.print", wgts))
    print("Reading output files")
    out <- read.printfiles(sprintf("%s/out.fit", wgts)) %>% setNames(gsub("^[^a-zA-Z]*", 
                                                                          "", names(.)))
    SS <- tryCatch(Rgadget:::read.gadget.lik.out(sprintf("%s/SS.print", 
                                               wgts)), error = function(e) "SS could not be read")
    print("Gathering results")
    stock.recruitment <- out[sprintf("%s.recruitment", names(stocks))] %>% 
      purrr::set_names(., names(stocks)) %>% dplyr::bind_rows(.id = "stock") %>% 
      dplyr::select(.data$stock, .data$year, .data$area, .data$step, 
                    recruitment = .data$number)
    stock.full <- out[sprintf("%s.full", names(stocks))] %>% 
      purrr::set_names(., names(stocks)) %>% dplyr::bind_rows(.id = "stock") %>% 
      dplyr::mutate(length = as.numeric(gsub("len", "", .data$length)))
    stock.std <- out[sprintf("%s.std", names(stocks))] %>% purrr::set_names(., 
                                                                            names(stocks)) %>% dplyr::bind_rows(.id = "stock")
    if (sum(grepl("prey$", names(out))) > 0) {
      stock.prey <- out[sprintf("%s.prey", names(stocks))] %>% 
        purrr::set_names(., names(stocks)) %>% dplyr::bind_rows(.id = "stock")
    }
    else {
      stock.prey <- tibble::tibble(year = NA_real_, step = NA_real_, 
                                   area = NA_character_, stock = NA_character_, age = NA_real_, 
                                   biomass_consumed = NA_real_, number_consumed = NA_real_, 
                                   mortality = NA_real_)
    }
    if (sum(grepl("prey", names(out))) > 0) {
      predator.prey <- out[grepl(".+\\.prey\\..+", names(out))] %>% 
        purrr::set_names(., names(.)) %>% purrr::keep(~"number_consumed" %in% 
                                                        names(.)) %>% dplyr::bind_rows(.id = "stock") %>% 
        tidyr::separate(.data$stock, c("prey", "predator"), 
                        sep = "\\.prey\\.") %>% dplyr::group_by(.data$year, 
                                                                .data$step, .data$prey, .data$predator) %>% dplyr::mutate(suit = .data$mortality/max(.data$mortality), 
                                                                                                                          suit = ifelse(is.finite(.data$suit), .data$suit, 
                                                                                                                                        0), length = gsub("len", "", .data$length) %>% 
                                                                                                                            as.numeric())
    }
    else {
      predator.prey <- tibble::tibble(year = NA_real_, step = NA_real_, 
                                      area = NA_character_, predator = NA_character_, prey = NA_character_, 
                                      length = NA_real_, suit = NA_real_, biomass_consumed = NA_real_, 
                                      mortality = NA_real_)
    }
    fleet.catches <- predator.prey %>% dplyr::group_by(.data$year, 
                                                       .data$area, .data$predator, .data$prey) %>% dplyr::summarise(amount = sum(.data$biomass_consumed)) %>% 
      dplyr::rename(fleet = .data$predator, stock = .data$prey)
    fleet.info <- stock.full %>% dplyr::left_join(predator.prey %>% 
                                                    dplyr::select(.data$year, .data$step, .data$area, fleet = .data$predator, 
                                                                  stock = .data$prey, .data$length, .data$suit), by = c("stock", 
                                                                                                                        "year", "step", "area", "length")) %>% dplyr::group_by(.data$year, 
                                                                                                                                                                               .data$step, .data$area, .data$fleet) %>% dplyr::summarise(harv.bio = sum(.data$suit * 
                                                                                                                                                                                                                                                          .data$number * .data$mean_weight)) %>% dplyr::left_join(fleet.catches %>% 
                                                                                                                                                                                                                                                                                                                    dplyr::group_by(.data$year, .data$fleet, .data$area) %>% 
                                                                                                                                                                                                                                                                                                                    dplyr::summarise(amount = sum(.data$amount)), by = c("year", 
                                                                                                                                                                                                                                                                                                                                                                         "area", "fleet")) %>% dplyr::group_by(.data$year, .data$step, 
                                                                                                                                                                                                                                                                                                                                                                                                               .data$area, .data$fleet) %>% dplyr::mutate(amount = ifelse(is.na(.data$amount), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          0, .data$amount), harv.rate = .data$amount/.data$harv.bio)
    if (!is.null(fleet.predict)) {
      d <- predator.prey %>% dplyr::filter(.data$predator %in% 
                                             fleet.predict$fleet)
    }
    else {
      d <- predator.prey
    }
    harv.suit <- d %>% dplyr::group_by(.data$year, .data$step, 
                                       .data$prey, .data$length) %>% dplyr::filter(.data$biomass_consumed > 
                                                                                     0) %>% dplyr::summarise(suit = sum(.data$biomass_consumed * 
                                                                                                                          .data$suit)/sum(.data$biomass_consumed)) %>% dplyr::rename(stock = .data$prey)
    print("Merging input and output")
    if ("surveyindices" %in% names(lik.dat$dat)) {
      sidat <- out[names(lik.dat$dat$surveyindices)] %>% purrr::set_names(., 
                                                                          names(.)) %>% dplyr::bind_rows(.id = "name") %>% 
        dplyr::left_join(lik$surveyindices %>% dplyr::select(.data$name, 
                                                             .data$stocknames, .data$sitype, .data$fittype), 
                         by = "name") %>% dplyr::bind_rows(tibble::tibble(length = NA, 
                                                                          age = NA, survey = NA, fleet = NA)) %>% dplyr::mutate(age = ifelse(.data$sitype == 
                                                                                                                                               "ages", .data$label, .data$age), length = ifelse(.data$sitype %in% 
                                                                                                                                                                                                  c("lengths", "fleets"), .data$label, .data$length), 
                                                                                                                                fleet = ifelse(.data$sitype == "effort", .data$label, 
                                                                                                                                               .data$fleet), survey = ifelse(.data$sitype == 
                                                                                                                                                                               "acoustic", .data$label, .data$survey)) %>% dplyr::left_join(lik.dat$dat$surveyindices %>% 
                                                                                                                                                                                                                                              purrr::set_names(., names(.)) %>% dplyr::bind_rows(.id = "name") %>% 
                                                                                                                                                                                                                                              dplyr::as_tibble() %>% dplyr::rename(observed = .data$number) %>% 
                                                                                                                                                                                                                                              dplyr::bind_rows(tibble::tibble(name = NA, year = NA, 
                                                                                                                                                                                                                                                                              step = NA, area = NA, length = NA, age = NA, 
                                                                                                                                                                                                                                                                              fleet = NA, survey = NA, upper = NA, lower = NA)) %>% 
                                                                                                                                                                                                                                              dplyr::filter(!is.na(.data$year)), by = c("name", 
                                                                                                                                                                                                                                                                                        "year", "step", "area", "age", "length", "fleet", 
                                                                                                                                                                                                                                                                                        "survey")) %>% dplyr::mutate(length = ifelse(.data$sitype %in% 
                                                                                                                                                                                                                                                                                                                                       c("lengths", "fleets"), paste(.data$lower, .data$upper, 
                                                                                                                                                                                                                                                                                                                                                                     sep = " - "), .data$length)) %>% dplyr::mutate(predicted = ifelse(grepl("loglinearfit", 
                                                                                                                                                                                                                                                                                                                                                                                                                                             tolower(.data$fittype)), exp(.data$intercept) * .data$number^.data$slope, 
                                                                                                                                                                                                                                                                                                                                                                                                                                       .data$intercept + .data$slope * .data$number)) %>% 
        dplyr::filter(!is.na(.data$name))
    }
    else {
      sidat <- NULL
    }
    if ("catchdistribution" %in% names(lik.dat$dat)) {
      dat.names <- names(lik.dat$dat$catchdistribution)
      aggs <- dat.names %>% purrr::set_names(., .) %>% purrr::map(~attr(lik.dat$dat$catchdistribution[[.]], 
                                                                        "len.agg")) %>% dplyr::bind_rows(.id = "name") %>% 
        tibble::as_tibble()
      catchdist.fleets <- lik.dat$dat$catchdistribution %>% 
        purrr::set_names(., names(.)) %>% purrr::map(. %>% 
                                                       dplyr::mutate(age = as.character(.data$age))) %>% 
        dplyr::bind_rows(.id = "name") %>% dplyr::right_join(out[dat.names] %>% 
                                                               purrr::set_names(., dat.names) %>% purrr::map(. %>% 
                                                                                                               dplyr::mutate(age = as.character(.data$age))) %>% 
                                                               dplyr::bind_rows(.id = "name") %>% dplyr::left_join(aggs, 
                                                                                                                   by = c("name", "length")), by = c("name", "length", 
                                                                                                                                                     "year", "step", "area", "age", "upper", "lower")) %>% 
        dplyr::ungroup() %>% dplyr::group_by(.data$name, 
                                             .data$year, .data$step, .data$area) %>% dplyr::mutate(total.catch = sum(.data$number.x, 
                                                                                                                     na.rm = TRUE), total.pred = sum(.data$number.y, na.rm = TRUE), 
                                                                                                   observed = .data$number.x/sum(.data$number.x, na.rm = TRUE), 
                                                                                                   predicted = .data$number.y/sum(.data$number.y, na.rm = TRUE)) %>% 
        dplyr::ungroup() %>% dplyr::group_by(.data$name, 
                                             .data$length, .data$age) %>% dplyr::mutate(upper = as.double(max(ifelse(is.na(.data$upper), 
                                                                                                                     0, .data$upper))), lower = as.double(max(ifelse(is.na(.data$lower), 
                                                                                                                                                                     0, .data$lower))), avg.length = as.numeric((.data$lower + 
                                                                                                                                                                                                                   .data$upper)/2), residuals = as.numeric(.data$observed - 
                                                                                                                                                                                                                                                             .data$predicted)) %>% dplyr::inner_join(lik$catchdistribution %>% 
                                                                                                                                                                                                                                                                                                       dplyr::select(.data$name, .data$fleetnames, .data$stocknames), 
                                                                                                                                                                                                                                                                                                     by = "name")
    }
    else {
      catchdist.fleets <- NULL
    }
    if (sum(grepl(".std", names(out), fixed = TRUE)) > 0) {
      if (is.null(f.age.range)) {
        f.age.range <- stock.prey %>% dplyr::group_by(.data$stock) %>% 
          dplyr::summarise(age.min = max(.data$age), age.max = max(.data$age))
      }
      f.by.year <- stock.prey %>% dplyr::left_join(f.age.range, 
                                                   by = "stock") %>% dplyr::group_by(.data$stock, .data$year, 
                                                                                     .data$area) %>% dplyr::summarise(catch = sum(.data$biomass_consumed), 
                                                                                                                      num.catch = sum(.data$number_consumed), F = mean(.data$mortality[.data$age >= 
                                                                                                                                                                                         .data$age.min & .data$age <= .data$age.max]))
      res.by.year <- stock.full %>% dplyr::filter(.data$step %in% 
                                                    steps) %>% dplyr::left_join(harv.suit, by = c("stock", 
                                                                                                  "year", "step", "length")) %>% dplyr::group_by(.data$stock, 
                                                                                                                                                 .data$year, .data$area, .data$step) %>% dplyr::summarise(total.number = sum(.data$number), 
                                                                                                                                                                                                          total.biomass = sum(.data$number * .data$mean_weight), 
                                                                                                                                                                                                          harv.biomass = sum(.data$number * .data$suit * .data$mean_weight), 
                                                                                                                                                                                                          ssb = sum(.data$mean_weight * logit(mat.par[1], mat.par[2], 
                                                                                                                                                                                                                                              .data$length) * .data$number)) %>% dplyr::left_join(f.by.year %>% 
                                                                                                                                                                                                                                                                                                    dplyr::mutate(area = .data$area), by = c("stock", 
                                                                                                                                                                                                                                                                                                                                             "year", "area")) %>% dplyr::left_join(stock.recruitment %>% 
                                                                                                                                                                                                                                                                                                                                                                                     dplyr::group_by(.data$stock, .data$year, .data$area) %>% 
                                                                                                                                                                                                                                                                                                                                                                                     dplyr::summarise(recruitment = sum(.data$recruitment)), 
                                                                                                                                                                                                                                                                                                                                                                                   by = c("stock", "year", "area")) %>% dplyr::ungroup()
    }
    else {
      res.by.year <- NULL
    }
    if ("stockdistribution" %in% names(lik.dat$dat)) {
      dat.names <- names(lik.dat$dat$stockdistribution)
      aggs <- dat.names %>% purrr::set_names(., .) %>% purrr::map(~attr(lik.dat$dat$stockdistribution[[.]], 
                                                                        "len.agg")) %>% dplyr::bind_rows(.id = "name") %>% 
        dplyr::as_tibble()
      stockdist <- lik.dat$dat$stockdistribution %>% purrr::set_names(., 
                                                                      names(.)) %>% dplyr::bind_rows(.id = "name") %>% 
        dplyr::right_join(out[dat.names] %>% purrr::set_names(., 
                                                              dat.names) %>% dplyr::bind_rows(.id = "name") %>% 
                            dplyr::left_join(aggs, by = c("name", "length")), 
                          by = c("name", "length", "year", "step", "area", 
                                 "age", "stock", "upper", "lower"), suffix = c(".y", 
                                                                               ".x")) %>% dplyr::group_by(.data$name, .data$year, 
                                                                                                          .data$step, .data$area, .data$age, .data$length) %>% 
        dplyr::mutate(pred.ratio = .data$number.x/sum(.data$number.x, 
                                                      na.rm = TRUE), obs.ratio = .data$number.y/sum(.data$number.y)) %>% 
        dplyr::ungroup() %>% dplyr::mutate(length = (.data$lower + 
                                                       .data$upper)/2) %>% dplyr::inner_join(lik$stockdistribution %>% 
                                                                                               dplyr::select(.data$name, .data$fleetnames, .data$stocknames) %>% 
                                                                                               dplyr::distinct(), by = "name")
    }
    else {
      stockdist <- NULL
    }
    if ("stomachcontent" %in% names(lik.dat$dat)) {
      pred.agg <- names(lik.dat$dat$stomachcontent) %>% purrr::set_names(., 
                                                                         .) %>% purrr::map(~attr(lik.dat$dat$stomachcontent[[.]], 
                                                                                                 "pred.agg")) %>% dplyr::bind_rows(.id = "component")
      prey.agg <- names(lik.dat$dat$stomachcontent) %>% purrr::set_names(., 
                                                                         .) %>% purrr::map(~attr(lik.dat$dat$stomachcontent[[.]], 
                                                                                                 "prey.agg")) %>% dplyr::bind_rows(.id = "component")
      stomachcontent <- lik.dat$dat$stomachcontent %>% dplyr::bind_rows(.id = "component") %>% 
        dplyr::select(-c(.data$prey.lower, .data$prey.upper, 
                         .data$lower, .data$upper)) %>% dplyr::right_join(out[lik.dat$dat$stomachcontent %>% 
                                                                                names()] %>% dplyr::bind_rows(.id = "component") %>% 
                                                                            dplyr::left_join(prey.agg) %>% dplyr::left_join(pred.agg), 
                                                                          by = c("component", "predator", "prey", "year", "step", 
                                                                                 "area")) %>% dplyr::group_by(.data$component, 
                                                                                                              .data$year, .data$step, .data$predator) %>% dplyr::mutate(observed = .data$ratio/sum(.data$ratio, 
                                                                                                                                                                                                   na.rm = TRUE), predicted = .data$number/sum(.data$number, 
                                                                                                                                                                                                                                               na.rm = TRUE), prey.length = (.data$prey.lower + 
                                                                                                                                                                                                                                                                               .data$prey.upper)/2, pred.length = (.data$lower + 
                                                                                                                                                                                                                                                                                                                     .data$upper)/2) %>% dplyr::as_tibble()
    }
    else {
      stomachcontent <- NULL
    }
    if ("catchstatistics" %in% names(lik.dat$dat)) {
      catchstatistics <- lik.dat$dat$catchstatistics %>% names() %>% 
        purrr::set_names(., .) %>% purrr::map(function(x) {
          out[[x]] %>% dplyr::rename(fitted_mean = .data$mean, 
                                     fitted_number = .data$number) %>% dplyr::left_join(lik.dat$dat$catchstatistics[[x]], 
                                                                                        by = c("year", "step", "area", "age")) %>% dplyr::rename(observed_mean = .data$mean, 
                                                                                                                                                 observed_number = .data$number)
        }) %>% dplyr::bind_rows(.id = "name") %>% dplyr::as_tibble()
    }
    else {
      catchstatistics <- NULL
    }
    out <- list(sidat = sidat, resTable = resTable, nesTable = nesTable, 
                suitability = predator.prey %>% dplyr::select(.data$year, 
                                                              .data$step, stock = .data$prey, fleet = .data$predator, 
                                                              .data$length, .data$suit), stock.recruitment = stock.recruitment, 
                res.by.year = res.by.year, stomachcontent = stomachcontent, 
                likelihoodsummary = out$likelihoodsummary %>% dplyr::as_tibble(), 
                catchdist.fleets = catchdist.fleets, stockdist = stockdist, 
                SS = SS, stock.full = stock.full, stock.std = stock.std, 
                stock.prey = stock.prey, fleet.info = fleet.info, predator.prey = predator.prey, 
                params = params, catchstatistics = catchstatistics)
    class(out) <- c("gadget.fit", class(out))
    save(out, file = sprintf("%s/WGTS.Rdata", wgts))
    setwd(old.dir)
    return(out)
  }






# #####################################################################################################################
# 
# 
# my_gadget.iterative <-
# function (main.file = "main", gadget.exe = "gadget", params.file = "params.in",
#           rew.sI = TRUE, run.final = TRUE, resume.final = FALSE, wgts = "WGTS",
#           grouping = NULL, optinfofile = "optinfofile", run.serial = FALSE,
#           method = "lm", cv.floor = NULL, comp = NULL, inverse = FALSE,
#           gd = NULL, rew.cik = FALSE, ...)
# {
#   warning("Function deprecated - has been replaced by gadget_iterative_*")
#   if (!is.null(gd)) {
#     Sys.setenv(GADGET_WORKING_DIR = normalizePath(gd))
#     main.file <- paste(gd, attr(gd, "mainfile"), sep = "/")
#     wgts <- paste(gd, wgts, sep = "/")
#   }
#   if (!file.exists(main.file)) {
#     stop("Main file not found")
#   }
#   if (!file.exists(params.file)) {
#     stop("Parameter file not found")
#   }
#   if (!file.exists(optinfofile)) {
#     stop("Optinfofile not found")
#   }
#   dir.create(wgts, showWarnings = FALSE)
#   main <- Rgadget:::read.gadget.main(main.file)
#   printfile <- NULL
#   likelihood <- my_read.gadget.likelihood(main$likelihoodfiles)
#   if (!is.null(comp)) {
#     likelihood <- get.gadget.likelihood(likelihood, comp = comp,
#                                         inverse = inverse)
#   }
#   tmp <- unlist(grouping)
#   if (length(tmp) != length(intersect(tmp, likelihood$weights$name))) {
#     stop("Error - invalid grouping")
#   }
#   main.init <- main
#   main.init$printfiles <- NULL
#   main.init$likelihoodfiles <- paste(wgts, "likelihood.init",
#                                      sep = "/")
#   write.gadget.likelihood(likelihood, file = paste(wgts, "likelihood.init",
#                                                    sep = "/"))
#   write.gadget.main(main.init, file = paste(wgts, "main.init",
#                                             sep = "/"))
#   callGadget(s = 1, main = paste(wgts, "main.init", sep = "/"),
#              o = paste(wgts, "lik.init", sep = "/"), i = params.file,
#              gadget.exe = gadget.exe, ...)
#   time <- Rgadget:::read.gadget.file(".", main$timefile, file_type = "time")
#   lik.dat <- Rgadget:::read.gadget.data(likelihood, year_range = time[[1]]$firstyear:time[[1]]$lastyear)
#   restr <- !(likelihood$weights$type %in% c("penalty", "understocking",
#                                             "migrationpenalty", if (rew.cik) NULL else "catchinkilos"))
#   SS <- Rgadget:::read.gadget.lik.out(paste(wgts, "lik.init", sep = "/"))$data[likelihood$weights$name[restr]]
#   sI.weights <- function(lik.dat, method = "lm") {
#     warning("Estimating survey weigths using a linear model is now deprecated and will be removed at later stage")
#     if (method == "lm") {
#       dat <- plyr::ldply(lik.dat$dat$surveyindices, function(x) x)
#       dat$y <- log(dat$number)
#       dat$year <- as.factor(dat$year)
#       fit <- stats::lm(y ~ year + length + step, dat)
#       weights <- (lik.dat$df$surveyindices - tapply(dat$length,
#                                                     dat$name, function(x) length(unique(x))))/tapply(stats::resid(fit),
#                                                                                                      dat$name, function(x) sum(x^2))
#     }
#     else {
#       weights <- plyr::ldply(lik.dat$dat$surveyindices,
#                              function(x) {
#                                time <- x$year + (x$step - 1)/4
#                                fit <- stats::predict(stats::loess(log(x$number) ~
#                                                                     time))
#                                length(fit)/sum((fit - log(x$number))^2)
#                              })$V1
#     }
#     names(weights) <- names(lik.dat$dat$surveyindices)
#     return(weights)
#   }
#   restr.SI <- subset(likelihood$weights, likelihood$weightstype ==
#                        "surveyindices")$name
#   if (!rew.sI) {
#     if (is.null(grouping)) {
#       grouping <- list(SI = intersect(likelihood$weights$name,
#                                       restr.SI))
#     }
#     else {
#       grouping$SI <- intersect(likelihood$weights$name,
#                                restr.SI)
#     }
#     sIw <- sI.weights(lik.dat, method = method)
#   }
#   run.string <- c(likelihood$weights$name[restr & !(likelihood$weights$name %in%
#                                                       unlist(grouping))])
#   run.string <- as.list(run.string)
#   names(run.string) <- c(likelihood$weights$name[restr & !(likelihood$weights$name %in%
#                                                              unlist(grouping))])
#   run.string <- append(run.string, grouping)
#   main.base <- main.init
#   main.base$likelihoodfiles <- paste(wgts, "likelihood.base",
#                                      sep = "/")
#   write.gadget.main(main.base, file = paste(wgts, "main.base",
#                                             sep = "/"))
#   likelihood.base <- likelihood
#   if (sum(is.infinite(1/unlist(SS)) > 0)) {
#     SS <- unlist(SS)
#     txt <- names(SS[is.infinite(1/SS)])
#     stop(sprintf("Model error, likelihood component %s returns a value of exactly 0\n",
#                  txt))
#   }
#   likelihood.base$weights[names(SS), "weight"] <- 1/as.numeric(SS)
#   run.iterative <- function(comp) {
#     likelihood <- likelihood.base
#     which.comp <- likelihood$weights$name %in% comp
#     likelihood$weights$weight[which.comp] <- 10000 * likelihood$weights$weight[which.comp]
#     comp <- paste(comp, collapse = ".")
#     print(sprintf("Running %s", comp))
#     write.gadget.likelihood(likelihood, file = paste(wgts,
#                                                      paste("likelihood", comp, sep = "."), sep = "/"))
#     main <- main.base
#     main$likelihoodfiles <- paste(wgts, paste("likelihood",
#                                               comp, sep = "."), sep = "/")
#     write.gadget.main(main, file = paste(wgts, paste("main",
#                                                      comp, sep = "."), sep = "/"))
#     callGadget(l = 1, main = paste(paste(wgts, "main", sep = "/"),
#                                    comp, sep = "."), i = params.file, p = paste(wgts,
#                                                                                 paste("params", comp, sep = "."), sep = "/"), opt = optinfofile,
#                gadget.exe = gadget.exe, ...)
#     callGadget(s = 1, main = paste(wgts, paste("main", comp,
#                                                sep = "."), sep = "/"), i = paste(wgts, paste("params",
#                                                                                              comp, sep = "."), sep = "/"), o = paste(wgts, paste("lik",
#                                                                                                                                                  comp, sep = "."), sep = "/"), gadget.exe = gadget.exe,
#                ...)
#     print(sprintf("Comp %s completed", comp))
#   }
#   if (!resume.final) {
#     if (run.serial) {
#       res <- lapply(run.string, run.iterative)
#     }
#     else {
#       res <- parallel::mclapply(run.string, run.iterative,
#                                 mc.cores = parallel::detectCores(logical = TRUE))
#     }
#   }
#   if (run.final) {
#     res <- plyr::ldply(run.string, function(x) {
#       tmp <- Rgadget:::read.gadget.lik.out(paste(wgts, paste("lik",
#                                                    paste(x, collapse = "."), sep = "."), sep = "/"))$data
#       tmp[likelihood$weights$name[restr]]
#     })
#     row.names(res) <- res$.id
#     res$.id <- NULL
#     run.final <- function(comp) {
#       print(sprintf("Running %s", comp))
#       callGadget(l = 1, main = sprintf("%s/main.%s", wgts,
#                                        comp), i = params.file, p = sprintf("%s/params.%s",
#                                                                            wgts, comp), opt = optinfofile, gadget.exe = gadget.exe,
#                  ...)
#       callGadget(s = 1, main = sprintf("%s/main.%s", wgts,
#                                        comp), i = sprintf("%s/params.%s", wgts, comp),
#                  o = sprintf("%s/lik.%s", wgts, comp), gadget.exe = gadget.exe,
#                  ...)
#       print(sprintf("Comp %s completed", comp))
#     }
#     SS <- plyr::ldply(names(run.string), function(x) data.frame(group = x,
#                                                                 comp = run.string[[x]], SS = as.numeric(res[x, run.string[[x]]])))
#     df <- plyr::ldply(lik.dat$df, function(x) data.frame(df = x,
#                                                          comp = names(x)))
#     SS <- dplyr::mutate(plyr::join(SS, df), sigmahat = SS/df)
#     SS$comp <- as.character(SS$comp)
#     write.files <- function(comp, weights) {
#       if (!is.null(cv.floor)) {
#         weights$sigmahat[weights$comp %in% restr.SI] <- pmax(weights$sigmahat[weights$comp %in%
#                                                                                 restr.SI], cv.floor)
#       }
#       if (sum(weights$sigmahat == 0) > 0) {
#         warning(sprintf("Perfect fit for component %s, weight 10*df used",
#                         weights$comp[weights$sigmahat == 0]))
#         weights$sigmahat[weights$sigmahat == 0] <- 0.1
#       }
#       main <- main.base
#       main$likelihoodfiles <- sprintf("%s/likelihood.%s",
#                                       wgts, comp)
#       write.gadget.main(main, sprintf("%s/main.%s", wgts,
#                                       comp))
#       likelihood <- likelihood.base
#       if (sum(is.infinite(1/weights$sigmahat)) > 0) {
#         txt <- weights %>% dplyr::filter(is.infinite(1/weights$sigmahat)) %>%
#           .$comp
#         stop(sprintf("Model error, likelihood component %s returns a value of exactly 0",
#                      txt))
#       }
#       likelihood$weights[weights$comp, "weight"] <- 1/weights$sigmahat
#       write.gadget.likelihood(likelihood, file = sprintf("%s/likelihood.%s",
#                                                          wgts, comp))
#     }
#     write.files("final", SS)
#     write.files("nodf", SS %>% dplyr::mutate(sigmahat = SS))
#     comp <- c("final", "nodf")
#     if (!rew.sI) {
#       SS[restr.SI, "sigmahat"] <- sIw[restr.SI]
#     }
#     if (!rew.sI) {
#       write.files("sIw", SS)
#       comp <- as.list(c("final", "nodf", "sIw"))
#     }
#     if (run.serial) {
#       lapply(comp, run.final)
#     }
#     else {
#       parallel::mclapply(comp, run.final, mc.cores = parallel::detectCores(logical = TRUE))
#     }
#   }
#   else {
#     comp <- NULL
#   }
#   invisible(list(comp = run.string, final = comp, wgts = wgts))
# }
# 



##################################################################################################################


my_gadget_fit <- 
  function (gd, params.in = attr(gd, "params_in"), fit = "FIT", 
            f.age.range = NULL, weighted.mortality = FALSE, rec.age = NULL, 
            rec.steps = NULL, steps = 1) 
  {
    fit <- Rgadget:::variant_strip_path(gd, fit)
    main <- read.gadget.file(gd, attr(gd, "mainfile"), recursive = FALSE)
    attr(main, "file_name") <- "main"
    stocks <- main$stock$stockfiles %>% purrr::set_names(., .) %>% 
      purrr::map(~read.gadget.file(path = gd, file_name = ., 
                                   file_type = "stock", recursive = FALSE))
    names(stocks) <- stocks %>% purrr::map(~.[[1]]["stockname"]) %>% 
      purrr::flatten()
    lik <- main$likelihood$likelihoodfiles %>% purrr::set_names(., 
                                                                .) %>% purrr::map(~read.gadget.file(path = gd, file_name = ., 
                                                                                                    file_type = "likelihood", recursive = TRUE) %>% purrr::discard(~.$type %in% 
                                                                                                                                                                     c("penalty", "understocking", "migrationpenalty"))) %>% 
      purrr::flatten(.)
    lik <- lik %>% purrr::set_names(., lik %>% purrr::map("name"))
    lik.dat <- lik %>% purrr::map(lik_to_tibble)
    
    browser()
    
    
    fleets <- main$fleet$fleetfiles %>% purrr::set_names(., .) %>% 
      purrr::map(~read.gadget.file(path = gd, file_name = ., 
                                   file_type = "fleet", recursive = TRUE, missingOkay = TRUE))
    gv <- gadget.variant.dir(gd, variant_dir = fit)
    write.gadget.file(main, gv)
    print <- gadgetprintfile("printfile", gv, missingOkay = TRUE)
    for (comp in (lik %>% purrr::map("name"))) {
      print <- print %>% gadget_update("likelihoodprinter", 
                                       printfile = comp, likelihoodcomponent = comp)
    }
    for (stock in names(stocks)) {
      print <- print %>% gadget_update("stockstdprinter", printfile = paste(stock, 
                                                                            "std", sep = "."), stockname = stock) %>% gadget_update("stockprinter", 
                                                                                                                                    printfile = paste(stock, "full", sep = "."), stocknames = stock, 
                                                                                                                                    area = livesonareas(stocks[[stock]]) %>% purrr::set_names(., 
                                                                                                                                                                                              .) %>% as.list(), age = list(allages = age_range(stocks[[stock]])), 
                                                                                                                                    len = tibble::tibble(lower = utils::head(length_range(stocks[[stock]]), 
                                                                                                                                                                             -1), upper = utils::tail(length_range(stocks[[stock]]), 
                                                                                                                                                                                                      -1)) %>% dplyr::mutate(label = as.ordered(.data$lower)) %>% 
                                                                                                                                      split(.$label) %>% purrr::set_names(., paste0("len", 
                                                                                                                                                                                    names(.))) %>% purrr::map(dplyr::select, -.data$label) %>% 
                                                                                                                                      purrr::map(unlist)) %>% gadget_update("stockprinter", 
                                                                                                                                                                            printfile = paste(stock, "recruitment", sep = "."), 
                                                                                                                                                                            stocknames = stock, area = livesonareas(stocks[[stock]]) %>% 
                                                                                                                                                                              purrr::set_names(., .) %>% as.list(), age = list(recage = ifelse(is.null(rec.age), 
                                                                                                                                                                                                                                               stocks[[stock]][[1]]$minage, rec.age)), len = list(alllen = length_range(stocks[[stock]])))
    }
    prey.subset <- stocks %>% purrr::keep(~.$iseaten$iseaten == 
                                            1) %>% names()
    pred.subset <- stocks %>% purrr::keep(~.$doeseat$doeseat == 
                                            1) %>% names()
    for (prey in prey.subset) {
      if (length(fleets) > 0) {
        print <- print %>% gadget_update("predatorpreyprinter", 
                                         printfile = sprintf("%s.prey", prey), predatornames = fleets[[1]] %>% 
                                           purrr::map(1), preynames = prey, area = livesonareas(stocks[[prey]]) %>% 
                                           purrr::set_names(., .) %>% as.list(), age = age_range(stocks[[prey]]) %>% 
                                           purrr::set_names(., .) %>% as.list(), len = list(alllen = length_range(stocks[[prey]])))
      }
      for (predator in c(pred.subset, fleets[[1]] %>% purrr::map(1))) {
        print <- print %>% gadget_update("predatorpreyprinter", 
                                         printfile = sprintf("%s.prey.%s", prey, predator), 
                                         predatornames = predator, preynames = prey, area = livesonareas(stocks[[prey]]) %>% 
                                           purrr::set_names(., .) %>% as.list(), age = list(allages = age_range(stocks[[prey]])), 
                                         len = tibble::tibble(lower = utils::head(length_range(stocks[[prey]]), 
                                                                                  -1), upper = utils::tail(length_range(stocks[[prey]]), 
                                                                                                           -1)) %>% dplyr::mutate(label = as.ordered(.data$lower)) %>% 
                                           dplyr::arrange(.data$lower) %>% split(.$label) %>% 
                                           purrr::set_names(., paste0("len", names(.))) %>% 
                                           purrr::map(dplyr::select, -.data$label) %>% 
                                           purrr::map(unlist))
      }
    }
    write.gadget.file(print, gv)
    gv <- gadget_evaluate(gv, params.in = params.in, params.out = tempfile(), 
                          log = paste(attr(gv, "variant_dir"), "fit.log", sep = "/"))
    out <- gadgetprintfile("printfile", gv) %>% purrr::set_names(., 
                                                                 purrr::map(., ~attr(.$printfile, "file_name")) %>% gsub("out/", 
                                                                                                                         "", .)) %>% purrr::map(print_to_tibble)
    print("Gathering results")
    stock.recruitment <- out[sprintf("%s.recruitment", names(stocks))] %>% 
      purrr::set_names(., names(stocks)) %>% dplyr::bind_rows(.id = "stock") %>% 
      dplyr::select(.data$stock, .data$year, .data$area, .data$step, 
                    recruitment = .data$number)
    if (!is.null(rec.steps)) {
      if (all(rec.steps %in% unique(stock.recruitment$step))) {
        stock.recruitment <- stock.recruitment %>% dplyr::filter(step %in% 
                                                                   rec.steps)
      }
      else {
        warning(paste0("Output not filtered by recruitment step because specified steps were not found. Available steps: ", 
                       paste(unique(stock.recruitment$step), collapse = ","), 
                       ", specified steps: ", paste(rec.steps, collapse = ",")))
      }
    }
    stock.full <- out[sprintf("%s.full", names(stocks))] %>% 
      purrr::set_names(., names(stocks)) %>% dplyr::bind_rows(.id = "stock") %>% 
      dplyr::mutate(length = as.numeric(gsub("len", "", .data$length)))
    stock.std <- out[sprintf("%s.std", names(stocks))] %>% purrr::set_names(., 
                                                                            names(stocks)) %>% dplyr::bind_rows(.id = "stock")
    if (sum(grepl("prey$", names(out))) > 0) {
      stock.prey <- out[sprintf("%s.prey", names(stocks))] %>% 
        purrr::set_names(., names(stocks)) %>% dplyr::bind_rows(.id = "stock")
    }
    else {
      stock.prey <- tibble::tibble(year = NA_real_, step = NA_real_, 
                                   area = NA_character_, stock = NA_character_, age = NA_real_, 
                                   biomass_consumed = NA_real_, number_consumed = NA_real_, 
                                   mortality = NA_real_)
    }
    if (sum(grepl("prey", names(out))) > 0) {
      predator.prey <- out[grepl(".+\\.prey\\..+", names(out))] %>% 
        purrr::set_names(., names(.)) %>% purrr::keep(~"number_consumed" %in% 
                                                        names(.)) %>% dplyr::bind_rows(.id = "stock") %>% 
        tidyr::separate(.data$stock, c("prey", "predator"), 
                        sep = "\\.prey\\.") %>% dplyr::group_by(.data$year, 
                                                                .data$step, .data$prey, .data$predator) %>% dplyr::mutate(suit = .data$mortality/max(.data$mortality), 
                                                                                                                          suit = ifelse(is.finite(.data$suit), .data$suit, 
                                                                                                                                        0), length = gsub("len", "", .data$length) %>% 
                                                                                                                            as.numeric())
    }
    else {
      predator.prey <- tibble::tibble(year = NA_real_, step = NA_real_, 
                                      area = NA_character_, predator = NA_character_, prey = NA_character_, 
                                      length = NA_real_, suit = NA_real_, biomass_consumed = NA_real_, 
                                      mortality = NA_real_)
    }
    fleet.catches <- predator.prey %>% dplyr::group_by(.data$year, 
                                                       .data$area, .data$predator, .data$prey) %>% dplyr::summarise(amount = sum(.data$biomass_consumed)) %>% 
      dplyr::rename(fleet = .data$predator, stock = .data$prey)
    fleet.info <- stock.full %>% dplyr::left_join(predator.prey %>% 
                                                    dplyr::select(.data$year, .data$step, .data$area, fleet = .data$predator, 
                                                                  stock = .data$prey, .data$length, .data$suit), by = c("stock", 
                                                                                                                        "year", "step", "area", "length")) %>% dplyr::group_by(.data$year, 
                                                                                                                                                                               .data$step, .data$area, .data$fleet) %>% dplyr::summarise(harv.bio = sum(.data$suit * 
                                                                                                                                                                                                                                                          .data$number * .data$mean_weight)) %>% dplyr::left_join(fleet.catches %>% 
                                                                                                                                                                                                                                                                                                                    dplyr::group_by(.data$year, .data$fleet, .data$area) %>% 
                                                                                                                                                                                                                                                                                                                    dplyr::summarise(amount = sum(.data$amount)), by = c("year", 
                                                                                                                                                                                                                                                                                                                                                                         "area", "fleet")) %>% dplyr::group_by(.data$year, .data$step, 
                                                                                                                                                                                                                                                                                                                                                                                                               .data$area, .data$fleet) %>% dplyr::mutate(amount = ifelse(is.na(.data$amount), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          0, .data$amount), harv.rate = .data$amount/.data$harv.bio)
    d <- predator.prey
    harv.suit <- d %>% dplyr::group_by(.data$year, .data$step, 
                                       .data$prey, .data$length) %>% dplyr::filter(.data$biomass_consumed > 
                                                                                     0) %>% dplyr::summarise(suit = sum(.data$biomass_consumed * 
                                                                                                                          .data$suit)/sum(.data$biomass_consumed)) %>% dplyr::rename(stock = .data$prey)
    print("Merging input and output")
    for (comp in names(lik.dat)) {
      out[[comp]] <- out[[comp]] %>% dplyr::mutate(area = as.character(.data$area))
      if (lik.dat[[comp]]$type[1] == "surveyindices") {
        if (lik.dat[[comp]]$sitype[1] == "lengths") {
          out[[comp]] <- out[[comp]] %>% dplyr::rename(length = .data$label)
        }
        else if (lik.dat[[comp]]$sitype[1] == "age") {
          out[[comp]] <- out[[comp]] %>% dplyr::rename(age = .data$label)
        }
        else if (lik.dat[[comp]]$sitype[1] == "effort") {
          out[[comp]] <- out[[comp]] %>% dplyr::rename(fleet = .data$label)
        }
        else if (lik.dat[[comp]]$sitype[1] == "acoustic") {
          out[[comp]] <- out[[comp]] %>% dplyr::rename(survey = .data$label)
        }
        else if (lik.dat[[comp]]$sitype[1] == "fleet") {
          out[[comp]] <- out[[comp]] %>% dplyr::rename(length = .data$label)
        }
      }
      lik.dat[[comp]] <- lik.dat[[comp]] %>% dplyr::left_join(out[[comp]], 
                                                              by = intersect(names(out[[comp]]), names(lik.dat[[comp]])))
    }
    if ("surveyindices" %in% (lik %>% purrr::map("type"))) {
      sidat <- lik.dat %>% purrr::keep(~"surveyindices" %in% 
                                         .$type) %>% dplyr::bind_rows() %>% dplyr::bind_rows(tibble::tibble(length = NA, 
                                                                                                            age = NA, survey = NA, fleet = NA)) %>% dplyr::mutate(length = ifelse(.data$sitype %in% 
                                                                                                                                                                                    c("lengths", "fleets"), paste(.data$lower, .data$upper, 
                                                                                                                                                                                                                  sep = " - "), .data$length)) %>% dplyr::mutate(number = .data$predicted, 
                                                                                                                                                                                                                                                                 predicted = ifelse(grepl("loglinearfit", tolower(.data$fittype)), 
                                                                                                                                                                                                                                                                                    exp(.data$intercept) * .data$number^.data$slope, 
                                                                                                                                                                                                                                                                                    .data$intercept + .data$slope * .data$number)) %>% 
        dplyr::filter(!is.na(.data$name))
    }
    else {
      sidat <- NULL
    }
    if ("catchdistribution" %in% (lik %>% purrr::map("type"))) {
      catchdist.fleets <- lik.dat %>% purrr::keep(~"catchdistribution" %in% 
                                                    .$type) %>% dplyr::bind_rows() %>% dplyr::group_by(.data$name, 
                                                                                                       .data$year, .data$step, .data$area) %>% dplyr::mutate(total.catch = sum(.data$observed, 
                                                                                                                                                                               na.rm = TRUE), total.pred = sum(.data$predicted, 
                                                                                                                                                                                                               na.rm = TRUE), obs = .data$observed, pred = .data$predicted, 
                                                                                                                                                             observed = .data$observed/sum(.data$observed, na.rm = TRUE), 
                                                                                                                                                             predicted = .data$predicted/sum(.data$predicted, 
                                                                                                                                                                                             na.rm = TRUE)) %>% dplyr::ungroup() %>% dplyr::group_by(.data$name, 
                                                                                                                                                                                                                                                     .data$length, .data$age) %>% dplyr::mutate(upper = as.double(max(ifelse(is.na(.data$upper), 
                                                                                                                                                                                                                                                                                                                             0, .data$upper))), lower = as.double(max(ifelse(is.na(.data$lower), 
                                                                                                                                                                                                                                                                                                                                                                             0, .data$lower))), avg.length = as.numeric((.data$lower + 
                                                                                                                                                                                                                                                                                                                                                                                                                           .data$upper)/2), residuals = as.numeric(.data$observed - 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                     .data$predicted))
    }
    else {
      catchdist.fleets <- NULL
    }
    if (sum(grepl(".std", names(out), fixed = TRUE)) > 0) {
      if (is.null(f.age.range)) {
        weighted.mortality <- FALSE
        f.age.range <- stock.prey %>% dplyr::group_by(.data$stock) %>% 
          dplyr::summarise(age.min = max(.data$age), age.max = max(.data$age))
      }
      f.by.year <- stock.prey %>% dplyr::left_join(f.age.range, 
                                                   by = "stock") %>% dplyr::group_by(.data$stock, .data$year, 
                                                                                     .data$area) %>% dplyr::summarise(catch = sum(.data$biomass_consumed), 
                                                                                                                      num.catch = sum(.data$number_consumed), F = mean(.data$mortality[.data$age >= 
                                                                                                                                                                                         .data$age.min & .data$age <= .data$age.max]))
      if (weighted.mortality) {
        f.by.year <- f.by.year %>% select(-F) %>% left_join(stock.std %>% 
                                                              dplyr::select(-c(number_consumed, biomass_consumed)) %>% 
                                                              dplyr::left_join(stock.prey %>% dplyr::select(year, 
                                                                                                            step, age, area, stock, number_consumed, biomass_consumed, 
                                                                                                            mortality)) %>% dplyr::left_join(f.age.range, 
                                                                                                                                             by = "stock") %>% tibble::as_tibble() %>% dplyr::filter(.data$age >= 
                                                                                                                                                                                                       .data$age.min & .data$age <= .data$age.max) %>% 
                                                              dplyr::group_by(year, step) %>% dplyr::summarise(F = -log(1 - 
                                                                                                                          sum(number_consumed)/sum(number))/0.25) %>% dplyr::ungroup() %>% 
                                                              dplyr::group_by(year) %>% dplyr::summarise(F = mean(F)))
      }
      res.by.year <- stock.full %>% dplyr::filter(.data$step %in% 
                                                    steps) %>% dplyr::left_join(harv.suit, by = c("stock", 
                                                                                                  "year", "step", "length")) %>% dplyr::group_by(.data$stock, 
                                                                                                                                                 .data$year, .data$area, .data$step) %>% dplyr::summarise(total.number = sum(.data$number), 
                                                                                                                                                                                                          total.biomass = sum(.data$number * .data$mean_weight), 
                                                                                                                                                                                                          harv.biomass = sum(.data$number * .data$suit * .data$mean_weight)) %>% 
        dplyr::left_join(f.by.year %>% dplyr::mutate(area = .data$area), 
                         by = c("stock", "year", "area")) %>% dplyr::left_join(stock.recruitment %>% 
                                                                                 dplyr::group_by(.data$stock, .data$year, .data$area) %>% 
                                                                                 dplyr::summarise(recruitment = sum(.data$recruitment)), 
                                                                               by = c("stock", "year", "area")) %>% dplyr::ungroup()
    }
    else {
      res.by.year <- NULL
    }
    if ("stockdistribution" %in% (lik %>% purrr::map("type"))) {
      stockdist <- lik.dat %>% purrr::keep(~"stockdistribution" %in% 
                                             .$type) %>% dplyr::bind_rows() %>% dplyr::group_by(.data$name, 
                                                                                                .data$year, .data$step, .data$area, .data$age, .data$length) %>% 
        dplyr::mutate(pred.ratio = .data$predicted/sum(.data$predicted, 
                                                       na.rm = TRUE), obs.ratio = .data$observed/sum(.data$observed)) %>% 
        dplyr::ungroup() %>% dplyr::mutate(length = (.data$lower + 
                                                       .data$upper)/2)
    }
    else {
      stockdist <- NULL
    }
    if ("stomachcontent" %in% (lik %>% purrr::map("type"))) {
      stomachcontent <- lik.dat %>% purrr::keep(~"stomachcontent" %in% 
                                                  .$type) %>% dplyr::bind_rows() %>% dplyr::group_by(.data$component, 
                                                                                                     .data$year, .data$step, .data$predator) %>% dplyr::mutate(observed = .data$ratio/sum(.data$ratio, 
                                                                                                                                                                                          na.rm = TRUE), predicted = .data$number/sum(.data$number, 
                                                                                                                                                                                                                                      na.rm = TRUE), prey.length = (.data$prey.lower + 
                                                                                                                                                                                                                                                                      .data$prey.upper)/2, pred.length = (.data$lower + 
                                                                                                                                                                                                                                                                                                            .data$upper)/2) %>% dplyr::as_tibble()
    }
    else {
      stomachcontent <- NULL
    }
    if ("catchstatistics" %in% (lik %>% purrr::map("type"))) {
      catchstatistics <- lik.dat %>% purrr::keep(~"catchstatistics" %in% 
                                                   .$type) %>% dplyr::bind_rows()
    }
    else {
      catchstatistics <- NULL
    }
    out <- list(sidat = sidat, suitability = predator.prey %>% 
                  dplyr::select(.data$year, .data$step, stock = .data$prey, 
                                fleet = .data$predator, .data$length, .data$suit), 
                stock.recruitment = stock.recruitment, res.by.year = res.by.year, 
                stomachcontent = stomachcontent, likelihoodsummary = out$likelihoodsummary %>% 
                  dplyr::as_tibble(), catchdist.fleets = catchdist.fleets, 
                stockdist = stockdist, stock.full = stock.full, stock.std = stock.std, 
                stock.prey = stock.prey, fleet.info = fleet.info, predator.prey = predator.prey, 
                catchstatistics = catchstatistics)
    class(out) <- c("gadget.fit", class(out))
    save(out, file = sprintf("%s/fit.Rdata", variant_full_path(gd, 
                                                               variant_within_path(gd, fit))))
    return(out)
  }
