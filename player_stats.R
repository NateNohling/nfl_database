#' Get Player Stats from Play-By-Play
#'
#' @param pbp nflfastR play-by-play data set
#' @param weekly If TRUE returns stats by week, otherwise entire data set
#'
#' @return A tibble with player stats
#' @export
#'
#' @import magrittr
#' @import dplyr
#' @import fastrmodels
#' @importFrom glue glue
#' @importFrom rlang get_env current_env
#' @importFrom tibble tibble rowid_to_column
#' @importFrom data.table fcase
#' @importFrom mgcv predict.gam
calculate_player_stats <- function(pbp, weekly = FALSE) {
  add_dakota <- function(add_to_this, pbp, weekly) {
    url <- glue::glue(
      "https://github.com/nflverse/nflfastR-data/blob/master/models/",
      "dakota_model.Rdata?raw=true"
    ) %>%
      base::url()

    base::load(url, envir = rlang::current_env())

    dakota_model <- rlang::env_get(
      env = rlang::current_env(),
      nm = "dakota_model"
    )

    base::suppressMessages({
      df <- pbp %>%
        dplyr::filter(.data$pass == 1) %>%
        dplyr::filter(
          !base::is.na(.data$posteam) &
            !base::is.na(.data$qb_epa) &
            !base::is.na(.data$id) &
            !base::is.na(.data$down)
        ) %>%
        dplyr::mutate(epa = dplyr::if_else(
          .data$qb_epa < -4.5, -4.5, .data$qb_epa
        ))
    })

    if (base::isTRUE(weekly)) {
      relevant_players <- add_to_this %>%
        dplyr::filter(.data$pa >= 5) %>%
        dplyr::mutate(
          filter_id =
            base::paste(.data$season, .data$player_id, .data$week, sep = "_")
        ) %>%
        dplyr::pull(.data$filter_id)

      model_data <- df %>%
        dplyr::group_by(.data$season, .data$id, .data$week) %>%
        dplyr::summarize(
          n_plays = dplyr::n(),
          epa_per_play = base::sum(.data$epa) / .data$n_plays,
          cpoe = base::mean(.data$cpoe, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(cpoe = dplyr::if_else(
          base::is.na(.data$cpoe), 0, .data$cpoe
        )) %>%
        dplyr::rename(player_id = .data$id) %>%
        dplyr::mutate(
          filter_id =
            base::paste(.data$season, .data$player_id, .data$week, sep = "_")
        ) %>%
        dplyr::filter(.data$filter_id %in% relevant_players)

      model_data$dakota <-
        mgcv::predict.gam(dakota_model, model_data) %>%
        base::as.vector()

      out <- add_to_this %>%
        dplyr::left_join(
          model_data %>%
            dplyr::select(
              .data$season,
              .data$player_id,
              .data$week,
              .data$dakota
            ),
          by = c("season", "player_id", "week")
        )
    } else {
      relevant_players <- add_to_this %>%
        dplyr::filter(.data$pa >= 5) %>%
        dplyr::pull(.data$player_id)

      model_data <- df %>%
        dplyr::group_by(.data$id) %>%
        dplyr::summarize(
          n_plays = dplyr::n(),
          epa_per_play = base::sum(.data$epa) / .data$n_plays,
          cpoe = base::mean(.data$cpoe, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(cpoe = dplyr::if_else(
          base::is.na(.data$cpoe), 0, .data$cpoe
        )) %>%
        dplyr::rename(player_id = .data$id) %>%
        dplyr::filter(.data$player_id %in% relevant_players)

      model_data$dakota <-
        mgcv::predict.gam(dakota_model, model_data) %>%
        base::as.vector()

      out <- add_to_this %>%
        dplyr::left_join(
          model_data %>%
            dplyr::select(.data$player_id, .data$dakota),
          by = c("player_id")
        )
    }

    return(out)
  }
  add_xyac_fcase <- function(data) {
    add_context_cols <- function(data) {
      data %>%
        dplyr::mutate(
          era0 = dplyr::if_else(.data$season <= 2001, 1, 0),
          era1 = dplyr::if_else(.data$season > 2001 & .data$season <= 2005, 1, 0),
          era2 = dplyr::if_else(.data$season > 2005 & .data$season <= 2013, 1, 0),
          era3 = dplyr::if_else(.data$season > 2013 & .data$season <= 2017, 1, 0),
          era4 = dplyr::if_else(.data$season > 2017, 1, 0),
          era = dplyr::case_when(
            .data$era0 == 1 ~ 0,
            .data$era1 == 1 ~ 1,
            .data$era2 == 1 ~ 2,
            .data$era3 == 1 | era4 == 1 ~ 3
          ),
          era = base::as.factor(.data$era),
          down1 = dplyr::if_else(.data$down == 1, 1, 0),
          down2 = dplyr::if_else(.data$down == 2, 1, 0),
          down3 = dplyr::if_else(.data$down == 3, 1, 0),
          down4 = dplyr::if_else(.data$down == 4, 1, 0),
          home = dplyr::if_else(.data$posteam == .data$home_team, 1, 0),
          model_roof = dplyr::if_else(
            base::is.na(.data$roof) |
              .data$roof == "open" |
              .data$roof == "closed",
            base::as.character("retractable"),
            base::as.character(.data$roof)
          ),
          model_roof = base::as.factor(.data$model_roof),
          retractable = dplyr::if_else(.data$model_roof == "retractable", 1, 0),
          dome = dplyr::if_else(.data$model_roof == "dome", 1, 0),
          outdoors = dplyr::if_else(.data$model_roof == "outdoors", 1, 0)
        )
    }
    cols_xyac_model <- {
      c(
        "air_yards",
        "yardline_100",
        "ydstogo",
        "distance_to_goal",
        "down1",
        "down2",
        "down3",
        "down4",
        "air_is_zero",
        "pass_middle",
        "era2",
        "era3",
        "era4",
        "qb_hit",
        "home",
        "outdoors",
        "retractable",
        "dome",
        "distance_to_sticks"
      )
    }
    cols_xyac <- {
      c(
        "xyac_epa",
        "xyac_mean_yardage",
        "xyac_median_yardage",
        "xyac_success",
        "xyac_fd"
      )
    }

    xyac_model_data <- data %>%
      dplyr::select(-dplyr::any_of(cols_xyac)) %>%
      tibble::rowid_to_column("index")

    passes <- xyac_model_data %>%
      add_context_cols() %>%
      dplyr::mutate(
        pass_middle = dplyr::if_else(.data$pass_location == "middle", 1, 0),
        air_is_zero = dplyr::if_else(.data$air_yards == 0, 1, 0),
        distance_to_sticks = .data$air_yards - .data$ydstogo,
        distance_to_goal = .data$yardline_100 - .data$air_yards,
        valid_pass = dplyr::if_else(
          (.data$complete_pass == 1 |
             .data$incomplete_pass == 1 |
             .data$interception == 1) &
            !base::is.na(.data$air_yards) &
            .data$air_yards >= -15 &
            .data$air_yards < 70 &
            !base::is.na(.data$receiver_player_name) &
            !base::is.na(.data$pass_location),
          1,
          0
        )
      ) %>%
      dplyr::filter(.data$valid_pass == 1, .data$distance_to_goal != 0)

    if (!nrow(passes) == 0) {
      join_data <- passes %>%
        dplyr::select(
          .data$index,
          .data$distance_to_goal,
          .data$week,
          .data$home_team,
          .data$roof,
          .data$half_seconds_remaining,
          .data$down,
          .data$ydstogo,
          .data$posteam_timeouts_remaining,
          .data$defteam_timeouts_remaining,
          original_spot = .data$yardline_100,
          .data$air_epa,
          .data$air_yards
        ) %>%
        dplyr::mutate(
          dplyr::across(c(.data$down, .data$ydstogo), base::as.integer),
          original_ydstogo = .data$ydstogo
        ) %>%
        dplyr::relocate(.data$original_ydstogo, .after = .data$ydstogo)

      xyac_model_results <-
        stats::predict(
          fastrmodels::xyac_model,
          passes %>%
            dplyr::select(dplyr::any_of(cols_xyac_model)) %>%
            base::as.matrix()
        ) %>%
        tibble::as_tibble_col("prob") %>%
        dplyr::bind_cols(
          tibble::tibble(
            yac = base::rep_len(
              x = -5:70,
              length.out = base::nrow(passes) * 76
            ),
            index = base::rep(
              x = passes$index,
              times = base::rep_len(
                x = 76,
                length.out = base::nrow(passes)
              )
            )
          )
        ) %>%
        dplyr::left_join(join_data, by = "index")

      xyac_model_mutations <- xyac_model_results %>%
        dplyr::group_by(.data$index) %>%
        dplyr::mutate(
          max_loss =
            dplyr::if_else(.data$distance_to_goal < 95,
                           -5,
                           .data$distance_to_goal - 99
            ),
          max_gain =
            dplyr::if_else(.data$distance_to_goal > 70,
                           70,
                           .data$distance_to_goal
            ),
          cum_prob = base::cumsum(.data$prob),
          prob = data.table::fcase(
            .data$yac == .data$max_loss, .data$cum_prob,
            .data$yac > .data$max_loss & .data$yac < .data$max_gain, .data$prob,
            .data$yac == .data$max_gain, 1 - dplyr::lag(.data$cum_prob, 1)
          ),
          yardline_100 = .data$distance_to_goal - .data$yac
        ) %>%
        dplyr::filter(
          .data$yac >= .data$max_loss,
          .data$yac <= .data$max_gain
        ) %>%
        dplyr::select(-.data$cum_prob) %>%
        dplyr::mutate(
          gain = .data$original_spot - .data$yardline_100
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$index, .data$gain, .data$prob)

      xyac_data <- data %>%
        tibble::rowid_to_column("index") %>%
        dplyr::left_join(xyac_model_mutations, by = c("index")) %>%
        dplyr::select(-.data$index)
    } else {
      xyac_data <- data %>%
        dplyr::mutate(
          gain = NA_real_,
          prob = NA_real_
        )
    }

    return(xyac_data)
  }


  # Prepare data ------------------------------------------------------------

  # load plays with multiple laterals
  url <- glue::glue(
    "https://github.com/mrcaseb/nfl-data/blob/master/data/",
    "lateral_yards/multiple_lateral_yards.rds?raw=true"
  )

  mult_lats <- base::url(url) %>%
    base::readRDS() %>%
    dplyr::mutate(
      season = base::substr(.data$game_id, 1, 4),
      week = base::substr(.data$game_id, 6, 7),
      dplyr::across(.data$season:.data$week, base::as.integer)
    ) %>%
    dplyr::filter(.data$yards != 0) %>%
    dplyr::group_by(.data$game_id, .data$play_id) %>%
    dplyr::slice(seq_len(dplyr::n() - 1)) %>%
    dplyr::ungroup()

  base::suppressMessages({
    data <- pbp %>%
      dplyr::filter(
        !base::is.na(.data$down),
        .data$play_type %in% c("pass", "qb_kneel", "qb_spike", "run")
      )

    two_points <- pbp %>%
      dplyr::filter(.data$two_point_conv_result == "success") %>%
      dplyr::select(
        .data$week,
        .data$season,
        .data$posteam,
        .data$pass_attempt,
        .data$rush_attempt,
        .data$passer_player_name,
        .data$passer_player_id,
        .data$rusher_player_name,
        .data$rusher_player_id,
        .data$lateral_rusher_player_name,
        .data$lateral_rusher_player_id,
        .data$receiver_player_name,
        .data$receiver_player_id,
        .data$lateral_receiver_player_name,
        .data$lateral_receiver_player_id
      )
  })

  if (!"special" %in% names(pbp)) { # we need this column for the special teams tds
    pbp <- pbp %>%
      dplyr::mutate(
        special = dplyr::if_else(
          .data$play_type %in% c("extra_point", "field_goal", "kickoff", "punt"),
          1, 0
        )
      )
  }

  s_type <- pbp %>%
    dplyr::select(
      .data$season,
      .data$season_type,
      .data$week
    ) %>%
    dplyr::distinct()

  # Team stats -----------------------------------------------------------

  team_data <- data %>%
    dplyr::filter(.data$play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(.data$season, .data$posteam, .data$week) %>%
    dplyr::summarize(
      team_pa = base::sum(
        .data$complete_pass == 1 |
          .data$incomplete_pass == 1 |
          .data$interception == 1
      ),
      team_ay = base::sum(.data$air_yards, na.rm = TRUE),
      .groups = "drop"
    )

  #
  # Expected stats -----------------------------------------------------------

  xyac_data <- data %>%
    dplyr::filter(
      .data$play_type %in% c("pass"),
      .data$pass_attempt == 1,
      .data$two_point_attempt == 0,
      !base::is.na(.data$receiver_id)
    ) %>%
    add_xyac_fcase() %>%
    dplyr::mutate(
      py_gain = dplyr::if_else(.data$yardline_100 == .data$air_yards,
                               .data$yardline_100,
                               .data$gain
      ),
      td_gain = dplyr::if_else(.data$py_gain == .data$yardline_100, 1, 0),
      cmp_exp = .data$cp,
      yac_prob = dplyr::if_else(.data$yardline_100 == .data$air_yards,
                                1,
                                .data$prob
      ),
      cmp_run_prob = .data$cp * .data$yac_prob,
      py_exp = .data$py_gain * .data$cmp_run_prob,
      tdp_exp = .data$td_gain * .data$cmp_run_prob,
      recy_exp = .data$gain * .data$cmp_run_prob
    ) %>%
    dplyr::select(
      .data$season,
      .data$passer_player_id,
      .data$receiver_player_id,
      .data$week,
      .data$posteam,
      .data$game_id,
      .data$play_id,
      .data$py_gain,
      .data$td_gain,
      .data$cmp_exp,
      .data$cmp_run_prob,
      .data$py_exp,
      .data$tdp_exp,
      .data$recy_exp
    )

  passing_data_exp <- xyac_data %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$passer_player_id) %>%
    dplyr::mutate(pa = dplyr::if_else(dplyr::row_number() == 1, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$season,
      .data$passer_player_id,
      .data$week,
      .data$posteam
    ) %>%
    dplyr::summarize(
      cmp_exp = base::sum(.data$cmp_exp * .data$pa, na.rm = T),
      py_exp = base::sum(.data$py_exp, na.rm = T),
      tdp_exp = base::sum(.data$tdp_exp, na.rm = T),
      dkp_exp = .data$py_exp / 25 + .data$tdp_exp * 4,
      .groups = "drop"
    ) %>%
    dplyr::rename(
      player_id = .data$passer_player_id,
      team = .data$posteam
    )

  receiving_data_exp <- xyac_data %>%
    dplyr::group_by(.data$game_id, .data$play_id, .data$receiver_player_id) %>%
    dplyr::mutate(trg = dplyr::if_else(dplyr::row_number() == 1, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$season,
      .data$receiver_player_id,
      .data$week,
      .data$posteam
    ) %>%
    dplyr::summarize(
      rec_exp = base::sum(.data$cmp_exp * .data$trg, na.rm = T),
      recy_exp = base::sum(.data$py_exp, na.rm = T),
      tdrec_exp = base::sum(.data$tdp_exp, na.rm = T),
      dkrec_exp = .data$rec_exp + .data$recy_exp / 10 + .data$tdrec_exp * 6,
      .groups = "drop"
    ) %>%
    dplyr::rename(
      player_id = .data$receiver_player_id,
      team = .data$posteam
    )

  # Passing stats -----------------------------------------------------------

  passing_data <- data %>%
    dplyr::filter(.data$play_type %in% c("pass", "qb_spike")) %>%
    dplyr::group_by(
      .data$season,
      .data$passer_player_id,
      .data$week,
      .data$posteam
    ) %>%
    dplyr::summarize(
      pass_plays = dplyr::n(),
      pa = base::sum(
        .data$complete_pass == 1 |
          .data$incomplete_pass == 1 |
          .data$interception == 1
      ),
      cmp = base::sum(.data$complete_pass == 1),
      pass_ay = base::sum(.data$air_yards, na.rm = TRUE),
      pass_adot = .data$pass_ay / .data$pa,
      pass_yac = base::sum(
        (.data$passing_yards - .data$air_yards) * .data$complete_pass,
        na.rm = TRUE
      ),
      py = base::sum(.data$passing_yards, na.rm = TRUE),
      tdp = base::sum(
        .data$touchdown == 1 &
          .data$td_team == .data$posteam &
          .data$complete_pass == 1
      ),
      int = base::sum(.data$interception),
      pass_fuml = base::sum(
        .data$fumble_lost == 1 &
          .data$fumbled_1_player_id == .data$passer_player_id
      ),
      sck = base::sum(.data$sack),
      sck_yds = -1 * base::sum(.data$yards_gained * .data$sack),
      fdp = base::sum(.data$first_down_pass),
      cpoe = base::mean(.data$cpoe, na.rm = TRUE),
      pass_epa_p = base::sum(.data$qb_epa, na.rm = TRUE) / .data$pass_plays,
      pass_epa = base::sum(.data$qb_epa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      player_id = .data$passer_player_id,
      team = .data$posteam
    )

  if (base::isTRUE(weekly)) {
    passing_data <- add_dakota(passing_data, pbp = pbp, weekly = T)
  }

  pass_2pt <- two_points %>%
    dplyr::filter(.data$pass_attempt == 1) %>%
    dplyr::group_by(
      .data$season,
      .data$passer_player_id,
      .data$week,
      .data$posteam
    ) %>%
    dplyr::summarise(
      pass_2pt = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      player_id = .data$passer_player_id,
      team = .data$posteam
    )

  passing_data <- passing_data %>%
    dplyr::left_join(
      passing_data_exp,
      by = c("season", "player_id", "week", "team")
    ) %>%
    dplyr::full_join(
      pass_2pt,
      by = c("season", "player_id", "week", "team")
    ) %>%
    dplyr::mutate(
      pass_2pt =
        dplyr::if_else(
          base::is.na(.data$pass_2pt),
          0L,
          .data$pass_2pt
        )
    ) %>%
    dplyr::select(
      .data$season:.data$cmp,
      .data$cmp_exp,
      .data$pass_ay:.data$py,
      .data$py_exp,
      .data$tdp,
      .data$tdp_exp,
      .data$int:.data$fdp,
      .data$pass_2pt,
      .data$cpoe:.data$dkp_exp
    ) %>%
    dplyr::filter(!base::is.na(.data$player_id))

  # Rushing stats -----------------------------------------------------------

  rushes <- data %>%
    dplyr::filter(.data$play_type %in% c("run", "qb_kneel")) %>%
    dplyr::group_by(
      .data$season,
      .data$rusher_player_id,
      .data$week,
      .data$posteam
    ) %>%
    dplyr::summarize(
      ra = dplyr::n(),
      ry = base::sum(.data$rushing_yards, na.rm = TRUE),
      ypc = .data$ry / .data$ra,
      tdr = base::sum(.data$td_player_id == .data$rusher_player_id,
                      na.rm = TRUE
      ),
      rush_fuml = base::sum(
        .data$fumble_lost == 1 &
          .data$fumbled_1_player_id == .data$rusher_player_id &
          base::is.na(.data$lateral_rusher_player_id)
      ),
      fdr = base::sum(
        .data$first_down_rush &
          base::is.na(.data$lateral_rusher_player_id)
      ),
      rush_epa_p = base::sum(.data$epa, na.rm = TRUE) / .data$ra,
      rush_epa = base::sum(.data$epa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      player_id = .data$rusher_player_id,
      team = .data$posteam
    )

  laterals <- data %>%
    dplyr::filter(!base::is.na(.data$lateral_rusher_player_id)) %>%
    dplyr::group_by(
      .data$season, .data$lateral_rusher_player_id, .data$week
    ) %>%
    dplyr::summarize(
      la = dplyr::n(),
      ly = base::sum(.data$lateral_rushing_yards, na.rm = TRUE),
      fdl = base::sum(.data$first_down_rush, na.rm = TRUE),
      tdl = base::sum(.data$td_player_id == .data$lateral_rusher_player_id,
                      na.rm = TRUE
      ),
      lat_fuml = base::sum(.data$fumble_lost, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(player_id = .data$lateral_rusher_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_rushing" &
            .data$season %in% data$season &
            .data$week %in% data$week
        ) %>%
        dplyr::select(
          .data$season,
          player_id = .data$gsis_player_id,
          ly = .data$yards
        ) %>%
        dplyr::mutate(tdl = 0L, la = 1L)
    )

  rushing_data <- rushes %>%
    dplyr::left_join(laterals, by = c("season", "player_id", "week")) %>%
    dplyr::mutate(
      dplyr::across(.data$la:.data$lat_fuml, ~ tidyr::replace_na(., 0))
    ) %>%
    dplyr::mutate(
      ry = .data$ry + .data$ly,
      tdr = .data$tdr + .data$tdl,
      fdr = .data$fdr + .data$fdl,
      rush_fuml = .data$rush_fuml + .data$lat_fuml
    ) %>%
    dplyr::select(.data$season:.data$rush_epa)

  rush_2pt <- two_points %>%
    dplyr::filter(.data$rush_attempt == 1) %>%
    dplyr::group_by(
      .data$season,
      .data$rusher_player_id,
      .data$week,
      .data$posteam
    ) %>%
    dplyr::summarise(
      rush_2pt = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      player_id = .data$rusher_player_id,
      team = .data$posteam
    )

  rushing_data <- rushing_data %>%
    dplyr::full_join(
      rush_2pt,
      by = c("season", "player_id", "week", "team")
    ) %>%
    dplyr::mutate(
      rush_2pt =
        dplyr::if_else(
          base::is.na(.data$rush_2pt),
          0L,
          .data$rush_2pt
        )
    ) %>%
    dplyr::relocate(.data$rush_2pt, .after = .data$fdr) %>%
    dplyr::filter(!base::is.na(.data$player_id))

  # Receiving stats ---------------------------------------------------------

  rec <- data %>%
    dplyr::filter(!base::is.na(.data$receiver_player_id)) %>%
    dplyr::group_by(
      .data$season,
      .data$receiver_player_id,
      .data$week,
      .data$posteam
    ) %>%
    dplyr::summarize(
      trg = dplyr::n(),
      rec = base::sum(.data$complete_pass == 1),
      recy = base::sum(.data$receiving_yards, na.rm = TRUE),
      ypt = .data$recy / .data$trg,
      ay = base::sum(.data$air_yards, na.rm = TRUE),
      adot = .data$ay / .data$trg,
      yac = base::sum(.data$yards_after_catch, na.rm = TRUE),
      tdrec = base::sum(.data$td_player_id == .data$receiver_player_id,
                        na.rm = TRUE
      ),
      rec_fuml = base::sum(.data$fumble_lost == 1 &
                             .data$fumbled_1_player_id == .data$receiver_player_id &
                             base::is.na(.data$lateral_receiver_player_id)),
      fdrec = base::sum(.data$first_down_pass &
                          base::is.na(.data$lateral_receiver_player_id)),
      racr = .data$recy / .data$ay,
      rec_epa_p = base::sum(.data$epa, na.rm = TRUE) / .data$trg,
      rec_epa = base::sum(.data$epa, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(team_data, by = c("season", "posteam", "week")) %>%
    dplyr::mutate(
      trg_share = (.data$trg / .data$team_pa) * 100,
      ay_share = (.data$ay / .data$team_ay) * 100,
      wopr = .data$trg_share * 1.5 + .data$ay_share * .7,
      .after = .data$racr
    ) %>%
    dplyr::relocate(dplyr::contains("team"), .after = .data$wopr) %>%
    dplyr::rename(
      player_id = .data$receiver_player_id,
      team = .data$posteam,
    )

  laterals <- data %>%
    dplyr::filter(!base::is.na(.data$lateral_receiver_player_id)) %>%
    dplyr::group_by(
      .data$season, .data$lateral_receiver_player_id, .data$week
    ) %>%
    dplyr::summarize(
      la = dplyr::n(),
      ly = base::sum(.data$lateral_receiving_yards, na.rm = TRUE),
      tdl = base::sum(
        .data$td_player_id == .data$lateral_receiver_player_id,
        na.rm = TRUE
      ),
      fdl = base::sum(.data$first_down_pass, na.rm = T),
      lat_fuml = base::sum(.data$fumble_lost, na.rm = T),
      .groups = "drop"
    ) %>%
    dplyr::rename(player_id = .data$lateral_receiver_player_id) %>%
    dplyr::bind_rows(
      mult_lats %>%
        dplyr::filter(
          .data$type == "lateral_receiving" &
            .data$season %in% data$season &
            .data$week %in% data$week
        ) %>%
        dplyr::select(
          .data$season,
          player_id = .data$gsis_player_id,
          ly = .data$yards
        ) %>%
        dplyr::mutate(tdl = 0L, la = 1L)
    )

  receiving_data <- rec %>%
    dplyr::left_join(laterals, by = c("season", "player_id", "week")) %>%
    dplyr::mutate(
      dplyr::across(.data$la:.data$lat_fuml, ~ tidyr::replace_na(., 0))
    ) %>%
    dplyr::mutate(
      recy = .data$recy + .data$ly,
      yac = .data$yac + .data$ly,
      tdrec = .data$tdrec + .data$tdl,
      fdrec = .data$fdrec + .data$fdl,
      rec_fuml = .data$rec_fuml + .data$lat_fuml
    ) %>%
    dplyr::select(.data$season:.data$rec_epa)

  rec_2pt <- two_points %>%
    dplyr::filter(.data$pass_attempt == 1) %>%
    dplyr::group_by(
      .data$season,
      .data$receiver_player_id,
      .data$week,
      .data$posteam
    ) %>%
    dplyr::summarise(
      rec_2pt = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      player_id = .data$receiver_player_id,
      team = .data$posteam
    )

  receiving_data <- receiving_data %>%
    dplyr::left_join(
      receiving_data_exp,
      by = c("season", "player_id", "week", "team")
    ) %>%
    dplyr::full_join(
      rec_2pt,
      by = c("season", "player_id", "week", "team")
    ) %>%
    dplyr::mutate(
      rec_2pt =
        dplyr::if_else(
          base::is.na(.data$rec_2pt),
          0L,
          .data$rec_2pt
        )
    ) %>%
    dplyr::relocate(.data$rec_2pt, .after = .data$fdrec) %>%
    dplyr::filter(!base::is.na(.data$player_id))

  # Special Teams -----------------------------------------------------------

  st_data <- pbp %>%
    dplyr::filter(.data$special == 1 & !base::is.na(.data$td_player_id)) %>%
    dplyr::group_by(
      .data$season,
      .data$td_player_id,
      .data$week,
      .data$td_team
    ) %>%
    dplyr::summarise(
      tdst = base::sum(.data$touchdown, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      player_id = .data$td_player_id,
      team = .data$td_team
    )

  # Combine all stats -------------------------------------------------------

  player_stats <- passing_data %>%
    dplyr::full_join(rushing_data,
                     by = c("season", "player_id", "week", "team")
    ) %>%
    dplyr::full_join(receiving_data,
                     by = c("season", "player_id", "week", "team")
    ) %>%
    dplyr::full_join(st_data,
                     by = c("season", "player_id", "week", "team")
    ) %>%
    dplyr::filter(!base::is.na(.data$player_id)) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ tidyr::replace_na(., 0)
      )
    ) %>%
    dplyr::mutate(
      fuml = .data$pass_fuml + .data$rush_fuml + .data$rec_fuml,
      pass_bonus = dplyr::if_else(.data$py >= 300, 1, 0),
      rush_bonus = dplyr::if_else(.data$ry >= 100, 1, 0),
      rec_bonus = dplyr::if_else(.data$recy >= 100, 1, 0),
      bonus = .data$pass_bonus + .data$rush_bonus + .data$rec_bonus
    ) %>%
    dplyr::mutate(
      dkp =
        .data$tdp * 4 +
        .data$py / 25 -
        .data$int -
        .data$pass_fuml +
        .data$pass_bonus * 3,
      dkr =
        .data$tdr * 6 +
        .data$ry / 10 -
        .data$rush_fuml +
        .data$rush_bonus * 3,
      dkrec =
        .data$tdrec * 6 +
        .data$rec +
        .data$recy / 10 -
        .data$rec_fuml +
        .data$rec_bonus * 3,
      dk =
        .data$tdp * 4 +
        .data$py / 25 -
        .data$int +
        .data$tdr * 6 +
        .data$ry / 10 +
        .data$tdrec * 6 +
        .data$recy / 10 +
        .data$rec +
        .data$tdst * 6 -
        .data$fuml +
        .data$bonus * 3
    ) %>%
    dplyr::relocate(.data$dkp, .before = .data$dkp_exp)

  if (isFALSE(weekly)) {
    player_stats <- player_stats %>%
      dplyr::group_by(.data$season, .data$player_id, .data$team) %>%
      dplyr::summarise(
        games = dplyr::n(),
        cmp = base::sum(.data$cmp),
        cmp_exp = base::sum(.data$cmp_exp),
        pass_ay = base::sum(.data$pass_ay),
        pass_adot = .data$pass_ay / base::sum(.data$pa),
        pass_yac = base::sum(.data$pass_yac),
        py = base::sum(.data$py),
        py_exp = base::sum(.data$py_exp),
        tdp = base::sum(.data$tdp),
        tdp_exp = base::sum(.data$tdp_exp),
        int = base::sum(.data$int),
        pass_fuml = base::sum(.data$pass_fuml),
        sck = base::sum(.data$sck),
        sck_yds = base::sum(.data$sck_yds),
        fdp = base::sum(.data$fdp),
        fdp = base::sum(.data$fdp),
        cpoe = base::sum(.data$cpoe * .data$pa) / base::sum(.data$pa),
        pass_epa_p = base::sum(.data$pass_epa) / base::sum(.data$pass_plays),
        pass_epa = base::sum(.data$pass_epa),
        pass_plays = base::sum(.data$pass_plays),
        pa = base::sum(.data$pa),
        dkp = base::sum(.data$dkp),
        dkp_exp = base::sum(.data$dkp_exp),
        ra = base::sum(.data$ra),
        ry = base::sum(.data$ry),
        ypc = .data$ry / .data$ra,
        tdr = base::sum(.data$tdr),
        rush_fuml = base::sum(.data$rush_fuml),
        fdr = base::sum(.data$fdr),
        rush_epa_p = base::sum(.data$rush_epa) / .data$ra,
        rush_epa = base::sum(.data$rush_epa),
        dkr = base::sum(.data$dkr),
        trg = base::sum(.data$trg),
        rec = base::sum(.data$rec),
        rec_exp = base::sum(.data$rec_exp),
        recy = base::sum(.data$recy),
        recy_exp = base::sum(.data$recy_exp),
        ypt = .data$recy / .data$trg,
        ay = base::sum(.data$ay),
        adot = .data$ay / .data$trg,
        yac = base::sum(.data$yac),
        tdrec = base::sum(.data$tdrec),
        tdrec_exp = base::sum(.data$tdrec_exp),
        rec_fuml = base::sum(.data$rec_fuml),
        fdrec = base::sum(.data$fdrec),
        racr = .data$recy / .data$ay,
        trg_share = (.data$trg / base::sum(.data$team_pa)) * 100,
        ay_share = (.data$ay / base::sum(.data$team_ay)) * 100,
        wopr = .data$trg_share * 1.5 + .data$ay_share * .7,
        rec_epa_p = base::sum(.data$rec_epa) / .data$trg,
        rec_epa = base::sum(.data$rec_epa),
        dkrec = base::sum(.data$dkrec),
        dkrec_exp = base::sum(.data$dkrec_exp),
        epa_p = (.data$pass_epa + .data$rush_epa + .data$rec_epa) /
          (.data$pass_plays + .data$ra + .data$trg),
        epa = .data$pass_epa + .data$rush_epa + .data$rec_epa,
        dk = base::sum(.data$dk),
        .groups = "drop"
      ) %>%
      add_dakota(pbp = pbp, weekly = weekly) %>%
      dplyr::select(
        .data$season:.data$games,
        .data$pass_plays:.data$pa,
        .data$cmp:.data$pass_epa,
        .data$dakota,
        dplyr::everything()
      )
  }

  return(player_stats)
}

#' Summarise Player Stats
#'
#' @param player_stats a tibble of player stats by week or year
#' @param position  position group to return summary for,
#'                  one of: passer, rusher, receiver
#'
#' @return a tibble with summarised player stats
#' @export
#'
#' @importFrom stringr str_remove
get_stats_summary <- function(player_stats, position) {
  player_data <- rlang::env_get(env = .GlobalEnv, nm = "player_data")

  if (position == "passer") {
    stats_summary <- player_stats %>%
      dplyr::left_join(player_data, by = c("player_id")) %>%
      dplyr::filter(
        .data$pos == "QB",
        .data$pa > 0
      ) %>%
      dplyr::select(
        .data$season,
        passer = .data$player,
        passer_id = .data$player_id,
        .data$pos,
        .data$team,
        dplyr::any_of("week"),
        .data$pass_plays:.data$dakota,
        .data$dkp:.data$dkp_exp,
        -dplyr::contains("2pt")
      ) %>%
      dplyr::rename_with(
        ~ stringr::str_remove(., "pass_"),
        !dplyr::contains("epa")
      ) %>%
      dplyr::mutate(
        dplyr::across(
          where(is.numeric),
          ~ base::round(., 2)
        )
      ) %>%
      dplyr::arrange(-.data$dkp)
  }
  if (position == "rusher") {
    stats_summary <- player_stats %>%
      dplyr::left_join(player_data, by = c("player_id")) %>%
      dplyr::filter(
        .data$pos %in% c("QB", "RB", "WR", "TE"),
        .data$ra > 0
      ) %>%
      dplyr::select(
        .data$season,
        rusher = .data$player,
        rusher_id = .data$player_id,
        .data$pos,
        .data$team,
        .data$ra:.data$dkr
      ) %>%
      dplyr::mutate(
        dplyr::across(
          where(is.numeric),
          ~ base::round(., 2)
        )
      ) %>%
      dplyr::arrange(-.data$dkr)
  }
  if (position == "receiver") {
    stats_summary <- player_stats %>%
      dplyr::left_join(player_data, by = c("player_id")) %>%
      dplyr::filter(
        .data$pos %in% c("RB", "WR", "TE"),
        .data$trg > 0
      ) %>%
      dplyr::select(
        .data$season,
        receiver = .data$player,
        receiver_id = .data$player_id,
        .data$pos,
        .data$team,
        .data$trg:.data$dkrec_exp,
        -.data$rec_fuml
      ) %>%
      dplyr::mutate(
        dplyr::across(
          where(is.numeric),
          ~ base::round(., 2)
        )
      ) %>%
      dplyr::arrange(-.data$dkrec)
  }

  stats_summary <- stats_summary %>%
    dplyr::mutate(
      pos = base::factor(
        .data$pos,
        levels = c("QB", "RB", "WR", "TE")
      )
    )

  return(stats_summary)
}

#' Parse Height into inches
#'
#' @param data Dataset with height variable
#'
#' @return parsed height
#'
#' @importFrom stringr str_split
#' @importFrom purrr mao_dbl
parse_height <- function(data) {
  parsed_height <- stringr::str_split(data, "-") %>%
    purrr::map_dbl(function(x) {
      base::as.numeric(x[[1]]) * 12 +
        base::as.numeric(x[[2]])
    })

  return(parsed_height)
}

# Dumb code until tidyselect exports where
utils::globalVariables("where")

# Get Data ---------------------------------------------------------------------
library(magrittr)
season <- 2020

pbp <- nflfastR::load_pbp(2020) %>%
  dplyr::filter(
    .data$season %in% {{ season }}, .data$season_type == "REG"
  )

player_data_full <- nflfastR::fast_scraper_roster(season)

player_data <- player_data_full %>%
  dplyr::filter(!base::is.na(.data$height)) %>%
  dplyr::select(.data$season,
                dplyr::contains("name"),
                pos = .data$position,
                .data$team,
                .data$birth_date:.data$weight,
                dplyr::contains("_id")
  ) %>%
  dplyr::mutate(
    dplyr::across(.data$birth_date, base::as.character),
    dplyr::across(.data$height, functions:::parse_height),
    dplyr::across(.data$weight, readr::parse_number)
  )

player_stats_weekly <- calculate_player_stats(pbp, weekly = T)
player_stats_season <- calculate_player_stats(pbp, weekly = F)

# Player Stats Summary -----------------------------------------------------
passer_summary <- get_stats_summary(player_stats_season, "passer")
rusher_summary <- get_stats_summary(player_stats_season, "rusher")
receiver_summary <- get_stats_summary(player_stats_season, "receiver")

