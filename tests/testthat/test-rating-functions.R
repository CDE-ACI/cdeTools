test_that("get_cut_scores works", {
  cuts <- get_cut_scores()

  expect_true(is.list(cuts))
  expect_true("traditional" %in% names(cuts))
  expect_true("aec" %in% names(cuts))
  expect_true(is.data.frame(cuts$traditional))
})

test_that("get_rating works as expected (GRO simple case)", {
  cuts <- get_cut_scores()
  trad <- cuts$traditional

  # GRO only → filter on indicator + measure (GROWTH)
  gro_cuts <- trad |>
    dplyr::filter(indicator == "GRO", measure == "GROWTH")

  excd_cut <- gro_cuts |> dplyr::filter(rating_short == "excd") |> dplyr::pull(cutscore) |> dplyr::first()
  meet_cut <- gro_cuts |> dplyr::filter(rating_short == "meet") |> dplyr::pull(cutscore) |> dplyr::first()
  appr_cut <- gro_cuts |> dplyr::filter(rating_short == "appr") |> dplyr::pull(cutscore) |> dplyr::first()

  expect_equal(get_rating(excd_cut + 0.1, "GRO", "GROWTH", "E", "ELA", trad, 30), "Exceeds")
  expect_equal(get_rating(meet_cut + 0.1, "GRO", "GROWTH", "E", "ELA", trad, 30), "Meets")
  expect_equal(get_rating(appr_cut + 0.1, "GRO", "GROWTH", "E", "ELA", trad, 30), "Approaching")
  expect_equal(get_rating(appr_cut - 1, "GRO", "GROWTH", "E", "ELA", trad, 30), "Does Not Meet")

  # N < 20 case
  expect_equal(get_rating(75, "GRO", "GROWTH", "E", "ELA", trad, "n < 20"), "-")
})

test_that("get_pts works as expected", {
  expect_equal(get_pts("Exceeds", "ALL STUDENTS"), 8)
  expect_equal(get_pts("Meets", "ALL STUDENTS"), 6)
  expect_equal(get_pts("Approaching", "ALL STUDENTS"), 4)
  expect_equal(get_pts("Does Not Meet", "ALL STUDENTS"), 2)
  expect_equal(get_pts("-", "ALL STUDENTS"), 0)

  expect_equal(get_pts("Exceeds", "SUBGROUP"), 1)
  expect_equal(get_pts("Meets", "SUBGROUP"), 0.75)
})

test_that("apply_rating adds columns correctly", {
  cuts <- get_cut_scores()

  test_df <- data.frame(
    MEDIAN_SGP    = c(80, 50, 30, 15, NA),
    INDICATOR     = rep("GRO", 5),
    MEASURE       = rep("GROWTH", 5),
    EMH_CODE      = rep("M", 5),
    SUBJECT       = rep("ELA", 5),
    N_COUNT_SGP   = c(30, 30, 30, "n < 20", 30),
    CATEGORY      = rep("ALL STUDENTS", 5),
    stringsAsFactors = FALSE
  )

  rated_df <- apply_rating(
    test_df,
    score_col     = "MEDIAN_SGP",
    indicator_col = "INDICATOR",
    measure_col   = "MEASURE",
    emh_col       = "EMH_CODE",
    subject_col   = "SUBJECT",
    n_col         = "N_COUNT_SGP",
    category_col  = "CATEGORY",
    cuts_df       = cuts$traditional,
    rating_col    = "growth_rating",
    pts_col       = "growth_pts"
  )

  # columns created
  expect_true("growth_rating" %in% names(rated_df))
  expect_true("growth_pts"    %in% names(rated_df))

  # growth_rating is an ordered factor
  expect_s3_class(rated_df$growth_rating, "ordered")

  # check the 4th element: as.character() so "-" matches
  expect_equal(
    as.character(rated_df$growth_rating[4]),
    "-"
  )

  # check the 4th point value and its type
  expect_equal(rated_df$growth_pts[4], 0)
  expect_type(rated_df$growth_pts, "double")
})
