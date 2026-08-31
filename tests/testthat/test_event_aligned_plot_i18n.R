event_aligned_plot_i18n_fixture <- function() {
  list(
    message = "",
    raster = data.frame(
      event_name = "move",
      trial_id = "1",
      train = "n1",
      rel_time_sec = 0.1,
      pattern_label = "burst",
      raster_row = 1,
      train_index = 1,
      event_id = "event-1",
      stringsAsFactors = FALSE
    ),
    psth = data.frame(
      train = rep("n1", 2),
      rel_time_sec = c(-0.1, 0.1),
      mean_rate_hz = c(2, 4),
      sem_rate_hz = c(0.2, 0.3),
      stringsAsFactors = FALSE
    ),
    population = data.frame(
      rel_time_sec = c(-0.1, 0.1),
      mean_rate_hz = c(2, 4),
      sem_rate_hz = c(0.2, 0.3),
      stringsAsFactors = FALSE
    ),
    heatmap = data.frame(
      train = rep(c("n1", "n2"), each = 2),
      train_index = rep(1:2, each = 2),
      rel_time_sec = rep(c(-0.1, 0.1), 2),
      z_rate = c(-1, 1, -0.5, 0.5),
      stringsAsFactors = FALSE
    ),
    correlation = data.frame(
      train_x = rep(c("n1", "n2"), each = 2),
      train_y = rep(c("n1", "n2"), 2),
      correlation = c(1, 0.2, 0.2, 1),
      stringsAsFactors = FALSE
    ),
    correlogram = data.frame(
      pair = rep("n1 -> n2", 2),
      lag_sec = c(-0.1, 0.1),
      count_per_event = c(0.5, 1),
      stringsAsFactors = FALSE
    )
  )
}

event_aligned_plot_i18n_builders <- function(res, lang = "en") {
  list(
    raster = stpd_event_aligned_raster_plot(res, lang = lang),
    psth = stpd_event_aligned_psth_plot(res, lang = lang),
    population = stpd_event_aligned_population_plot(res, lang = lang),
    heatmap = stpd_event_aligned_heatmap_plot(res, lang = lang),
    correlation = stpd_event_aligned_correlation_plot(res, lang = lang),
    correlogram = stpd_event_aligned_correlogram_plot(res, lang = lang)
  )
}

event_aligned_plot_i18n_build <- function(p) {
  suppressMessages(suppressWarnings(plotly::plotly_build(p)))
}

event_aligned_plot_i18n_axis_title <- function(built, axis) {
  title <- built$x$layout[[axis]]$title
  if (is.list(title)) as.character(title$text) else as.character(title)
}

event_aligned_plot_i18n_trace_copy <- function(built) {
  unlist(lapply(built$x$data, function(trace) {
    colorbar_title <- if (is.list(trace$colorbar)) trace$colorbar$title else NULL
    c(trace$name, trace$hovertemplate, trace$text, colorbar_title)
  }), use.names = FALSE)
}

test_that("all event-aligned Plotly builders localize visible copy without mutating results", {
  skip_if_not_installed("plotly")

  res <- event_aligned_plot_i18n_fixture()
  original <- unserialize(serialize(res, NULL, version = 3))
  zh <- lapply(
    event_aligned_plot_i18n_builders(res, lang = "zh"),
    event_aligned_plot_i18n_build
  )
  en <- lapply(
    event_aligned_plot_i18n_builders(res, lang = "en"),
    event_aligned_plot_i18n_build
  )

  expect_identical(res, original)
  expect_identical(
    vapply(zh, function(x) x$x$layout$title$text, character(1)),
    c(
      raster = "事件对齐 raster",
      psth = "各神经元的事件周围放电率 / PSTH",
      population = "群体平均放电率 ± SEM",
      heatmap = "神经元级 z 标准化放电率热图",
      correlation = "事件时间箱间的脉冲计数相关性",
      correlogram = "任务事件周围的互相关图"
    )
  )
  expect_identical(
    vapply(en, function(x) x$x$layout$title$text, character(1)),
    c(
      raster = "Event-aligned raster",
      psth = "Peri-event firing rate / PSTH by neuron",
      population = "Population mean firing rate +/- SEM",
      heatmap = "Neuron-wise z-scored firing-rate heatmap",
      correlation = "Spike-count correlation across event bins",
      correlogram = "Cross-correlogram around task events"
    )
  )

  expect_identical(
    unname(vapply(
      zh[1:4],
      event_aligned_plot_i18n_axis_title,
      character(1),
      axis = "xaxis"
    )),
    rep("距事件起点的时间（s）", 4)
  )
  expect_identical(
    vapply(zh[c("raster", "psth", "population", "heatmap", "correlogram")],
           event_aligned_plot_i18n_axis_title, character(1), axis = "yaxis"),
    c(
      raster = "脉冲序列 / 事件试次",
      psth = "放电率（Hz）",
      population = "平均放电率（Hz/神经元）",
      heatmap = "脉冲序列 / 神经元",
      correlogram = "脉冲序列配对"
    )
  )
  expect_identical(
    event_aligned_plot_i18n_axis_title(zh$correlogram, "xaxis"),
    "时滞：目标 - 参考（s）"
  )

  zh_copy <- lapply(zh, event_aligned_plot_i18n_trace_copy)
  en_copy <- lapply(en, event_aligned_plot_i18n_trace_copy)
  expect_true(any(grepl("事件：move", zh_copy$raster, fixed = TRUE)))
  expect_true(any(grepl("时间=%{x:.3f}s", zh_copy$psth, fixed = TRUE)))
  expect_true(any(grepl("平均放电率=", zh_copy$population, fixed = TRUE)))
  expect_true(any(grepl("基线 z", zh_copy$heatmap, fixed = TRUE)))
  expect_true(any(grepl("%{y} 与 %{x}", zh_copy$correlation, fixed = TRUE)))
  expect_true(any(grepl("计数/事件", zh_copy$correlogram, fixed = TRUE)))
  expect_true(any(grepl("event: move", en_copy$raster, fixed = TRUE)))
  expect_true(any(grepl("rate=%{y:.3f} Hz", en_copy$psth, fixed = TRUE)))
  expect_true(any(grepl("Population mean", en_copy$population, fixed = TRUE)))
  expect_true(any(grepl("baseline z", en_copy$heatmap, fixed = TRUE)))
  expect_true(any(grepl("%{y} vs %{x}", en_copy$correlation, fixed = TRUE)))
  expect_true(any(grepl("count/event=%{z:.3f}", en_copy$correlogram, fixed = TRUE)))
})

test_that("event-aligned empty states are nonblank and translate known detector messages", {
  skip_if_not_installed("plotly")

  empty <- list(
    message = "",
    raster = data.frame(),
    psth = data.frame(),
    population = data.frame(),
    heatmap = data.frame(),
    correlation = data.frame(),
    correlogram = data.frame()
  )
  zh <- lapply(
    event_aligned_plot_i18n_builders(empty, lang = "zh"),
    event_aligned_plot_i18n_build
  )
  en <- lapply(
    event_aligned_plot_i18n_builders(empty, lang = "en"),
    event_aligned_plot_i18n_build
  )
  annotation <- function(x) x$x$layout$annotations[[1L]]$text

  expect_identical(
    vapply(zh, annotation, character(1)),
    c(
      raster = "暂无事件对齐的 spike。",
      psth = "暂无可计算的 PSTH。",
      population = "暂无可计算的群体放电率摘要。",
      heatmap = "暂无可计算的神经元热图。",
      correlation = "请至少选择两条脉冲序列，以计算脉冲计数相关性。",
      correlogram = "请至少选择两条脉冲序列，以计算互相关图。"
    )
  )
  expect_identical(
    vapply(en, annotation, character(1)),
    c(
      raster = "No event-aligned spikes are available.",
      psth = "No PSTH can be computed.",
      population = "No population-rate summary can be computed.",
      heatmap = "No neuron heatmap can be computed.",
      correlation = "Select at least two trains to compute spike-count correlation.",
      correlogram = "Select at least two trains to compute cross-correlograms."
    )
  )

  known <- stpd_event_aligned_empty_result("No spike trains are loaded.")
  known_zh <- event_aligned_plot_i18n_build(
    stpd_event_aligned_raster_plot(known, lang = "zh")
  )
  known_en <- event_aligned_plot_i18n_build(
    stpd_event_aligned_raster_plot(known, lang = "en")
  )
  expect_identical(annotation(known_zh), "尚未加载脉冲序列。")
  expect_identical(annotation(known_en), "No spike trains are loaded.")
})

test_that("every event-aligned Plotly builder declares the requested locale", {
  skip_if_not_installed("plotly")

  res <- event_aligned_plot_i18n_fixture()
  zh <- event_aligned_plot_i18n_builders(res, lang = "zh")
  en <- event_aligned_plot_i18n_builders(res, lang = "en")

  for (name in names(zh)) {
    expect_identical(zh[[name]]$x$config$locale, "zh-CN", info = name)
    expect_identical(en[[name]]$x$config$locale, "en", info = name)
    zh_dependencies <- vapply(zh[[name]]$dependencies, `[[`, character(1), "name")
    en_dependencies <- vapply(en[[name]]$dependencies, `[[`, character(1), "name")
    expect_true("plotly-locale-zh-CN" %in% zh_dependencies, info = name)
    expect_false(any(grepl("^plotly-locale-", en_dependencies)), info = name)
  }

  empty_zh <- stpd_event_aligned_empty_plot(lang = "zh")
  empty_en <- stpd_event_aligned_empty_plot(lang = "en")
  expect_identical(empty_zh$x$config$locale, "zh-CN")
  expect_identical(empty_en$x$config$locale, "en")
})
