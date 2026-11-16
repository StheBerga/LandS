#' Run univariate Cox proportional hazards models
#'
#' Fits separate Cox models for numeric and categorical predictors and returns
#' formatted hazard ratios, 95% confidence intervals and p-values.
#'
#' @param data Data frame containing survival outcomes and predictors.
#' @param vars Character vector of predictor names.
#' @param ptime Name of the survival time column.
#' @param pevent Name of the event indicator column (typically 0/1).
#' @param continuous_contrast Contrast for numeric predictors: "IQR" (75th vs
#'   25th percentile; default) or "unit" (one-unit increase).
#'
#' @return Data frame with columns `Var`, `HR`, `95%CI`, `pvalue` and numeric
#'   `pval_raw`. Numeric predictors have one row. For categorical predictors,
#'   a global test row is followed by the reference level and other levels.
#' @details The IQR is computed using complete cases for each univariate model.
#'   All global p-values use likelihood-ratio tests; p-values of individual
#'   non-reference categorical levels use Wald tests. Changing the contrast
#'   for numeric predictors changes HRs and CIs, not p-values.
#'   Categorical factors use their first observed level as reference.
#'   Unordered factors are used even when the original factor is ordered.
#' @author Luca Lalli
#' @examples
#' data(cancer, package="survival")
#' d <- subset(bladder, enum == 1)
#' d$rx <- factor(d$rx, levels = c(1, 2), labels = c("placebo", "thiotepa"))
#' univariate_cox(d, c("number", "size", "rx"), "stop", "event", "IQR")
#' univariate_cox(d, c("number", "size", "rx"), "stop", "event", "unit")
#' @export
univariate_cox <- function(data, vars, ptime, pevent,
                           continuous_contrast = c("IQR", "unit")) {

  continuous_contrast <- match.arg(continuous_contrast)

  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  if (!is.character(vars) || length(vars) == 0L || anyNA(vars)) {
    stop("'vars' must be a non-empty character vector without NA values.")
  }
  if (anyDuplicated(vars)) stop("'vars' contains duplicated names.")
  if (!is.character(ptime) || length(ptime) != 1L || is.na(ptime) ||
      !is.character(pevent) || length(pevent) != 1L || is.na(pevent)) {
    stop("'ptime' and 'pevent' must each be one column name.")
  }
  missing_columns <- setdiff(c(ptime, pevent, vars), names(data))
  if (length(missing_columns)) {
    stop("Missing columns in 'data': ", paste(missing_columns, collapse = ", "))
  }

  fmt_value <- function(x) {
    if (length(x) != 1L || is.na(x)) return(NA_character_)
    if (is.infinite(x)) return(if (x > 0) "Inf" else "-Inf")
    sprintf("%.3f", x)
  }
  fmt_p <- function(x) {
    if (length(x) != 1L || !is.finite(x)) return(NA_character_)
    if (x < 0.001) "<0.001" else sprintf("%.3f", x)
  }
  make_row <- function(variable, hr = NA_character_, ci = NA_character_,
                       p = NA_real_, p_label = NULL) {
    if (is.null(p_label)) p_label <- fmt_p(p)
    data.frame(
      Var = as.character(variable),
      HR = as.character(hr),
      "95%CI" = as.character(ci),
      pvalue = as.character(p_label),
      pval_raw = as.numeric(p),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  empty_row <- function(variable, reason) {
    warning(sprintf("Skipping '%s': %s", variable, reason), call. = FALSE)
    make_row(variable)
  }

  quantitative_rows <- list()
  categorical_rows <- list()

  for (variable in vars) {
    x_original <- data[[variable]]
    is_categorical <- is.factor(x_original) ||
      is.character(x_original) || is.logical(x_original)
    is_quantitative <- is.numeric(x_original) && !is.factor(x_original)

    if (!is_categorical && !is_quantitative) {
      categorical_rows[[variable]] <- empty_row(
        variable, "unsupported type (expected numeric, factor, character or logical)"
      )
      next
    }

    analysis_data <- data.frame(
      .time = data[[ptime]],
      .event = data[[pevent]],
      .predictor = x_original
    )
    analysis_data <- analysis_data[stats::complete.cases(analysis_data), , drop = FALSE]
    if (is.numeric(analysis_data$.time)) {
      analysis_data <- analysis_data[is.finite(analysis_data$.time), , drop = FALSE]
    }
    if (is_quantitative) {
      analysis_data <- analysis_data[is.finite(analysis_data$.predictor), , drop = FALSE]
    }

    if (nrow(analysis_data) < 2L) {
      row <- empty_row(variable, "fewer than two complete observations")
      if (is_quantitative) quantitative_rows[[variable]] <- row
      else categorical_rows[[variable]] <- row
      next
    }

    if (is_categorical) {
      if (is.factor(x_original)) {
        analysis_data$.predictor <- factor(
          as.character(analysis_data$.predictor),
          levels = levels(x_original),
          ordered = FALSE
        )
      } else {
        analysis_data$.predictor <- factor(analysis_data$.predictor)
      }
      analysis_data$.predictor <- droplevels(analysis_data$.predictor)

      if (nlevels(analysis_data$.predictor) < 2L) {
        categorical_rows[[variable]] <- empty_row(
          variable, "fewer than two observed factor levels"
        )
        next
      }
      analysis_data$.predictor <- stats::`contrasts<-`(
        analysis_data$.predictor,
        value = stats::contr.treatment(levels(analysis_data$.predictor))
      )
    } else {
      if (length(unique(analysis_data$.predictor)) < 2L) {
        quantitative_rows[[variable]] <- empty_row(
          variable, "the numeric predictor is constant"
        )
        next
      }
    }

    fit <- tryCatch(
      survival::coxph(
        survival::Surv(.time, .event) ~ .predictor,
        data = analysis_data,
        ties = "efron"
      ),
      error = function(e) {
        warning(sprintf("Cox model failed for '%s': %s",
                        variable, conditionMessage(e)), call. = FALSE)
        NULL
      }
    )
    if (is.null(fit)) {
      if (is_quantitative) quantitative_rows[[variable]] <- make_row(variable)
      else categorical_rows[[variable]] <- make_row(variable)
      next
    }

    fit_summary <- summary(fit)
    global_p <- as.numeric(fit_summary$logtest["pvalue"])
    beta <- stats::coef(fit)
    standard_errors <- sqrt(diag(stats::vcov(fit)))
    z <- stats::qnorm(0.975)

    if (is_quantitative) {
      delta <- if (continuous_contrast == "IQR") {
        unname(diff(stats::quantile(
          analysis_data$.predictor,
          probs = c(0.25, 0.75),
          na.rm = TRUE
        )))
      } else {
        1
      }

      if (!is.finite(delta) || delta <= 0) {
        warning(sprintf(
          "For '%s', the requested contrast is zero/undefined: HR and CI set to NA.",
          variable
        ), call. = FALSE)
        quantitative_rows[[variable]] <- make_row(variable, p = global_p)
        next
      }

      if (length(beta) < 1L || !is.finite(beta[1]) ||
          length(standard_errors) < 1L || !is.finite(standard_errors[1])) {
        quantitative_rows[[variable]] <- make_row(variable, p = global_p)
        next
      }

      hr <- exp(beta[1] * delta)
      lower <- exp((beta[1] - z * standard_errors[1]) * delta)
      upper <- exp((beta[1] + z * standard_errors[1]) * delta)

      quantitative_rows[[variable]] <- make_row(
        variable,
        hr = fmt_value(hr),
        ci = paste0(fmt_value(lower), "-", fmt_value(upper)),
        p = global_p
      )

    } else {
      category_levels <- levels(analysis_data$.predictor)
      coefficient_p <- fit_summary$coefficients[, "Pr(>|z|)"]

      rows <- list(
        make_row(variable, hr = "-", ci = "-", p = global_p),
        make_row(category_levels[1], hr = "ref", ci = "ref",
                 p_label = "ref")
      )

      for (j in seq_along(category_levels)[-1L]) {
        coefficient_index <- j - 1L
        b <- beta[coefficient_index]
        se <- standard_errors[coefficient_index]
        p <- as.numeric(coefficient_p[coefficient_index])

        if (is.finite(b) && is.finite(se)) {
          hr <- fmt_value(exp(b))
          ci <- paste0(
            fmt_value(exp(b - z * se)), "-",
            fmt_value(exp(b + z * se))
          )
        } else {
          hr <- NA_character_
          ci <- NA_character_
        }
        rows[[length(rows) + 1L]] <- make_row(
          category_levels[j], hr = hr, ci = ci, p = p
        )
      }
      categorical_rows[[variable]] <- do.call(rbind, rows)
    }
  }
  result <- do.call(rbind, c(quantitative_rows, categorical_rows))
  rownames(result) <- NULL
  return(result)
}
