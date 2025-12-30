suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(compositions); library(fixest); library(stringr); library(tibble)
})

# ================== 1. PARÁMETROS GLOBALES ==================
USE_FILTERED_PANEL <- TRUE      # usa panel filtrado para modelos y back-transform
CLUSTER_ASOC       <- TRUE      # ~cu+asoc si existe, si no ~cu
N_DRAWS            <- 3000      # simulación para IC95% en back-transform
FRAC_MIN           <- 0.95
CIERRE_TOL         <- c(0.9, 1.1)
TOP_AREA_P         <- 0.995


# ================== 2. UTILIDADES CoDA / AUX ==================
comp_mat <- function(df, cols = c("pB","pP","pV","pO"), eps_row = 1e-6){
  stopifnot(all(cols %in% names(df)))
  X <- as.matrix(df[, cols]); storage.mode(X) <- "double"
  X[!is.finite(X)] <- NA_real_
  X[X < 0] <- 0; X[X > 1] <- 1
  rs <- rowSums(X, na.rm = FALSE)
  ok <- is.finite(rs) & rs > 0
  if (!any(ok)) return(acomp(matrix(numeric(0), nrow = 0, ncol = length(cols))))
  X <- X[ok, , drop = FALSE]
  X <- X / rowSums(X)
  
  # imputación mínima de ceros
  z <- X == 0
  if (any(z)){
    k0    <- rowSums(z)
    delta <- ifelse(k0 > 0, eps_row / k0, 0)
    X[z]  <- X[z] + rep(delta, times = k0)
    X     <- X / rowSums(X)
  }
  acomp(X)
}

comp_center_ilr <- function(panel){
  A <- comp_mat(panel)
  if (nrow(as.matrix(A)) == 0) return(rep(NA_real_, 3))
  Z <- compositions::ilr(A)
  colMeans(Z)
}

rt_norm <- function(mu, v, ndraw = N_DRAWS){
  sdv <- sqrt(max(v, 0))
  mu + stats::rnorm(ndraw, sd = sdv)
}

make_cluster <- function(df){
  vars <- intersect(c("cu","asoc"), names(df))
  if (!length(vars)) return(~cu)
  as.formula(paste0("~", paste(vars, collapse = "+")))
}

# ================== 3. FILTRO PANEL ==================
filter_panel <- function(panel,
                         frac_min  = FRAC_MIN,
                         cierre_tol = CIERRE_TOL,
                         top_area_p = TOP_AREA_P){
  panel %>%
    mutate(cierre_chk = pB + pP + pV + pO) %>%
    filter(is.finite(cierre_chk),
           is.finite(frac_mapeada),
           frac_mapeada >= frac_min,
           cierre_chk >= cierre_tol[1],
           cierre_chk <= cierre_tol[2]) %>%
    group_by(periodo) %>%
    mutate(area_cap = pmin(area_uer_ha,
                           stats::quantile(area_uer_ha, top_area_p, na.rm = TRUE))) %>%
    ungroup() %>%
    select(-cierre_chk)
}

ensure_asoc <- function(panel, asoc_map = NULL){
  if ("asoc" %in% names(panel)) return(panel)
  if (is.data.frame(asoc_map) && all(c("cu","asoc") %in% names(asoc_map))){
    return(left_join(panel, asoc_map |> select(cu, asoc), by = "cu"))
  }
  panel$asoc <- factor("SIN_ASOC")
  panel
}

# dat <- dat %>% mutate(pO = pO + pQ)
panel_base <- if (USE_FILTERED_PANEL) filter_panel(moscal_variables_predios) else moscal_variables_predios

# ================== 4. DiD TWFE (nivel panel / asoc) ==================
prep_did_data <- function(panel){
  dat <- panel %>% filter(is.finite(pB + pP + pV + pO))
  trt_unit <- dat %>%
    group_by(cu) %>%
    summarise(trt = as.integer(any(grupo == "CON_PROYECTO")), .groups = "drop")
  dat <- left_join(dat, trt_unit, by = "cu")
  
  A <- comp_mat(dat); Z <- as.matrix(compositions::ilr(A))
  if (!nrow(Z)) return(tibble())
  colnames(Z) <- paste0("ilr", 1:ncol(Z))
  
  out <- cbind(dat[, c("cu","periodo","area_uer_ha","trt")], Z)
  out$cu      <- as.factor(out$cu)
  out$periodo <- droplevels(as.factor(out$periodo))
  out
}

prep_did_data_asoc <- function(panel, asoc_map = NULL){
  panel <- ensure_asoc(panel, asoc_map)
  dat <- panel %>%
    filter(is.finite(pB + pP + pV + pO)) %>%
    mutate(periodo = droplevels(periodo),
           trt     = as.integer(grupo == "CON_PROYECTO"))
  
  A <- comp_mat(dat); Z <- as.matrix(compositions::ilr(A))
  if (!nrow(Z)) return(tibble())
  colnames(Z) <- paste0("ilr", 1:ncol(Z))
  
  cbind(dat[, c("cu","periodo","asoc","trt")], Z)
}

run_did_ilr <- function(dat, cluster = ~cu){
  if (!nrow(dat)) return(list(ilr1 = NULL, ilr2 = NULL, ilr3 = NULL))
  f1 <- feols(ilr1 ~ trt:periodo | cu + periodo, data = dat, cluster = cluster)
  f2 <- feols(ilr2 ~ trt:periodo | cu + periodo, data = dat, cluster = cluster)
  f3 <- feols(ilr3 ~ trt:periodo | cu + periodo, data = dat, cluster = cluster)
  list(ilr1 = f1, ilr2 = f2, ilr3 = f3)
}

coef_by_period <- function(fit){
  if (is.null(fit)) return(tibble())
  cf <- stats::coef(fit); nm <- names(cf)
  keep <- grepl("^trt:periodo", nm)
  tibble(var = nm[keep], beta = unname(cf[keep])) %>%
    tidyr::extract(var, into = "periodo",
                   regex = "^trt:periodo(.+)$", remove = TRUE)
}

did_to_pp <- function(did_fit, panel, ndraw = N_DRAWS){
  ctr <- comp_center_ilr(panel)
  if (any(!is.finite(ctr))) return(tibble())
  
  b1 <- coef_by_period(did_fit$ilr1)
  b2 <- coef_by_period(did_fit$ilr2)
  b3 <- coef_by_period(did_fit$ilr3)
  per <- Reduce(intersect, list(b1$periodo, b2$periodo, b3$periodo))
  if (!length(per)) return(tibble())
  
  V <- function(m, p){
    nm <- paste0("trt:periodo", p)
    vc <- try(as.matrix(vcov(m))[nm, nm, drop = TRUE], silent = TRUE)
    if (inherits(vc, "try-error") || length(vc) == 0) return(0)
    as.numeric(vc)
  }
  
  out <- lapply(per, function(p){
    mu <- c(b1$beta[b1$periodo == p],
            b2$beta[b2$periodo == p],
            b3$beta[b3$periodo == p])
    v1 <- V(did_fit$ilr1, p)
    v2 <- V(did_fit$ilr2, p)
    v3 <- V(did_fit$ilr3, p)
    
    d1 <- rt_norm(mu[1], v1, ndraw)
    d2 <- rt_norm(mu[2], v2, ndraw)
    d3 <- rt_norm(mu[3], v3, ndraw)
    
    ilr_ctl <- matrix(rep(ctr, each = ndraw), ncol = 3, byrow = FALSE)
    ilr_trt <- cbind(ctr[1] + d1, ctr[2] + d2, ctr[3] + d3)
    
    C_ctl <- t(apply(ilr_ctl, 1, compositions::ilrInv))
    C_trt <- t(apply(ilr_trt, 1, compositions::ilrInv))
    dpp   <- (C_trt - C_ctl) * 100
    
    est <- colMeans(dpp)
    ci  <- apply(dpp, 2, stats::quantile, probs = c(.025, .975), na.rm = TRUE)
    
    tibble(
      periodo   = factor(p, levels = levels(panel$periodo)),
      Componente = c("Bosques","Pastizales","Vegetación secundaria","Otros"),
      Dif_pp    = as.numeric(est),
      CI95_lo   = as.numeric(ci[1, ]),
      CI95_hi   = as.numeric(ci[2, ])
    )
  })
  
  bind_rows(out) %>% arrange(Componente, periodo)
}

forest_pp <- function(df, titulo = "Δpp (CON − SIN) por periodo"){
  if (!nrow(df)) return(ggplot() + ggtitle("Sin estimaciones"))
  ggplot(df, aes(x = periodo, y = Dif_pp, ymin = CI95_lo, ymax = CI95_hi)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_pointrange() +
    facet_wrap(~ Componente, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Puntos porcentuales", title = titulo) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ================== 5. EVENT-STUDY ==================
entry_period_by_cu <- function(panel){
  per_levels <- levels(panel$periodo)
  per_map    <- setNames(seq_along(per_levels), per_levels)
  panel %>%
    mutate(t = per_map[as.character(periodo)]) %>%
    group_by(cu) %>%
    summarise(t0 = suppressWarnings(min(t[grupo == "CON_PROYECTO"], na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(t0 = if_else(is.finite(t0), t0, 1))
}

prep_event_data <- function(panel){
  per_levels <- levels(panel$periodo)
  per_map    <- setNames(seq_along(per_levels), per_levels)
  
  dat <- panel %>%
    mutate(t   = per_map[as.character(periodo)],
           trt = as.integer(grupo == "CON_PROYECTO"))
  
  e0 <- entry_period_by_cu(panel)
  dat <- left_join(dat, e0, by = "cu") %>%
    mutate(rel = as.integer(t - t0)) %>%
    filter(!is.na(rel))
  
  A <- comp_mat(dat); Z <- as.matrix(compositions::ilr(A))
  if (!nrow(Z)) return(tibble())
  colnames(Z) <- paste0("ilr", 1:ncol(Z))
  
  cbind(dat[, c("cu","periodo","trt","rel")], Z)
}

prep_event_data_asoc <- function(panel){
  stopifnot("asoc" %in% names(panel))
  per_levels <- levels(panel$periodo)
  per_map    <- setNames(seq_along(per_levels), per_levels)
  
  dat <- panel %>%
    mutate(
      t   = per_map[as.character(periodo)],
      trt = as.integer(grupo == "CON_PROYECTO")
    ) %>%
    group_by(cu) %>%
    mutate(t0 = suppressWarnings(min(t[grupo == "CON_PROYECTO"], na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(
      t0  = if_else(is.finite(t0), t0, 1),
      rel = as.integer(t - t0)
    ) %>%
    filter(!is.na(rel))
  
  A <- comp_mat(dat); Z <- as.matrix(compositions::ilr(A))
  if (!nrow(Z)) return(tibble())
  colnames(Z) <- paste0("ilr", 1:ncol(Z))
  
  cbind(dat[, c("cu","asoc","periodo","trt","rel")], Z)
}

.es_ref <- function(rel){
  vals <- sort(unique(rel))
  if (!length(vals)) return(0L)
  if (-1L %in% vals) return(-1L)
  pre_vals <- vals[vals < 0L]
  if (length(pre_vals)) return(max(pre_vals))
  min(vals)
}

run_event_ilr <- function(df, cluster = ~cu){
  if (!nrow(df)) return(list(ilr1 = NULL, ilr2 = NULL, ilr3 = NULL, ref = NA_integer_))
  ref_k <- .es_ref(df$rel)
  f1 <- feols(ilr1 ~ i(rel, trt, ref = ref_k) | cu + periodo, data = df, cluster = cluster)
  f2 <- feols(ilr2 ~ i(rel, trt, ref = ref_k) | cu + periodo, data = df, cluster = cluster)
  f3 <- feols(ilr3 ~ i(rel, trt, ref = ref_k) | cu + periodo, data = df, cluster = cluster)
  list(ilr1 = f1, ilr2 = f2, ilr3 = f3, ref = ref_k)
}

.es_grab <- function(f){
  if (is.null(f)) return(tibble(k = integer(), beta = numeric(), cname = character()))
  nm <- names(coef(f))
  m  <- stringr::str_match(nm, "(?:^.*)?rel::(-?\\d+):trt$")
  ok <- !is.na(m[, 2])
  if (!any(ok)) return(tibble(k = integer(), beta = numeric(), cname = character()))
  tibble(k = as.integer(m[ok, 2]),
         beta = unname(coef(f)[ok]),
         cname = nm[ok]) %>%
    arrange(k)
}

es_to_pp <- function(es_fit, panel, ndraw = N_DRAWS){
  ctr <- comp_center_ilr(panel)
  if (any(!is.finite(ctr))) return(tibble())
  
  b1 <- .es_grab(es_fit$ilr1)
  b2 <- .es_grab(es_fit$ilr2)
  b3 <- .es_grab(es_fit$ilr3)
  ks <- Reduce(intersect, list(b1$k, b2$k, b3$k))
  if (!length(ks)) return(tibble())
  
  Vpick <- function(f, cname){
    v <- try(as.matrix(vcov(f))[cname, cname, drop = TRUE], silent = TRUE)
    if (inherits(v, "try-error") || length(v) == 0) 0 else as.numeric(v)
  }
  
  out <- lapply(ks, function(k){
    mu <- c(b1$beta[b1$k == k],
            b2$beta[b2$k == k],
            b3$beta[b3$k == k])
    
    v1 <- Vpick(es_fit$ilr1, b1$cname[b1$k == k])
    v2 <- Vpick(es_fit$ilr2, b2$cname[b2$k == k])
    v3 <- Vpick(es_fit$ilr3, b3$cname[b3$k == k])
    
    d1 <- rt_norm(mu[1], v1, ndraw)
    d2 <- rt_norm(mu[2], v2, ndraw)
    d3 <- rt_norm(mu[3], v3, ndraw)
    
    ilr_ctl <- matrix(rep(ctr, each = ndraw), ncol = 3, byrow = FALSE)
    ilr_trt <- cbind(ctr[1] + d1, ctr[2] + d2, ctr[3] + d3)
    
    C_ctl <- t(apply(ilr_ctl, 1, compositions::ilrInv))
    C_trt <- t(apply(ilr_trt, 1, compositions::ilrInv))
    dpp   <- (C_trt - C_ctl) * 100
    
    est <- colMeans(dpp)
    ci  <- apply(dpp, 2, stats::quantile, probs = c(.025, .975), na.rm = TRUE)
    
    tibble(
      k          = k,
      Componente = c("Bosques","Pastizales","Vegetación secundaria","Otros"),
      Dif_pp     = as.numeric(est),
      CI95_lo    = as.numeric(ci[1, ]),
      CI95_hi    = as.numeric(ci[2, ])
    )
  })
  
  bind_rows(out) %>% arrange(Componente, k)
}

forest_es <- function(df, ref_k){
  if (!nrow(df)) return(ggplot() + ggtitle("Sin estimaciones"))
  ggplot(df, aes(x = k, y = Dif_pp, ymin = CI95_lo, ymax = CI95_hi)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_pointrange() +
    facet_wrap(~ Componente, scales = "free_y", ncol = 2) +
    labs(x = paste0("k = periodo − t0 (ref: ", ref_k, ")"),
         y = "Puntos porcentuales",
         title = "Event-study: Δpp relativo al ingreso")
}

# ================== 6. HETEROGENEIDAD POR ASOCIACIÓN ==================
prep_post_data <- function(panel){
  panel <- ensure_asoc(panel)
  per_levels <- levels(panel$periodo)
  per_map    <- setNames(seq_along(per_levels), per_levels)
  
  d0 <- panel %>%
    mutate(t   = per_map[as.character(periodo)],
           trt = as.integer(grupo == "CON_PROYECTO"))
  
  e0 <- entry_period_by_cu(panel)
  
  d1 <- left_join(d0, e0, by = "cu") %>%
    mutate(post = as.integer(!is.na(t0) & t > t0))
  
  A <- comp_mat(d1)
  Z <- as.matrix(compositions::ilr(A))
  if (!nrow(Z)) return(tibble())
  colnames(Z) <- paste0("ilr", seq_len(ncol(Z)))
  
  cbind(d1[, c("cu","periodo","asoc","trt","post")], Z)
}

run_asoc_ate_ilr <- function(df){
  if (!nrow(df)) return(list())
  df$cu      <- as.factor(df$cu)
  df$asoc    <- as.factor(df$asoc)
  df$periodo <- as.factor(df$periodo)
  
  out <- list()
  for (a in sort(unique(df$asoc))) {
    da <- df[df$asoc == a, , drop = FALSE]
    if (length(unique(da$trt)) < 2) next
    
    has_pre  <- any(da$trt == 1 & da$post == 0)
    has_post <- any(da$trt == 1 & da$post == 1)
    if (!has_pre || !has_post) next
    
    f1 <- feols(ilr1 ~ i(post, trt, ref = 0) | cu + periodo, data = da, cluster = ~cu)
    f2 <- feols(ilr2 ~ i(post, trt, ref = 0) | cu + periodo, data = da, cluster = ~cu)
    f3 <- feols(ilr3 ~ i(post, trt, ref = 0) | cu + periodo, data = da, cluster = ~cu)
    out[[as.character(a)]] <- list(ilr1 = f1, ilr2 = f2, ilr3 = f3)
  }
  out
}

modlist_to_pp <- function(modlist, panel, ndraw = N_DRAWS){
  if (!length(modlist)) return(tibble())
  ctr <- comp_center_ilr(panel)
  if (any(!is.finite(ctr))) return(tibble())
  
  grab_post1_trt <- function(f){
    if (is.null(f)) return(list(beta = NA_real_, v = NA_real_, name = NA_character_))
    nm <- names(stats::coef(f))
    idx <- which(grepl("post::1(:|#)trt$", nm))
    if (!length(idx)) {
      idx <- which(grepl("(^|.*)post::1(:|#)trt$", nm))
    }
    if (!length(idx)) {
      message("[modlist_to_pp] No se encontró 'post==1 x trt' en coeficientes.")
      return(list(beta = NA_real_, v = NA_real_, name = NA_character_))
    }
    name <- nm[idx[1]]
    beta <- unname(stats::coef(f)[idx[1]])
    vc   <- try(as.matrix(stats::vcov(f))[name, name, drop = TRUE], silent = TRUE)
    if (inherits(vc, "try-error") || !is.finite(vc)) vc <- NA_real_
    list(beta = beta, v = vc, name = name)
  }
  
  to_rows <- function(tag, f1, f2, f3){
    b1 <- grab_post1_trt(f1)
    b2 <- grab_post1_trt(f2)
    b3 <- grab_post1_trt(f3)
    if (any(!is.finite(c(b1$beta, b2$beta, b3$beta)))) return(NULL)
    
    d1 <- rt_norm(b1$beta, b1$v, ndraw)
    d2 <- rt_norm(b2$beta, b2$v, ndraw)
    d3 <- rt_norm(b3$beta, b3$v, ndraw)
    
    ilr_ctl <- matrix(rep(ctr, each = ndraw), ncol = 3, byrow = FALSE)
    ilr_trt <- cbind(ctr[1] + d1, ctr[2] + d2, ctr[3] + d3)
    
    C_ctl <- t(apply(ilr_ctl, 1, compositions::ilrInv))
    C_trt <- t(apply(ilr_trt, 1, compositions::ilrInv))
    dpp   <- (C_trt - C_ctl) * 100
    
    est <- colMeans(dpp)
    ci  <- apply(dpp, 2, stats::quantile, probs = c(.025, .975), na.rm = TRUE)
    
    tibble(
      asoc       = tag,
      Componente = c("Bosques","Pastizales","Vegetación secundaria","Otros"),
      Dif_pp     = as.numeric(est),
      CI95_lo    = as.numeric(ci[1, ]),
      CI95_hi    = as.numeric(ci[2, ])
    )
  }
  
  rows <- lapply(names(modlist), function(nm){
    m <- modlist[[nm]]
    to_rows(nm, m$ilr1, m$ilr2, m$ilr3)
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) {
    message("[modlist_to_pp] Resultado vacío al mapear coeficientes post::1(:|#)trt.")
  }
  out
}

forest_asoc <- function(df, titulo = "Δpp promedio post por asociación"){
  if (!nrow(df)) return(ggplot() + ggtitle("Sin estimaciones por asociación"))
  ggplot(df, aes(x = asoc, y = Dif_pp, ymin = CI95_lo, ymax = CI95_hi)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_pointrange() +
    facet_wrap(~ Componente, scales = "free_y", ncol = 2) +
    coord_flip() +
    labs(x = "Asociación", y = "Puntos porcentuales", title = titulo)
}

# ================== 7. LÍNEA BASE 2018 vs 2024 ==================
pick_2024 <- function(x){
  lv <- c("2023_JUL","2023_OCT","2024_ENE")
  y  <- intersect(lv, as.character(unique(x)))
  if (length(y) == 0) NA_character_ else y[1]
}

mean_comp <- function(A, w = NULL){
  Z <- compositions::ilr(A)
  if (is.null(w)) w <- rep(1, nrow(Z))
  w <- w / sum(w)
  m <- colSums(Z * w[col(Z)])
  as.numeric(compositions::ilrInv(m))
}

pp_estimandos <- function(dat){
  # dat: pB,pP,pV,pO, area_uer_ha
  if (nrow(dat) < 2) return(rep(NA_real_, 12))
  
  # 1) promedio composicional simple
  A  <- comp_mat(dat)
  e1 <- mean_comp(A) * 100
  
  # 2) promedio ponderado por área (trim p95)
  a  <- dat$area_uer_ha
  w  <- pmin(a, stats::quantile(a, 0.95, na.rm = TRUE))
  e2 <- mean_comp(A, w) * 100
  
  # 3) estratos por tamaño (K <= 5 según únicos)
  a_ok     <- is.finite(a)
  a_unique <- length(unique(a[a_ok]))
  K        <- min(5L, max(1L, a_unique))
  
  if (K < 2L){
    e3 <- rep(NA_real_, 4)
  } else {
    g <- rep(NA_integer_, nrow(dat))
    g[a_ok] <- dplyr::ntile(log1p(a[a_ok]), K)
    
    es <- matrix(NA_real_, 4, K)
    ws <- numeric(K)
    
    for (j in seq_len(K)){
      d <- dat[g == j, , drop = FALSE]
      if (nrow(d) >= 2){
        es[, j] <- mean_comp(comp_mat(d)) * 100
        ws[j]   <- nrow(d) / nrow(dat)
      } else {
        ws[j] <- 0
      }
    }
    e3 <- if (sum(ws) > 0) as.numeric(es %*% (ws / sum(ws))) else rep(NA_real_, 4)
  }
  
  c(e1, e2, e3)
}

plot_did_heatmap <- function(tab){
  ggplot(tab, aes(x = estim, y = asoc, fill = DiD_2018_2024)) +
    geom_tile() +
    facet_wrap(~ Componente, ncol = 2) +
    labs(x = "Estimando", y = "Asociación", fill = "ΔΔ pp (2018→2024)",
         title = "DiD agregado por asociación y componente") +
    scale_fill_gradient2() +
    theme(axis.text.y = element_text(size = 7))
}

plot_did_slope <- function(tab){
  long <- tab %>%
    pivot_longer(c(Dif_CON_SIN_2018, Dif_CON_SIN_2024),
                 names_to = "momento", values_to = "delta") %>%
    mutate(momento = factor(momento,
                            levels = c("Dif_CON_SIN_2018","Dif_CON_SIN_2024"),
                            labels = c("2018","2024")))
  ggplot(long, aes(x = momento, y = delta, group = asoc, color = asoc)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point() +
    geom_line(alpha = .6) +
    facet_grid(Componente ~ estim, scales = "free_y") +
    labs(x = NULL, y = "Δ (CON−SIN) pp",
         title = "Evolución del gap CON−SIN (2018 → 2024)")
}

rank_did <- function(tab, N = 10){
  tab %>%
    group_by(asoc) %>%
    summarise(Delta_mediana = stats::median(DiD_2018_2024, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(abs(Delta_mediana))) %>%
    slice_head(n = N)
}

resume_did <- function(tab){
  tab %>%
    group_by(Componente, estim) %>%
    summarise(
      n_asoc = n_distinct(asoc),
      med    = stats::median(DiD_2018_2024, na.rm = TRUE),
      q25    = stats::quantile(DiD_2018_2024, .25, na.rm = TRUE),
      q75    = stats::quantile(DiD_2018_2024, .75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Componente, estim)
}


# ================== 8. PIPELINE COMPLETO DESDE panel_ok ==================
# dat <- dat %>% mutate(pO = pO + pQ)
# panel_base <- if (USE_FILTERED_PANEL) filter_panel(dat) else dat

## 8.1. DiD TWFE
did_dat <- if ("asoc" %in% names(panel_base))
  prep_did_data_asoc(panel_base) else prep_did_data(panel_base)

cluster_did <- make_cluster(did_dat)
did_fit     <- run_did_ilr(did_dat, cluster = cluster_did)
did_pp      <- did_to_pp(did_fit, panel_base)
fig_did_pp  <- forest_pp(did_pp)

## 8.2. Event-study
es_dat <- if ("asoc" %in% names(panel_base)){
  prep_event_data_asoc(panel_base)
} else prep_event_data(panel_base)

cluster_es <- make_cluster(es_dat)
es_fit     <- run_event_ilr(es_dat, cluster = cluster_es)
es_pp      <- es_to_pp(es_fit, panel_base)
fig_es     <- forest_es(es_pp, ref_k = es_fit$ref)

## 8.3. Heterogeneidad por asociación
if ("asoc" %in% names(panel_base)){
  post_df   <- prep_post_data(panel_base)
  asoc_mods <- run_asoc_ate_ilr(post_df)
  asoc_pp   <- modlist_to_pp(asoc_mods, panel_base)
  fig_asoc  <- forest_asoc(asoc_pp)
}

## 9.4. Línea base 2017 vs 2023
per_2024 <- pick_2024(levels(panel_base$periodo))
per_2024 <- "2023_JUL"

if (is.na(per_2024)){
  message("No hay periodo 2024_* disponible; se omite baseline 2018 vs 2024.")
} else {
  panel_base <- ensure_asoc(panel_base)
  sel_asoc <- panel_base %>%
    group_by(asoc) %>%
    summarise(
      has_2017 = any(periodo == "2017_JUL" & grupo == "CON_PROYECTO"),
      has_2023 = any(startsWith(as.character(periodo),"2023")),
      .groups = "drop"
    ) %>%
    filter(has_2017 & has_2023) %>%
    pull(asoc)
  
  bf <- panel_base %>%
    filter(asoc %in% sel_asoc,
           periodo %in% c("2017_JUL", per_2024),
           grupo %in% c("CON_PROYECTO","SIN_PROYECTO"))
  
  base_final_pp <- bf %>%
    group_by(asoc, grupo, periodo) %>%
    group_modify(~{
      est <- pp_estimandos(.x)
      tibble(
        Componente = c("Bosques","Pastizales","Vegetación secundaria","Otros"),
        pp_predio  = est[1:4],
        pp_ha_trim = est[5:8],
        pp_strata  = est[9:12]
      )
    }) %>%
    ungroup()
  
  wide_pp <- base_final_pp %>%
    pivot_wider(
      names_from  = c(grupo, periodo),
      values_from = c(pp_predio, pp_ha_trim, pp_strata)
    )
  
  col_con_2024 <- paste0("CON_PROYECTO_", per_2024)
  col_sin_2024 <- paste0("SIN_PROYECTO_", per_2024)
  
  did_2018_2024 <- wide_pp %>%
    pivot_longer(starts_with("pp_"),
                 names_to  = "estim_var",
                 values_to = "valor") %>%
    tidyr::extract(
      col  = estim_var,
      into = c("pp","estim","grp","per"),
      regex = "^(pp)_(predio|ha_trim|strata)_(CON_PROYECTO|SIN_PROYECTO)_(\\d{4}_[A-Z]{3})$",
      remove = TRUE
    ) %>%
    filter(!is.na(pp), !is.na(estim), !is.na(grp), !is.na(per)) %>%
    pivot_wider(
      names_from  = c(grp, per),
      values_from = valor
    ) %>%
    filter(
      !is.na(.data[["CON_PROYECTO_2017_JUL"]]),
      !is.na(.data[["SIN_PROYECTO_2017_JUL"]]),
      !is.na(.data[[col_con_2024]]),
      !is.na(.data[[col_sin_2024]])
    ) %>%
    transmute(
      asoc, Componente, estim,
      Dif_CON_SIN_2018 = .data[["CON_PROYECTO_2017_JUL"]] - .data[["SIN_PROYECTO_2017_JUL"]],
      Dif_CON_SIN_2024 = .data[[col_con_2024]]            - .data[[col_sin_2024]],
      DiD_2018_2024    = Dif_CON_SIN_2024 - Dif_CON_SIN_2018
    )
  # luego: plot_did_heatmap(did_2018_2024), plot_did_slope(did_2018_2024), rank_did(...), resume_did(...)
}


# plot_did_heatmap(did_2018_2024)
# plot_did_slope(did_2018_2024)
# rank_did(did_2018_2024)
# resume_did(did_2018_2024)
# 
# fig_did_pp
# fig_es
# fig_asoc

