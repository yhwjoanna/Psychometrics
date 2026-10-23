# 期中作業　預試資料分析通用範本（R 腳本，與 JASP 設定相同）
# 用法：把本檔與自己組的預試資料 CSV 放在同一個資料夾，只修改下方「設定」一段，於 RStudio 開啟後按 Source
# 需要的套件：install.packages(c("psych", "GPArotation", "lavaan"))
# 對答案：設定維持原樣、資料檔改為「期中作業_玻璃心預試練習資料.csv」，結果應與 Topic 05 投影片相同

# ==================== 設定（各組只改這一段）====================
資料檔       <- "期中作業_玻璃心預試練習資料.csv"   # 自己組的 CSV 檔名，存成 CSV UTF-8
編號欄       <- "編號"
作答秒數欄   <- "作答秒數"     # 沒有作答時間欄時填 NA
檢核題欄     <- "檢核題"       # 沒有檢核題時填 NA
檢核題答案   <- 5              # 檢核題要求的選項數值；「本題請選同意」：六點量表 5、五點量表 4、七點量表 6
最短秒數     <- 30             # 低於此秒數剔除，建議題數 × 2；不使用填 NA
連續門檻     <- 10             # 連續選同一個選項達此題數剔除；不使用填 NA
量尺最小值   <- 1
量尺最大值   <- 6              # 五點量表填 5、七點量表填 7；反向計分＝最小值＋最大值－原始分數
題目         <- NULL           # NULL＝編號、作答秒數、檢核題以外的所有欄；也可以寫 c("A1", "A2", ...)
反向題       <- c("B2")        # 反向題的原始代號；沒有反向題寫 c()
向度 <- list(                   # 事例歸納出的預設向度，寫原始代號（反向題不用加 R）
  平復所需時間 = c("A1", "A2", "A3", "A4", "A5"),
  迴避回饋     = c("B1", "B2", "B3", "B4", "B5"),
  情緒反應強度 = c("C1", "C2", "C3", "C4", "C5")
)
因素數       <- NA             # NA＝依平行分析的建議；也可以直接填數字
負荷量門檻   <- .40            # 最高負荷量低於此值刪除
跨負荷門檻   <- .32            # 第二高負荷量達此值視為跨負荷，刪除
每向度最少題數 <- 3
亂數種子     <- 2026
# ===============================================================

suppressMessages({library(psych); library(GPArotation); library(lavaan)})
d <- read.csv(資料檔, fileEncoding = "UTF-8-BOM", check.names = FALSE)
非題目欄 <- c(編號欄, 作答秒數欄, 檢核題欄); 非題目欄 <- 非題目欄[!is.na(非題目欄)]
if (is.null(題目)) 題目 <- setdiff(names(d), 非題目欄)
缺欄 <- setdiff(c(非題目欄, 題目, 反向題, unlist(向度)), names(d))
if (length(缺欄)) stop("資料中找不到這些欄位，請檢查設定：", paste(缺欄, collapse = "、"))
items <- 題目

cat("== 0. 資料檢查 ==\n")
cat("讀入", nrow(d), "列、", length(items), "題；量尺", 量尺最小值, "到", 量尺最大值, "\n")
超出 <- sapply(items, function(i) sum(!is.na(d[[i]]) & (d[[i]] < 量尺最小值 | d[[i]] > 量尺最大值)))
if (any(超出 > 0)) stop("有作答超出量尺範圍，請檢查量尺設定或資料：", paste(names(超出)[超出 > 0], collapse = "、"))
遺漏 <- rowSums(is.na(d[items]))
if (any(遺漏 > 0)) cat("有", sum(遺漏 > 0), "位填答者的題目有遺漏值，分析時排除（請在企劃書 4.4 說明）\n")

cat("\n== 1. 草率作答篩檢（依序套用，已剔除者不重複計算）==\n")
run <- apply(d[items], 1, function(r) { r <- r[!is.na(r)]; if (!length(r)) 0 else max(rle(as.numeric(r))$lengths) })
r1 <- if (is.na(檢核題欄) || is.na(檢核題答案)) rep(FALSE, nrow(d)) else (is.na(d[[檢核題欄]]) | d[[檢核題欄]] != 檢核題答案)
r2 <- if (is.na(作答秒數欄) || is.na(最短秒數)) rep(FALSE, nrow(d)) else (!r1 & d[[作答秒數欄]] < 最短秒數)
r3 <- if (is.na(連續門檻)) rep(FALSE, nrow(d)) else (!r1 & !r2 & run >= 連續門檻)
r4 <- !r1 & !r2 & !r3 & 遺漏 > 0
cat("原始樣本", nrow(d), "；檢核題答錯", sum(r1), "；作答過快", sum(r2), "；連續相同", sum(r3))
if (any(r4)) cat("；題目遺漏", sum(r4))
x <- d[!(r1 | r2 | r3 | r4), items]
最低人數 <- max(length(items) * 5, 100)
cat("\n有效樣本", nrow(x), "；預試最低人數（題數 × 5，且不低於 100）", 最低人數,
    ifelse(nrow(x) >= 最低人數, "，達標\n", "，未達標\n"))

cat("\n== 2. 反向計分 ==\n")
for (i in 反向題) {
  x[[i]] <- 量尺最小值 + 量尺最大值 - x[[i]]
  names(x)[names(x) == i] <- paste0(i, "R")
  向度 <- lapply(向度, function(v) replace(v, v == i, paste0(i, "R")))
}
items <- names(x)
if (length(反向題)) cat(paste(反向題, collapse = "、"), "以", 量尺最小值 + 量尺最大值, "減原始分數，代號加 R\n") else cat("沒有反向題\n")
輸出檔 <- sub("\\.csv$", "_有效樣本.csv", 資料檔)
con <- file(輸出檔, open = "w", encoding = "UTF-8"); writeChar("﻿", con, eos = NULL)   # 加 BOM，Excel 開啟中文不亂碼
write.csv(cbind(d[!(r1 | r2 | r3 | r4), 編號欄, drop = FALSE], x), con, row.names = FALSE); close(con)
cat("篩檢與反向計分後的資料已存成", 輸出檔, "，可交給 JASP\n")

cat("\n== 3. 項目分析（全量表）==\n")
tot <- rowSums(x); cut <- quantile(tot, c(.27, .73))                 # 與 Excel PERCENTILE 相同
hi <- tot >= cut[2]; lo <- tot <= cut[1]
cat("高分組切點", cut[2], "（", sum(hi), "人）；低分組切點", cut[1], "（", sum(lo), "人）\n")
a_all <- psych::alpha(x, check.keys = FALSE, warnings = FALSE)
ia <- data.frame(題目 = items,
  決斷值 = sapply(items, function(i) unname(t.test(x[hi, i], x[lo, i])$statistic)),   # Welch t
  校正題總相關 = sapply(items, function(i) cor(x[[i]], tot - x[[i]])),
  刪除後alpha = a_all$alpha.drop$raw_alpha)
ia$判定 <- ifelse(ia$決斷值 >= 3 & ia$校正題總相關 >= .30, "保留", "檢查")
cat("全量表 alpha =", round(a_all$total$raw_alpha, 3), "；標準：決斷值 ≥ 3.00、校正題總相關 ≥ .30\n")
print(format(ia, digits = 3), row.names = FALSE)

cat("\n== 4. KMO 與 Bartlett ==\n")
k <- KMO(x); b <- cortest.bartlett(cor(x), n = nrow(x))
cat("KMO =", round(k$MSA, 3), "；Bartlett chi2(", b$df, ") =", round(b$chisq, 2), "，p =", format.pval(b$p.value), "\n")
cat("各題 MSA："); print(round(k$MSAi, 2))

cat("\n== 5. 平行分析 ==\n")
set.seed(亂數種子); pa <- fa.parallel(x, fm = "minres", fa = "pc", plot = FALSE)
cat("建議因素數 =", pa$ncomp, "；實際特徵值：", round(pa$pc.values[1:min(6, length(items))], 2), "\n")
nf <- if (is.na(因素數)) pa$ncomp else 因素數
cat("本次分析採用", nf, "個因素；事例歸納的預設向度為", length(向度), "個",
    ifelse(nf == length(向度), "，兩者一致\n", "，兩者不一致，請在企劃書 5.2 說明\n"))

cat("\n== 6. 探索性因素分析（最小殘差法、promax、相關矩陣），一次刪一題 ==\n")
keep <- items; step <- 1
repeat {
  e <- fa(x[keep], nfactors = nf, fm = "minres", rotate = if (nf > 1) "promax" else "none", cor = "cor")
  L <- unclass(e$loadings); a <- abs(L)
  top <- apply(a, 1, max)
  second <- if (nf > 1) apply(a, 1, function(v) sort(v, decreasing = TRUE)[2]) else setNames(rep(0, length(keep)), keep)
  cat("\n第", step, "次：", length(keep), "題\n")
  print(round(cbind(L, 共同性 = e$communality, 獨特性 = e$uniquenesses), 3))
  low <- names(which(top < 負荷量門檻)); cross <- names(which(second >= 跨負荷門檻))
  if (length(low)) { drop <- names(which.min(top[low])); cat("刪除", drop, "：最高負荷量", round(top[drop], 3), "低於", 負荷量門檻, "\n") }
  else if (length(cross)) { drop <- names(which.max(second[cross])); cat("刪除", drop, "：第二負荷量", round(second[drop], 3), "達", 跨負荷門檻, "\n") }
  else { cat(if (nf > 1) "每一題都只落在一個因素，停止\n" else "每一題的負荷量都達門檻，停止\n"); break }
  keep <- setdiff(keep, drop); step <- step + 1
  if (length(keep) <= nf) stop("題目刪到只剩 ", length(keep), " 題，請檢查因素數或資料")
}
cat("累積解釋變異量 =", round(e$Vaccounted[if (nf > 1) "Cumulative Var" else "Proportion Var", nf], 3), "\n")
if (nf > 1) { cat("因素間相關：\n"); print(round(e$Phi, 3)) }
cat("\n各因素保留的題目（依最高負荷量歸屬）：\n")
歸屬 <- split(rownames(L), colnames(L)[apply(abs(L), 1, which.max)])
for (f in names(歸屬)) cat(f, "：", paste(歸屬[[f]], collapse = "、"),
                          if (length(歸屬[[f]]) < 每向度最少題數) "（少於最少題數，請檢查）" else "", "\n")

cat("\n== 7. 信度（逐向度，只計入保留的題目）==\n")
omega_jasp <- function(v) {                                          # 與 JASP 預設相同：單因素 CFA，未標準化負荷量
  m <- cfa(paste("f =~", paste(v, collapse = " + ")), data = x, std.lv = TRUE)
  p <- parameterEstimates(m); l <- p$est[p$op == "=~"]; th <- p$est[p$op == "~~" & p$lhs == p$rhs & p$lhs != "f"]
  sum(l)^2 / (sum(l)^2 + sum(th)) }
for (n in names(向度)) {
  v <- intersect(向度[[n]], keep)
  if (length(v) < 2) { cat(n, "：保留題目少於 2 題，無法計算信度\n"); next }
  a <- psych::alpha(x[v], check.keys = FALSE, warnings = FALSE)
  cat(n, "（", paste(v, collapse = "、"), "）：alpha =", round(a$total$raw_alpha, 3),
      "；omega =", if (length(v) >= 3) round(omega_jasp(v), 3) else "題數不足 3 題，不計算",
      if (length(v) < 每向度最少題數) "；少於最少題數" else "", "\n")
}
