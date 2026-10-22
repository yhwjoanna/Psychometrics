# 期中作業　玻璃心預試資料分析（R 腳本，與 JASP 設定相同）
# 用法：把本檔與「期中作業_玻璃心預試練習資料.csv」放在同一個資料夾，於 RStudio 開啟後按 Source
# 需要的套件：install.packages(c("psych", "GPArotation", "lavaan"))
suppressMessages({library(psych); library(GPArotation); library(lavaan)})
d <- read.csv("期中作業_玻璃心預試練習資料.csv", fileEncoding = "UTF-8-BOM", check.names = FALSE)
items <- c(paste0("A", 1:5), paste0("B", 1:5), paste0("C", 1:5))

cat("== 1. 草率作答篩檢 ==\n")
run <- apply(d[items], 1, function(r) max(rle(r)$lengths))          # 最長連續相同作答
r1 <- d[["檢核題"]] != 5; r2 <- !r1 & d[["作答秒數"]] < 30; r3 <- !r1 & !r2 & run >= 10
cat("原始樣本", nrow(d), "；檢核題答錯", sum(r1), "；作答過快", sum(r2), "；連續相同", sum(r3), "\n")
x <- d[!(r1 | r2 | r3), items]; cat("有效樣本", nrow(x), "\n")

cat("\n== 2. 反向計分 ==\n")
x$B2 <- 7 - x$B2; names(x)[names(x) == "B2"] <- "B2R"; items <- names(x)
cat("B2 以 7 減原始分數，改名 B2R\n")

cat("\n== 3. 項目分析（全量表）==\n")
tot <- rowSums(x); cut <- quantile(tot, c(.27, .73))                 # 與 Excel PERCENTILE 相同
hi <- tot >= cut[2]; lo <- tot <= cut[1]
cat("高分組切點", cut[2], "（", sum(hi), "人）；低分組切點", cut[1], "（", sum(lo), "人）\n")
a_all <- psych::alpha(x, check.keys = FALSE, warnings = FALSE)
ia <- data.frame(題目 = items,
  決斷值 = sapply(items, function(i) unname(t.test(x[hi, i], x[lo, i])$statistic)),   # Welch t
  校正題總相關 = sapply(items, function(i) cor(x[[i]], tot - x[[i]])),
  刪除後alpha = a_all$alpha.drop$raw_alpha)
cat("全量表 alpha =", round(a_all$total$raw_alpha, 3), "\n"); print(format(ia, digits = 3), row.names = FALSE)

cat("\n== 4. KMO 與 Bartlett ==\n")
k <- KMO(x); b <- cortest.bartlett(cor(x), n = nrow(x))
cat("KMO =", round(k$MSA, 3), "；Bartlett chi2(", b$df, ") =", round(b$chisq, 2), "，p =", format.pval(b$p.value), "\n")

cat("\n== 5. 平行分析 ==\n")
set.seed(2026); pa <- fa.parallel(x, fm = "minres", fa = "pc", plot = FALSE)
cat("建議因素數 =", pa$ncomp, "；實際特徵值：", round(pa$pc.values[1:5], 2), "\n")

cat("\n== 6. 探索性因素分析（最小殘差法、promax、相關矩陣），一次刪一題 ==\n")
keep <- items; step <- 1
repeat {
  e <- fa(x[keep], nfactors = 3, fm = "minres", rotate = "promax", cor = "cor")
  L <- unclass(e$loadings); a <- abs(L)
  top <- apply(a, 1, max); second <- apply(a, 1, function(v) sort(v, decreasing = TRUE)[2])
  cat("\n第", step, "次：", length(keep), "題\n")
  print(round(cbind(L, 共同性 = e$communality, 獨特性 = e$uniquenesses), 3))
  low <- names(which(top < .40)); cross <- names(which(second >= .32))
  if (length(low)) { drop <- names(which.min(top[low])); cat("刪除", drop, "：最高負荷量", round(max(a[drop, ]), 3), "低於 .40\n") }
  else if (length(cross)) { drop <- names(which.max(second[cross])); cat("刪除", drop, "：第二負荷量", round(second[drop], 3), "達 .32\n") }
  else { cat("每一題都只落在一個因素，停止\n"); break }
  keep <- setdiff(keep, drop); step <- step + 1
}
cat("累積解釋變異量 =", round(e$Vaccounted["Cumulative Var", 3], 3), "\n因素間相關：\n"); print(round(e$Phi, 3))

cat("\n== 7. 信度（逐向度）==\n")
omega_jasp <- function(v) {                                          # 與 JASP 預設相同：單因素 CFA，未標準化負荷量
  m <- cfa(paste("f =~", paste(v, collapse = " + ")), data = x, std.lv = TRUE)
  p <- parameterEstimates(m); l <- p$est[p$op == "=~"]; th <- p$est[p$op == "~~" & p$lhs == p$rhs & p$lhs != "f"]
  sum(l)^2 / (sum(l)^2 + sum(th)) }
dims <- list(平復所需時間 = c("A1","A2","A3","A4"), 迴避回饋 = c("B1","B2R","B3","B4","B5"), 情緒反應強度 = c("C1","C2","C3","C5"))
for (n in names(dims)) {
  v <- dims[[n]]; a <- psych::alpha(x[v], warnings = FALSE)
  cat(n, "：alpha =", round(a$total$raw_alpha, 3), "；omega =", round(omega_jasp(v), 3), "\n")
}
