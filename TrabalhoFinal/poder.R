# Poder a priori para d* = 0,5 (teste t de duas amostras, bilateral, alpha = 0,05)
# Tamanhos reais dos níveis de cada fator (LA3, 104 indivíduos)
alpha <- 0.05
d     <- 0.5
fatores <- data.frame(
  fator = c("Diagnóstico", "Sexo", "Idade"),
  n1    = c(48, 46, 36),   # Controle | F | 6–14 anos
  n2    = c(56, 58, 68)    # ND       | M | >= 15 anos
)

poder_t2n <- function(n1, n2, d, alpha) {
  gl    <- n1 + n2 - 2
  ncp   <- d * sqrt(n1 * n2 / (n1 + n2))   # parâmetro de não centralidade
  tcrit <- qt(1 - alpha / 2, gl)
  pt(-tcrit, gl, ncp) + 1 - pt(tcrit, gl, ncp)
}

fatores$poder <- mapply(poder_t2n, fatores$n1, fatores$n2,
                        MoreArgs = list(d = d, alpha = alpha))

cat("\n===== PODER POR FATOR (d* = 0,5; alpha = 0,05) =====\n")
for (i in seq_len(nrow(fatores))) {
  cat(sprintf("%-12s n1 = %2d  n2 = %2d  poder = %.3f\n",
              fatores$fator[i], fatores$n1[i], fatores$n2[i], fatores$poder[i]))
}
cat("====================================================\n\n")

# Conferência: n por grupo para poder 0,8 com grupos iguais (dá 63,8 -> 64)
print(power.t.test(delta = d, sd = 1, sig.level = alpha, power = 0.8))

# Gráfico
rotulos <- sprintf("%s\n(%d vs %d)", fatores$fator, fatores$n1, fatores$n2)
desenha <- function() {
  b <- barplot(fatores$poder, names.arg = rotulos, ylim = c(0, 1), col = "#2E6F95",
               border = NA, las = 1, ylab = "Poder",
               main = expression("Poder para " * d^"*" * " = 0,5 (" * alpha * " = 0,05)"))
  abline(h = 0.8, lty = 2, col = "#B23A48")
  text(b, fatores$poder, sprintf("%.2f", fatores$poder), pos = 3)
  text(max(b) + 0.4, 0.8, "0,8", pos = 3, col = "#B23A48", xpd = TRUE)
}

# 1) Na tela (painel Plots)
desenha()

# 2) Em arquivo
png("poder.png", width = 1600, height = 1100, res = 200)
desenha()
dev.off()
cat("Gráfico salvo em:", normalizePath("poder.png"), "\n")
