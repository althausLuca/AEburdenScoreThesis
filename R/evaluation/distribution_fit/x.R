 #' x values to comptue model CDFs at
x <- c(seq(-30, -10, by = 2),
       seq(-9, 10, by = 0.3),
       seq(11, 30, by = 0.5),
       seq(31, 50, by = 1.5),
       seq(51, 100, by = 3),
       seq(100, 200, by = 20),
       seq(200, 600, by = 50)
)

pos_log <- c(0,pracma::logseq(0.1, 600, n = 90))

 #' x values to comptue model CDFs at
x_log_anova_c_0.001 <- c(c(-30, -0.001),
                         seq( -0.001, 0, length.out = 5),
                         pos_log
)


LOG_ANOVA(c = 0.001)