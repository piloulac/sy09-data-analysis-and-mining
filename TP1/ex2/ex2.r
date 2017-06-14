png(file = "TP1/ex2/boxplot_fl_espece.png")
boxplot(crabs$FL[crabs$sp == "B"], crabs$FL[crabs$sp == "O"], main = "Étude du caractere FL en fonction des espèces",  col = c("blue", "orange"), names = c("Bleu", "Orange"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "TP1/ex2/boxplot_rw_espece.png")
boxplot(crabs$RW[crabs$sp == "B"], crabs$RW[crabs$sp == "O"], main = "Étude du caractere RW en fonction des espèces",  col = c("blue", "orange"), names = c("Bleu", "Orange"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "TP1/ex2/boxplot_cl_espece.png")
boxplot(crabs$CL[crabs$sp == "B"], crabs$CL[crabs$sp == "O"], main = "Étude du caractere CL en fonction des espèces",  col = c("blue", "orange"), names = c("Bleu", "Orange"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "TP1/ex2/boxplot_cw_espece.png")
boxplot(crabs$CW[crabs$sp == "B"], crabs$CW[crabs$sp == "O"], main = "Étude du caractere CW en fonction des espèces",  col = c("blue", "orange"), names = c("Bleu", "Orange"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "TP1/ex2/boxplot_bd_espece.png")
boxplot(crabs$BD[crabs$sp == "B"], crabs$BD[crabs$sp == "O"], main = "Étude du caractere BD en fonction des espèces",  col = c("blue", "orange"), names = c("Bleu", "Orange"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "TP1/ex2/boxplot_fl_sexe.png")
boxplot(crabs$FL[crabs$sex == "M"], crabs$FL[crabs$sex == "F"], main = "Étude du caractere FL en fonction du sexe",  col = c("blue", "orange"), names = c("Bleu", "Orange"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "TP1/ex2/boxplot_rw_sexe.png")
boxplot(crabs$RW[crabs$sex == "M"], crabs$RW[crabs$sex == "F"], main = "Étude du caractere RW en fonction du sexe",  col = c("blue", "orange"), names = c("Bleu", "Orange"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "TP1/ex2/boxplot_cl_sexe.png")
boxplot(crabs$CL[crabs$sex == "M"], crabs$CL[crabs$sex == "F"], main = "Étude du caractere CL en fonction du sexe",  col = c("blue", "orange"), names = c("Bleu", "Orange"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "TP1/ex2/boxplot_cw_sexe.png")
boxplot(crabs$CW[crabs$sex == "M"], crabs$CW[crabs$sex == "F"], main = "Étude du caractere CW en fonction du sexe",  col = c("blue", "orange"), names = c("Bleu", "Orange"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()

png(file = "TP1/ex2/boxplot_bd_sexe.png")
boxplot(crabs$BD[crabs$sex == "M"], crabs$BD[crabs$sex == "F"], main = "Étude du caractere BD en fonction du sexe",  col = c("blue", "orange"), names = c("Bleu", "Orange"), ylab = "nombre de crabes", notch = TRUE, outline = FALSE)
dev.off()
