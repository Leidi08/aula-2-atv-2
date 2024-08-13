dados_carros <- mtcars

# a) DIAGRAMA DE DISPERSÃO
ggplot(data = dados_carros, aes(x = hp, y = mpg) )+
  geom_point()+
  theme_classic()+
  xlab("hp")+
  ylab("mpg")+
  geom_smooth(method = "lm", se = FALSE)
# b)COEFICIENTE DE CORRELAÇÃO LINEAR
cor(dados_carros$hp, dados_carros$mpg)
#correlação = -0.7761684

# c) AJUSTA O MODELO
modelo_1 <- lm(formula = hp ~ mpg, data = dados_carros)
#mostra resultado do modelo
summary(modelo_1)
#alpha = 30.09886
#beta = -0.06823

# d) INCREMENTO DE UMA UNIDADE HP QUANTO AUMENTA OU DIMINUI MPG
modelo_2 <- lm(formula = mpg ~ hp, data = dados_carros)
summary(modelo_2)

# e) coeficiente é 0.6024

# f) GRÁFICO DE RESÍDUOS E HPÓTESE DE HOMOSCEDASTICIDADE
plot (dados_carros$hp, standard(modelo_2), xlab = “mpg”, ylab = “hp”) abline (0,0)

# g) FAÇA UMA PREDIÇÃO DE MPG PARA HP = 200
dado_novo <- data.frame(hp = 200)
predict(modelo_2, dado_novo)





