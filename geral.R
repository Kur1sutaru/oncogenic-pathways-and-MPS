#########################
#### mentoria ggplot ####
####### 500ws poa #######
#########################

# 
# Simple Gantt charts in R with ggplot2 and Microsoft Excel
# https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/

rm()

# Instalar e carregar pacotes:

# install.packages("tidyverse")
# install.packages("ggplot2")
library("tidyverse")


# We are using data from the National Morbidity and Mortality Air Pollution Study (NMMAPS). 
# To make the plots manageable we are limiting the data to Chicago and 1997-2000.

chic <- readr::read_csv(
  "https://raw.githubusercontent.com/Z3tt/R-Tutorials/master/ggplot2/chicago-nmmaps.csv")
# :: namespace (não é necessário carregar o pacote 'libray()')
# quando usamos mtos pacotes em um único script.  

tibble::glimpse(chic)
class(chic)
head(chic, 10) # variáveis categóricas, numéricas, discretas, contínuas...

# ggplot2 is a system for declaratively creating graphics, based on 
# The Grammar of Graphics. You provide the data, tell ggplot2 how to map 
# variables to aesthetics, what graphical primitives to use, and it takes care 
# of the details.

################################################################################

# elementos básicos ggplot:

# 1 -Data: The raw data that you want to plot.

# 2- Geometries 'geom_': The geometric shapes that will represent the data.

# 3- Aesthetics 'aes()': Aesthetics of the geometric and statistical objects, 
# such as position, color, size, shape, and transparency

# 4- Scales 'scale_': Maps between the data and the aesthetic dimensions, such as 
# data range to plot width or factor values to colors.

# 5- Statistical transformations 'stat_': Statistical summaries of the data, 
# such as quantiles, fitted curves, and sums.

# 6- Coordinate system 'coord_': The transformation used for mapping data 
# coordinates into the plane of the data rectangle.

# 7- Facets 'facet_': The arrangement of the data into a grid of plots.

# 8- Visual themes 'theme()': The overall visual defaults of a plot, 
# such as background, grids, axes, default typeface, sizes and colors.


##########################################################################


# com os dados + argumento 'aes' posisicionais (x e y)
ggplot(data = chic, aes(x = date, y = temp))


# atribuindo a um ggobejto ('g'<-)
(g <- ggplot(chic, aes(x = date, y = temp)))



g + geom_point()

g + geom_line()

g + geom_line() + geom_point()


# mudando as propriedades da 'geom_*()':

g + geom_point(color = "firebrick", 
               shape = "diamond", 
               size = 2)
# cores: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# paletas de cores: RColorBrewer::display.brewer.all()

# RColorBrewer::brewer.pal(n = 3, name = "Set1")

g + geom_point(color = "firebrick",
               shape = "diamond",
               size = 2) +
  geom_line(color = "firebrick", 
            linetype = "dotted", 
            size = .3)


# tema
theme_set(theme_bw())

g + geom_point(color = "firebrick")

# mudar a cor 


# pacotes com temas prontos:
# install.packages("ggthemes")
# install.packages("hrbrthemes")

# mudar a cor das linhas e pontos:
ggplot(chic, aes(x = date, y = o3)) +
  geom_line(color = "gray") +
  geom_point(color = "darkorange2") +
  labs(x = "Year", y = "Ozone")


# Títulos dos eixos:

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)")

# outra opção:
ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  xlab("Year") +
  ylab("Temperature (°F)")


# sobreescritos no texto (^):

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", 
       y = expression
       (paste("Temperature (", degree ~ F, ")"^"(Hey, why should we use metric units?!)")))


# ajustes de espaço entre eixos e títulos dos eixos
# theme()
# vjust (alinhamento vertical)

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15))




# margin
# margin(t, r, b, l)
# top, right, bottom, left
# t-r-oub-l-e 


ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 15),
        axis.title.y = element_text(margin = margin(r = 10), size = 15))


# estetica dos títulos dos eixos:
# dentro de 'theme()'
# modificar o elemento 'axis.title = ' ou 'axis.title.x' e 'axis.title.y'
# dentro de 'element_text = ':
# size
# color
# face (bol, italic, bold.italic)


# dois títulos iguais:
ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(axis.title = element_text(size = 15, color = "firebrick",
                                  face = "italic"))

# formatações independentes dos títuos x e y:
ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(axis.title.x = element_text(color = "sienna", size = 15),
        axis.title.y = element_text(color = "orangered", size = 15))



# formatação básica dos dois eixos e alterações específicas em x e y:
# ('axis.title = ' + 'axis.title.y')

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(axis.title = element_text(color = "sienna", size = 15, face = "bold"),
        axis.title.y = element_text(face = "bold.italic"))


# alterando estética do texto dos eixos:
# semelhante ao tópico anterior mas com 'axis.text' ou 'axis.text.x' e'axis.text.y'

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(axis.text = element_text(color = "dodgerblue", size = 12),
        axis.text.x = element_text(face = "italic"))

# rotacionar o texto do eixo:
# 'angle" dentro de 'theme(axis.text = (anlge = ))'

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12))




# remover títulos dos eixos:
# labs()

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = NULL, y = "")
# Note that NULL removes the element (similarly to element_blank()) 
# while empty quotes "" will keep the spacing for the axis title and simply 
# print nothing.


# Limitar a amplitude do eixo:
# subsetting: (subsets the data first)
ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  ylim(c(0, 50))

# zooming: (sem espaço)
ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
coord_cartesian(ylim = c(0, 50))

# boxplot (ano como fator)
# zoom
ggplot(chic, aes(x = factor(year), y = temp)) +
  geom_boxplot(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  coord_cartesian(ylim = c(0, 50))

# subset (recorta os dados antes da plotagem )
ggplot(chic, aes(x = factor(year), y = temp)) +
  geom_boxplot(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  ylim(c(0, 50))


##############################################################################


# Títulos:

# Adicionar:

g +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  ggtitle("Temperatures in Chicago")


# labs() Para adicionar mais argumentos:

g +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)",
       title = "Temperatures in Chicago",
       subtitle = "Seasonal pattern of daily temperatures from 1997 to 2001",
       caption = "Data: NMMAPS",
       tag = "Fig. 1")


# ajustar as propriedades do título 'theme()'
# funciona para:
# plot.subtitle, plot.caption, plot.caption, legend.title, 
# legend.text, and axis.title and axis.text

g +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)",
       title = "Temperatures in Chicago") +
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))

# order of the margin arguments is "t-r-oub-l-e" 
# that resembles the first letter of the four sides


# posiçao do título:
# hjust
ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = NULL,
       title = "Temperatures in Chicago",
       caption = "Data: NMMAPS") +
  theme(plot.title = element_text(hjust = 1, size = 16, face = "bold.italic"))



# mudando o espaço e quebrando linhas

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  ggtitle("Temperatures in Chicago\nfrom 1997 to 2001") +
  theme(plot.title = element_text(lineheight = .8, size = 16))



################################################################################

# legendas:
#  color = season

ggplot(chic,
       aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)")

# remover legenda:
# theme(legend.position = "none")

ggplot(chic,
       aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(legend.position = "none")

# guides(color = "none")
# Here, for example, we keep the legend for the shapes while 
# discarding the one for the colors.

ggplot(chic,
       aes(x = date, y = temp,
           color = season, shape = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)") +
  guides(color = "none")

# remover título da legenda:

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(legend.title = element_blank())


# posição da legenda:
# opções:
# top", "right" (which is the default), "bottom", and "left"

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(legend.position = "top")

# dentro do gráfico:
ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)",
       color = NULL) +
  theme(legend.position = c(.15, .15),
        legend.background = element_rect(fill = "transparent"))

# mudar a direção da legenda (horizontal):

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(legend.position = c(.5, .97),
        legend.background = element_rect(fill = "transparent")) +
  guides(color = guide_legend(direction = "horizontal"))

# mudar o estilo do título da legenda
# theme(legend.title = ())

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(legend.title = element_text(color = "chocolate",
                                    size = 14, face = "bold"))


# mudar o nome da leganda:
ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)",
       color = "Seasons\nindicated\nby colors:") +
  theme(legend.title = element_text(color = "chocolate",
                                    size = 14, face = "bold"))

# mudar a ordem dos itens da legenda:

chic$season <-
  factor(chic$season,
         levels = c("Winter", "Spring", "Summer", "Autumn"))
# determinar a ordem dos níveis 

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)")

# alterar os nomes dos níveis da legenda:
# scale_color_discrete()

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)") +
  scale_color_discrete(
    name = "Seasons:", 
    labels = c("Mar-May", "Jun-Aug", "Sep-Nov", "Dec-Feb")
  ) +
  theme(legend.title = element_text(
     color = "chocolate", size = 14, face = 2
  ))

# alterar o preencimento dos ítens da legenda:
# legend.key()

ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(legend.key = element_rect(fill = "darkgoldenrod1"),
        legend.title = element_text(color = "chocolate",
                                    size = 14, face = 2)) +
  scale_color_discrete("Seasons:")

#sem preenchimento:
# fill = NA or fill = "transparent"

# alterar o tamanho dos simbolos da legenda:
ggplot(chic, aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(legend.key = element_rect(fill = NA),
        legend.title = element_text(color = "chocolate",
                                    size = 14, face = 2)) +
  scale_color_discrete("Seasons:") +
  guides(color = guide_legend(override.aes = list(size = 6)))


# Usar outros estilos de legenda:

# default:
ggplot(chic,
       aes(x = date, y = temp, color = temp)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)", color = "Temperature (°F)")
# If you map a continuous variable to an aesthetic, {ggplot2} will 
# by default not use guide_legend() but guide_colorbar() (or guide_colourbar())

# However, by using guide_legend() you can force the legend to show discrete 
# colors for a given number of breaks as in case of a categorical variable:

ggplot(chic,
       aes(x = date, y = temp, color = temp)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)", color = "Temperature (°F)") +
  guides(color = guide_legend())

# ou limitar as escalas
ggplot(chic,
       aes(x = date, y = temp, color = temp)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)", color = "Temperature (°F)") +
  guides(color = guide_bins())

# ou escala de cores discretas:
ggplot(chic,
       aes(x = date, y = temp, color = temp)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°F)", color = "Temperature (°F)") +
  guides(color = guide_colorsteps())


############################################################################


# planos de fundo e linhas:

# dentro de theme():

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "#1D8565", size = 2) +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(panel.background = element_rect(
    fill = "#64D2AA", color = "#64D2AA", size = 2)
  )

# alterando a borda
# Note that the true color-the outline of the panel background-did not change 
# even though we specified it. This is because there is a layer on top of 
# the panel.background, namely panel.border


ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "#1D8565", size = 2) +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(panel.border = element_rect(
    fill = "#64D2AA99", color = "#64D2AA", size = 2)
  )

# alterar linhas:
# panel.grid or 
# for each set of gridlines separately, panel.grid.major and panel.grid.minor

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(panel.grid.major = element_line(color = "gray10", size = .5),
        panel.grid.minor = element_line(color = "gray70", size = .25))

# especificar detalhadamente:
ggplot(chic, aes(x = date, y = temp)) +
geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(panel.grid.major = element_line(size = .5, linetype = "dashed"),
        panel.grid.minor = element_line(size = .25, linetype = "dotted"),
        panel.grid.major.x = element_line(color = "red1"),
        panel.grid.major.y = element_line(color = "blue1"),
        panel.grid.minor.x = element_line(color = "red4"),
        panel.grid.minor.y = element_line(color = "blue4"))

# remover:

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(panel.grid.minor = element_blank())

# remover todas:

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(panel.grid = element_blank())
# ou theme_minimal()


# alterar a cor do fundo:
ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "gray60",
                                       color = "gray30", size = 2))

############################################################################

ggplot(chic, aes(x = date, y = temp)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°F)") +
  theme(plot.background = element_rect(fill = "gray60"),
        plot.margin = margin(t = 1, r = 3, b = 1, l = 8, unit = "cm"))
