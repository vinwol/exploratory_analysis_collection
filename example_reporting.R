library(ggplot2)
library(ggpubr)
library(hash)
library(lubridate)
library(XML)
library(R6)

source('./edawb_code/html_report.R')

title <- 'Report'
name <- 'My name'
date <- lubridate::today()

file <- './reporting/tables/table_1.html'
save_as_static_html_table(file, mtcars)

file <- './reporting/tables/table_2.html'
save_as_dynamic_html_table(file, mtcars)

g1 <- ggplot(diamonds) + 
        geom_point(aes(x = carat, y = price, color = cut)) + 
        geom_smooth(aes(x = carat, y = price, color = cut))
ggsave('./reporting/imgs/ggplot_example_1.png', g1, height = 5, width = 8)

g2 <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point() +
        geom_smooth(method = lm) +
        ggpubr::stat_cor(method = "pearson", label.x = 20)
ggsave('./reporting/imgs/ggplot_example_2.png', g2, height = 5, width = 8)

# Combine plots via ggarrange() from ggpubr.
g3 <- ggpubr::ggarrange(g1, g2, ncol = 2, nrow = 1, align = 'h')
ggsave('./reporting/imgs/ggplot_example_3.png', g3, height = 5, width = 10)

# Generate HTML Report.
html_template <- './reporting/template_side_menu_and_main_panel.txt'
report <- HTMLReporter$new(html_template) 
report$add_title(title, name, date) 
report$add_image('section-1',
                 'ggplot example 1-1',
                 './imgs/ggplot_example_1.png', 
                 '800px', 
                 '600px')
report$add_image('section-1',
                 'ggplot example 1-2',
                 './imgs/ggplot_example_2.png', 
                 '800px', 
                 '600px')
report$add_table('section-1',
                 'example static table', 
                 './tables/table_1.html', 
                 '100%', 
                 '100vh')
report$add_table('section-1',
                 'example dynamic table',
                 './tables/table_2.html', 
                 '100%', 
                 '700px')
report$add_text('section-1','The above table is an example for a dynamic table.')
report$add_image('section-2',
                 'ggplot example 2-1',
                 './imgs/ggplot_example_1.png', 
                 '800px', 
                 '600px')
report$add_image('section-2',
                 'ggplot example 2-2',
                 './imgs/ggplot_example_2.png', 
                 '800px', 
                 '600px')
report$add_image('section-2',
                 'ggplot example 2-3',
                 './imgs/ggplot_example_3.png', 
                 '1600px', 
                 '600px')
report$add_two_tables_horizontally('section-2',
                                   'horizontal table example:',
                                   './tables/table_1.html',
                                   './tables/table_1.html',
                                   '800px', 
                                   '1300px')
report$add_image('section-3',
                 'ggplot example 3-1',
                 './imgs/ggplot_example_1.png', 
                 '800px', 
                 '600px')
report$add_image('section-3',
                 'ggplot example 3-2',
                 './imgs/ggplot_example_2.png', 
                 '800px', 
                 '600px')

html_report <- './reporting/html_report.html'

section_list <- c('section-1', 'section-2', 'section-3')

report$create_html_webpage(html_report, section_list) 




