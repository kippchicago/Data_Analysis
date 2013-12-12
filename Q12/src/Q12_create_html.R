library(ProjectTemplate)

load.project()

knit2html("src/Q12.Rmd", output="reports/Q12_figs.html", options=c('fragment_only', 'skip_style'))
system('cp ./reports/Q12_figs.html ~/Sites/test.local/')
system('cp -r ./figure/* ~/Sites/test.local/figure/')
