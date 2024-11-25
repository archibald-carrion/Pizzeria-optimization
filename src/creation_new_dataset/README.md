

to install rmarkdown package
```
install.packages("rmarkdown")
```

to create a html file from Rmd file
```bash
Rscript -e "rmarkdown::render('pizza_ingredients_analysis.Rmd')"
```

to create a pdf file from Rmd file
```bash
Rscript -e "rmarkdown::render('pizza_ingredients_analysis.Rmd', output_format = 'pdf_document')"
```

once the latex file is created, you can compile it to pdf using command line tools or overleaf