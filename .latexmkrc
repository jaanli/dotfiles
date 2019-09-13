#
# General
#

# xelatex
$pdfxe = '-no-pdf -interaction=nonstopmode -synctex=1 %O %S';
push @extra_xelatex_options, '-synctex=1 -interaction=nonstopmode -no-pdf --shell-escape';

# Needed for the dot2texi package which invokes GraphViz.
$latex = 'latex --shell-escape';
$pdflatex = 'pdflatex -synctex=1 -interaction=nonstopmode --shell-escape';

# from https://tex.stackexchange.com/questions/236329/auctexs-c-c-c-c-issue-with-latexmk
$pdflatex = 'pdflatex -interaction=nonstopmode -synctex=1 %O %S';
$pdf_previewer = 'open -a skim';
$clean_ext = 'bbl rel %R-blx.bib %R.synctex.gz';
@generated_exts = (@generated_exts, 'synctex.gz');

# 
# Mac OS
#
$pdf_previewer = "open -a Skim";
#$clean_ext = "paux lox pdfsync out";