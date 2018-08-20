#
# General
#

# Needed for the dot2texi package which invokes GraphViz.
$latex = 'latex --shell-escape';
$pdflatex = 'pdflatex -synctex=1 -interaction=nonstopmode --shell-escape';

@generated_exts = (@generated_exts, 'synctex.gz');

# 
# Mac OS
#
$pdf_previewer = "open -a Skim";
$clean_ext = "paux lox pdfsync out";