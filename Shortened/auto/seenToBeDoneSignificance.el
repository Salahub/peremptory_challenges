(TeX-add-style-hook
 "seenToBeDoneSignificance"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("natbib" "longnamesfirst") ("geometry" "margin=2.5cm")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "tabularx"
    "subcaption"
    "pdfpages"
    "amsbsy"
    "amssymb"
    "graphicx"
    "natbib"
    "amsmath"
    "enumerate"
    "relsize"
    "color"
    "listings"
    "times"
    "geometry")
   (TeX-add-symbols
    '("TODO" 1)
    "R"
    "svthefootnote"
    "thefootnote")
   (LaTeX-add-labels
    "eq:vishyp"
    "fig:racedefmob"
    "tab:modcomp"
    "fig:modallcoef"
    "fig:stubcomp"
    "fig:philcomp"
    "fig:suncomp"
    "fig:racedefalldata")
   (LaTeX-add-bibliographies
    "myReferences")
   (LaTeX-add-color-definecolors
    "background-color"
    "steelblue"
    "brickred"
    "bluegray"
    "amethyst"))
 :latex)

