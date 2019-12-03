(TeX-add-style-hook
 "ShortenedThesis"
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
    "Introduction"
    "History"
    "Data"
    "Analysis"
    "Summary"
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
   (LaTeX-add-bibliographies
    "myReferences"))
 :latex)

