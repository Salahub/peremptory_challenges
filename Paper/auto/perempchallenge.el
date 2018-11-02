(TeX-add-style-hook
 "perempchallenge"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("caption" "font=footnotesize") ("subcaption" "font=footnotesize") ("graphicx" "pdftex") ("babel" "american")))
   (add-to-list 'LaTeX-verbatim-environments-local "alltt")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "url"
    "times"
    "amsthm"
    "amsmath"
    "amssymb"
    "caption"
    "subcaption"
    "graphicx"
    "wrapfig"
    "epstopdf"
    "babel"
    "color"
    "xspace"
    "float"
    "tabularx"
    "multirow"
    "alltt"
    "multicol"
    "blindtext"
    "scrextend"
    "geometry"
    "algorithmic"
    "algorithm")
   (TeX-add-symbols
    '("unit" 1)
    '("cardinality" 2)
    '("elements" 3)
    '("set" 1))
   (LaTeX-add-labels
    "sec:Intro"
    "sec:background"
    "subsec:history"
    "subsec:modprac"
    "sec:data"
    "subsec:jspdata"
    "subsec:norcardata"
    "subsec:phillydata"
    "subsec:datacleaning"
    "sec:analysis"
    "sec:comparison"
    "sec:conclusion")
   (LaTeX-add-bibliographies)
   (LaTeX-add-amsthm-newtheorems
    "definition"))
 :latex)

