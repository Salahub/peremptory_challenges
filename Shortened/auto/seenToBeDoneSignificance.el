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
    "R"
    "svthefootnote"
    "thefootnote")
   (LaTeX-add-labels
    "tab:margrace"
    "eq:vishyp"
    "fig:racedefmob"
    "fig:racedefci"
    "tab:modcomp"
    "fig:modallcoef"
    "fig:stubcomp"
    "fig:philcomp"
    "fig:suncomp"
    "fig:racedefalldata"
    "sec:casesum"
    "subsec:struckjur"
    "subsec:vistrend"
    "fig:trialprodef"
    "fig:defraceprop"
    "fig:proraceprop"
    "fig:defproprop")
   (LaTeX-add-bibliographies
    "myReferences"))
 :latex)

