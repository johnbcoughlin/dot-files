(TeX-add-style-hook
 "header"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("color" "usenames") ("ulem" "normalem")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "color"
    "ulem"
    "amsmath"
    "amssymb"
    "textcomp"
    "graphicx"
    "wrapfig"
    "grffile"))
 :latex)

