\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{mycustomclass}

\LoadClass{article}

\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{enumitem}
\usepackage[dvipsnames]{xcolor}
\RequirePackage{environ}
\RequirePackage{listings}
\RequirePackage{tcolorbox}
\RequirePackage{titlesec}
\RequirePackage[left=0.4in, right=0.4in, top=0.4in, bottom=0.4in]{geometry}

\pagenumbering{gobble}

\titleformat{\section}
  {\normalfont\Large\bfseries\color{RoyalBlue}}
  {\thesection}{1em}{}

\titleformat{\subsection}
  {\normalfont\large\bfseries\color{Cerulean}}
  {\thesubsection}{1em}{}

\titleformat{\subsubsection}
  {\normalfont\normalsize\bfseries\color{Turquoise}}
  {\thesubsubsection}{1em}{}

\newenvironment{alphlist}{\begin{enumerate}[label={(\alph*)}]}{\end{enumerate}}
\newenvironment{romanlist}{\begin{enumerate}[label={(\roman*)}]}{\end{enumerate}}
\newenvironment{arabiclist}{\begin{enumerate}[label={(\arabic*)}]}{\end{enumerate}}

\tcbuselibrary{theorems}

\newtcbtheorem[no counter]{tims_defn}{Definition}
{
    titlerule=0mm,
    fonttitle=\bfseries,
    coltitle=black,
    colbacktitle=green!20!white,
    colback=green!10!white,
}{defn}

\newtcbtheorem[no counter]{tims_thrm}{Theorem}
{
    titlerule=0mm,
    fonttitle=\bfseries,
    coltitle=black,
    colbacktitle=purple!20!white,
    colback=purple!10!white,
}{thrm}

\newcommand{\defn}[2]{
    \begin{tims_defn}{#1}{}
        #2
    \end{tims_defn}
}

\newcommand{\thrm}[2]{
    \begin{tims_thrm}{#1}{}
        #2
    \end{tims_thrm}
}

\endinput
