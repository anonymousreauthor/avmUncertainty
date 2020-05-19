 

\newif\if@colTwo
\DeclareOption{colTwo}{%
\@colTwotrue}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}
\renewcommand{\normalsize}{\fontsize{9.54pt}{11.44pt}\selectfont}
\RequirePackage{amsfonts,amssymb,amsbsy,amsmath,amsthm}
\RequirePackage{epigrafica}
\RequirePackage[OT1]{fontenc}
\RequirePackage[spanish,english]{babel}
\RequirePackage{graphicx}
\RequirePackage{latexsym,ifthen,rotating,calc,textcase,booktabs,color,endnotes}
\RequirePackage{titlesec}
\RequirePackage[errorshow]{tracefnt}
\usepackage[colorlinks,bookmarksopen,bookmarksnumbered,citecolor=red,urlcolor=red]{hyperref}

\RequirePackage[
type={CC},
modifier={by},
version={3.0},
]{doclicense}
\definecolor{grisMedio}{cmyk}{0,0,0,0.5}

 
\RequirePackage{graphicx,color} % needed for logo and pictures
\usepackage[utf8]{inputenc}
\usepackage{amsthm,amssymb,latexsym}
\usepackage{verbatim}               
\usepackage{graphicx}
\usepackage{epigrafica}
\usepackage[OT1]{fontenc}
\usepackage{enumerate}                         
\usepackage{array}                              
\usepackage{multicol}
\usepackage{color}
\definecolor{gris-oscuro}{cmyk}{0,0,0,0.8}
\usepackage[multiple]{footmisc}
\usepackage{url}
\renewcommand{\UrlFont}{\normalsize}
\setlength{\parindent}{0pt}	% indentation on new paragraph
\setlength{\parskip}{5pt}	% vertical spacing on new paragraph
\setlength{\lineskip}{1pt}	% vertical spacing between lines
\setlength\columnsep{0.6cm} 
\@twosidetrue
 
\if@colTwo
\usepackage[text={174mm,258mm},%
papersize={210mm,297mm},%
%columnsep=12pt,%
%headsep=21pt,%
centering]{geometry}
\usepackage{ftnright}
\addtolength{\skip\footins}{0pt}
 

\hyphenpenalty=1000
\pretolerance=8000
\tolerance=9500
\hbadness=8000
\vbadness=9000
\displaywidowpenalty=0
\clubpenalty=10000
\widowpenalty=10000
\lefthyphenmin=3%
\righthyphenmin=3%
\brokenpenalty=10000%
 
\newcommand{\FontsizeTi}{\fontsize{17.27pt}{19.2pt}\selectfont}
\newcommand{\FontsizeSubTi}{\fontsize{14.03pt}{15.6pt}\selectfont}
\newcommand{\FontsizeAuthor}{\fontsize{10.79pt}{12pt}\selectfont}
\newcommand{\refsize}{\fontsize{9}{12pt}\selectfont}
\renewcommand{\footnotesize}{\fontsize{8}{10pt}\selectfont}
\renewcommand{\scriptsize}{\fontsize{7}{8.5pt}\selectfont}
\renewcommand{\large}{\fontsize{12}{14pt}\selectfont}




 
\newbox\absbox
\def\abstract{\lrbox\absbox\minipage{\textwidth}}
\def\endabstract{\endminipage\endlrbox}

\newenvironment{abstracts}
{\global\setbox\absbox=\vbox\bgroup
	\hsize=\textwidth
	\linespread{1}\fontsize{9.03pt}{10pt}\selectfont}
{\egroup}

\renewenvironment{abstract}[1][]
{\if\relax\detokenize{#1}\relax\else\selectlanguage{#1}\fi
{\noindent\@keywords}\par\bigskip\noindent{\fontsize{10.04pt}{11pt}\selectfont\MakeUppercase{\textbf{\abstractname}}}\par\ignorespaces}
{\par \noindent\@palwords}
 
 

\def\keywords#1{\gdef\@keywords{\begin{minipage}{\textwidth}{\fontsize{9.03pt}{10pt}\selectfont \textbf{Keywords:}}   {#1}\end{minipage}}}
 \let\@keywords\@empty
\def\palwords#1{%
	\gdef\@palwords{\begin{minipage}{\textwidth}{\fontsize{9.03pt}{10pt}\selectfont  \textbf{Palabras clave:}} {#1}\end{minipage}}}
 \let\@palwords\@empty

 
\gdef\footnoterule{\kern-3\p@ {\color{grisMedio}\hrule width 2in height 0.5pt} \kern 2mm}

\renewcommand{\thefootnote}{\fnsymbol{footnote}}
\long\def\@makefntext#1{\parindent 0em%
\noindent{$\m@th^{\@thefnmark}$}#1}

\newcommand{\CCbY}[1]{%
  \gdef\@CCbY{%
\footnotetext[0]{\begin{minipage}{0.47\textwidth} \fontsize{7.03pt}{8.4pt}\selectfont\doclicenseImage[imagewidth=6em] Attribution 4.0 International (CC BY 4.0) Share - Adapt\end{minipage} }}}
\let\@CCbY\@empty

 
 
\def\Howtocite#1{\gdef\@Howtocite{%
\footnotetext[0]{\par\vskip-3pt\noindent\fbox{\begin{minipage}{0.47\textwidth}\noindent\textbf{How to cite:} \fontsize{7.03pt}{8.4pt}\selectfont #1\end{minipage}}\par\vskip 1pt}}}
\let\@Howtocite\@empty


\def\affiliation#1{%
  \gdef\@affiliation{%
    \footnotetext[0]{\fontsize{7.03pt}{8.4pt}\selectfont #1}}}
\let\@affiliation\@empty

\def\affilnum#1{$^{\text{{#1}}}$}


\def\@subtitle{\relax}
\newcommand{\subtitle}[1]{\gdef\@subtitle{#1}} 
 

\def\@doi{\relax}
\newcommand{\doi}[1]{\gdef\@doi{http://dx.doi.org/#1}} 

%\newcommand{\date}[1]{\def\@date{#1}}
 

\renewcommand\maketitle{\par
  \begingroup
     \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
       \fi
    \else
      \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
    \thispagestyle{title}\label{FirstPage}\@affiliation\@Howtocite\@CCbY%
  \endgroup
  %\setcounter{footnote}{0}%
  \global\let\affiliation\relax
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\relax
  \global\let\@title\@empty
  \global\let\@affiliation\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\and\relax
}
\def\@maketitle{%
 

\begin{center}
 
\vspace*{-0.7cm}
\begin{minipage}[t]{\textwidth}
		{\flushright\fontsize{7.02pt}{8.4pt}\selectfont\@doi\par}
	\vskip 1.5em%
\end{minipage} 
\begin{minipage}[t]{\textwidth-28mm}
  \vskip 12.5pt%
  	{\centering\usefont{T1}{fnc}{b}{n}\FontsizeTi\@title\par}
     \vskip 1.5em%
    \end{minipage} 
\begin{minipage}[t]{\textwidth-28mm}
	\vskip 12.5pt%
	{\centering\usefont{T1}{fnc}{b}{n}\FontsizeSubTi\@subtitle\par}
	\vskip 1.5em%
	\vskip 12.5mm%
\end{minipage} 
{\par%
 
{\centering\usefont{T1}{fnc}{m}{n}\FontsizeAuthor\@author\par}}
{\noindent\usebox\absbox\par}
    
    {\vspace{0pt}%
      %
  }
 
      \end{center}
 
  }
 
\def\startpage{\pageref{FirstPage}}
\def\endpage{\pageref{LastPage}}
\def\volumeyear{}
\def\volumenumber{}
\def\issuenumber{}
\def\journalname{\scshape{Zillow Analytics Working Paper}}


\def\authorrunning#1{\markboth{}{{#1}}}
\def\@titlerunning{}
\def\titlerunning#1{%
	\gdef\@titlerunning{#1}}


\def\ps@title{%
\def\@oddhead{\parbox{\textwidth}{\noindent{\color{grisMedio}\rule{\textwidth}{1.5pt}}\\\mbox{\fontsize{9.02pt}{10.8pt}\selectfont \journalname\ \volumenumber(\issuenumber)}\\[-5pt]
{\noindent\color{grisMedio}\rule{\textwidth}{0.5pt}}}}%
\let\@evenhead\@oddhead
\def\@oddfoot{\parbox[t]{\textwidth}{\includegraphics{logo.pdf}
		\ \fontsize{9.02pt}{10.8pt}\selectfont\thepage}}
\let\@evenfoot\@oddfoot}


\def\ps@headings{%
	\let\@mkboth\@gobbletwo
	\def\@evenhead{\parbox{\textwidth}{			\fontsize{9.02pt}{10.8pt}\selectfont\scshape{\@titlerunning}\\[-6pt]
			\noindent\color{grisMedio}\rule{\textwidth}{0.5pt}}}
	\def\@oddhead{\parbox{\textwidth}{\hfill
			\fontsize{9.02pt}{10.8pt}\selectfont\itshape{\rightmark}\\[-6pt]
			\noindent\color{grisMedio}\rule{\textwidth}{0.5pt}}}
	\def\@evenfoot{\parbox[t]{\textwidth}{\fontsize{9.02pt}{10.8pt}\selectfont \includegraphics{logo.pdf}\ \thepage \quad \journalname\ \volumenumber(\issuenumber)}}
	\def\@oddfoot{\parbox[t]{\textwidth}{\hfill\fontsize{9.02pt}{10.8pt}\selectfont\journalname\ \volumenumber(\issuenumber) \quad \thepage \ \includegraphics{logo.pdf} }}
}



% Titling (section/subsection)
%\usepackage{titlesec}
\setcounter{secnumdepth}{0}
\renewcommand\section{\@startsection{section}{1}{0pt}%
	{-2.5ex plus -0.5ex minus -.2ex}%
	{0.1ex plus .1ex}%
	{\fontsize{12.044pt}{14.4pt}\selectfont\bfseries}}
\renewcommand\subsection{\@startsection{subsection}{2}{0pt}%
	{-2.0ex plus -0.5ex minus -.2ex}%
	{0.1ex plus .1ex}%
	{\fontsize{11.04pt}{13.2pt}\selectfont\it}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{0pt}%
	{-2.0ex plus -0.5ex minus -.2ex}%
	{0.1ex plus .1ex}%
	{\normalsize\it}}
 

%%%%%%%%%
%%caption table and figure
%%%%%%%%%
\usepackage{floatrow}
\usepackage{caption}
\DeclareCaptionFont{mysizeFONT}{\fontsize{8.03pt}{6.6pt}\selectfont} 
\DeclareCaptionFormat{citation}{\raggedright #1#2#3\par%
	\ifx\captioncitation\relax\relax\else
	\captioncitation\par
	\fi
	\par}
\newcommand*\setcaptioncitation[1]{\def\captioncitation{\textbf{Source:}~#1}}
\let\captioncitation\relax
\captionsetup[figure]{format=citation,labelsep=period,labelfont={bf},font=mysizeFONT}
 
 \newcommand{\source}[1]{\par\medskip\raggedright\fontsize{8.03pt}{6.6pt}\selectfont\textbf{Source:} {#1}}
\captionsetup[table]{justification=raggedright,
	singlelinecheck=false,labelsep=period,labelfont={bf},font=mysizeFONT}
\floatsetup[table]{capposition=top}


 

\AtBeginDocument{%
	\renewcommand\tablename{Table}
}
\AtBeginDocument{%
	\renewcommand\figurename{Figure}
}
 
 
 
%%Bibliografia
\usepackage[harvard,dcucite]{harvard}
\bibstyle{harvard}
\bibliographystyle{dcu}
\citationstyle{dcu}
\let\oldcite\cite
\renewcommand\cite[1]{\begingroup\protected\def\harvardand{\&}
	\oldcite{#1}\endgroup}
\protected\def\harvardand{and}
\usepackage{etoolbox}
\apptocmd{\thebibliography}{\color{gris-oscuro}\fontsize{9.03pt}{10.6pt}\selectfont}{}{}%

\AtEndDocument{%
\label{LastPage}}

\pagestyle{headings}
 
 
 \renewcommand\labelitemii{$\square$}


\if@colTwo
\twocolumn
\fi

\sloppy