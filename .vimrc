set nocompatible


"4-space tab
set tabstop=4
"Indent with <<>> 4 spaces
set shiftwidth=4
"Insert spaces instead of tabs
set expandtab
"Backspace and delete work like with tabs
set softtabstop=4
"Autoindent next line
set autoindent

"Python automatic indentation after definitions
autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class

"Shell
set sh=/bin/bash

highlight BadWhitespace ctermbg=red guibg=red

"Editor customization
set ruler
"set nohls
set incsearch
set number
colorscheme jellybeans
syntax on

"Switch plugins, indentation and filetype detection on
filetype plugin indent on

"Run pathogen for bundle plugs
