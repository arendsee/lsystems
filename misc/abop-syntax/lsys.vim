" Vim syntax file
" Language: lsys
" Maintainer: Zebulun Arendsee
" -----------------------------------------------------------------------------
" INSTALLATION
" Run the following in your UNIX terminal
" $ mkdir -p ~/.vim/syntax/
" $ mkdir -p ~/.vim/ftdetect/
" $ cp lsys.vim ~/.vim/syntax/
" $ echo 'au BufRead,BufNewFile *.lsys set filetype=lsys' > ~/.vim/ftdetect/lsys.vim

if exists("b:current_syntax")
  finish
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Global syntax - shared between all sections

syn match macro /#.*/

" primitives
syn match variable /[A-Za-z0-9_]\+/
syn match number '\d\+'

" abstractions
syn match function /[A-Za-z]\+([^)]\+)/

" keyword
"   This is a bit hacky, but I want the commands to be repeatable without
"   spaces. So `FFFF` is a keyword representing `F F F F` not the variable
"   `FFFF`.
syn keyword action F
syn keyword action f
syn keyword action ff
syn keyword action l
syn keyword action ll
syn keyword action r
syn keyword action L
syn keyword action R
syn keyword action G
syn match action /FF\+/
syn match action /fff\+/
syn match action /lll\+/
syn match action /rr\+/
syn match action /LL\+/
syn match action /RR\+/
syn match action /GG\+/
syn match action /[+^&\\/|$[\]{}.~'%-]/

syn match operator /->/
syn match operator /:/
syn match operator /=/
syn match operator /</
syn match operator />/


let b:current_syntax = "lsys"

hi def link macro    Macro
hi def link variable Identifier
hi def link comment  Comment
hi def link number   Number
hi def link operator Operator
hi def link action   Keyword
hi def link function Function 
hi def link type     Type
