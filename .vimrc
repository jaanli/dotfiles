syntax on
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
" call pathogen#infect('bundle/{}')
filetype plugin indent on

set nocompatible
set shiftround  " Round indent to multiple of 'shiftwidth'

" Set the tab width
let s:tabwidth=2
exec 'set tabstop='    .s:tabwidth
exec 'set shiftwidth=' .s:tabwidth
exec 'set softtabstop='.s:tabwidth
" for ctrl p. need to run `:helptags ~/.vim/bundle/ctrlp.vim/doc` in vim
set runtimepath^=~/.vim/bundle/ctrlp.vim
set autoindent
" autocmd Filetype python setlocal tabstop=2
" set smartindent
set cindent
set cinkeys-=0#
set indentkeys-=0#
" ignore case in search
set ic


" save with one key
map <F3> :w<CR>

set pastetoggle=<F2>

" remove status line
set nosmd

" 80 char ruler
set colorcolumn=80


" strip trailing whitespace no save with vim better whitespace plugin
autocmd BufWritePre * StripWhitespace

" autocompletion. neocomplete requires vim-nox or vim compiled with lua
" install: sudo apt-get install vim-nox on ubuntu
let g:neocomplete#enable_at_startup = 1
inoremap <expr><Down> pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr><Up> pumvisible() ? "\<C-p>" : "\<Up>"

" Python-mode options
" remove autocomplete python-mode help bar
set completeopt=menu
" Disable pylint checking every save
let g:pymode_lint_write = 0

" for vimux
let g:VimuxOrientation = "h"
let g:VimuxHeight = "60"
" Run the current file with rspec
map <Leader>rb :call VimuxRunCommand("clear; python " . bufname("%"))<CR>
