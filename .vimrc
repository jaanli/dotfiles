set nocompatible              " be iMproved, required
filetype off                  " required

" to fix vim-sensible plugin breaking runtimepath
syntax enable

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Bundle 'ctrlpvim/ctrlp.vim'
Bundle 'vim-airline/vim-airline'
Bundle 'ntpeters/vim-better-whitespace'
Bundle 'ConradIrwin/vim-bracketed-paste'
Bundle 'altercation/vim-colors-solarized'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-eunuch'
Bundle 'pangloss/vim-javascript'
Bundle 'tpope/vim-pathogen'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-sensible'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'benmills/vimux'
Bundle 'easymotion/vim-easymotion'
Bundle 'davidhalter/jedi-vim'
Bundle 'saltstack/salt-vim'
" To install YouCompleteMe on mac:
" brew install vim
" cd ~/.vim/bundle/YouCompleteMe
" /usr/bin/python install.py
Bundle 'valloric/YouCompleteMe'
Plugin 'avakhov/vim-yaml'
Plugin 'w0rp/ale'
Plugin 'wikitopian/hardmode'

" Hardmode always
autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardMode()

" Ale options
let g:ale_sign_column_always = 1

" Google code formatting plugin
" Add maktaba and codefmt to the runtimepath.
" (The latter must be installed before it can be used.)
Plugin 'google/vim-maktaba'
Plugin 'google/vim-codefmt'
" Also add Glaive, which is used to configure codefmt's maktaba flags. See
" `:help :Glaive` for usage.
Plugin 'google/vim-glaive'

" All of your Plugins must be added before the following line
call vundle#end()            " required

" Google code formatting plugin
" the glaive#Install() should go after the "call vundle#end()"
call glaive#Install()
" Optional: Enable codefmt's default mappings on the <Leader>= prefix.
Glaive codefmt plugin[mappings]

augroup autoformat_settings
  autocmd FileType bzl AutoFormatBuffer buildifier
  autocmd FileType c,cpp,proto,stan,javascript AutoFormatBuffer clang-format
  autocmd FileType dart AutoFormatBuffer dartfmt
  autocmd FileType go AutoFormatBuffer gofmt
  autocmd FileType gn AutoFormatBuffer gn
  autocmd FileType html,css,json AutoFormatBuffer js-beautify
  autocmd FileType java AutoFormatBuffer google-java-format
  " autocmd FileType python AutoFormatBuffer yapf
  autocmd FileType python AutoFormatBuffer autopep8
augroup END

filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on

autocmd FileType python setlocal expandtab shiftwidth=2 softtabstop=2
autocmd BufRead wscript set filetype=python
autocmd FileType stan setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType sh setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType zshrc setlocal expandtab shiftwidth=1 softtabstop=2
autocmd FileType c setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType js setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType cpp setlocal expandtab shiftwidth=2 softtabstop=2
" autocmd FileType yml setlocal expandtab shiftwidth=2 softtabstop=2
autocmd BufRead,BufNewFile *.stan set syntax=cpp

" copy to mac system clipboard
set clipboard=unnamed


" for mypy static analysis of python files
let g:syntastic_python_checkers = ['mypy']

" for ctrl p. need to run `:helptags ~/.vim/bundle/ctrlp.vim/doc` in vim
set runtimepath^=~/.vim/bundle/ctrlp.vim
" :inoremap # X<BS>#

" ignore case in search
set ic

" line numbers
set number

" save with one key
map <F3> :w<CR>

" toggle paste mode
set pastetoggle=<F2>

" highlight current line
set cursorline

" remove status line
set nosmd

" 80 char ruler
set colorcolumn=81
" set highlight colorcolumn ctermbg=7


" strip trailing whitespace no save with vim better whitespace plugin
autocmd BufWritePre * StripWhitespace

" Python-mode options
" remove autocomplete python-mode help bar
set completeopt=menu
" Disable pylint checking every save
let g:pymode_lint_write = 0

" for vimux
let g:VimuxOrientation = "h"
let g:VimuxHeight = "60"
" Run the last comand
map <F5> :call VimuxRunLastCommand()<CR>

" set space to leader
nnoremap <SPACE> <Nop>
" let mapleader = " "
let mapleader = "\<Space>"

nnoremap <leader><shift> <Nop>

" use easymotion defaults
let g:EasyMotion_do_mapping = 1
" map <Leader> <Plug>(easymotion-prefix)

" Go to tab by number
noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt
noremap <leader>0 :tablast<cr>

" Go to last active tab
au TabLeave * let g:lasttab = tabpagenr()
nnoremap <silent> <c-l> :exe "tabn ".g:lasttab<cr>
vnoremap <silent> <c-l> :exe "tabn ".g:lasttab<cr>
