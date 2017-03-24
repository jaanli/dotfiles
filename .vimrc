syntax on
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
" call pathogen#infect('bundle/{}')

filetype plugin indent on
autocmd FileType python setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType sh setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType c setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType js setlocal expandtab shiftwidth=2 softtabstop=2
autocmd FileType cpp setlocal expandtab shiftwidth=2 softtabstop=2

set nocompatible

" for ctrl p. need to run `:helptags ~/.vim/bundle/ctrlp.vim/doc` in vim
set runtimepath^=~/.vim/bundle/ctrlp.vim
" :inoremap # X<BS>#

" ignore case in search
set ic

" save with one key
map <F3> :w<CR>

" toggle paste mode
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
" Run the last comand
map <F5> :call VimuxRunLastCommand()<CR>

let g:neocomplete#enable_auto_select = 1

" set space to leader
nnoremap <SPACE> <Nop>
" let mapleader = " "
let mapleader = "\<Space>"

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


" syntastic for pylint
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_python_checkers = ['pylint']

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_mode_map = { 'mode': 'passive' }
