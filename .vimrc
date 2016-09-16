" Welcome to Vim (http://go/vim).
"
" If you see this file, your homedir was just created on this workstation.
" That means either you are new to Google (in that case, welcome!) or you
" got yourself a faster machine.
" Either way, the main goal of this configuration is to help you be more
" productive; if you have ideas, praise or complaints, direct them to
" vi-users@google.com (http://g/vi-users). We'd especially like to hear from you
" if you can think of ways to make this configuration better for the next
" Noogler.
" If you want to learn more about Vim at Google, see http://go/vimintro.

" Enable modern Vim features not compatible with Vi spec.
execute pathogen#infect()
set nocompatible
set smartcase

" Use the 'google' package by default (see http://go/vim/packages).
source /usr/share/vim/google/google.vim

" Plugin configuration.
" See http://google3/devtools/editors/vim/examples/ for example configurations
" and http://go/vim/plugins for more information about vim plugins at Google.

" Plugin loading is commented out below - uncomment the plugins you'd like to
" load.

" Load google's formatting plugins (http://go/vim/plugins/codefmt-google).
" The default mapping is \= (or <leader>= if g:mapleader has a custom value),
" with
" - \== formatting the current line or selected lines in visual mode
"   (:FormatLines).
" - \=b formatting the full buffer (:FormatCode).
"
" To bind :FormatLines to the same key in insert and normal mode, add:
noremap <C-K> :FormatLines<CR>
inoremap <C-K> <C-O>:FormatLines<CR>
Glug codefmt plugin[mappings] gofmt_executable="goimports"
Glug codefmt-google

" Enable autoformatting on save for the languages at Google that enforce
" formatting, and for which all checked-in code is already conforming (thus,
" autoformatting will never change unrelated lines in a file).
augroup autoformat_settings
" For BUILD files and Go all of Google's files must be formatted.
autocmd FileType bzl AutoFormatBuffer buildifier
autocmd FileType go AutoFormatBuffer gofmt
augroup END

" Load YCM (http://go/ycm) for semantic auto-completion and quick syntax
" error checking. Pulls in a google3-enabled version of YCM itself and
" a google3-specific default configuration.
Glug youcompleteme-google

" Load the automated blaze dependency integreation for Go.
" Note: for Go, blazedeps uses the Go team's glaze tool, which is fully
" supported by the Go team; for other languages. Note that the plugin is
" currently unsupported for other languages.
"Glug blazedeps auto_filetypes=`['go']`

" Load piper integration (http://wiki/Main/VimPerforce).
Glug piper plugin[mappings]

" Load the Critique integration. Use :h critique for more details.
Glug critique plugin[mappings]

" Load blaze integration (http://go/blazevim).
Glug blaze plugin[mappings]

" Load the syntastic plugin (http://go/vim/plugins/syntastic-google).
" Note: this requires installing the upstream syntastic plugin from
" https://github.com/scrooloose/syntastic.
Glug syntastic-google

" Load the ultisnips plugin (http://go/ultisnips).
" Note: this requires installing the upstream ultisnips plugin from
" https://github.com/SirVer/ultisnips.
Glug ultisnips-google

" Solarized
syntax enable
set background=dark
" solarized options
let g:solarized_visibility = "high"
let g:solarized_contrast = "high"
colorscheme solarized
" All of your plugins must be added before the following line.

" Enable file type based indent configuration and syntax highlighting.
" Note that when code is pasted via the terminal, vim by default does not detect
" that the code is pasted (as opposed to when using vim's paste mappings), which
" leads to incorrect indentation when indent mode is on.
" To work around this, use ":set paste" / ":set nopaste" to toggle paste mode.
" You can also use a plugin to:
" - enter insert mode with paste (https://github.com/tpope/vim-unimpaired)
" - auto-detect pasting (https://github.com/ConradIrwin/vim-bracketed-paste)

filetype plugin indent on
syntax on
set colorcolumn=80
set ic
autocmd BufWritePre * StripWhitespace

" paste formatting
set pastetoggle=<F2>
set runtimepath^=~/.vim/bundle/ctrlp.vim

" save with one key
map <F3> :w<CR>
