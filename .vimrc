let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
endif

set number
set cursorline
colorscheme molokai
:set viminfo^=%

filetype plugin indent on
syntax on

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

" Install ansible plugin
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'dense-analysis/ale'
Plug 'rust-lang/rust.vim'

    " assuming you're using vim-plug: https://github.com/junegunn/vim-plug
    Plug 'ncm2/ncm2'
    Plug 'roxma/nvim-yarp'

    " enable ncm2 for all buffers
    autocmd BufEnter * call ncm2#enable_for_buffer()

    " IMPORTANT: :help Ncm2PopupOpen for more information
    set completeopt=noinsert,menuone,noselect

    " NOTE: you need to install completion sources to get completions. Check
    " our wiki page for a list of sources: https://github.com/ncm2/ncm2/wiki
    Plug 'ncm2/ncm2-bufword'
    Plug 'ncm2/ncm2-path'

Plug 'neoclide/coc.nvim', {'branch': 'release'}
" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
call plug#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
command! -bang -nargs=* GGrep
  \ call fzf#vim#grep(
  \   'git grep --line-number -- '.shellescape(<q-args>), 0,
  \   fzf#vim#with_preview({'dir': systemlist('git rev-parse --show-toplevel')[0]}), <bang>0)
let g:rustfmt_autosave = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Rust Setup
autocmd BufNewFile,BufRead *.rs set filetype=rust

let g:ale_linters = {
\  'rust': ['analyzer'],
\}

let g:ale_fixers = { 'rust': ['rustfmt', 'trim_whitespace', 'remove_trailing_lines'] }

" Optional, configure as-you-type completions
set completeopt=menu,menuone,preview,noselect,noinsert
" let g:ale_completion_enabled = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
inoremap <c-x><c-s> <c-\><c-o>:w<cr>
nnoremap <C-LeftMouse> :ALEGoToDefinition<CR>

let mapleader="\\"
noremap <Leader>s :update<CR>
noremap <Leader>f :Files<CR>
noremap <Leader>b :Buffers<CR>
noremap <Leader>\ :bprevious<CR>
noremap <Leader>gg :GGrep<CR>

" esc to quit terminal
:tnoremap <Esc> <C-\><C-n>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
