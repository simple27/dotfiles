set nocompatible
let mapleader=

fu! SplitScroll()
    :wincmd v
    :wincmd w
    execute
    :set scrollbind
    :wincmd w
    :set scrollbind
endfu

nmap <leader>sb :call SplitScroll()<CR>
cmap W! w !sudo tee % >/dev/null
map <leader>td <Plug>TaskList
let g:pep8_map='<leader>8'
nmap <silent><Leader>tf <Esc>:Pytest file<CR>
nmap <silent><Leader>tc <Esc>:Pytest class<CR>
nmap <silent><Leader>tm <Esc>:Pytest method<CR>
nmap <silent><Leader>tn <Esc>:Pytest next<CR>
nmap <silent><Leader>tp <Esc>:Pytest previous<CR>
nmap <silent><Leader>te <Esc>:Pytest error<CR>
map <leader>dt :set makeprg=python\ manage.py\ test\|:call MakeGreen()<CR>
map <silent> <leader>V :source ~/.vimrc<CR>:filetype detect<CR>:exe
nmap <leader>c :copen<CR>
nmap <leader>cc :cclose<CR>
cmap w!! w !sudo tee % >/dev/null
imap <C-W> <C-O><C-W>
map <leader>f :CommandT<CR>
map <leader>j :RopeGotoDefinition<CR>
map <leader>r :RopeRename<CR>
filetype off

syntax on
filetype on
filetype plugin indent on
set number
set numberwidth=1
set background=dark
set title
set wildmenu
set wildmode=full
set noerrorbells
set vb t_vb=
set wildignore+=*.o,*.obj,.git,*.pyc
set wildignore+=eggs/**
set wildignore+=*.egg-info/**
set grepprg=ack

nnoremap <leader>. :lcd %:p:h<CR>







set completeopt=menuone,longest,preview
set pumheight=6
set cursorline
set ruler
set nostartofline
set virtualedit=block
set scrolloff=3
set backspace=2
set showmatch
set nowrap
set linebreak
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set shiftround
set matchpairs+=<:>
inoremap # #


autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif


set noautowrite
set noautowriteall
set noautoread
set modeline
set modelines=5
set ffs=unix,dos,mac
set ls=2
set vb t_vb=
set confirm
set showcmd
set report=0
set shortmess+=a
set ruler
set laststatus=2
set listchars=tab:>-,eol:$,trail:-,precedes:<,extends:>
set list
set ignorecase
set smartcase
set smarttab
set hlsearch
set incsearch


if has(
    colorscheme desert
    set guioptions-=m
    set guioptions-=T
else
    colorscheme desert
endif

map <leader>p
nnoremap <leader>q :q<CR>
nnoremap <leader><space> :nohlsearch<cr>
nnoremap <leader>S :%s/\s\+$//<cr>:let @/=''<CR>
inoremap <expr> <CR> pumvisible() ?

au BufRead *.js set makeprg=jslint\ %

let g:acp_completeoptPreview=1

autocmd BufNewFile,BufRead *.mako,*.mak,*.jinja2 setlocal ft=html
autocmd FileType html,xhtml,xml,css setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2


if has('python')
py << EOF
import os.path
import sys
import vim
if 'VIRTUAL_ENV' in os.environ:
    project_base_dir = os.environ['VIRTUAL_ENV']
    sys.path.insert(0, project_base_dir)
    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
    execfile(activate_this, dict(__file__=activate_this))
EOF
endif

if filereadable($VIRTUAL_ENV . '/.vimrc')
    source $VIRTUAL_ENV/.vimrc
endif

