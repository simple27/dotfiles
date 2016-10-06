set nocompatible
cmap W! w !sudo tee % >/dev/null
let g:pep8_map='<leader>8'
cmap w!! w !sudo tee % >/dev/null
imap <C-W> <C-O><C-W>
filetype off

syntax on
filetype on
filetype plugin indent on
set scrolloff=999
set number
set numberwidth=1
set background=dark
set title
set wildmenu
set wildmode=full
set noerrorbells
set vb t_vb=
set wildignore+=*.o,*.obj,.git,*.pyc
set grepprg=ack

set completeopt=menuone,longest,preview
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

nnoremap <leader>q :q<CR>
nnoremap <leader><space> :nohlsearch<cr>
nnoremap <leader>S :%s/\s\+$//<cr>:let @/=''<CR>

au BufRead *.js set makeprg=jslint\ %

let g:acp_completeoptPreview=1
