" use , as <leader> instead of default \, on danish keyboard you need to press altgr+< for \, and ',' is used for something obscure (with reverse t which is the same as f which is a going forward by counts of a single character under cursor), not very usefull.
let mapleader =","
"if vimplugged is not installed, install it to ~/.config/nvim/autoload/
if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
	
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
" Use release branch (recommend)
"Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'vimwiki/vimwiki'
Plug 'junegunn/goyo.vim'
Plug 'morhetz/gruvbox'
" syntax highlighting for a lot of languages
Plug 'sheerun/vim-polyglot'
" rainbow delimiters
Plug 'luochen1990/rainbow'
" fugitive, git client like magit
Plug 'tpope/vim-fugitive'
" script to toggle comments like "C-x C-;" in emacs
Plug 'preservim/nerdcommenter'
call plug#end()

"'tpope/vim-fugitive', { 'on': [] }
let g:rainbow_active = 1 "set to 0 if you want to enable it later via :RainbowToggle
" coc plugins

" remember to install node.js(nodejs) and configure coc language server config for command autocompletion to work
	" coc-python
	" coc-pretty
	" coc-pairs # make two container characters(),"" when pressing once
	" coc-snippets
"Plug 'tpope/vim-fugitive', { 'on': [] }
" fugitive lazy load
" lazy loading vim-fugitive
"command! Gstatus call LazyLoadFugitive('Gstatus')
"command! Gdiff call LazyLoadFugitive('Gdiff')
"command! Glog call LazyLoadFugitive('Glog')
""command! Gblame call LazyLoadFugitive('Gblame')

"function! LazyLoadFugitive(cmd)
"  call plug#load('vim-fugitive')
"  call fugitive#detect(expand('%:p'))
"  exe a:cmd
"endfunction
set title
set bg=light
set go=a
set hlsearch
set showmode
set ruler
set laststatus=0
set noshowcmd
" Some basics:
	set nocompatible
	filetype plugin on
	syntax on
	set encoding=utf-8
	set smartindent
" number for lines
	set number
	set relativenumber 
" line wrapping(orphans) to 72 characters, i think or around 90 ?
"au BufReadPre *.txt set tw=72
" show which lines are wrapped, prefixes ··
"show the pattern ++++ on wrapped lines
set showbreak=++++
"do not search for pattern when i am writing it
" set noincsearch
" Ignore case, but if pattern is with upper chars, search case sensitive 
set ignorecase smartcase

" indicate indents.
	set list
	" shows whitespace in convient ways,'set list' is needed for this to work.
set listchars=eol:$,nbsp:_,tab:>-,trail:~,extends:>,precedes:<
" indents are 4 whitespaces
set ts=8
set shiftwidth=8
" Enable autocompletion for commands:
	set wildmode=longest,list,full
" Use 256bit color(used for gruvbox theme), remove if you terminal does not support 256bit color, st and uxvt do  support 256bit color.
set termguicolors
" load gruvbox colorscheme file
autocmd vimenter * ++nested colorscheme gruvbox
" Enable italics for gruvbox, some terminals do not support this, st (suckless terminal)does so
let g:gruvbox_italic=1
" dark gruvbox theme
" bind <leader>l to light background, d
	nnoremap <leader>l :set background=light<CR>
	nnoremap <leader>d :set background=dark<CR>
" more gruvbox, search highlighlight toggle or not
"nnoremap <silent> [oh :call gruvbox#hls_show()<CR>
"nnoremap <silent> ]oh :call gruvbox#hls_hide()<CR>
"nnoremap <silent> coh :call gruvbox#hls_toggle()<CR>

nnoremap * :let @/ = ""<CR>:call gruvbox#hls_show()<CR>*
nnoremap / :let @/ = ""<CR>:call gruvbox#hls_show()<CR>/
nnoremap ? :let @/ = ""<CR>:call gruvbox#hls_show()<CR>?


" Disables automatic commenting on newline:
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" Perform dot commands over visual blocks:
	vnoremap . :normal .<CR>
" Goyo plugin makes text more readable when writing prose:
	map <leader>f :Goyo \| set bg=light \| set linebreak<CR>
" Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
	set splitbelow splitright
" not used: vimling plugin by luke smith, it's used for accented characters on us keyboard, not usefull when danish keyboard has this built in,´`¨
	"nm <leader><leader>d :call ToggleDeadKeys()<CR>
	"imap <leader><leader>d <esc>:call ToggleDeadKeys()<CR>a
	"nm <leader><leader>i :call ToggleIPA()<CR>
	"imap <leader><leader>i <esc>:call ToggleIPA()<CR>a
	"nm <leader><leader>q :call ToggleProse()<CR>

" Shortcutting split navigation, saving a keypress:
	map <C-h> <C-w>h
	map <C-j> <C-w>j
	map <C-k> <C-w>k
	map <C-l> <C-w>l
" Replace ex mode with gq
	"map Q gq
"alt+w for write
noremap <A-w> <Esc>:w!<CR>
"alt+q for :q, not forcing, because it can be misstyped, use ZQ to :q!, and ZZ for :wq
noremap <A-q>  <Esc>:q<CR>

" Use CTRL-S for saving, also in Insert mode
noremap <C-S> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <C-O>:update<CR>

"ex mode er til vim-komandoer der kan bruges i som stream processor, ligesom sed
" Perform dot commands over visual blocks:
	vnoremap . :normal .<CR>
" Spell-check set to <leader>en, 'en' for 'english', 'da for danish':
map <leader>en :setlocal spell! spelllang=en_us<CR>
map <leader>da :setlocal spell! spelllang=da<CR>

" more powerful backspacing
set backspace=indent,eol,start  

"default clipboard to systemclipboard(ctrl+v), not X11 selection
set clipboard+=unnamedplus

" IKKE BRUGT vimling:
	"nm <leader><leader>d :call ToggleDeadKeys()<CR>
	"imap <leader><leader>d <esc>:call ToggleDeadKeys()<CR>a
	"nm <leader><leader>i :call ToggleIPA()<CR>
	"imap <leader><leader>i <esc>:call ToggleIPA()<CR>a
	"nm <leader><leader>q :call ToggleProse()<CR>
" Check file in shellcheck:
	"map <leader>s :!clear && shellcheck -x %<CR>

" fold manually
set foldmethod=syntax

"save folds
augroup remember_folds
  autocmd!
  autocmd BufWinLeave * mkview
  autocmd BufWinEnter * silent! loadview
augroup END

" Open my bibliography file in split
	map <leader>b :vsp<space>$BIB<CR>
	map <leader>r :vsp<space>$REFER<CR>
" Replace all is aliased to S.
	nnoremap S :%s//g<Left><Left>
" Compile document, be it groff/LaTeX/markdown/etc. compiler is luke smiths scipt to determine document type and then compile it,found under in his github dotfiles repo under voidrice 
	map <leader>c :w! \| !compiler.sh "<c-r>%"<CR>
" Open corresponding .pdf/.html or preview
	map <leader>p :!opout <c-r>%<CR><CR>
" Search for whole words binding
nnoremap <leader>/ /\<\><left><left>
" Runs a script that cleans out tex build files whenever I close out of a .tex file, also found on in luke smiths dotfile repo
	autocmd VimLeave *.tex !texclear %
" Ensure files are read as what I want, use vimwiki syntax for these file extensions
" let g:vimwiki_ext2syntax = {'.Rmd': 'markdown', '.rmd': 'markdown','.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
"	map <leader>v :VimwikiIndex
" 	let g:vimwiki_list = [{'path': '~/vimwiki', 'syntax': 'markdown', 'ext': '.md'}]
" calcurse files automaticly get's a mimetype association
	autocmd BufRead,BufNewFile /tmp/calcurse*,~/.calcurse/notes/* set filetype=markdown
	autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
	autocmd BufRead,BufNewFile *.tex set filetype=tex
" Save file as sudo on files that require root permission
command! W execute 'w !sudo tee % > /dev/null' <bar> edit!
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!
" VIRKER IKKE Automatically deletes all trailing whitespace and newlines at end of file on save.
	"autocmd BufWritePre * %s/\s\+$//
	"autocmd BufWritePre * %s/\n\+\%$//
	"autocmd BufWritePre *.[ch] %s/\%$/\r/e
"switch function of ctrl+f and ctrl+b, f for forward(down in file), b for backwards(up in file)
"nnoremap <C-f> <C-b>
"nnoremap <C-b> <C-f>
" 
" custom things i haven't made, 
"ex mode er til vim-komandoer der kan bruges i som stream processor, ligesom sed
" Goyo plugin makes text more readable when writing prose:
" IKKE BRUGT vimling plugin by luke smith:
	"imap <leader><leader>d <esc>:call ToggleDeadKeys()<CR>a
	"imap <leader><leader>i <esc>:call ToggleIPA()<CR>a
"keyremaps for vimling(prose writing")
	"map <leader>s :!clear && shellcheck -x %<CR>
	"nm <leader><leader>d :call ToggleDeadKeys()<CR>
	"nm <leader><leader>i :call ToggleIPA()<CR>
	"nm <leader><leader>q :call ToggleProse()<CR>
	
"swap them as you indent more than you deindent, set command doesn't work with nnoremap only, it has to be a command, why?
" autocmd BufReadPre * ++once noremap < >
" autocmd BufReadPre * ++once noremap > <
noremap < >
noremap > <
"conficts with coc, tab to autocomplete
"tab remapped to >> eg. indentation once
"nnoremap <TAB> >>
"nnoremap <S-TAB> <<
"conlicts with tab to autoclete feature from coc
"vnoremap <TAB> >gv
"vnoremap <S-TAB> <gv

" conficts with ctrl+l for switching to right split, <Ctrl-l> redraws the screen and removes any search highlighting.
"nnoremap <silent> <C-l> :nohl<CR><C-l>


