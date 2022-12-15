let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/stuff/development/wallhaven-downloader
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +7 app/Main.hs
badd +406 term://~/stuff/development/wallhaven-downloader//93488:/bin/zsh
badd +25 wallhaven-downloader.cabal
badd +61 app/Wallhaven/Favorites.hs
badd +10 app/Retry.hs
badd +1 app/Util/Time.hs
badd +3 app/Util/List.hs
badd +13 app/Util/Batch.hs
badd +7 app/Util/HTTP.hs
argglobal
%argdel
edit app/Wallhaven/Favorites.hs
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 78 + 79) / 158)
exe 'vert 2resize ' . ((&columns * 79 + 79) / 158)
argglobal
balt app/Main.hs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
silent! normal! zE
let &fdl = &fdl
let s:l = 40 - ((16 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 40
normal! 04|
wincmd w
argglobal
if bufexists(fnamemodify("term://~/stuff/development/wallhaven-downloader//93488:/bin/zsh", ":p")) | buffer term://~/stuff/development/wallhaven-downloader//93488:/bin/zsh | else | edit term://~/stuff/development/wallhaven-downloader//93488:/bin/zsh | endif
if &buftype ==# 'terminal'
  silent file term://~/stuff/development/wallhaven-downloader//93488:/bin/zsh
endif
balt app/Wallhaven/Favorites.hs
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 406 - ((40 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 406
normal! 0
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 78 + 79) / 158)
exe 'vert 2resize ' . ((&columns * 79 + 79) / 158)
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
nohlsearch
let g:this_session = v:this_session
let g:this_obsession = v:this_session
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
