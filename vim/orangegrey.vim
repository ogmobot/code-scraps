set background=dark
highlight clear
if exists("syntax_on")
    syntax reset
endif

let color_name = "orangegrey"

" 67 is steel blue
"202 is orange red

hi Normal ctermfg=LightGray ctermbg=Black guifg=#dddddd guibg=Black

hi Comment cterm=NONE ctermfg=DarkGray gui=NONE
hi Constant cterm=NONE ctermfg=67 gui=NONE
hi Identifier cterm=NONE ctermfg=202 gui=NONE
hi Statement cterm=NONE ctermfg=White gui=NONE

hi PreProc cterm=NONE ctermfg=202 gui=NONE
hi Type cterm=NONE ctermfg=White gui=NONE
hi Special cterm=NONE ctermfg=LightRed gui=NONE
hi Delimiter cterm=None ctermfg=White gui=NONE

