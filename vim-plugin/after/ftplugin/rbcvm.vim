" Vim filetype plugin file
" Language:     rbcvm
" Maintainer:   Patrick Gingras
" URL:          https://github.com/p7g/rust-bytecode-vm

setlocal suffixesadd+=.rbcvm

if exists('b:undo_ftplugin')
  let b:undo_ftplugin .= ' | setlocal suffixesadd<'
else
  let b:undo_ftplugin = 'setlocal suffixesadd<'
endif
