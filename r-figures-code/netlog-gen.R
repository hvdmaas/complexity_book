green <- function(text){
  paste0('<span class="green">',text,'</span>')
}
purple <- function(text){
  paste0('<span class="purple">',text,'</span>')
}
blue <- function(text){
  paste0('<span class="blue">',text,'</span>')
}
comm <- function(text){
  paste0('<span class="comm">',text,'</span>')
}
text <- function(text){
  paste0('<span class="text">',text,'</span>')
}
val <- function(text){
  paste0('<span class="val">',text,'</span>')
}
brk <- function(){
  paste0('<br>')
}
int <- function(){
  paste0('&nbsp;&nbsp;&nbsp;&nbsp;')
}

cat(
  green('to'), text('setup'), brk(),
  int(), purple('clear-all'), brk(),
  int(), purple('reset-clicks'), brk(),
  green('end')
)
cat(
  green('to'), text('go'), brk(),
  int(), purple('tick'), brk(),
  green('end')
)

#patches-own [new-state]
cat(
  green('patches-own'), text('[new-state]')
)

#ask patches
#[set pcolor one-of [white blue]] ; white is dead, blue is alive
cat(
  blue('ask'), purple('patches'), brk(),
  text('['),blue('set'), purple('pcolor one-of'),text('['),val('white blue'),
  text(']]'), comm('; white is dead, blue is alive')
)

#ask patches[
#  if(count(neighbors with [pcolor = blue]) > 3) [set new-state white]
#  if(count(neighbors with [pcolor = blue]) < 2) [set new-state white]
#  if(count(neighbors with [pcolor = blue]) = 3) [set new-state blue]
#]

cat(
  blue('ask'), purple('patches'), text(']'), brk(),
  int(), blue('if'), text('('), purple('neighbors with'), text('['), purple('pcolor ='), val('blue'), text('])'), purple('>'), val('3'), text(')'), text('['), blue('set'), text('new-state'), val('white'), text(']'),brk(),
  int(), blue('if'), text('('), purple('neighbors with'), text('['), purple('pcolor ='), val('blue'), text('])'), purple('<'), val('2'), text(')'), text('['), blue('set'), text('new-state'), val('white'), text(']'),brk(),
  int(), blue('if'), text('('), purple('neighbors with'), text('['), purple('pcolor ='), val('blue'), text('])'), purple('='), val('3'), text(')'), text('['), blue('set'), text('new-state'), val('blue'), text(']'),brk(),
  text(']')
)

#
#ask patches [set pcolor new-state]
cat(
  blue('ask'), purple('patches'),text('['),blue('set'),purple('pcolor'), text('new-state'),text(']')
)

# let Ediff 2 * spin * sum [ spin ] of neighbors4  
cat(
  blue('let'), text('Ediff'), val('2'), purple('*'), text('spin'), purple('* sum'), text('[ spin ]'), purple('of neighbors4')
)

#let 0.001 * Ediff 2 * spin * sum [ spin ] of patches
cat(
  blue('let'), val('0.001'), purple('*'), text('Ediff'), val('2'), purple('*'), text('spin'), purple('* sum'), text('[ spin ]'), purple('of patches')
)
