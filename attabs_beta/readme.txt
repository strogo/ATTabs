linux: onmouseup - onmousemove don't happen if mouse out of control.
this causes impossible to drop tabs to other attabs. even cursor don't change to "drop allow".
test build: make timer which checks IsMousePressed and does drop-
internal drop (inside one attabs) done on onmouseup;
external drop (to other attabs) done in ontimer.

maybe race exists? do check on Win. do check on D7.


