# The magic script that gets called by all the others to filter and forward the key combo
import re

h = store.get_global_value('hotkey')
s = store.get_global_value('newkey')

if re.match('.*(Emacs|gnome-terminal|konsole)', window.get_active_class()):
    keyboard.send_keys(h)
    store.set_global_value('ignored', True)
else:
    keyboard.send_keys(s)
    store.set_global_value('ignored', False)
