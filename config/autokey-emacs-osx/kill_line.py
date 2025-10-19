# Enter script code
import re

if re.match('.*(Emacs|Gnome-terminal|konsole)', window.get_active_class()):
    keyboard.send_keys('<ctrl>+k')
else:
    # we need to select to the end and del
    keyboard.send_keys('<shift>+<end>')
    keyboard.send_keys('<delete>')

    # we should turn off mark if it's on
    store.set_global_value('shift_on', False)
