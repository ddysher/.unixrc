# set the actions
hotkey = '<ctrl>+b'
newkey = '<left>'

# save the hotkey for passthrough
store.set_global_value('hotkey', hotkey)
store.set_global_value('newkey', newkey)

# see if we're going to add shift
sh_on = store.get_global_value('shift_on')
if sh_on:
    # add shift
    sh_mod = '<shift>+'
else:
    sh_mod = ''
engine.set_return_value(sh_mod + newkey)

# process the key
engine.run_script('process_key')
