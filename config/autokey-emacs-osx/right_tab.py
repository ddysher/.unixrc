# set the actions
hotkey = '<alt>+]'
newkey = '<ctrl>+<page_down>'

# save the hotkey for passthrough
store.set_global_value('hotkey', hotkey)
store.set_global_value('newkey', newkey)

# set the new key
engine.set_return_value(newkey)

# process the key
engine.run_script('process_key')

# we should turn off shift
ignored = store.get_global_value('ignored')
if not ignored:
    store.set_global_value('shift_on', False)
