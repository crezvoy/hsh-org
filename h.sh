#! /bin/sh

# this script is called after each git command, after 
# the 'bundle' and 'remove' command which have no git equivalent, and after the
# extraction of a bundle.
# The following environment variables are set:
#     - HSH_ROOT: current hsh root;
#     - HSH_REPOSITORY: current repository name;
#     - HSH_ACTION: current git or hsh action, for "bundle" command, HSH_ACTION
#       is set to 'bundle-in', when extracting a bundle the action is set to
#       'bundle-out'.
# Additionally, for bundle-in and bundle-out actions the HSH_BUNDLE_ROOT is set
# to the bundle content root.

case "$HSH_ACTION" in
    clone|bundle-out)
	if command -v gsettings >/dev/null
	then
	    a="$(gsettings get org.gnome.settings-daemon.plugins.media-keys custom-keybindings | sed 's/@as //g' )"
	    path="/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/capture-shortcut/"

	    if ! echo $a | grep -q "capture-shortcut"; then
		comma=", "
		if echo "$a" | grep -q "\[\]"; then
		    comma=""
		fi
		new_a="$(echo $a | sed 's/]$//g' )"
		new_a="$new_a$comma"
	    fi
	    new_a="$new_a '$path' ]"

	    gsettings set \
		      org.gnome.settings-daemon.plugins.media-keys \
		      custom-keybindings \
		      "$new_a"

	    gsettings set \
		      "org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:$path" \
		      name \
		      "capture-shortcut"

	    gsettings set \
		      "org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:$path" \
		      command \
		      "emacsclient -e \"(my/setup-capture-frame)\""

	    gsettings set \
		      "org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:$path" \
		      binding \
		      "<Super>x"
	fi
	;;
esac

