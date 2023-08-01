kill_prog() {
	killall -q $1

	# Wait until the processes have been shut down
	while pgrep -u $UID -x $1 >/dev/null; do sleep 1; done
}

kill_prog ckb-next

[[ $(hostname) == 'sol' ]] && ckb-next -b 2>&1 >/dev/null &
