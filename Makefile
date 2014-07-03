log:
	#minicom -D /dev/tty.usbmodem1411 -b 19200 -C poll_`date '+%Y-%m-%d-%H%M%S'`.csv
	minicom -D /dev/tty.usbmodem1d1141 -b 19200 -C poll_`date '+%Y-%m-%d-%H%M%S'`.csv
	#minicom -D /dev/tty.usbmodem1d1111 -b 19200 -C poll_`date '+%Y-%m-%d-%H%M%S'`.csv
