log:
	#minicom -D /dev/tty.usbmodem1411 -b 19200 -C poll_`date '+%Y-%m-%d-%H%M%S'`.csv

log1111:
	minicom -D /dev/tty.usbmodem1d1111 -b 19200 -C poll_`date '+%Y-%m-%d-%H%M%S'`.csv

log1141:
	minicom -D /dev/tty.usbmodem1d1141 -b 19200 -C poll_`date '+%Y-%m-%d-%H%M%S'`.csv

devs:
	ls /dev/tty.*
