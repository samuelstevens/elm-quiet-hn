dev:
	serve debug &
	find src | entr ./dev.sh
