.PHONY: start

start:
	iex -S mix phoenix.server

start_prod:
	rel/house/bin/house start

restart_prod:
	rel/house/bin/house restart

release:
	MIX_ENV=prod mix phoenix.digest
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix release
