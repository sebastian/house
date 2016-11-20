.PHONY: start

start:
	bash -c "source .env && iex -S mix phoenix.server"

start_prod:
	bash -c "source .env && rel/house/bin/house console"

release:
	MIX_ENV=prod mix phoenix.digest
	MIX_ENV=prod mix compile
	MIX_ENV=prod mix release
