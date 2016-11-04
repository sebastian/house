.PHONY: start

start:
	bash -c "source .env && iex -S mix phoenix.server"
