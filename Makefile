build-container:
	bash scripts/create-container.sh

arm-executable:
	bash scripts/compile-in-container.sh

ghcid:
	ghcid src/*.hs src/hue/*.hs src/house/*.hs

update:
	bash scripts/build-and-upload.sh
