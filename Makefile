init:
	git config core.hooksPath .githooks

compose-up:
	docker compose --env-file .env.${network} -p explorer-${network} up -d --build

compose-down:
	docker compose --env-file .env.${network} -p explorer-${network} down

compose-build:
	docker compose --env-file .env.${network} -p explorer-${network} build --no-cache
