# CARDANO-EXPLORER-API

<p align="left">
<img alt="Tests" src="https://github.com/cardano-foundation/cf-explorer-api/actions/workflows/tests.yaml/badge.svg" />
<img alt="Release" src="https://github.com/cardano-foundation/cf-explorer-api/actions/workflows/release.yaml/badge.svg?branch=main" />
<img alt="Publish" src="https://github.com/cardano-foundation/cf-explorer-api/actions/workflows/publish.yaml/badge.svg?branch=main" />
</p>

## Getting Started

### Prerequisites

- Docker && Docker Compose

### Installing

- Clone the repository
- Copy `./.m2/settings.default.xml` to `./.m2/settings.xml` 
- Fill `{username_github}` and `{token_github}` in `./.m2/settings.xml` with your github username and token. Guide to generate a token with `read:packages` scope [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token#creating-a-personal-access-token-classic)
- Copy `.env.example`  to `.env`
- Fill the `.env` file with your values (explain below)
- Run `docker-compose -f docker-compose-local.yml up -d` to start the containers


### Environment variables

- `SPRING_PROFILES_ACTIVE` : Spring profiles (dev, prod, test, local), plus Redis Profiles. See Below. Default is dev. In your case, you should use local
- `HOST` : Database host (default is 172.16.1.230). In your case, you should use `db.cardano.sotatek.works`
- `PORT_DB` : Database port (default is my postgres port: 54321)
- `USERNAME_DB`: Database username (in your case, do not fill this field because local profile is using a read-only user)
- `PASSWORD_DB`: Database password (in your case, do not fill this field because local profile is using a read-only user)
- `SCHEMA`: Schema of database
- `DB`: Database name


### We have 3 options for redis cache:
- `redis standalone`
    - `REDIS_STANDALONE_HOST` : Redis hostname eg. `127.0.0.1`.
    - `REDIS_STANDALONE_PORT` : Redis ort, eg. `6379`.
    - `REDIS_STANDALONE_PASSWORD` : Redis password. Default bitnami.
    -
- `redis sentinel`
    - `REDIS_MASTER_NAME` : Redis master name. Default is mymaster.
    - `REDIS_SENTINEL_PASS` : Redis sentinel password. Default is redis_sentinel_pass.
    - `REDIS_SENTINEL_HOST` : Redis sentinel host. Default is  cardano.redis.sentinel.

- `redis-cluster`
    -  `NODE_ADDRESSES`: List of redis cluster nodes host and port.
    -  `REDIS_CLUSTER_PASSWORD`: Password of redis cluster.



