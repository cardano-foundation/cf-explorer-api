# Explorer API

<p align="left">
<img alt="Tests" src="https://github.com/cardano-foundation/cf-explorer-api/actions/workflows/tests.yaml/badge.svg?branch=main" />
<img alt="Coverage" src="https://cardano-foundation.github.io/cf-explorer-api/badges/jacoco.svg" />
<img alt="Release" src="https://github.com/cardano-foundation/cf-explorer-api/actions/workflows/release.yaml/badge.svg?branch=main" />
<img alt="Publish" src="https://github.com/cardano-foundation/cf-explorer-api/actions/workflows/publish.yaml/badge.svg?branch=main" />
<a href="https://app.fossa.com/reports/ea0c65ee-d23b-4ad6-8c5f-1777d0b38339"><img alt="FOSSA Status" src="https://app.fossa.com/api/projects/custom%2B41588%2Fgit%40github.com%3Acardano-foundation%2Fcf-explorer-api.git.svg?type=small"/></a>
<a href="https://conventionalcommits.org"><img alt="conventionalcommits" src="https://img.shields.io/badge/Conventional%20Commits-1.0.0-%23FE5196?logo=conventionalcommits" /></a>
</p>

This service provides access to blockchain data that is compatible with the PostgreSQL database of DBSync. It serves as a data source for the explorer frontend, enabling users and developers to gain valuable insights into the Cardano blockchain.

👉 Check the [Explorer repository](https://github.com/cardano-foundation/cf-explorer) to understand how the microservices work together

## 🧪 Test Reports

To ensure the stability and reliability of this project, unit and mutation tests have been implemented. By clicking on the links below, you can access the detailed test reports and review the outcomes of the tests performed.

📊 [Postman Report](https://cardano-foundation.github.io/cf-explorer-api/html-report/reporthtml.html)

📊 [Postman Report (allure format)](https://cardano-foundation.github.io/cf-explorer-api/allure-report/)

📊 [LoadTest Report](https://cardano-foundation.github.io/cf-explorer-api/loadtest-report/k6_results.csv)

📊 [Mutation Report](https://cardano-foundation.github.io/cf-explorer-api/mutation-report/)

## 🚀 Getting Started

### Prerequisites

- Docker && Docker Compose

### Installing

- Clone this repository
- Copy `./.m2/settings.default.xml` to `./.m2/settings.xml`
- Fill `{username_github}` and `{token_github}` in `./.m2/settings.xml` with your github username and token. Guide to generate a token with `read:packages` scope [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token#creating-a-personal-access-token-classic)
- Copy `.env.example`  to `.env`
- Fill the `.env` file with your values (explain below)
- Run `docker-compose -f docker-compose-local.yml up -d` to start the containers


## 🌱 Environment Variables

- `SPRING_PROFILES_ACTIVE`: Spring profiles are `dev`, `prod`, `test` and `local`. There are more profiles managing the rewards and redis configurations (scroll down for . Default is `dev`. The `local` profile should match the most use-cases.

- `PORT`: Port of application. Default is 8080.
- `NETWORK`: Network of blockchain. Default is `mainnet`.

- `DB_MAXIUM_POOL_SIZE`: Maximum pool size of database connection pool. Default is 32.
- `LEDGER_SYNC_HOST`: Ledger-sync database host.
- `LEDGER_SYNC_PORT`: Ledger-sync database port
- `LEDGER_SYNC_USER`: Ledger-sync database username
- `LEDGER_SYNC_PASSWORD`: Ledger-sync database password
- `LEDGER_SYNC_DB`: Ledger-sync database name
- `LEDGER_SYNC_SCHEMA`: Ledger-sync database schema

- `EXPLORER_HOST`: Explorer database host.
- `EXPLORER_PORT`: Explorer database port
- `EXPLORER_USER`: Explorer database username
- `EXPLORER_PASSWORD`: Explorer database password
- `EXPLORER_DB`: Explorer database name
- `EXPLORER_SCHEMA`: Explorer database schema

- `LEDGER_SYNC_AGG_HOST`: Ledger-sync aggregate database host.
- `LEDGER_SYNC_AGG_PORT`: Ledger-sync aggregate database port
- `LEDGER_SYNC_AGG_USER`: Ledger-sync aggregate database username
- `LEDGER_SYNC_AGG_PASSWORD`: Ledger-sync aggregate database password
- `LEDGER_SYNC_AGG_DB`: Ledger-sync aggregate database name
- `LEDGER_SYNC_AGG_SCHEMA`: Ledger-sync aggregate database schema

- `JACKSON_INCLUSION`: Jackson inclusion. Default is NON_NULL for ignore null value. Using USE_DEFAULTS for not ignore null value.

- `KAFKA_BOOTSTRAP_SERVER`: Kafka bootstrap server. Default is kafka:9092.
- `KAFKA_REPORTS_TOPIC`: Kafka reports topic. Default is dev.explorer.api.mainnet.reports

- `S3_ACCESS_KEY`: the AWS access key
- `S3_SECRET_KEY`: the AWS secret key
- `S3_REGION`: the AWS region
- `S3_BUCKET_NAME`: the AWS bucket
- `S3_STORAGE_ENDPOINT`: the storage endpoint, only for S3 clone (either on localhost, Minio, etc.)
- `PATH_STYLE_ENABLED`:

- `LOG_PATH`: the path to store log files

- `PRIVATE_MVN_REGISTRY_URL`: the url of private maven registry
- `PRIVATE_MVN_REGISTRY_USER`: the username of private maven registry
- `PRIVATE_MVN_REGISTRY_PASS`: the password of private maven registry
- `SETTINGS_XML_TPL`: the template of settings.xml file

- `API_NEWS_URL`: URL for get news data.
- `API_NEWS_CACHE_TIME`: Cache time for news data. Default is 120s.
- `API_MICAR_OVERVIEW`: URL for get carbon emission overview.
- `API_MICAR_HISTORICAL`: URL for get carbon emission historical.
- `API_MICAR_KEY`: Key for calling MiCAR overview and historical url.
- `API_BOLNISI_OFFCHAIN_URL`: URL for get bolnisi offchain data by cid.
- `API_BOLNISI_PUBLIC_KEY_PRIMARY_URL`: Primary URL for get bolnisi public key data by wineryId.
- `API_BOLNISI_PUBLIC_KEY_FALLBACK_URL`: Fallback URL for get bolnisi public key data by wineryId.
- `API_BOLNISI_PUBLIC_KEY_CONFORMITY_CERT_URL`: NWA URL for get bolnisi public key data for conformity cert.

- `EPOCH_DAYS` : Number of days in an epoch. Default is 5.
- `REPORT_LIMIT_PER_24HOURS`: Limit of reports per `24` hours for each user. Default is `2`.
- `BLOCK_TIME_THRESHOLD_IN_SECOND` : The maximum allowable time difference, in seconds, between the timestamp of the latest block and the current time (for sync status)
- `INSERTED_TIME_THRESHOLD_IN_SECOND`: The maximum allowable time difference, in seconds, between the timestamp of the latest inserted block and the current time (for sync status)
- `AUTH_FILE_PATH`: path file of authenticate configuration

### Options for the token logo storage with profiles:
- For profile: `prod`
    - `TOKEN_LOGO_S3_REGION`: token logo aws s3 region
    - `TOKEN_LOGO_S3_BUCKET_NAME`: token logo aws s3 bucket name
- For profile: `dev`
    - `TOKEN_LOGO_ENDPOINT`: token logo endpoint

### Options to configure the redis cache:
- `redis standalone`
    - `REDIS_STANDALONE_HOST` : Redis hostname eg. `127.0.0.1`.
    - `REDIS_STANDALONE_PORT` : Redis ort, eg. `6379`.
    - `REDIS_STANDALONE_PASSWORD` : Redis password. Default bitnami.
    -
- `redis sentinel`
    - `REDIS_SENTINEL_MASTER_NAME` : Redis master name. Default is mymaster.
    - `REDIS_SENTINEL_PASSWORD` : Redis sentinel password. Default is redis_sentinel_pass.
    - `REDIS_SENTINEL_HOST` : Redis sentinel host. Default is  cardano.redis.sentinel.
    - `REDIS_SENTINEL_PORT` : Redis sentinel port. Default is 26379.

- `redis-cluster`
    -  `NODE_ADDRESSES`: List of redis cluster nodes host and port.
    -  `REDIS_CLUSTER_PASSWORD`: Password of redis cluster.

- `REDIS_PUBSUB_TOPIC`: Redis pubsub topic.

### There are two options to get the reward data, epoch_stake, ada_pot, pool_info and pool_history:
- `koios`: The koios service will be used to collect the data
    - `API_CHECK_REWARD_URL`: URL for get reward data from koios service. Default is `http://localhost:8888/api/v1/rewards/fetch`.
    - `API_CHECK_POOL_HISTORY_URL`: URL for get pool history data from koios service. Default is `http://localhost:8888/api/v1/pool-history/fetch`.
    - `API_CHECK_POOL_INFO_URL`: URL for get pool info data from koios service. Default is `http://localhost:8888/api/v1/pool-info/fetch`.
    - `API_CHECK_EPOCH_STAKE_URL`: URL for get epoch stake data from koios service. Default is `http://localhost:8888/api/v1/epoch-stake/fetch`.
    - `API_CHECK_ADA_POTS_URL`: URL for get ada pots data from koios service. Default is `http://localhost:8888/api/v1/ada-pots/fetch`.
    - `API_CHECK_EPOCH_URL`: URL for get epoch data from koios service. Default is `http://localhost:8888/api/v1/epochs/fetch`.
- without `koios`: A pre-filled database will be used to serve the data

### Configuration of the genesis file path:
  - `SHELLEY_GENESIS_FILE`: Shelley file path
  - `BYRON_GENESIS_FILE`: Byron file path
  - `CONWAY_GENESIS_FILE`: Conway file path

## 🍀 Local Environments Tests

### Execute postman collection using docker - newman

```shell
docker run --rm -v "./:/tmp" -t postman/newman run /tmp/src/test/Postman/Cardano-Explorer-API.postman_collection.json -e /tmp/src/test/Postman/DevInt.postman_environment.json -r cli,htmlextra,allure --reporter-htmlextra-export=/tmp/reporthtml/reporthtml.html --timeout-request 1500
```

### Convert postman collection into K6 config file

```shell
docker run -it --rm -v "./:/tmp" -t loadimpact/postman-to-k6  /tmp/src/test/Postman/Cardano-Explorer-API.postman_collection.json --environment /tmp/src/test/Postman/DevInt.postman_environment.json  -o /tmp/k6-script.js --skip-post -i 10;
```
### Execute K6 Loadtest using postman collection

```shell
docker run -it  --rm -v "./:/tmp"  grafana/k6 run --vus 10 --out csv=/tmp/report/k6_results.csv /tmp/k6-script.js
```
