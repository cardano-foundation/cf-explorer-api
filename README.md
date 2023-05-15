# CARDANO-EXPLORER-API

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

- `SPRING_PROFILES_ACTIVE` : Spring profile (dev, prod, test,local). Default is dev. In your case, you should use local
- `HOST_DB` : Database host (default is 172.16.1.230). In your case, you should use `db.cardano.sotatek.works`
- `PORT_DB` : Database port (default is my postgres port: 54321)
- `USERNAME_DB`: Database username (in your case, do not fill this field because local profile is using a read-only user)
- `PASSWORD_DB`: Database password (in your case, do not fill this field because local profile is using a read-only user)
- `SCHEMA`: Schema of database
- `DB`: Database name
- `S3_ACCESS_KEY`: the AWS access key
- `S3_SECRET_KEY`: the AWS secret key
- `S3_REGION`: the AWS region
- `S3_BUCKET_NAME`: the AWS bucket
- `S3_ENDPOINT`: the storage endpoint, only for S3 clone (either on localhost, Minio, etc.)

