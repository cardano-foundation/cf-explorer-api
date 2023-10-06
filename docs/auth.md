Explorer support setup auth by a json file

default will get from `src/main/resources/permission/policy.json`
we can config this file path by `AUTH_FILE_PATH` variable

example

``` 
{
  "auth": [
    {
      "uri": "/api/v1/pool-report/**",
      "method": "*"
    },
    {
      "uri": "/api/v1/protocols/**",
      "method": "GET",
      "roles": [
        "ROLE_VIEW_PROTOCOL"
      ]
    }
  ],
  "role": [
    {
      "name": "ROLE_ELEVATED",
      "function": [
        {
          "uri": "/api/v1/staking-lifecycle/report/stake-key",
          "method": "POST",
          "description": {
            "reportLimitPer24Hours": -1
          }
        },
        {
          "uri": "/api/v1/pool-report/create",
          "method": "POST",
          "description": {
            "reportLimitPer24Hours": -1
          }
        }
      ]
    },
    {
      "name": "ROLE_PUBLIC",
      "function": [
        {
          "uri": "/api/v1/staking-lifecycle/report/stake-key",
          "method": "POST",
          "description": {
            "reportLimitPer24Hours": 10
          }
        },
        {
          "uri": "/api/v1/pool-report/create",
          "method": "POST",
          "description": {
            "reportLimitPer24Hours": 2
          }
        }
      ]
    }
  ]
}
```

`auth` all endpoint need to authentication and authorization

```
{
  "uri": "/api/v1/pool-report/**",
  "method": "*"
}
```
all url has prefix is `/api/v1/pool-report/` with all http method
need to authentication

```
{
  "uri": "/api/v1/protocols/**",
  "method": "GET",
  "roles": [
    "ROLE_VIEW_PROTOCOL"
  ]
}
```

all url has prefix is `/api/v1/protocols/` with method `GET` need to authentication
. Only user has role `ROLE_VIEW_PROTOCOL` can access this 


`role` contain the business logic for authorization

```
{
  "name": "ROLE_PUBLIC",
  "function": [
    {
      "uri": "/api/v1/staking-lifecycle/report/stake-key",
      "method": "POST",
      "description": {
        "reportLimitPer24Hours": 10
      }
    },
    {
      "uri": "/api/v1/pool-report/create",
      "method": "POST",
      "description": {
        "reportLimitPer24Hours": 2
      }
    }
  ]
}
```

User has role "ROLE_PUBLIC" can create 10 stake report per 24h and 2 pool report per 24h

reportLimitPer24Hours = -1 it means unlimited report
