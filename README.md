# Ad Pricer

## Assumptions
- Special customers can only have 1 deal per ad type
- Deal types are relatively static, but the deals themselves need to be easily updated

## Design decisions
- Pricing rules and checkout items are represented as maps to make sure there will be no duplicates on a type level
- Since configuration for prices and pricing rules is meant to be configurable it's represented as a `config.json` file which uses ad and customer ids as keys, which prevents duplicates and makes the file editable by humans.
- Increased ergonomics of json file means that automatic deriving of `FromJSON` in some cases is not possible, so custom instances had to be written

## App design
App is split into 3 modules:
- ShoppingCart - converts incoming data (customer id, purchased ads) and config into a `ShoppingCart` type ready to be processed
- Checkout - main logic of the app - calculates total by applying pricing rules if necessary
- Api - web server using Servant with a single endpoint `/checkout` accepting:
```json
{
    "customer": "nike",
    "skus": ["premium", "classic"]
}
```
and returning result:
```json
{
    "total": {
      "denominator": 1,
      "numerator": 1000
    }
}
```

## Running the app
```bash
stack build && stack exec ad-pricer-exe
```

Test request:
```bash
curl -X POST \
  http://localhost:8080/checkout \
  -H 'Content-Type: application/json' \
  -H 'Postman-Token: f147b723-9fe8-4064-a99a-4a04f854a934' \
  -H 'cache-control: no-cache' \
  -d '{
  "customer": "nike",
  "skus": ["premium", "premium", "premium", "premium"]
  }'
```

The response should be:
```json
{
  "total": {
    "denominator": 25,
    "numerator": 37999
  }
}
```

## Running tests

```bash
```
