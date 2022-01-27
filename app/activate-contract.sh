#!/bin/bash
curl -H "Content-Type: application/json" --request POST -d @activation-init.json localhost:9080/api/contract/activate

