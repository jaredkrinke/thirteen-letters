#!/bin/sh

curl --header "Content-Type: application/json" -d "{\"value1\":\"Thirteen Letters!\"}" https://maker.ifttt.com/trigger/push/with/key/$IFTTT_KEY
