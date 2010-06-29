#!/bin/sh

erl -pa ./ebin \
    -eval "application:start(local132)"