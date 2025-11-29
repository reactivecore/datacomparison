all: build test check-lint

build:
  ./mill _.compile

test:
  ./mill _.test

check-lint:
  ./mill _.fix --check

lint:
  ./mill _.fix

