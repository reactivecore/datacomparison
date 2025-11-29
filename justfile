all: build test check-lint

build:
  ./mill _.compile

test:
  ./mill _.test

check-lint:
  ./mill _.fix --check

lint: scalafix scalafmt

scalafix:
  ./mill _.fix

scalafmt:
  ./mill mill.scalalib.scalafmt/
