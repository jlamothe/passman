# Changelog for passman

## current

- set maximum version of transformers package

## 0.3.0.2

- more dependency versions

## 0.3.0.1

- updated to latest stackage LTS
- specified versions for dependencies

## 0.3.0

- updated to more recent LTS snapshot
- use microlens instead of lens

## 0.2.1

- refactoring
- store the database where Windows can find it
- confirm master password before creating new service

## 0.2

- implemented manual saving
- added a warning when changing master password
- some code cleanup as suggested by [Stephen Paul Weber](https://github.com/singpolyma)

## 0.1.1

- corrected a bug that was causing the pwGenerate function to hang occasionally.
  - this may cause some passwords to be generated differently
