# Revision history for arghs

## Unreleased

* Use `formatting` instead of `printf`.

## 0.2.0.1 -- 2023-04-21

* Improved README.
* Removed redundant version bounds of `base` for the executable.
* Reimplemented `getAllArgs` using `liftA2`.
* Reimplemented `lenToWidth` using repeated division.
* Reimplemented `main` using `traverse_` instead of `mapM_`.

## 0.2.0.0 -- 2023-04-20

* Refactor the project as a library plus an executable.

## 0.1.0.1 -- 2023-04-20

* Fix package metadata.

## 0.1.0.0 -- 2023-04-19

* First version. Released on an unsuspecting world.
