# Revision history for lumberjack

## 1.0.3.0 -- 2023-07-28

  * Update for GHC 9.6 (`base-4.18.*`).
  * Remove `lumberjack`'s dependency on `mtl`, which went unused.

## 1.0.2.0 -- 2023-01-03

  * Update for GHC 9.4 (base-4.17-*).

## 1.0.1.0 -- 2022-03-09

  * Update for GHC 9.2 (base-4.16-*) [thanks to Ryan Scott].

## 1.0.0.1 -- 2021-06-27

  * Fix issue #2: use eta expansion example to avoid loss of deep
    skolemisation support under the simplified subsumption rules in
    GHC 9 [thanks to Felix Yan for the report].

## 1.0.0.0 -- 2020-12-20

  * No longer Beta, so major version is now 1.0, although functionality
    has not changed other than the addition of the `|#` operator.

  * Added the `|#` convenience infix operator.

## 0.1.0.3 -- 2020-11-30

* Bump prettyprinter upper-bound to allow versions in the 1.7.x range.

## 0.1.0.2 -- 2020-05-21

* Enable support for GHC 8.4.
* Add missing dependency for building the example.

## 0.1.0.1 -- 2020-02-14

* Updates to documentation and internal code formatting.  No functionality updates.

## 0.1.0.0 -- 2020-02-13

* Initial Lumberjack logger implementation, based on internal usage.
