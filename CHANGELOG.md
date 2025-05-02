# Changelog

## Unreleased
### [Changed]
- [#49] Make the background of modules tinted.
- [#52] Reword the error message if no files are found to instruct the user to build with -fwrite-ide-info

### [Added]
- [#47] Add landscape layout feature.
- [#48] Add orthogonal edges feature.

## 0.1.7
### [Added]
- [#46] GHC 9.8, 9.10 support

## 0.1.6
### [Changed]
- Internal change; partially undid the Prelude structure implemented in [#22] and [#27], since it was causing issues (see [#32])

## 0.1.5
### [Added]
- [#19] [#20] GHC 9.6 support

## 0.1.4
### [Added]
- [#16] [#17] GHC 9.4 support

## 0.1.3

### [Changed]
- [#7] When encountering overlapping declarations, this will now keep the first one we find instead of throwing an error. Overlapping declarations are the result of TH slices. Since we don't have any guarantees for those anyways, producing garbage instead of an error seems like a net win.

## 0.1.2

### [Added]

- `--collapse-modules` option to collapse entire modules into a single node

## 0.1.1

### [Changed]
- [#2] [#3] Ignore all identifiers that have a zero-width span. These are the result of generated code, and should be rejected elsewhere, but apparently can occasionally creep through.

## 0.1.0

### [Added]
- Initial release
