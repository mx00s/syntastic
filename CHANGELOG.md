# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.0] - 2020-11-28
### Fixed
- Change AST value, variable, and operator types into traits so consumers must be consistent about their associated sorts (and arities for operators).
- Restrict visibility of AST's variants to prevent construction invalid operation nodes.

### Changed
- AST node serializations no longer include sorts and arities because they are determined from the trait implementations.

## [0.2.2] - 2020-11-27
### Added
- Represent, display, serialize, and deserialize generic abstract syntax trees.

[Unreleased]: https://github.com/mx00s/mx00s/compare/0.2.2...HEAD
[0.2.2]: https://github.com/mx00s/syntastic/compare/0.1.0...0.2.2
