# Change Log
All notable changes to this project will be documented in this file.

## [1.0.1]
### Changed
- The defaults for the font and history files are now implicit
- Fixed error when reading font file outside of app directory

## [1.0.0]
### Changed
- Major refactor taking maintainability, clarity and performance into account
- Dependency updates
- Major performance improvements
- Runs in a window, not in the shell
- Extended support for a larger majority of key bindings
- Custom RGB colours support
- Custom font support
- Custom release for every operating system

## [0.2.1]
### Changed
- Fixed agent pool leak coming from CIDER nrepl
- Fixed unnecessary CPU resource consumption when idle
- Performance improvements

### Removed
- (Temporarily) output capturing of async actions in REPL

## [0.2.0]
### Changed
- Added automatic parens matching
- Added parametricity, arity and documentation lookup
- Added undo/redo support
- Added support for Java 1.9+ and Clojure 1.9 
- All together improved performance 
- Added automatic building and releasing task
- Fixed bugs with syntax highlighting and rendering
- Now uses the `CIDER` nREPL middleware for all its REPL backend features
- Configuration has been simplified to just key bindings and colour scheme
- Printed outputs of concurrent processes now feed back into the REPL

### Removed
- The ability to disable syntax highlighting, input suggestions and scrolling
- Dependency on `clojure-lanterna`
- `ritz` nREPL backend

## [0.1.0-BETA]   
### Added
- Structural editing
- Multi-line input
- Multi-view input
- Syntax highlighting
- (manual) Code formatting
- (manual) Parens matching
- Selection system
- Selection expansion
- Input suggestions
- View scrolling
- Copy/Cut/Paste/Overwrite
- Persistent REPL history
- Dependency resolution and runtime library binding
- Configurability
- Graceful failures

[0.2.0]: https://github.com/AvramRobert/omnia/releases/tag/0.2.0
[0.1.0-BETA]: https://github.com/AvramRobert/omnia/releases/tag/0.1.0-BETA
