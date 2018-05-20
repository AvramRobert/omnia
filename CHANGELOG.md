# Change Log
All notable changes to this project will be documented in this file.
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
- Project now has a separate documentation page

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
