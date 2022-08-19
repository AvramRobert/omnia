# Cheat Sheet

The config is found in the file `config.edn`. Everything inside can be changed.

## Key Bindings

**Note**: Due to either operating system key priorities and/or the terminal backend _omnia_ uses,
some key bindings might not work directly out of the box.

The key bindings can be changed. For more information, take a look at [configuration](https://github.com/AvramRobert/omnia/blob/master/docs/configuration.md).

### Supported keys

|Key | Config |
|----|--------|
| All character keys | \\\<some-char\>|
| Up arrow | :up |
| Down arrow | :down |
| Left arrow | :left | 
| Right arrow | :right |
| Enter / Return | :enter |
| Space | :space |
| Backspace | :backspace |
| Tab | :tab |
| Page Up | :page-up |
| Page Down | :page-down |
| Escape | :escape |
| Home | :home |
| End | :end |
| Insert | :insert |
| Delete | :delete |
| F1 through F9 | :f1 ... :f9 |

### Unsupported keys

| Operating System | Key |
|------------------|-----|
| MacOS | Command |

### Unsupported key combinations (incomplete)

**Note:** The _command_ key on MacOS 

| Combinations |
|--------------|
| :ctrl + i    |
| :ctrl + z    |
| :ctrl + y    |
| :ctrl + :backspace |
| ... |

### Config parameters

| Operation | Config | Default key binding | Description |
|--------------|----------------|-------------|-----------------|
|Input char| :character | \\\<input char\> | Inserts character |
|Move up | :move-up | :up | Moves cursor up |
|Move down | :move-down | :down | Moves cursor down |
|Move left | :move-left | :left | Moves cursor left |
|Move right | :move-right | :right | Moves cursor right |
|New line | :new-line | :enter | Creates a new line |
|Delete previous | :delete-previous | :backspace | Deletes the previous character |
|Delete current | :delete-current | :delete | Deletes the current character |
|Evaluate| :evaluate | :ctrl + :enter  | Evaluates the input |
|Clear | :clear | :ctrl + r | Clears screen
|Reformat| :reformat | :ctrl + :alt + l | Reformats input |
|Suggestion | :suggestion | :tab | Lists matching suggestions for input. Suggestions can be scrolled through by repeatedly pressing the key
|Signature | :signature | :ctrl + p | Lists all signatures of the function |
|Documentation | :documentation | :ctrl + q | Displays a scrollable view of the function's documentation |
|Jump left | :jump-left | :ctrl + :left | Jumps left over words and spaces |
|Jump right | :jump-right | :ctrl + :right | Jumps right over words and spaces |
|Select left | :select-left | :shift + :left | Selects input to the left |
|Select right | :select-right | :shift + :right | Selects input to the right |
|Select up | :select-up | :shift + :up | Selects input upwards |
|Select down | :select-down | :shift + :down | Selects input downwards |
|Jump select left | :jump-select-left | :ctrl + :shift + :left | Selects by jumping over words/spaces to the left |
|Jump select right | :jump-select-right | :ctrl + :shift + :right | Selects by jumping over words/spaces to the right |
|Expand selection| :expand-selection | :ctrl + w | Selects by gradually expanding from words to expressions |
|Select all | :select-all | :ctrl + a | Selects the whole input  |
|Copy | :copy | :ctrl + c | Copies the current selection |
|Cut  | :cut | :ctrl + x | Cuts the current selection |
|Paste | :paste | :ctrl + p | Pastes the cut/copied selection |
|Scroll up  | :scroll-up | :page-up | Scrolls up the view |
|Scroll down | :scroll-down | :page-down | Scrolls down the view |
|Previous evaluation | :previous-evaluation | :ctrl + :up | Goes to the previous evaluation in the history |
|Next evaluation | :next-evaluation | :ctrl + :down | Goes to the following evaluation in the history |
|Undo | :undo | :alt + z | Undoes the last action |
|Redo | :redo | :alt + y | Redoes the last undo |
|Exit | :exit | :ctrl + d | Exits the REPL |

## Colours

### Preset

|Colour | Config | Value (rbg) |
|------|--------|-----|
| Yellow | :yellow | `rgb(180,148,6)` |
| Blue | :blue | `rgb(85,148,187)` |
| Green | :green | `rbg(0,170,0)` |
| Cyan | :cyan | `rgb(0,170,170)` |
| Magenta | :magenta | `rgb(170,0,170)` |
| Red | :red | `rgb(170,0,0)`|
| Black | :black | `rgb(0,0,0)`|
| White | :white | `rgb(171,174,168)` |

### Custom

RGB-Triple represented with a vector: `[180, 148, 187]`

## Syntax highlighting

|Construct | Config | Default colour |
|----------|--------|------------------|
|Lists |  :lists | :white |
|Vectors | :vectors | :white |
|Maps | :maps | :white |
|Numbers | :numbers | :blue |
|Characters | :characters |:green |
|Strings | :green | :strings |
|Keywords | :keywords | :cyan |
|Comments | :comments | :magenta |
|Special words| :words | :yellow |
|Function calls |  :functions | :yellow |
|Text | :texts | :white |
|Commas| :commas | :white |
|Selections | :selections | :blue |

## Fonts

The font used is [Hasklig](https://github.com/i-tu/Hasklig).

### Config parameters

| Config | Description | Default value |
|-----------|-------------|---------------|
|:font-path | Path to font file (otf) | `./default-font.otf` |
|:font-size | Size of the font | 15 |

### Custom font

Reference the path to a different `otf` file in the `:font-path` field.

## History

Where to persist evaluations and how many of them.

| Config | Description | Default value |
|-------|-------------|---------------|
| :history-file-path | Where the evaluation history is persisted | `.omnia.history` |
| :history-size | How many previous evaluations are kept | 50 |
