# SysML v2 Mode for Emacs

Major mode for editing SysML v2 (Systems Modeling Language) files with
syntax highlighting and validation integration.

## Features

### Core Editing Features
- **Syntax Highlighting**: Optimized with compile-time regex compilation
  for keywords, types, operators, comments, and documentation blocks
- **Automatic Indentation**: Smart indentation based on block structure
- **Electric Pairs**: Automatic insertion of matching delimiters
  (`{}`, `[]`, `()`, `""`, `<>`)
- **Comments**: Support for `//` single-line and `/* */` multi-line
  comments

### Navigation and Code Intelligence
- **Imenu Support**: Navigate to any definition via menu (`M-x imenu`)
- **Which-Function Mode**: Shows current definition in mode line
- **Code Folding**: Hide/show blocks with hideshow mode
  - `C-c @ C-h` - Hide block
  - `C-c @ C-s` - Show block
  - `C-c @ C-c` - Toggle hiding
- **ElDoc Support**: Inline documentation in minibuffer when cursor is
  on keywords or operators

### Visual Enhancements
- **Prettify Symbols**: Display operators with Unicode characters
  - `:>` → `⊃` (specialization)
  - `:>>` → `⊇` (redefinition)
  - `::` → `∷` (qualified names)
  - `and` → `∧`, `or` → `∨`, `not` → `¬`
  - `[*]` → `∞` (unbounded multiplicity)

### Development Tools
- **Quick Reference**: `C-c C-h` displays a syntax guide with examples
- **Validation**: Integration with `validate-sysml.sh` for on-save
  validation (`C-c C-v`)
- **Spell Checking**: Smart spell checking for comments and strings only
  (`C-c C-s`)

### Auto-mode Association

The mode automatically associates with `.sysml` files. To manually
enable:

```
M-x sysml-mode
```

## Configuration

### Enable Validation on Save

```elisp
(setq sysml-validate-on-save t)
```

### Set Validator Script Path

By default, the mode auto-detects `validate-sysml.sh` in the project
directory tree. To set explicitly:

```elisp
(setq sysml-validator-script "/path/to/validate-sysml.sh")
```

### Example Configuration

```elisp
;; Load SysML mode
(load-file "sysml-mode.el")

;; Enable validation on save
(setq sysml-validate-on-save t)

;; Optional: Enable which-function-mode globally to see current definition
(which-function-mode 1)

;; Optional: Enable hideshow mode by default for code folding
(add-hook 'sysml-mode-hook 'hs-minor-mode)

;; Optional: Customize prettify symbols (or disable them)
(setq sysml-enable-prettify-symbols t)  ; Set to nil to disable

;; Optional: Customize prettify symbols mapping
(setq sysml-prettify-symbols-alist
      '((":>" . ?⊃)
        (":>>" . ?⊇)
        ("and" . ?∧)
        ("or" . ?∨)))

;; Optional: Set custom tab width
(add-hook 'sysml-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode t)))
```

## Key Bindings

### Core Commands
| Key       | Command                     | Description                    |
|-----------|-----------------------------|--------------------------------|
| `C-c C-v` | `sysml-validate-buffer`     | Validate current file          |
| `C-c C-h` | `sysml-quick-reference`     | Show quick reference guide     |
| `C-c C-s` | `sysml-spell-check-buffer`  | Spell check comments/strings   |

### Code Folding (Hideshow)
| Key         | Command | Description          |
|-------------|---------|----------------------|
| `C-c @ C-h` | Hide    | Hide current block   |
| `C-c @ C-s` | Show    | Show hidden block    |
| `C-c @ C-c` | Toggle  | Toggle block hiding  |
| `C-c @ C-l` | Hide All| Hide all blocks      |
| `C-c @ C-a` | Show All| Show all blocks      |

## Syntax Highlighting

SysML v2 Mode follows Emacs conventions for declarative languages
(like HTML, CSS, XML). The highlighting distinguishes between language
keywords, definitions, usages, and references.

### Highlighting Legend

The mode uses standard Emacs faces to highlight different syntactic elements:

| Face                             | Color (typical) | What It Represents                    | Examples                                                                                       |
|----------------------------------|-----------------|---------------------------------------|------------------------------------------------------------------------------------------------|
| **font-lock-keyword-face**       | Purple/Magenta  | Language keywords and operators       | `package`, `part`, `def`, `attribute`, `state`, `transition`, `:>`, `:>>`, `::`, `~`           |
| **font-lock-function-name-face** | Blue            | Type definitions and packages         | `part def Vehicle` → `Vehicle`<br>`package SimpleVehicleModel` → `SimpleVehicleModel`          |
| **font-lock-variable-name-face** | Yellow/Orange   | Property/instance declarations        | `attribute mass` → `mass`<br>`state normal` → `normal`<br>`transition off_To_on` → `off_To_on` |
| **font-lock-type-face**          | Green           | Type references                       | `: Real`, `: Boolean`, `: Time::DateTime`                                                      |
| **font-lock-builtin-face**       | Cyan            | Direction keywords, logical operators | `in`, `out`, `inout`, `and`, `or`, `not`                                                       |
| **font-lock-constant-face**      | Blue/Cyan       | Literals, multiplicity                | `500`, `true`, `false`, `[*]`, `[0..1]`                                                        |
| **font-lock-string-face**        | Red             | String literals                       | `"text value"`                                                                                 |
| **font-lock-comment-face**       | Gray/Dim        | Comments                              | `// comment`, `/* comment */`                                                                  |
| **font-lock-doc-face**           | Gray/Dim        | Documentation blocks                  | `doc /* documentation */`                                                                      |
| **font-lock-preprocessor-face**  | Purple          | Metadata annotations                  | `@MetadataName`, `#logical`, `#physical`                                                       |
| **Default face**                 | White/Black     | References to existing names          | `first normal`, `do senseTemperature`                                                          |

### Highlighting Philosophy

Following declarative language conventions:

- **Definitions** (creating new types) are colored like HTML element names
- **Usages** (declaring properties) are colored like HTML attributes
- **Type references** (type annotations) are colored
- **References** (using existing names) are NOT colored, like HTML text content

### Complete Example

```sysml
package SimpleVehicleModel {        // package=keyword, SimpleVehicleModel=function-name
    part def Vehicle {              // part,def=keyword, Vehicle=function-name
        attribute mass : Real;      // attribute=keyword, mass=variable-name, Real=type
        port ignitionPort : IgnitionCmdPort;  // port=keyword, ignitionPort=variable-name

        exhibit state vehicleStates {   // exhibit,state=keyword, vehicleStates=variable-name
            state off;              // state=keyword, off=variable-name
            state on {              // state=keyword, on=variable-name
                constraint {electricalPower <= 500[W]}  // constraint=keyword, 500=constant
            }

            transition off_To_on    // transition=keyword, off_To_on=variable-name
                first off           // first=keyword, off=default (reference, not colored)
                then on;            // then=keyword, on=default (reference, not colored)
        }
    }

    requirement def MassRequirement {   // requirement,def=keyword, MassRequirement=function-name
        doc /* Vehicle mass constraint */  // doc=keyword, comment=doc-face
        attribute massActual :> ISQ::mass;  // :>=operator, ISQ::mass=qualified type
    }
}
```

### What Gets Colored vs. Not Colored

**Colored** (structure and declarations):
- ✓ Language keywords (`part`, `state`, `first`, `then`)
- ✓ Operators (`:>`, `::`)
- ✓ Type definitions (`part def Vehicle` → `Vehicle`)
- ✓ Property declarations (`attribute mass` → `mass`)
- ✓ State/transition names (`state normal` → `normal`)
- ✓ Type references (`: Real`)
- ✓ Literals (`500`, `"text"`, `true`)

**Not Colored** (references and content):
- ✗ References to existing elements (`first off` → `off` not colored)
- ✗ Action/method calls (`do senseTemperature` → `senseTemperature` not colored)
- ✗ State references in transitions (`then on` → `on` not colored)

This creates a visual distinction between **declarations** (colored)
and **usage** (not colored), following standard declarative language
patterns.

The mode provides color highlighting for:

### Keywords
- **Package**: `package`, `import`, `private`, `public`, `standard`,
  `library`
- **Definitions**: `def`, `part`, `attribute`, `ref`, `item`, `port`,
  `action`, `state`, `requirement`, `constraint`
- **Behavioral**: `exhibit`, `perform`, `then`, `first`, `accept`,
  `send`, `entry`, `exit`, `do`
- **Relationships**: `specializes`, `subsets`, `redefines`, `bind`,
  `connect`, `satisfy`, `verify`

### Operators
- `:>` (specialization)
- `:>>` (redefinition)
- `::` (qualified names)

### Types
- Built-in types: `String`, `Boolean`, `Integer`, `Real`, `Natural`,
  `DataValue`
- User-defined types in type position
- Qualified names: `Package::SubPackage::Type`

### Literals
- Strings: `"text"`
- Numbers: `123`, `3.14`
- Booleans: `true`, `false`
- Multiplicity: `[0..1]`, `[0..*]`, `[1]`

### Comments and Documentation
- Single-line: `// comment`
- Multi-line: `/* comment */`
- Documentation: `doc /* documentation */`

## Documentation Features

### ElDoc Mode (Inline Help)

ElDoc is enabled by default. Move your cursor over any keyword or
operator to see a brief description in the minibuffer (echo area).

Examples:
- Cursor on `:>` shows: "specialization operator - inherit from
  supertype"
- Cursor on `part def` shows: "defines a structural component type
  (specializes Parts::Part)"
- Cursor on `:>>` shows: "redefinition operator - redefine inherited
  feature"

### Quick Reference Guide

Press `C-c C-h` to open a comprehensive quick reference buffer showing:
- Operator syntax and examples
- Common code patterns
- Multiplicity notation
- Key bindings
- Links to authoritative documentation sources

The quick reference is designed as a cheat sheet for quick lookups
without leaving Emacs.

### Authoritative Sources

The mode links to these official documentation sources:
- **OMG SysML v2 Specification**: https://www.omg.org/spec/SysML/2.0/
- **SysML v2 Release Repository**:
  https://github.com/Systems-Modeling/SysML-v2-Release

## Testing

Use the SysML textual notation file for the example Simple Vehicle Model
published by OMG: https://www.omg.org/cgi-bin/doc?ptc/25-04-31.sysml

## Spell Checking

The mode provides intelligent spell checking that only checks strings
and comments, not code identifiers or keywords.

### Batch Spell Checking

Simply use the standard `ispell-buffer` command:

```
M-x ispell-buffer
```

The mode automatically remaps this to `ispell-comments-and-strings`,
which checks only:
- String literals: `"text to check"`
- Single-line comments: `// comment text`
- Multi-line comments: `/* comment text */`
- Documentation blocks: `doc /* documentation */`

### On-the-Fly Spell Checking (flyspell)

For real-time spell checking as you type:

```elisp
(add-hook 'sysml-mode-hook 'flyspell-prog-mode)
```

The `flyspell-prog-mode` function is the standard Emacs way to enable
spell checking in programming modes - it only checks comments and
strings.

## Validation Integration

When `sysml-validate-on-save` is enabled, the mode runs
`validate-sysml.sh` after saving. Validation errors appear in the
`*compilation*` buffer with GNU format:

```
filename:line:column: error: message
```

Click on errors to jump to the corresponding location.

### Manual Validation

Run validation at any time with `C-c C-v` or:

```
M-x sysml-validate-buffer
```

## Compatibility

- Emacs 24.3 or later
- Based on OMG SysML v2 specification (https://www.omg.org/spec/SysML/2.0/)
- Compatible with SysML v2 Pilot Implementation validator

## Troubleshooting

### Validation not working

1. Check that `validate-sysml.sh` is in your project directory
2. Verify the script is executable: `chmod +x validate-sysml.sh`
3. Set `sysml-validator-script` explicitly if auto-detection fails
4. Check the `*Messages*` buffer for error messages

### Syntax highlighting issues

1. Reload the mode: `M-x revert-buffer`
2. Force font-lock refresh: `M-x font-lock-fontify-buffer`
3. Check that the file has `.sysml` extension

<!--  LocalWords:  v2 ElDoc minibuffer sysml elisp setq DataValue OMG
<!--  LocalWords:  Booleans supertype lookups ispell flyspell chmod
<!--  LocalWords:  fontify
 -->
 -->
 -->
