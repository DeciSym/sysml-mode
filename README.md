# SysML v2 Mode for Emacs

Major mode for editing SysML v2 (Systems Modeling Language) files with
syntax highlighting and validation integration.

## Features

- **Syntax Highlighting**: Keywords, types, operators, comments, and
  documentation blocks
- **ElDoc Support**: Inline documentation in minibuffer when cursor is
  on keywords or operators
- **Quick Reference**: `C-c C-h` displays a syntax guide with examples
- **Context-Sensitive Help**: `C-c C-d` opens web documentation for
  the keyword at point
- **Validation**: Integration with `validate-sysml.sh` for on-save
  validation (`C-c C-v`)
- **Spell Checking**: Intelligent spell checking that only checks
  strings and comments, not code
- **Indentation**: Automatic indentation based on block structure
- **Comments**: Support for `//` single-line and `/* */` multi-line
  comments

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

;; Optional: Set custom tab width
(add-hook 'sysml-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode t)))
```

## Key Bindings

| Key       | Command                     | Description                    |
|-----------|-----------------------------|--------------------------------|
| `C-c C-v` | `sysml-validate-buffer`     | Validate current file          |
| `C-c C-h` | `sysml-quick-reference`     | Show quick reference guide     |
| `C-c C-d` | `sysml-browse-doc-at-point` | Open documentation for keyword |

## Syntax Highlighting

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

### Context-Sensitive Documentation

Press `C-c C-d` with cursor on any keyword to open the relevant
section of the OMG SysML v2 specification in your web browser. This
provides access to the authoritative documentation for detailed
information.

### Authoritative Sources

The mode links to these official documentation sources:
- **OMG SysML v2 Specification**: https://www.omg.org/spec/SysML/2.0/
- **Normative Example Model**:
  https://www.omg.org/cgi-bin/doc?ptc/25-04-31.sysml
- **SysML v2 Release Repository**:
  https://github.com/Systems-Modeling/SysML-v2-Release

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

## License

Copyright (C) 2025 DeciSym, LLC

<!--  LocalWords:  v2 ElDoc minibuffer sysml elisp setq DataValue OMG
<!--  LocalWords:  Booleans supertype lookups ispell flyspell chmod
<!--  LocalWords:  fontify
 -->
 -->
 -->
