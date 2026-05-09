# SysML v2 Mode for Emacs

Copyright (c) 2026 DeciSym, LLC <support@decisym.ai>

See LICENSE file for license information.

Major mode for editing SysML v2 files with syntax highlighting,
indentation, completion, navigation, comment/string spell checking,
and optional validation through an external `validate-sysml` command.

## Features

- Syntax highlighting for SysML keywords, operators, declarations,
  type positions, literals, comments, and documentation comments.
- Indentation and filling support for SysML blocks and comments.
- Completion for SysML keywords and symbols already present in the
  buffer.
- ElDoc descriptions for SysML keywords and operators.
- Imenu and `which-function-mode` support for definitions.
- Electric pairs for common SysML delimiters.
- Optional prettified display for selected operators.
- Hideshow setup for brace-delimited SysML blocks.
- Comment/string spell checking.
- Template insertion commands for common SysML declarations.
- Project-local navigation commands for definitions and references.
- Optional validation after save or on demand with `validate-sysml`.

The mode associates automatically with `.sysml` files.

## Syntax Highlighting Legend

`sysml-mode` colors text by SysML syntactic role, using standard Emacs
`font-lock` faces. The exact color depends on the active Emacs theme;
use this legend to interpret what a color means in your theme.

| Face role                   | SysML meaning                                                                           | Examples                                                                     |
|-----------------------------|-----------------------------------------------------------------------------------------|------------------------------------------------------------------------------|
| Keyword                     | Language forms, behavior words, relationships, and operators                            | `package`, `import`, `part`, `def`, `perform`, `transition`, `:>`, `::`, `~` |
| Definition name             | Named packages and named type definitions that navigation commands treat as definitions | `SimpleVehicleModel`, `Vehicle` in `part def Vehicle`                        |
| Variable/member name        | Local usages, owned features, ports, states, transitions, and parameters                | `mass`, `ignitionCmdPort`, `vehicleStates`, `normal_To_degraded`, `temp`     |
| Type reference              | Type names in typing positions after `:`                                                | `IgnitionCmdPort`, `Time::DateTime`, `Real`                                  |
| Builtin/operator word       | Direction and logical words                                                             | `in`, `out`, `inout`, `and`, `or`, `not`                                     |
| Constant/declaration marker | Declaration helper words, multiplicities, quoted identifiers, numbers, and booleans     | `doc`, `ref`, `[0..*]`, `'quoted name'`, `500`, `true`                       |
| String                      | Double-quoted string literals                                                           | `"REQ-001"`                                                                  |
| Comment/doc comment         | `//`, `/* ... */`, and `doc /* ... */` text                                             | `// 2023-02 release`, `doc /* ... */`                                        |
| Metadata/annotation         | Metadata tags and annotations                                                           | `#logical`, `#physical`, `@Trace`                                            |

The mode intentionally does not treat names such as `String`, `Real`,
`Boolean`, or `ISQ::mass` as built-in language tokens everywhere they
appear. In SysML v2 these names come from imported libraries, so they
are highlighted as type references only when they appear in type
positions.

## Installation

Put `sysml-mode.el` somewhere on `load-path`, then load it from your
Emacs initialization file:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'sysml-mode)
```

With `use-package` and a local checkout:

```elisp
(use-package sysml-mode
  :load-path "~/src/sysml-mode"
  :mode "\\.sysml\\'")
```

## Configuration

Validation is disabled by default. To run validation after saving:

```elisp
(setq sysml-validate-on-save t)
```

By default, the mode looks for an executable named `validate-sysml` from
the current file's directory upward, then on `exec-path`. The command is
called with the current `.sysml` file path as its argument.
To set the command explicitly:

```elisp
(setq sysml-validator-script "/path/to/validate-sysml")
```

To disable prettified operators:

```elisp
(setq sysml-enable-prettify-symbols nil)
```

## Commands

| Key       | Command                          | Description                        |
|-----------|----------------------------------|------------------------------------|
| `C-c C-v` | `sysml-validate-buffer`          | Validate the current file          |
| `C-c C-s` | `sysml-spell-check-buffer`       | Spell check comments and strings   |
| `M-.`     | `sysml-find-definition-at-point` | Find definition of symbol at point |
| `M-?`     | `sysml-find-references`          | Find references to symbol at point |
| `C-c C-l` | `sysml-list-definitions`         | List definitions in project files  |
| `C-c C-c` | Template prefix                  | Insert SysML declaration templates |

Use `C-h m` in a SysML buffer to see the active keymap, including the
available template suffixes under `C-c C-c`.

## Spell Checking

`C-c C-s` runs `sysml-spell-check-buffer`, which checks comments and
strings with `ispell-comments-and-strings`.

For on-the-fly spell checking:

```elisp
(add-hook 'sysml-mode-hook 'flyspell-prog-mode)
```

## Validation

Run validation manually with `C-c C-v` or:

```elisp
M-x sysml-validate-buffer
```

Validation output appears in Emacs' compilation buffer, so diagnostics
that use the usual `file:line:column: message` form are navigable with
the standard compilation commands.

If a validator is added or moved while a buffer is open, clear the
buffer-local validator lookup cache with:

```elisp
M-x sysml-clear-validator-cache
```

## Testing

Run the test suite from the repository root:

```sh
emacs -Q --batch -L . -l test/sysml-mode-tests.el -f ert-run-tests-batch-and-exit
```

<!-- LocalWords: SysML sysml ElDoc Imenu prettified elisp setq el
 Hideshow keymap ispell flyspell prog emacs ert font lock DateTime
 IgnitionCmdPort ISQ Trace normal_To_degraded -->
