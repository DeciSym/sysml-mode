;;; sysml-mode.el --- Major mode for SysML v2 (Systems Modeling Language)  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 DeciSym, LLC
;; Author: Donald Anthony Pellegrino Jr., Ph.D.
;; Keywords: languages, modeling, systems engineering
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; This package provides a major mode for editing SysML v2 (Systems Modeling Language)
;; files with syntax highlighting based on the OMG SysML v2 normative specification.
;;
;; SysML v2 specification: https://www.omg.org/spec/SysML/2.0/
;; Normative example: https://www.omg.org/cgi-bin/doc?ptc/25-04-31.sysml
;;
;; Features:
;; - Syntax highlighting for keywords, operators, types
;; - Comment support (single-line // and multi-line /* */)
;; - Documentation blocks (doc /* ... */)
;; - Indentation support
;; - Integration with validate-sysml.sh for on-save validation
;;
;; Installation:
;; Add to your .emacs or init.el:
;;   (load-file "/path/to/sysml-mode.el")
;;   (add-to-list 'auto-mode-alist '("\\.sysml\\'" . sysml-mode))

;;; Code:

;; Required for electric pairs functionality
(require 'elec-pair)
;; Required for code folding
(require 'hideshow)

(defgroup sysml nil
  "Major mode for editing SysML v2 files."
  :prefix "sysml-"
  :group 'languages)

(defcustom sysml-validator-script nil
  "Path to the validate-sysml.sh script.
If nil, auto-detect from buffer's directory."
  :type '(choice (const :tag "Auto-detect" nil)
                 (file :tag "Script path"))
  :group 'sysml)

(defcustom sysml-validate-on-save nil
  "Whether to validate SysML files on save."
  :type 'boolean
  :group 'sysml)

;; Syntax highlighting
(defconst sysml-font-lock-keywords
  (eval-when-compile
    (let* (
         ;; Keyword categories based on OMG SysML v2 specification

         ;; Package and namespace keywords
         (package-keywords '("package" "import" "private" "public" "standard" "library"))

         ;; Definition keywords (def constructs)
         (definition-keywords '("def" "part" "attribute" "item" "port" "action" "state"
                                "requirement" "constraint" "connection" "interface" "allocation"
                                "verification" "use" "case" "calc" "analysis" "concern"
                                "viewpoint" "view" "metadata" "individual" "snapshot" "timeslice"
                                "enum" "variation" "variant" "stream" "message" "flow"
                                "succession" "feature" "abstract" "readonly" "derived" "end"
                                "binding" "succession"))

         ;; Behavioral and structural keywords
         (behavioral-keywords '("exhibit" "perform" "then" "first" "accept" "send" "via"
                                "entry" "exit" "do" "transition" "if" "else" "while" "for"
                                "return" "assert" "assume" "require" "fork" "join" "merge"
                                "decide" "loop" "parallel" "when" "at" "new"))

         ;; Relationship and constraint keywords
         (relationship-keywords '("specializes" "subsets" "redefines" "references" "conjugate"
                                  "chains" "inverse" "readonly" "derived" "composite"
                                  "bind" "connect" "satisfy" "verify" "subject" "objective"
                                  "from" "to" "about" "actor" "stakeholder"))

         ;; Directional keywords
         (direction-keywords '("in" "out" "inout"))

         ;; Special declaration keywords
         (declaration-keywords '("doc" "comment" "language" "inv" "pre" "post" "body" "result"
                                 "filter" "rendering" "expose" "ref" "all" "that" "self"
                                 "featured" "by" "typing" "chaining" "inverting" "owned"
                                 "member" "multiplicity" "ordered" "nonunique" "sequence"
                                 "alias" "for" "occurrence" "meta"))

         ;; Logical operators
         (logical-operators '("and" "or" "xor" "not" "implies"))

         ;; Built-in types - NONE, SysML v2 has no built-in types
         ;; All types (String, Boolean, Integer, Real, etc.) are defined in standard library
         (builtin-types '())

         ;; Generate regexp patterns
         (package-keywords-regexp (regexp-opt package-keywords 'words))
         (definition-keywords-regexp (regexp-opt definition-keywords 'words))
         (behavioral-keywords-regexp (regexp-opt behavioral-keywords 'words))
         (relationship-keywords-regexp (regexp-opt relationship-keywords 'words))
         (direction-keywords-regexp (regexp-opt direction-keywords 'words))
         (declaration-keywords-regexp (regexp-opt declaration-keywords 'words))
         (logical-operators-regexp (regexp-opt logical-operators 'words))
         (builtin-types-regexp (regexp-opt builtin-types 'words)))

    `(
      ;; Documentation blocks: doc /* ... */
      ("\\<doc\\>\\s-*\\(/\\*\\(?:.\\|\n\\)*?\\*/\\)"
       (0 font-lock-doc-face))

      ;; Multi-line comments /* ... */
      ("/\\*\\(?:.\\|\n\\)*?\\*/" . font-lock-comment-face)

      ;; Single-line comments //
      ("//.*$" . font-lock-comment-face)

      ;; Package and namespace keywords
      (,package-keywords-regexp . font-lock-keyword-face)

      ;; Definition keywords
      (,definition-keywords-regexp . font-lock-keyword-face)

      ;; Behavioral keywords
      (,behavioral-keywords-regexp . font-lock-keyword-face)

      ;; Relationship keywords
      (,relationship-keywords-regexp . font-lock-keyword-face)

      ;; Direction keywords
      (,direction-keywords-regexp . font-lock-builtin-face)

      ;; Declaration keywords
      (,declaration-keywords-regexp . font-lock-constant-face)

      ;; Logical operators
      (,logical-operators-regexp . font-lock-builtin-face)

      ;; Built-in types
      (,builtin-types-regexp . font-lock-type-face)

      ;; Definition type names: "part def Vehicle" - Vehicle is colored
      ("\\<\\(part\\|attribute\\|item\\|port\\|action\\|state\\|requirement\\|constraint\\|connection\\|interface\\|allocation\\|verification\\|use\\|case\\|calc\\|analysis\\|concern\\|viewpoint\\|view\\|metadata\\|individual\\|enum\\)\\s-+def\\s-+\\([A-Za-z_<][A-Za-z0-9_>]*\\)"
       (2 font-lock-function-name-face))

      ;; Package names: "package SimpleVehicleModel" - SimpleVehicleModel is colored
      ("\\<package\\s-+\\([A-Za-z][A-Za-z0-9_]*\\)" 1 font-lock-function-name-face)

      ;; Instance/property names with typing: "attribute mass :", "port x :"
      ("\\<\\(part\\|attribute\\|ref\\|port\\|item\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*:"
       (2 font-lock-variable-name-face))

      ;; State usages: "state normal;", "state healthStates {"
      ("\\<state\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*[{;]"
       (1 font-lock-variable-name-face))

      ;; Transition definitions: "transition normal_To_maintenance"
      ("\\<transition\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)"
       (1 font-lock-variable-name-face))

      ;; Action usages after entry/exit: "entry action initial"
      ("\\<\\(entry\\|exit\\)\\s-+action\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)"
       (2 font-lock-variable-name-face))

      ;; Parameters with item: "in item ignitionCmd"
      ("\\<\\(in\\|out\\|inout\\)\\s-+item\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)"
       (2 font-lock-variable-name-face))

      ;; Parameters without item: "out temp;"
      ("\\<\\(in\\|out\\|inout\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*[;:]"
       (2 font-lock-variable-name-face))

      ;; Type references after : (not :> or :>>): ": Real", ": Time::DateTime"
      ("\\(?:^\\|[^:]\\)\\(:\\)\\s-*\\([A-Z][A-Za-z0-9_:]*\\)" 2 font-lock-type-face)

      ;; Operators: :> (specializes), :>> (redefines), :: (qualified name), ~ (conjugate)
      (":\\(>>\\|>\\)" . font-lock-keyword-face)
      ("::" . font-lock-keyword-face)
      ("~" . font-lock-keyword-face)

      ;; Multiplicity: [0..1], [0..*], [1], etc.
      ("\\[\\([0-9]+\\)\\.\\.\\([0-9*]+\\)\\]" . font-lock-constant-face)
      ("\\[\\([0-9*]+\\)\\]" . font-lock-constant-face)

      ;; Single-quoted identifiers (SysML v2 quoted names)
      ("'[^']*'" . font-lock-constant-face)

      ;; String literals
      ("\"[^\"]*\"" . font-lock-string-face)

      ;; Numeric literals (not part of identifiers like Wheel_1)
      ;; Match numbers that are NOT preceded by word characters or underscore
      ("\\(?:^\\|[^A-Za-z0-9_]\\)\\([0-9]+\\(\\.[0-9]+\\)?\\)\\>" 1 font-lock-constant-face)

      ;; Boolean literals
      ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)

      ;; Metadata tags: #logical, #physical
      ("#\\(logical\\|physical\\)" . font-lock-preprocessor-face)

      ;; Annotations: @MetadataName
      ("@\\([A-Za-z][A-Za-z0-9_]*\\)" 1 font-lock-preprocessor-face))))
  "Keyword highlighting for SysML mode.")

;; Syntax table
(defvar sysml-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; C-style comments
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; Strings - only double quotes
    ;; Single quotes are used for quoted names in SysML, not strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "w" st)  ; Word constituent for quoted names

    ;; Operators
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?= "." st)

    ;; Parentheses and brackets
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)

    st)
  "Syntax table for SysML mode.")

;; Indentation
(defun sysml-indent-line ()
  "Indent current line as SysML code.
Properly handles brace-based indentation for SysML blocks."
  (interactive)
  (let ((indent-level 0)
        (pos (- (point-max) (point))))
    (save-excursion
      (beginning-of-line)
      (let* ((ppss (syntax-ppss))
             (depth (nth 0 ppss))          ; Get paren/brace depth (0 is depth, not 1!)
             (in-string (nth 3 ppss))      ; Check if in string
             (in-comment (nth 4 ppss)))    ; Check if in comment
        (unless (or in-string in-comment)
          ;; Calculate base indentation from nesting depth
          (setq indent-level (* (or depth 0) tab-width))
          ;; Decrease indent for closing braces on current line
          (when (looking-at "^[ \t]*}")
            (setq indent-level (max 0 (- indent-level tab-width)))))))
    ;; Apply the indentation
    (indent-line-to indent-level)
    ;; Restore point position relative to end of buffer
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))))

;; Imenu support for code navigation
(defconst sysml-imenu-generic-expression
  '(("Packages" "^\\s-*package\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Parts" "^\\s-*part\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Attributes" "^\\s-*attribute\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Items" "^\\s-*item\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Ports" "^\\s-*port\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Actions" "^\\s-*action\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("States" "^\\s-*state\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Requirements" "^\\s-*requirement\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Constraints" "^\\s-*constraint\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Connections" "^\\s-*connection\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Interfaces" "^\\s-*interface\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Allocations" "^\\s-*allocation\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Use Cases" "^\\s-*use\\s-+case\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Calculations" "^\\s-*calc\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Analyses" "^\\s-*analysis\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Views" "^\\s-*view\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Viewpoints" "^\\s-*viewpoint\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
    ("Enumerations" "^\\s-*enum\\s-+def\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1))
  "Imenu expressions for SysML mode.
Provides menu-based navigation to all major definition types.")

;; Which-function support
(defun sysml-which-function ()
  "Return current function name for which-function-mode.
Returns the name of the current SysML definition (part, action, state, etc.)."
  (save-excursion
    (let ((case-fold-search nil))
      ;; Search backward for any definition
      (when (re-search-backward
             (concat "^\\s-*"
                     "\\("  ; Group 1: the type
                     "\\(?:part\\|attribute\\|item\\|port\\|action\\|state\\|"
                     "requirement\\|constraint\\|connection\\|interface\\|"
                     "allocation\\|verification\\|use case\\|calc\\|analysis\\|"
                     "concern\\|viewpoint\\|view\\|metadata\\|enum\\)\\s-+def\\|"
                     "package"
                     "\\)"
                     "\\s-+"
                     "\\([A-Za-z_][A-Za-z0-9_]*\\)")  ; Group 2: the name
             nil t)
        (let ((type (match-string 1))
              (name (match-string 2)))
          (if (and type (string-match "def$" type))
              (format "%s %s" (replace-regexp-in-string "\\s-+def$" "" type) name)
            name))))))

;; Electric pairs configuration
(defvar sysml-mode-electric-pairs
  '((?{ . ?})       ; Braces for blocks
    (?\[ . ?\])     ; Brackets for multiplicity
    (?\( . ?\))     ; Parentheses for expressions
    (?\" . ?\")     ; Double quotes for strings
    (?< . ?>))      ; Angle brackets for generics
  "Electric pairs for SysML mode.
Automatically inserts matching closing delimiters.")

;; Fill paragraph function - only handle comments
(defun sysml-fill-paragraph (&optional justify)
  "Fill paragraph in SysML mode.
Only handles filling when inside comments.
Returns nil for non-comments to use default behavior.
Optional argument JUSTIFY controls text justification."
  (interactive "*P")
  (let* ((ppss (syntax-ppss))
         (in-comment (nth 4 ppss))
         (comment-start-pos (nth 8 ppss)))
    (cond
     ;; Handle /* */ comments specially
     ((and in-comment comment-start-pos
           (save-excursion
             (goto-char comment-start-pos)
             (looking-at "/\\*")))
      ;; Let Emacs handle the filling with proper settings
      (let ((fill-prefix
             (save-excursion
               (goto-char comment-start-pos)
               (beginning-of-line)
               (skip-chars-forward " \t")
               (make-string (current-column) ?\s))))
        (save-excursion
          (goto-char comment-start-pos)
          (search-forward "*/" nil t)
          (let ((comment-end (point)))
            ;; Use the standard fill-region-as-paragraph
            (fill-region-as-paragraph comment-start-pos comment-end justify))))
      t)
     ;; Handle // comments
     ((and in-comment comment-start-pos
           (save-excursion
             (goto-char comment-start-pos)
             (looking-at "//")))
      (fill-comment-paragraph justify)
      t)
     ;; Not in a comment - don't do anything
     (t nil))))

;; Prettify symbols configuration
(defcustom sysml-prettify-symbols-alist
  '(;; Operators
    (":>" . ?⊃)       ; Specialization (subset/superset symbol)
    (":>>" . ?⊇)      ; Redefinition (subset-or-equal symbol)
    ("::" . ?∷)       ; Qualified name separator (proportion symbol)
    ("<=" . ?≤)       ; Less than or equal
    (">=" . ?≥)       ; Greater than or equal
    ("!=" . ?≠)       ; Not equal
    ("->" . ?→)       ; Arrow (for flows/transitions)
    ("=>" . ?⇒)       ; Implies
    ("<->" . ?↔)      ; Bidirectional
    ("and" . ?∧)      ; Logical AND
    ("or" . ?∨)       ; Logical OR
    ("not" . ?¬)      ; Logical NOT
    ("xor" . ?⊕)      ; Logical XOR
    ("[*]" . ?∞))     ; Unbounded multiplicity
  "Alist of symbol prettifications for SysML mode.
Each element is a cons cell (STRING . CHAR) where STRING is the text
to replace and CHAR is the Unicode character to display instead.
Set to nil to disable prettification."
  :type '(alist :key-type string :value-type character)
  :group 'sysml)

(defcustom sysml-enable-prettify-symbols t
  "Whether to enable prettify-symbols-mode in SysML buffers."
  :type 'boolean
  :group 'sysml)

;; Validation integration
(defun sysml-find-validator ()
  "Find the validate-sysml.sh script in the current directory tree."
  (let ((current-dir (file-name-directory (or buffer-file-name default-directory))))
    (or sysml-validator-script
        (locate-dominating-file current-dir "validate-sysml.sh"))))

(defun sysml-validate-buffer ()
  "Validate the current SysML buffer using validate-sysml.sh.
The validator can validate either a single file or a directory."
  (interactive)
  (let* ((validator-dir (sysml-find-validator))
         (validator-script (when validator-dir
                            (expand-file-name "validate-sysml.sh" validator-dir))))
    (if (and validator-script (file-exists-p validator-script))
        (let ((default-directory (file-name-directory validator-script)))
          (compile (format "%s %s" validator-script
                          (shell-quote-argument buffer-file-name))))
      (message "SysML validator script not found. Set sysml-validator-script or ensure validate-sysml.sh is in project."))))

(defun sysml-validate-on-save ()
  "Validate SysML file on save if enabled."
  (when (and sysml-validate-on-save
             (eq major-mode 'sysml-mode))
    (sysml-validate-buffer)))

;; Documentation support

(defconst sysml-eldoc-keywords
  '(;; Operators
    (":>" . "specialization operator - inherit from supertype")
    (":>>" . "redefinition operator - redefine inherited feature")
    ("::" . "qualified name separator - Package::Type")

    ;; Package keywords
    ("package" . "declares a namespace container")
    ("import" . "imports elements from another package")
    ("private" . "visibility modifier - accessible only within package")
    ("public" . "visibility modifier - accessible from outside package")
    ("standard" . "marks as part of standard library")
    ("library" . "marks as library package")

    ;; Definition keywords
    ("part" . "structural element definition or usage")
    ("def" . "defines a type")
    ("attribute" . "data value definition or usage")
    ("ref" . "reference to another element (not composition)")
    ("abstract" . "cannot be instantiated directly")

    ;; Documentation
    ("doc" . "documentation comment block")

    ;; Common patterns
    ("part def" . "defines a structural component type (specializes Parts::Part)")
    ("attribute def" . "defines a data type (specializes Base::DataValue)")
    ("ref system" . "reference to external system (not composition)"))
  "ElDoc documentation for SysML keywords and operators.")

(defun sysml-eldoc-function ()
  "Provide ElDoc documentation for SysML keywords at point."
  (let* ((word (thing-at-point 'symbol t))
         ;; Also check for operators
         (operator (save-excursion
                    (skip-syntax-backward "^w")
                    (buffer-substring-no-properties
                     (point)
                     (min (+ (point) 3) (point-max)))))
         (preceding (save-excursion
                     (ignore-errors
                       (backward-word)
                       (thing-at-point 'symbol t))))
         (two-word (when (and preceding word)
                     (concat preceding " " word)))
         (doc nil))
    ;; Try different lookups
    (setq doc (or
               ;; Try two-word combination
               (and two-word (cdr (assoc two-word sysml-eldoc-keywords)))
               ;; Try word
               (and word (cdr (assoc word sysml-eldoc-keywords)))
               ;; Try operator
               (cdr (assoc operator sysml-eldoc-keywords))
               ;; Try shorter operator
               (cdr (assoc (substring operator 0 (min 2 (length operator)))
                          sysml-eldoc-keywords))))
    doc))

(defun sysml-quick-reference ()
  "Display a quick reference guide for SysML v2 syntax."
  (interactive)
  (let ((buf (get-buffer-create "*SysML Quick Reference*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "SysML v2 Quick Reference\n")
        (insert "========================\n\n")

        (insert "Operators\n")
        (insert "---------\n")
        (insert "  :>   - Specialization (inheritance)\n")
        (insert "       Example: part def Car :> Vehicle\n\n")
        (insert "  :>>  - Redefinition (override inherited feature)\n")
        (insert "       Example: attribute :>> name = \"MyName\"\n\n")
        (insert "  ::   - Qualified name separator\n")
        (insert "       Example: ScalarValues::String\n\n")

        (insert "Common Patterns\n")
        (insert "---------------\n")
        (insert "Package declaration:\n")
        (insert "  package MyPackage {\n")
        (insert "    private import ScalarValues::*;\n")
        (insert "  }\n\n")

        (insert "Part definition:\n")
        (insert "  part def MyComponent {\n")
        (insert "    attribute name : String;\n")
        (insert "    ref system : System[0..1];\n")
        (insert "  }\n\n")

        (insert "Part usage (instance):\n")
        (insert "  part myInstance : MyComponent {\n")
        (insert "    attribute :>> name = \"Instance1\";\n")
        (insert "  }\n\n")

        (insert "Attribute definition:\n")
        (insert "  attribute def Region :> String;\n\n")

        (insert "Multi-valued features:\n")
        (insert "  ref :>> tools = (tool1, tool2, tool3);\n\n")

        (insert "Documentation\n")
        (insert "-------------\n")
        (insert "  doc\n")
        (insert "  /*\n")
        (insert "   * Documentation comment\n")
        (insert "   */\n\n")

        (insert "Multiplicity\n")
        (insert "------------\n")
        (insert "  [0..1]  - Zero or one\n")
        (insert "  [1]     - Exactly one\n")
        (insert "  [0..*]  - Zero or more\n")
        (insert "  [1..*]  - One or more\n\n")

        (insert "Key Bindings\n")
        (insert "------------\n")
        (insert "  C-c C-v  - Validate current file\n")
        (insert "  C-c C-h  - Show this quick reference\n\n")

        (insert "Authoritative Documentation\n")
        (insert "---------------------------\n")
        (insert "OMG SysML v2 Specification:\n")
        (insert "  https://www.omg.org/spec/SysML/2.0/\n\n")
        (insert "Normative Example Model:\n")
        (insert "  https://www.omg.org/cgi-bin/doc?ptc/25-04-31.sysml\n\n")
        (insert "SysML v2 Release (specifications and examples):\n")
        (insert "  https://github.com/Systems-Modeling/SysML-v2-Release\n\n"))

      (help-mode)
      (goto-char (point-min)))
    (display-buffer buf)))

;; Spell checking support
(defun sysml-flyspell-verify ()
  "Function to verify if flyspell should check word at point.
Only spell-check strings and comments, not code."
  (let ((face (get-text-property (point) 'face)))
    (or
     ;; Check strings
     (eq face 'font-lock-string-face)
     ;; Check comments
     (eq face 'font-lock-comment-face)
     ;; Check documentation blocks
     (eq face 'font-lock-doc-face)
     ;; Also handle when face is a list
     (and (listp face)
          (or (memq 'font-lock-string-face face)
              (memq 'font-lock-comment-face face)
              (memq 'font-lock-doc-face face))))))

(defun sysml-spell-check-buffer ()
  "Spell check comments and strings in SysML buffer.
This is a SysML-specific wrapper around ispell-comments-and-strings
that ensures only comments and strings are checked, not code."
  (interactive)
  (ispell-comments-and-strings))

;; Template insertion skeletons
;; Following html-mode convention with C-c C-c prefix for structured insertions

(define-skeleton sysml-insert-package
  "Insert a SysML package declaration."
  "Package name: "
  "package " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-part-def
  "Insert a SysML part definition."
  "Part name: "
  "part def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-attribute-def
  "Insert a SysML attribute definition."
  "Attribute name: "
  "attribute def " str " :> "
  (skeleton-read "Base type (String, Real, Integer, Boolean): ") ";")

(define-skeleton sysml-insert-item-def
  "Insert a SysML item definition."
  "Item name: "
  "item def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-port-def
  "Insert a SysML port definition."
  "Port name: "
  "port def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-action-def
  "Insert a SysML action definition."
  "Action name: "
  "action def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-state-def
  "Insert a SysML state definition."
  "State name: "
  "state def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-requirement-def
  "Insert a SysML requirement definition."
  "Requirement name: "
  "requirement def " str " {" \n
  > "doc /* " (skeleton-read "Description: ") " */" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-constraint-def
  "Insert a SysML constraint definition."
  "Constraint name: "
  "constraint def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-connection-def
  "Insert a SysML connection definition."
  "Connection name: "
  "connection def " str " {" \n
  > "end source : " (skeleton-read "Source type: ") ";" \n
  > "end target : " (skeleton-read "Target type: ") ";" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-interface-def
  "Insert a SysML interface definition."
  "Interface name: "
  "interface def " str " {" \n
  > "end p1 : " (skeleton-read "Port 1 type: ") ";" \n
  > "end p2 : " (skeleton-read "Port 2 type: ") ";" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-allocation-def
  "Insert a SysML allocation definition."
  "Allocation name: "
  "allocation def " str " {" \n
  > "end source;" \n
  > "end target;" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-verification-def
  "Insert a SysML verification definition."
  "Verification name: "
  "verification def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-use-case-def
  "Insert a SysML use case definition."
  "Use case name: "
  "use case def " str " {" \n
  > "doc /* " (skeleton-read "Description: ") " */" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-calc-def
  "Insert a SysML calculation definition."
  "Calculation name: "
  "calc def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-analysis-def
  "Insert a SysML analysis definition."
  "Analysis name: "
  "analysis def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-concern-def
  "Insert a SysML concern definition."
  "Concern name: "
  "concern def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-viewpoint-def
  "Insert a SysML viewpoint definition."
  "Viewpoint name: "
  "viewpoint def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-view-def
  "Insert a SysML view definition."
  "View name: "
  "view def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-metadata-def
  "Insert a SysML metadata definition."
  "Metadata name: "
  "metadata def " str " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-individual-def
  "Insert a SysML individual definition."
  "Individual name: "
  "individual def " str " :> "
  (skeleton-read "Type: ") ";")

(define-skeleton sysml-insert-enum-def
  "Insert a SysML enumeration definition."
  "Enum name: "
  "enum def " str " {" \n
  > (skeleton-read "First value: ") ";" \n
  > _ \n
  "}" >)

;; Instance templates (usages)
(define-skeleton sysml-insert-part-usage
  "Insert a SysML part usage (instance)."
  "Part name: "
  "part " str " : "
  (skeleton-read "Type: ") " {" \n
  > _ \n
  "}" >)

(define-skeleton sysml-insert-attribute-usage
  "Insert a SysML attribute usage (instance)."
  "Attribute name: "
  "attribute " str " : "
  (skeleton-read "Type: ") ";")

;; Mode map with template insertion keybindings
(defvar sysml-mode-map
  (let ((map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap)))
    ;; Core commands
    (define-key map (kbd "C-c C-v") 'sysml-validate-buffer)
    (define-key map (kbd "C-c C-h") 'sysml-quick-reference)
    (define-key map (kbd "C-c C-s") 'sysml-spell-check-buffer)

    ;; Template insertion commands (C-c C-c prefix)
    ;; Package and structural definitions
    (define-key prefix-map (kbd "k") 'sysml-insert-package)
    (define-key prefix-map (kbd "p") 'sysml-insert-part-def)
    (define-key prefix-map (kbd "a") 'sysml-insert-attribute-def)
    (define-key prefix-map (kbd "i") 'sysml-insert-item-def)
    (define-key prefix-map (kbd "o") 'sysml-insert-port-def)

    ;; Behavioral definitions
    (define-key prefix-map (kbd "t") 'sysml-insert-action-def)
    (define-key prefix-map (kbd "s") 'sysml-insert-state-def)

    ;; Requirements and verification
    (define-key prefix-map (kbd "r") 'sysml-insert-requirement-def)
    (define-key prefix-map (kbd "n") 'sysml-insert-constraint-def)
    (define-key prefix-map (kbd "f") 'sysml-insert-verification-def)

    ;; Connections and interfaces
    (define-key prefix-map (kbd "c") 'sysml-insert-connection-def)
    (define-key prefix-map (kbd "x") 'sysml-insert-interface-def)
    (define-key prefix-map (kbd "l") 'sysml-insert-allocation-def)

    ;; Use cases and analysis
    (define-key prefix-map (kbd "u") 'sysml-insert-use-case-def)
    (define-key prefix-map (kbd "b") 'sysml-insert-calc-def)
    (define-key prefix-map (kbd "y") 'sysml-insert-analysis-def)

    ;; Views and viewpoints
    (define-key prefix-map (kbd "w") 'sysml-insert-viewpoint-def)
    (define-key prefix-map (kbd "v") 'sysml-insert-view-def)
    (define-key prefix-map (kbd "g") 'sysml-insert-concern-def)

    ;; Metadata and enumerations
    (define-key prefix-map (kbd "m") 'sysml-insert-metadata-def)
    (define-key prefix-map (kbd "e") 'sysml-insert-enum-def)
    (define-key prefix-map (kbd "d") 'sysml-insert-individual-def)

    ;; Usage templates (instances)
    (define-key prefix-map (kbd ".") 'sysml-insert-part-usage)
    (define-key prefix-map (kbd ",") 'sysml-insert-attribute-usage)

    ;; Bind the prefix map
    (define-key map (kbd "C-c C-c") prefix-map)
    map)
  "Keymap for SysML mode.")

;; Mode definition
;;;###autoload
(define-derived-mode sysml-mode prog-mode "SysML"
  "Major mode for editing SysML v2 (Systems Modeling Language) files.

Features:
- Syntax highlighting with optimized compile-time regex compilation
- ElDoc support showing keyword documentation in minibuffer
- Imenu support for code navigation (M-x imenu)
- Which-function-mode support (shows current definition in mode line)
- Code folding with hideshow mode (C-c @ C-h to hide, C-c @ C-s to show)
- Electric pairs - automatic insertion of matching delimiters
- Prettify symbols - display operators with Unicode characters
- Quick reference guide (C-c C-h)
- File validation with validate-sysml.sh (C-c C-v)
- Smart spell checking for comments and strings only (C-c C-s)
- Improved automatic indentation

Key bindings:
  C-c C-v    - Validate current file
  C-c C-h    - Show quick reference guide
  C-c C-s    - Spell check comments and strings
  C-c C-c    - Template insertion prefix (followed by letter for template type)
  C-c @ C-h  - Hide block (code folding)
  C-c @ C-s  - Show block (code folding)
  C-c @ C-c  - Toggle hiding

Spell checking:
  M-x ispell-buffer  - Automatically checks only comments and strings
  M-x flyspell-prog-mode  - Enable on-the-fly spell checking

ElDoc mode is enabled by default - move cursor over keywords to see
brief descriptions in the minibuffer.

\\{sysml-mode-map}"
  :syntax-table sysml-mode-syntax-table

  ;; Set up font-lock
  (setq-local font-lock-defaults '(sysml-font-lock-keywords))

  ;; Comments - support both // and /* */ styles like C
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end-skip "\\s *\\(\\*+/\\)?")
  (setq-local comment-multi-line t)

  ;; Set up comment continuation for proper fill-paragraph behavior
  (setq-local comment-continue " * ")
  (setq-local comment-style 'extra-line)

  ;; Configure adaptive fill to handle comment prefixes properly
  (setq-local adaptive-fill-mode t)
  (setq-local adaptive-fill-regexp "[ \t]*\\(?:\\(?:/\\*+\\|\\*+\\)[ \t]*\\|//+[ \t]*\\)?")
  (setq-local adaptive-fill-first-line-regexp adaptive-fill-regexp)

  ;; Use our fill-paragraph function that only acts in comments
  (setq-local fill-paragraph-function 'sysml-fill-paragraph)

  ;; Indentation
  (setq-local indent-line-function 'sysml-indent-line)
  (setq-local tab-width 4)

  ;; ElDoc support for inline documentation
  (setq-local eldoc-documentation-function 'sysml-eldoc-function)
  (eldoc-mode 1)

  ;; Imenu support for code navigation
  (setq-local imenu-generic-expression sysml-imenu-generic-expression)
  (setq-local imenu-case-fold-search nil)  ; Case-sensitive for SysML

  ;; Which-function support (shows current definition in mode line)
  (setq-local which-func-functions '(sysml-which-function))

  ;; Electric pairs - automatically insert matching delimiters
  (setq-local electric-pair-pairs sysml-mode-electric-pairs)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (electric-pair-local-mode 1)

  ;; Code folding with hideshow - use simple brace-based folding
  (setq-local hs-block-start-regexp "{")
  (setq-local hs-block-end-regexp "}")
  (setq-local hs-forward-sexp-func 'forward-sexp)
  (setq-local hs-special-modes-alist
              `((sysml-mode
                 "{"     ; block start
                 "}"     ; block end
                 "/[*/]" ; comment start
                 forward-sexp
                 nil)))  ; no adjust-block-beginning function
  ;; Enable hideshow minor mode by default
  (hs-minor-mode 1)

  ;; Prettify symbols - display operators with Unicode characters
  (when sysml-enable-prettify-symbols
    (setq-local prettify-symbols-alist sysml-prettify-symbols-alist)
    (prettify-symbols-mode 1))

  ;; Spell checking configuration
  ;; flyspell-prog-mode and ispell-comments-and-strings work automatically
  ;; with our syntax table to check only comments and strings
  (setq-local flyspell-generic-check-word-predicate 'sysml-flyspell-verify)

  ;; Validation on save
  (add-hook 'after-save-hook 'sysml-validate-on-save nil t))

;; Auto-load for .sysml files
;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.sysml\\'" . sysml-mode))
  (add-to-list 'auto-mode-alist '("\\.sysmli\\'" . sysml-mode)))  ; Interface files

(provide 'sysml-mode)

;;; sysml-mode.el ends here
