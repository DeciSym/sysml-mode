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
                                "decide" "loop"))

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
                                 "all"))

         ;; Logical operators
         (logical-operators '("and" "or" "xor" "not" "implies"))

         ;; Built-in types (from ScalarValues library)
         (builtin-types '("String" "Boolean" "Integer" "Real" "Natural" "Number"
                         "Complex" "Rational" "ScalarValue" "DataValue" "Anything"))

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

      ;; Operators: :> (specializes), :>> (redefines), :: (qualified name)
      (":\\(>>\\|>\\)" . font-lock-keyword-face)
      ("::" . font-lock-keyword-face)

      ;; Type annotations: : TypeName
      (":\\s-*\\([A-Z][A-Za-z0-9_]*\\)" 1 font-lock-type-face)

      ;; Multiplicity: [0..1], [0..*], [1], etc.
      ("\\[\\([0-9]+\\)\\.\\.\\([0-9*]+\\)\\]" . font-lock-constant-face)
      ("\\[\\([0-9*]+\\)\\]" . font-lock-constant-face)

      ;; Qualified names: Package::SubPackage::Type
      ;; Match identifiers followed by :: (use word boundaries to avoid splitting camelCase)
      ("\\<\\([A-Za-z][A-Za-z0-9_]*\\)::" 1 font-lock-type-face)

      ;; Attribute/part definitions: "part def TypeName" or "attribute def TypeName"
      ("\\<\\(part\\|attribute\\|item\\|port\\|action\\|state\\)\\s-+def\\s-+\\([A-Z][A-Za-z0-9_]*\\)"
       (2 font-lock-type-face))

      ;; Instance declarations: "part instanceName : TypeName"
      ("\\<\\(part\\|attribute\\|ref\\|port\\)\\s-+\\([a-z][A-Za-z0-9_]*\\)\\s-*:"
       (2 font-lock-variable-name-face))

      ;; Single-quoted identifiers (SysML v2 quoted names)
      ("'[^']*'" . font-lock-constant-face)

      ;; String literals
      ("\"[^\"]*\"" . font-lock-string-face)

      ;; Numeric literals
      ("\\<[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)

      ;; Boolean literals
      ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)

      ;; Annotations: @MetadataName
      ("@\\([A-Za-z][A-Za-z0-9_]*\\)" 1 font-lock-preprocessor-face)))
  "Keyword highlighting for SysML mode.")

;; Syntax table
(defvar sysml-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; C-style comments
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; Strings (both double and single quotes)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "\"" st)

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
  "Indent current line as SysML code."
  (interactive)
  (let ((indent-level 0))
    (save-excursion
      (beginning-of-line)
      ;; Check if we're inside a block
      (let ((ppss (syntax-ppss)))
        (setq indent-level (* (car ppss) tab-width))))

    ;; Adjust for closing braces
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^[ \t]*}")
        (setq indent-level (max 0 (- indent-level tab-width)))))

    ;; Apply indentation
    (indent-line-to indent-level)))

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
         (preceding (save-excursion
                     (ignore-errors
                       (backward-word)
                       (thing-at-point 'symbol t))))
         (two-word (when (and preceding word)
                     (concat preceding " " word)))
         (doc (or (and two-word (cdr (assoc two-word sysml-eldoc-keywords)))
                  (and word (cdr (assoc word sysml-eldoc-keywords))))))
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
        (insert "  C-c C-h  - Show this quick reference\n")
        (insert "  C-c C-d  - Open documentation at point\n\n")

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

(defconst sysml-doc-urls
  '(;; Operators and core syntax
    (":>" . "https://www.omg.org/spec/SysML/2.0/")
    (":>>" . "https://www.omg.org/spec/SysML/2.0/")

    ;; Definition keywords - point to spec sections
    ("package" . "https://www.omg.org/spec/SysML/2.0/")
    ("part" . "https://www.omg.org/spec/SysML/2.0/")
    ("attribute" . "https://www.omg.org/spec/SysML/2.0/")
    ("requirement" . "https://www.omg.org/spec/SysML/2.0/")
    ("action" . "https://www.omg.org/spec/SysML/2.0/")
    ("state" . "https://www.omg.org/spec/SysML/2.0/")
    ("constraint" . "https://www.omg.org/spec/SysML/2.0/")

    ;; Default to normative example
    (t . "https://www.omg.org/cgi-bin/doc?ptc/25-04-31.sysml"))
  "URLs to documentation for SysML keywords.")

(defun sysml-browse-doc-at-point ()
  "Open documentation for the SysML keyword at point in a web browser."
  (interactive)
  (let* ((word (thing-at-point 'symbol t))
         (url (or (cdr (assoc word sysml-doc-urls))
                  (cdr (assoc t sysml-doc-urls)))))
    (if url
        (progn
          (message "Opening SysML documentation for '%s'..." (or word "syntax"))
          (browse-url url))
      (message "No documentation URL found for '%s'" word))))

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

(defun sysml-ispell-buffer-advice (orig-fun &rest args)
  "Advice to make ispell-buffer only check comments and strings in SysML mode.
ORIG-FUN is the original function, ARGS are its arguments."
  (if (eq major-mode 'sysml-mode)
      (ispell-comments-and-strings)
    (apply orig-fun args)))

;; Mode map
(defvar sysml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v") 'sysml-validate-buffer)
    (define-key map (kbd "C-c C-h") 'sysml-quick-reference)
    (define-key map (kbd "C-c C-d") 'sysml-browse-doc-at-point)
    map)
  "Keymap for SysML mode.")

;; Mode definition
;;;###autoload
(define-derived-mode sysml-mode prog-mode "SysML"
  "Major mode for editing SysML v2 (Systems Modeling Language) files.

Features:
- Syntax highlighting for keywords, operators, types, and comments
- ElDoc support showing keyword documentation in minibuffer
- Quick reference guide (C-c C-h)
- Context-sensitive documentation browser (C-c C-d)
- File validation with validate-sysml.sh (C-c C-v)
- Spell checking for comments and strings only
- Automatic indentation

Key bindings:
  C-c C-v  - Validate current file
  C-c C-h  - Show quick reference guide
  C-c C-d  - Open documentation for keyword at point

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

  ;; Indentation
  (setq-local indent-line-function 'sysml-indent-line)
  (setq-local tab-width 4)

  ;; ElDoc support for inline documentation
  (setq-local eldoc-documentation-function 'sysml-eldoc-function)
  (eldoc-mode 1)

  ;; Spell checking configuration
  ;; flyspell-prog-mode and ispell-comments-and-strings work automatically
  ;; with our syntax table to check only comments and strings
  (setq-local flyspell-generic-check-word-predicate 'sysml-flyspell-verify)

  ;; Make ispell-buffer call ispell-comments-and-strings in this mode
  (advice-add 'ispell-buffer :around #'sysml-ispell-buffer-advice '((local t)))

  ;; Validation on save
  (add-hook 'after-save-hook 'sysml-validate-on-save nil t))

;; Auto-load for .sysml files
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sysml\\'" . sysml-mode))

(provide 'sysml-mode)

;;; sysml-mode.el ends here
