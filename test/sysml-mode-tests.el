;;; sysml-mode-tests.el --- Tests for sysml-mode  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'imenu)

(defvar sysml-mode-initialized)
(defvar sysml-mode-map)
(defvar sysml-imenu-generic-expression)
(defvar sysml-mode-electric-pairs)
(defvar sysml-prettify-symbols-alist)
(defvar sysml-validate-on-save)
(defvar sysml-validator-script)
(defvar sysml--cached-validator)
(defvar electric-pair-pairs)
(defvar which-func-functions)

(declare-function sysml-mode "sysml-mode")
(declare-function sysml-find-validator "sysml-mode" (&optional force-refresh))
(declare-function sysml-clear-validator-cache "sysml-mode")
(declare-function sysml-mode-lazy-init "sysml-mode" (buffer))
(declare-function sysml-validate-buffer "sysml-mode")
(declare-function sysml-validate-on-save "sysml-mode")
(declare-function sysml-find-definition-at-point "sysml-mode")
(declare-function sysml-find-references "sysml-mode")
(declare-function sysml-list-definitions "sysml-mode")
(declare-function sysml--definition-search-regexp "sysml-mode" (symbol))
(declare-function sysml--search-files "sysml-mode" (regexp))
(declare-function sysml-eldoc-function "sysml-mode")
(declare-function sysml-which-function "sysml-mode")
(declare-function sysml-indent-line "sysml-mode")
(declare-function sysml-fill-paragraph "sysml-mode" (&optional justify))
(declare-function sysml-completion-at-point "sysml-mode")
(declare-function sysml-collect-buffer-symbols "sysml-mode")
(declare-function sysml-spell-check-buffer "sysml-mode")
(declare-function sysml-insert-part-def "sysml-mode")
(declare-function imenu--make-index-alist "imenu" (&optional noerror))

(load-file
 (expand-file-name "../sysml-mode.el"
                   (file-name-directory (or load-file-name buffer-file-name))))

(defun sysml-mode-tests--write-file (file contents)
  "Write CONTENTS to FILE and return FILE."
  (make-directory (file-name-directory file) t)
  (write-region contents nil file nil 'silent)
  file)

(defun sysml-mode-tests--write-executable (file)
  "Write an executable placeholder command to FILE and return FILE."
  (sysml-mode-tests--write-file file "#!/bin/sh\nexit 0\n")
  (set-file-modes file #o755)
  file)

(defmacro sysml-mode-tests--with-temp-dir (var &rest body)
  "Bind VAR to a temporary directory while running BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "sysml-mode-test " t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defun sysml-mode-tests--face-at (text)
  "Return the face at the start of TEXT in the current buffer."
  (goto-char (point-min))
  (search-forward text)
  (get-text-property (match-beginning 0) 'face))

(defun sysml-mode-tests--face-includes-p (face expected)
  "Return non-nil when FACE includes EXPECTED."
  (if (listp face)
      (memq expected face)
    (eq face expected)))

(defmacro sysml-mode-tests--should-face (text face)
  "Assert that TEXT is fontified with FACE."
  `(should (sysml-mode-tests--face-includes-p
            (sysml-mode-tests--face-at ,text)
            ',face)))

(defconst sysml-mode-tests--all-definition-examples
  '(("Packages" "Example" "package Example {}")
    ("Parts" "Vehicle" "part def Vehicle {}")
    ("Attributes" "Mass" "attribute def Mass :> Real;")
    ("Items" "Cargo" "item def Cargo {}")
    ("Ports" "PowerPort" "port def PowerPort {}")
    ("Actions" "Start" "action def Start {}")
    ("States" "Operational" "state def Operational {}")
    ("Requirements" "Performance" "requirement def Performance {}")
    ("Constraints" "Limit" "constraint def Limit {}")
    ("Connections" "Link" "connection def Link {}")
    ("Interfaces" "Bus" "interface def Bus {}")
    ("Allocations" "Mapping" "allocation def Mapping {}")
    ("Verifications" "TestCase" "verification def TestCase {}")
    ("Use Cases" "Mission" "use case def Mission {}")
    ("Calculations" "Load" "calc def Load {}")
    ("Analyses" "Trade" "analysis def Trade {}")
    ("Concerns" "Safety" "concern def Safety {}")
    ("Viewpoints" "StakeholderViewpoint" "viewpoint def StakeholderViewpoint {}")
    ("Views" "SystemView" "view def SystemView {}")
    ("Metadata" "TraceTag" "metadata def TraceTag {}")
    ("Individuals" "SampleVehicle" "individual def SampleVehicle :> Vehicle;")
    ("Enumerations" "Region" "enum def Region {}"))
  "Representative definition forms used by navigation and completion tests.")

(defun sysml-mode-tests--definition-source ()
  "Return a SysML source string containing all test definition forms."
  (mapconcat (lambda (definition)
               (nth 2 definition))
             sysml-mode-tests--all-definition-examples
             "\n"))

(ert-deftest sysml-mode-auto-mode-associations ()
  "SysML files should activate sysml-mode."
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "example.sysml" temporary-file-directory))
    (set-auto-mode)
    (should (eq major-mode 'sysml-mode))
    (cancel-function-timers #'sysml-mode-lazy-init)))

(ert-deftest sysml-mode-key-bindings ()
  "Core key bindings should dispatch SysML mode commands."
  (should (eq (lookup-key sysml-mode-map (kbd "C-c C-v"))
              #'sysml-validate-buffer))
  (should (eq (lookup-key sysml-mode-map (kbd "C-c C-s"))
              #'sysml-spell-check-buffer))
  (should (eq (lookup-key sysml-mode-map (kbd "M-."))
              #'sysml-find-definition-at-point))
  (should (eq (lookup-key sysml-mode-map (kbd "M-?"))
              #'sysml-find-references))
  (should (eq (lookup-key sysml-mode-map (kbd "C-c C-l"))
              #'sysml-list-definitions))
  (should (keymapp (lookup-key sysml-mode-map (kbd "C-c C-c"))))
  (should (eq (lookup-key sysml-mode-map (kbd "C-c C-c p"))
              #'sysml-insert-part-def)))

(ert-deftest sysml-mode-configures-buffer-local-features ()
  "Mode activation should install the editing features claimed in README."
  (with-temp-buffer
    (sysml-mode)
    (cancel-function-timers #'sysml-mode-lazy-init)
    (should (eq (car font-lock-defaults) 'sysml-font-lock-keywords))
    (should (eq indent-line-function #'sysml-indent-line))
    (should (eq fill-paragraph-function #'sysml-fill-paragraph))
    (should (eq eldoc-documentation-function #'sysml-eldoc-function))
    (should (bound-and-true-p eldoc-mode))
    (should (eq imenu-generic-expression sysml-imenu-generic-expression))
    (should (equal which-func-functions '(sysml-which-function)))
    (should (equal electric-pair-pairs sysml-mode-electric-pairs))
    (should (bound-and-true-p electric-pair-mode))
    (should (memq #'sysml-validate-on-save after-save-hook))))

(ert-deftest sysml-mode-fontifies-claimed-syntax-elements ()
  "Syntax highlighting should cover the elements listed in README."
  (with-temp-buffer
    (sysml-mode)
    (cancel-function-timers #'sysml-mode-lazy-init)
    (font-lock-set-defaults)
    (insert "package Example {\n"
            "  part def Vehicle :> Base {\n"
            "    attribute mass : Real = 42;\n"
            "    doc /* Documentation text */\n"
            "    // comment text\n"
            "    attribute label : String = \"hello\";\n"
            "  }\n"
            "}\n")
    (font-lock-ensure)
    (sysml-mode-tests--should-face "package" font-lock-keyword-face)
    (sysml-mode-tests--should-face "Vehicle" font-lock-function-name-face)
    (sysml-mode-tests--should-face "mass" font-lock-variable-name-face)
    (sysml-mode-tests--should-face "Real" font-lock-type-face)
    (sysml-mode-tests--should-face ":>" font-lock-keyword-face)
    (sysml-mode-tests--should-face "42" font-lock-constant-face)
    (sysml-mode-tests--should-face "Documentation" font-lock-comment-face)
    (sysml-mode-tests--should-face "comment" font-lock-comment-face)
    (sysml-mode-tests--should-face "hello" font-lock-string-face)))

(ert-deftest sysml-mode-eldoc-describes-keywords-and-operators ()
  "ElDoc should describe SysML keywords and operators."
  (with-temp-buffer
    (sysml-mode)
    (cancel-function-timers #'sysml-mode-lazy-init)
    (insert "part def Vehicle :> Base\n")
    (goto-char (point-min))
    (search-forward "part")
    (should (string-match-p "structural element"
                            (sysml-eldoc-function)))
    (goto-char (point-min))
    (search-forward ":>")
    (should (string-match-p "specialization operator"
                            (sysml-eldoc-function)))))

(ert-deftest sysml-mode-imenu-and-which-function-find-definitions ()
  "Imenu and which-function support should find SysML definitions."
  (with-temp-buffer
    (sysml-mode)
    (cancel-function-timers #'sysml-mode-lazy-init)
    (insert "package Example {\n  part def Vehicle {}\n}\n")
    (goto-char (point-max))
    (should (equal (sysml-which-function) "part Vehicle"))
    (let* ((index (imenu--make-index-alist t))
           (parts (cdr (assoc "Parts" index)))
           (packages (cdr (assoc "Packages" index))))
      (should (assoc "Vehicle" parts))
      (should (assoc "Example" packages)))))

(ert-deftest sysml-mode-navigation-handles-all-definition-forms ()
  "Navigation helpers should share one complete definition grammar."
  (with-temp-buffer
    (sysml-mode)
    (cancel-function-timers #'sysml-mode-lazy-init)
    (insert (sysml-mode-tests--definition-source))
    (goto-char (point-min))
    (search-forward "Mission")
    (should (equal (sysml-which-function) "use case Mission"))
    (let ((index (imenu--make-index-alist t)))
      (dolist (definition sysml-mode-tests--all-definition-examples)
        (let* ((category (nth 0 definition))
               (name (nth 1 definition))
               (matches (cdr (assoc category index))))
          (should (assoc name matches)))))))

(ert-deftest sysml-mode-finds-project-local-validator ()
  "Validator lookup should prefer validate-sysml in the file tree."
  (sysml-mode-tests--with-temp-dir root
    (let* ((validator (sysml-mode-tests--write-executable
                       (expand-file-name "validate-sysml" root)))
           (model (sysml-mode-tests--write-file
                   (expand-file-name "models/example.sysml" root)
                   "part def Vehicle {}\n"))
           (sysml-validator-script nil))
      (with-temp-buffer
        (setq buffer-file-name model)
        (setq sysml--cached-validator nil)
        (should (equal (sysml-find-validator t) validator))))))

(ert-deftest sysml-mode-finds-validator-on-exec-path ()
  "Validator lookup should fall back to executable search."
  (sysml-mode-tests--with-temp-dir root
    (let* ((bin-dir (expand-file-name "bin" root))
           (validator (sysml-mode-tests--write-executable
                       (expand-file-name "validate-sysml" bin-dir)))
           (model (sysml-mode-tests--write-file
                   (expand-file-name "model.sysml" root)
                   "part def Vehicle {}\n"))
           (exec-path (list bin-dir))
           (sysml-validator-script nil))
      (with-temp-buffer
        (setq buffer-file-name model)
        (setq sysml--cached-validator nil)
        (should (equal (sysml-find-validator t) validator))))))

(ert-deftest sysml-mode-quotes-validator-command ()
  "Validation should quote both validator command and model file paths."
  (sysml-mode-tests--with-temp-dir root
    (let* ((validator (sysml-mode-tests--write-executable
                       (expand-file-name "bin/validate sysml;tool" root)))
           (model (sysml-mode-tests--write-file
                   (expand-file-name "model files/example;one.sysml" root)
                   "part def Vehicle {}\n"))
           (expected (mapconcat #'shell-quote-argument
                                (list validator model)
                                " "))
           (sysml-validator-script validator)
           captured-command)
      (with-temp-buffer
        (setq buffer-file-name model)
        (set-buffer-modified-p nil)
        (cl-letf (((symbol-function 'compile)
                   (lambda (command &rest _)
                     (setq captured-command command))))
          (sysml-validate-buffer))
        (should (equal captured-command expected))))))

(ert-deftest sysml-mode-validation-runs-from-model-directory ()
  "Validation should run with the model file directory as `default-directory'."
  (sysml-mode-tests--with-temp-dir root
    (let* ((validator (sysml-mode-tests--write-executable
                       (expand-file-name "bin/validate-sysml" root)))
           (model (sysml-mode-tests--write-file
                   (expand-file-name "models/example.sysml" root)
                   "part def Vehicle {}\n"))
           (sysml-validator-script validator)
           captured-directory)
      (with-temp-buffer
        (setq buffer-file-name model)
        (set-buffer-modified-p nil)
        (cl-letf (((symbol-function 'compile)
                   (lambda (_command &rest _)
                     (setq captured-directory default-directory))))
          (sysml-validate-buffer))
        (should (equal captured-directory
                       (file-name-directory (expand-file-name model))))))))

(ert-deftest sysml-mode-validation-on-save-obeys-option ()
  "Validation on save should dispatch only when enabled for a file buffer."
  (let ((calls 0))
    (cl-letf (((symbol-function 'sysml-validate-buffer)
               (lambda ()
                 (setq calls (1+ calls)))))
      (with-temp-buffer
        (sysml-mode)
        (cancel-function-timers #'sysml-mode-lazy-init)
        (setq buffer-file-name "example.sysml")
        (let ((sysml-validate-on-save t))
          (sysml-validate-on-save)))
      (should (= calls 1))
      (with-temp-buffer
        (sysml-mode)
        (cancel-function-timers #'sysml-mode-lazy-init)
        (setq buffer-file-name "example.sysml")
        (let ((sysml-validate-on-save nil))
          (sysml-validate-on-save)))
      (should (= calls 1))
      (with-temp-buffer
        (sysml-mode)
        (cancel-function-timers #'sysml-mode-lazy-init)
        (let ((sysml-validate-on-save t))
          (sysml-validate-on-save)))
      (should (= calls 1)))))

(ert-deftest sysml-mode-clears-validator-cache ()
  "Validator cache clearing should reset the buffer-local cached path."
  (with-temp-buffer
    (setq sysml--cached-validator "/tmp/validate-sysml")
    (cl-letf (((symbol-function 'message) #'ignore))
      (sysml-clear-validator-cache))
    (should-not sysml--cached-validator)))

(ert-deftest sysml-mode-searches-sysml-project-files ()
  "Native project search should find SysML definitions and references."
  (sysml-mode-tests--with-temp-dir root
    (let* ((definition (sysml-mode-tests--write-file
                        (expand-file-name "model files/example.sysml" root)
                        "package Example {\n  part def Vehicle {}\n}\n"))
           (usage (sysml-mode-tests--write-file
                   (expand-file-name "model files/usage.sysml" root)
                   "part car : Vehicle {}\n"))
           (default-directory root))
      (should (equal (sysml--search-files
                      (sysml--definition-search-regexp "Vehicle"))
                     (list (list definition 2 "  part def Vehicle {}"))))
      (let ((matches (sysml--search-files "\\_<Vehicle\\_>")))
        (should (= (length matches) 2))
        (should (cl-some (lambda (match)
                           (equal match
                                  (list definition 2 "  part def Vehicle {}")))
                         matches))
        (should (cl-some (lambda (match)
                           (equal match
                                  (list usage 1 "part car : Vehicle {}")))
                         matches))))))

(ert-deftest sysml-mode-project-search-handles-all-definition-forms ()
  "Project search should find every definition form supported by templates."
  (sysml-mode-tests--with-temp-dir root
    (let* ((model (sysml-mode-tests--write-file
                   (expand-file-name "definitions.sysml" root)
                   (concat (sysml-mode-tests--definition-source) "\n")))
           (default-directory root))
      (dolist (definition sysml-mode-tests--all-definition-examples)
        (let* ((name (nth 1 definition))
               (text (nth 2 definition))
               (matches (sysml--search-files
                         (sysml--definition-search-regexp name))))
          (should (member (list model (1+ (cl-position definition
                                                       sysml-mode-tests--all-definition-examples))
                                text)
                          matches)))))))

(ert-deftest sysml-mode-lazy-init-handles-unloaded-hideshow ()
  "Lazy init should load hideshow safely for the intended SysML buffer."
  (when (featurep 'hideshow)
    (unload-feature 'hideshow t))
  (autoload 'hs-minor-mode "hideshow" nil t)
  (when (boundp 'hs-minor-mode)
    (makunbound 'hs-minor-mode))
  (let ((sysml-buffer (generate-new-buffer " *sysml-test*"))
        (other-buffer (generate-new-buffer " *sysml-other*")))
    (unwind-protect
        (progn
          (with-current-buffer sysml-buffer
            (sysml-mode)
            (cancel-function-timers #'sysml-mode-lazy-init)
            (should-not sysml-mode-initialized))
          (should-not (featurep 'hideshow))
          (with-current-buffer other-buffer
            (sysml-mode-lazy-init sysml-buffer)
            (should-not (bound-and-true-p hs-minor-mode)))
          (with-current-buffer sysml-buffer
            (should sysml-mode-initialized)
            (should (bound-and-true-p hs-minor-mode))
            (should (featurep 'hideshow))
            (should (bound-and-true-p prettify-symbols-mode))
            (should (equal prettify-symbols-alist
                           sysml-prettify-symbols-alist))))
      (when (buffer-live-p sysml-buffer)
        (kill-buffer sysml-buffer))
      (when (buffer-live-p other-buffer)
        (kill-buffer other-buffer)))))

(ert-deftest sysml-mode-indents-nested-members ()
  "Members inside braces should indent one level and closing braces align."
  (with-temp-buffer
    (sysml-mode)
    (cancel-function-timers #'sysml-mode-lazy-init)
    (insert "part def Vehicle {\nattribute mass : Real;\n}\n")
    (goto-char (point-min))
    (forward-line 1)
    (sysml-indent-line)
    (should (= (current-indentation) 4))
    (forward-line 1)
    (sysml-indent-line)
    (should (= (current-indentation) 0))))

(ert-deftest sysml-mode-fills-comments-not-code ()
  "Comment filling should be active without taking over code text."
  (with-temp-buffer
    (sysml-mode)
    (cancel-function-timers #'sysml-mode-lazy-init)
    (setq fill-column 30)
    (insert "/* This is a long SysML comment that should wrap when filled. */")
    (goto-char (point-min))
    (search-forward "long")
    (should (sysml-fill-paragraph))
    (should (string-match-p "\n" (buffer-string))))
  (with-temp-buffer
    (sysml-mode)
    (cancel-function-timers #'sysml-mode-lazy-init)
    (insert "part def Vehicle {}\n")
    (goto-char (point-min))
    (search-forward "Vehicle")
    (should-not (sysml-fill-paragraph))))

(ert-deftest sysml-mode-spell-check-buffer-uses-comments-and-strings ()
  "Spell checking command should delegate to comments/string checking."
  (let ((called nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_command) "/bin/true"))
              ((symbol-function 'ispell-comments-and-strings)
               (lambda ()
                 (setq called t))))
      (with-temp-buffer
        (sysml-mode)
        (cancel-function-timers #'sysml-mode-lazy-init)
        (sysml-spell-check-buffer)))
    (should called)))

(ert-deftest sysml-mode-completes-keywords-and-buffer-symbols ()
  "Completion should include SysML keywords and symbols from the buffer."
  (with-temp-buffer
    (sysml-mode)
    (cancel-function-timers #'sysml-mode-lazy-init)
    (insert "package Example {\n  part def Vehicle {}\n}\n\npa")
    (let* ((completion (sysml-completion-at-point))
           (table (nth 2 completion)))
      (should (member "package" (all-completions "pa" table)))))
  (with-temp-buffer
    (sysml-mode)
    (cancel-function-timers #'sysml-mode-lazy-init)
    (insert "package Example {\n  part def Vehicle {}\n}\n\nVe")
    (let* ((completion (sysml-completion-at-point))
           (table (nth 2 completion)))
      (should (member "Vehicle" (all-completions "Ve" table))))))

(ert-deftest sysml-mode-completion-collects-all-definition-forms ()
  "Completion symbol collection should use the shared definition grammar."
  (with-temp-buffer
    (sysml-mode)
    (cancel-function-timers #'sysml-mode-lazy-init)
    (insert (sysml-mode-tests--definition-source)
            "\npart vehicleInstance : Vehicle;\n")
    (let ((symbols (sysml-collect-buffer-symbols)))
      (dolist (definition sysml-mode-tests--all-definition-examples)
        (should (member (nth 1 definition) symbols)))
      (should (member "vehicleInstance" symbols)))))

;;; sysml-mode-tests.el ends here
