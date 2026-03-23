(require 'ert)

(defvar sysml-mode-initialized)

(declare-function sysml-mode "sysml-mode")
(declare-function sysml-mode-lazy-init "sysml-mode" (buffer))

(load-file
 (expand-file-name "../sysml-mode.el"
                   (file-name-directory (or load-file-name buffer-file-name))))

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
            (should (featurep 'hideshow))))
      (when (buffer-live-p sysml-buffer)
        (kill-buffer sysml-buffer))
      (when (buffer-live-p other-buffer)
        (kill-buffer other-buffer)))))
