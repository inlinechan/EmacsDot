(require 'auto-complete-clang)

(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)

(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)

(add-hook 'c++-mode-hook (lambda ()
               (add-to-list 'ac-sources 'ac-source-clang)))
(add-hook 'c++-mode-hook
      (lambda ()
        (local-set-key (kbd "C-c .") 'ac-complete-clang)))

(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
          (split-string
           "
 /usr/include/c++/4.6
 /usr/include/c++/4.6/x86_64-linux-gnu/.
 /usr/include/c++/4.6/backward
 /usr/lib/gcc/x86_64-linux-gnu/4.6/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.6/include-fixed
 /usr/include/x86_64-linux-gnu
 /usr/include
"
           )))

(dolist (dir
         (mapcar (lambda (item)(concat "-I" item))
                 (split-string
                  "
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDeclarative
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDeclarative/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDeclarative/5.2.1/QtDeclarative
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDeclarative/5.2.1/QtDeclarative/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtCLucene
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtCLucene/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtCLucene/5.2.1/QtCLucene
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtCLucene/5.2.1/QtCLucene/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSensors
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSensors/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSensors/5.2.1/QtSensors
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSensors/5.2.1/QtSensors/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWebKit
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWebKit/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWebKit/5.2.1/QtWebKit
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWebKit/5.2.1/QtWebKit/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSvg
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSvg/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSvg/5.2.1/QtSvg
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSvg/5.2.1/QtSvg/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtHelp
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtHelp/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtHelp/5.2.1/QtHelp
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtHelp/5.2.1/QtHelp/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDBus
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDBus/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDBus/5.2.1/QtDBus
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDBus/5.2.1/QtDBus/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDesigner
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDesigner/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDesigner/5.2.1/QtDesigner
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDesigner/5.2.1/QtDesigner/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtMultimedia
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtMultimedia/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtMultimedia/5.2.1/QtMultimedia
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtMultimedia/5.2.1/QtMultimedia/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtXml
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPositioning
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPositioning/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPositioning/5.2.1/QtPositioning
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPositioning/5.2.1/QtPositioning/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtMultimediaWidgets
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtMultimediaWidgets/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtMultimediaWidgets/5.2.1/QtMultimediaWidgets
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtMultimediaWidgets/5.2.1/QtMultimediaWidgets/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtCore
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtCore/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtCore/5.2.1/QtCore
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtCore/5.2.1/QtCore/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtOpenGLExtensions
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuickTest
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuickTest/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuickTest/5.2.1/QtQuickTest
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuickTest/5.2.1/QtQuickTest/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtScript
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtScript/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtScript/5.2.1/QtScript
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtScript/5.2.1/QtScript/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPrintSupport
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPrintSupport/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPrintSupport/5.2.1/QtPrintSupport
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPrintSupport/5.2.1/QtPrintSupport/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPrintSupport/5.2.1/QtPrintSupport/qpa
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQml
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQml/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQml/5.2.1/QtQml
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQml/5.2.1/QtQml/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtOpenGL
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtOpenGL/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtOpenGL/5.2.1/QtOpenGL
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtOpenGL/5.2.1/QtOpenGL/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtTest
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtTest/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtTest/5.2.1/QtTest
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtTest/5.2.1/QtTest/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuickParticles
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuickParticles/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuickParticles/5.2.1/QtQuickParticles
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuickParticles/5.2.1/QtQuickParticles/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtX11Extras
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtNfc
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtNfc/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtNfc/5.2.1/QtNfc
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtNfc/5.2.1/QtNfc/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPlatformSupport
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPlatformSupport/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPlatformSupport/5.2.1/QtPlatformSupport
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtPlatformSupport/5.2.1/QtPlatformSupport/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSql
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSql/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSql/5.2.1/QtSql
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSql/5.2.1/QtSql/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtUiTools
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtUiTools/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtUiTools/5.2.1/QtUiTools
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtUiTools/5.2.1/QtUiTools/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDesignerComponents
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDesignerComponents/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDesignerComponents/5.2.1/QtDesignerComponents
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtDesignerComponents/5.2.1/QtDesignerComponents/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWidgets
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWidgets/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWidgets/5.2.1/QtWidgets
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWidgets/5.2.1/QtWidgets/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtScriptTools
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtScriptTools/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtScriptTools/5.2.1/QtScriptTools
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtScriptTools/5.2.1/QtScriptTools/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtMultimediaQuick_p
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtZlib
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWebKitWidgets
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWebKitWidgets/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWebKitWidgets/5.2.1/QtWebKitWidgets
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtWebKitWidgets/5.2.1/QtWebKitWidgets/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtNetwork
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtNetwork/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtNetwork/5.2.1/QtNetwork
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtNetwork/5.2.1/QtNetwork/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtBluetooth
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtBluetooth/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtBluetooth/5.2.1/QtBluetooth
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtBluetooth/5.2.1/QtBluetooth/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtGui
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtGui/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtGui/5.2.1/QtGui
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtGui/5.2.1/QtGui/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtGui/5.2.1/QtGui/qpa
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSerialPort
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSerialPort/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSerialPort/5.2.1/QtSerialPort
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtSerialPort/5.2.1/QtSerialPort/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtConcurrent
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuick
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuick/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuick/5.2.1/QtQuick
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtQuick/5.2.1/QtQuick/private
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtXmlPatterns
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtXmlPatterns/5.2.1
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtXmlPatterns/5.2.1/QtXmlPatterns
/home/hyungchan/Qt5.2.1/5.2.1/gcc_64/include/QtXmlPatterns/5.2.1/QtXmlPatterns/private
"
                  )))
  (add-to-list 'ac-clang-flags dir))
