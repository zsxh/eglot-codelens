# Eglot-CodeLens MVP 设计文档

**版本: v0.2**

> 本文档实现并非最终实现，仅为初始阶段与 ai 讨论的归档记录

## 项目概述

基于 LSP 3.17 CodeLens 规范，设计并实现 eglot-codelens.el - 一个独立的 Eglot 扩展包，为 Emacs 的 Eglot LSP 客户端添加 CodeLens 功能支持。采用非侵入式设计，通过 Eglot 提供的扩展机制实现功能集成。

## 功能需求分析

### 1. 基础 CodeLens 支持

根据 LSP 3.17 CodeLens 规范，eglot-codelens 需要实现：

- **完整的 LSP 协议支持**：
  - `textDocument/codeLens` 请求：获取文档中的 CodeLens 列表
  - `codeLens/resolve` 请求：解析 CodeLens 的详细信息（延迟解析）
  - 支持两阶段解析机制，优化性能

### 2. 用户界面集成

- **Overlay 显示系统**：
  - 使用 Emacs overlay 系统显示 CodeLens
  - 类似 VS Code 的显示位置：在代码上方显示
  - 支持自定义样式和外观

- **交互功能**：
  - 鼠标点击执行 CodeLens 命令
  - Return 键执行当前 CodeLens

- **视觉一致性**：
  - 与现有 Emacs UI 风格保持一致
  - 参考现有 eglot inlay hints 的设计模式
  - 支持明暗主题适配

### 3. 性能优化

- **延迟解析机制**：
  - 初始只显示 CodeLens 占位符
  - 用户交互时才解析详细信息
  - 支持按需加载，减少初始响应时间
  - 实现解析状态管理和 loading 指示

- **简单的版本检查**：
  - 基于 LSP 文档版本号判断 CodeLens 是否失效
  - 文档版本变化时重新获取 CodeLens
  - 避免复杂的缓存逻辑，保持实现简洁

### 4. 配置和定制化

- **显示选项**：
  - 自定义样式和外观
  - 配置显示延迟（用户输入后多长时间显示 CodeLens）

- **集成配置**：
  - 与 eglot 现有配置系统集成
  - 支持通过 `customize-group` 界面配置

## 技术架构设计

### 1. 项目结构

采用单文件设计，所有功能集成在一个主文件中：

```
eglot-codelens/
└── eglot-codelens.el          # 主扩展文件，包含所有功能模块：
                               # - LSP 协议处理
                               # - 配置系统
                               # - UI 显示和交互
                               # - Eglot 集成逻辑
```

### 2. 核心数据结构

#### 缓存管理系统
```lisp
;; 直接存储 LSP CodeLens 对象（plist 格式）
;; 格式示例：
;; '(:range ((start . ((line . 10) (character . 0)))
;;           (end . ((line . 10) (character . 20))))
;;   :command ((title . "3 references") ...)
;;   :data (...))
(defvar-local eglot-codelens--cache nil
  "Cache for raw LSP CodeLens objects in current buffer.")

;; 文档版本管理
(defvar-local eglot-codelens--version nil
  "Document version for cached CodeLens.")
```

#### 数据访问辅助函数
```lisp
(defun eglot-codelens--get-range (codelens)
  "Get range from CODELENS plist."
  (plist-get codelens :range))

(defun eglot-codelens--get-command (codelens)
  "Get command from CODELENS plist."
  (plist-get codelens :command))

(defun eglot-codelens--resolved-p (codelens)
  "Check if CODELENS is resolved."
  (plist-get codelens :command))
```

### 3. LSP 协议实现

#### textDocument/codeLens 请求处理
```lisp
(defun eglot-codelens--fetch-codelens (server buffer)
  "Fetch CodeLens for BUFFER from SERVER."
  (let ((params (list :textDocument
                      (list :uri (eglot-path-to-uri (buffer-file-name buffer))))))
    (eglot--request server 'textDocument/codeLens params)))
```

#### codeLens/resolve 请求处理
```lisp
(defun eglot-codelens--resolve-codelens (server codelens)
  "Resolve CODELENS details from SERVER."
  (eglot--request server 'codeLens/resolve codelens))
```

### 4. UI 显示系统

#### Overlay 创建和管理
采用类似 VS Code 的显示策略，使用 `before-string` 在代码上方显示，确保严格按 LSP 返回顺序：

```lisp
(defun eglot-codelens--make-overlay (line-start codelens index total-codelens)
  "Create overlay for CODELENS at LINE-START with INDEX and TOTAL-CODELENS count."
  (let ((ov (make-overlay line-start line-start)))

    ;; priority 从小递增：0, 1, 2... 与 LSP 返回顺序一致
    (overlay-put ov 'priority index)

    ;; 添加专属标识，方便后期清理
    (overlay-put ov 'eglot-codelens t)

    ;; 构建显示字符串
    (let* ((is-first (= index 0))
           (is-last (= index (1- total-codelens)))
           (indentation (if is-first
                            (save-excursion
                              (goto-char line-start)
                              (make-string (current-indentation) ? ))
                          ""))
           (separator (if is-last "" " | ")))

      (overlay-put ov 'before-string
                   (propertize (concat indentation
                                      (eglot-codelens--format-text codelens)
                                      separator)
                               'face 'eglot-codelens-face
                               'mouse-face 'eglot-codelens-mouse-face
                               'help-echo (eglot-codelens--help-text codelens)
                               'keymap eglot-codelens-map
                               'eglot-codelens codelens)))
    ov))
```

#### Overlay 清理机制
```lisp
(defun eglot-codelens--cleanup-overlays ()
  "Clean up all CodeLens overlays in current buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'eglot-codelens)
      (delete-overlay ov))))
```

#### 样式定义
```lisp
(defface eglot-codelens-face
  '((t :inherit font-lock-comment-face :height 0.9))
  "Face for displaying CodeLens information.")

(defface eglot-codelens-mouse-face
  '((t :inherit highlight :box (:line-width 1 :color "gray")))
  "Face for CodeLens when mouse is over them.")
```

### 5. 交互处理

#### 键盘和鼠标事件处理
```lisp
(defvar eglot-codelens-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'eglot-codelens-execute)
    (define-key map [return] #'eglot-codelens-execute)
    map)
  "Keymap for CodeLens interactions.")
```

#### 命令执行
```lisp
(defun eglot-codelens-execute ()
  "Execute CodeLens command at point."
  (interactive)
  (let ((codelens (get-text-property (point) 'eglot-codelens)))
    (when codelens
      ;; 执行命令逻辑
      )))
```

### 6. Eglot 集成策略

#### eglot-codelens-mode 定义
```lisp
;;;###autoload
(define-minor-mode eglot-codelens-mode
  "Minor mode for displaying LSP CodeLens in Eglot."
  :lighter " CL"
  :keymap eglot-codelens-map
  (cond
   (eglot-codelens-mode
    ;; 启用模式时的初始化
    (when (eglot-codelens--server-supports-codelens)
      (eglot-codelens--setup-buffer)
      (eglot-codelens--update-buffer)))
   (t
    ;; 禁用模式时的清理
    (eglot-codelens--cleanup-buffer))))
```

#### 钩子系统集成
通过 `eglot-managed-mode-hook` 实现自动启用/禁用：

```lisp
(defun eglot-codelens--managed-mode-handler ()
  "Handle eglot-managed-mode changes."
  (if eglot-managed-mode
      ;; Eglot 管理模式启用时，检查服务器支持并启用 CodeLens
      (when (eglot-codelens--server-supports-codelens)
        (eglot-codelens-mode 1))
    ;; Eglot 管理模式禁用时，清理 CodeLens
    (eglot-codelens-mode -1)))

;; 使用 Eglot 钩子
(add-hook 'eglot-managed-mode-hook #'eglot-codelens--managed-mode-handler)
```

#### 缓冲区设置和清理
```lisp
(defun eglot-codelens--setup-buffer ()
  "Setup CodeLens for current buffer."
  (when (eglot-codelens-mode)
    ;; 初始化缓冲区本地变量
    (setq eglot-codelens--cache nil
          eglot-codelens--version nil)
    ;; 添加 Eglot 文档变更钩子
    (add-hook 'eglot--document-changed-hook #'eglot-codelens--on-document-change nil t)))

(defun eglot-codelens--cleanup-buffer ()
  "Cleanup CodeLens for current buffer."
  ;; 移除所有 overlay
  (eglot-codelens--cleanup-overlays)
  ;; 清理缓存和版本
  (setq eglot-codelens--cache nil
        eglot-codelens--version nil)
  ;; 移除 Eglot 文档变更钩子
  (remove-hook 'eglot--document-changed-hook #'eglot-codelens--on-document-change t))

(defun eglot-codelens--on-document-change (&rest _args)
  "Handle document changes via Eglot's document-changed hook."
  (when (and eglot-codelens-mode
             (eglot-codelens--server-supports-codelens))
    (eglot-codelens--update-buffer)))

(defun eglot-codelens--update-buffer ()
  "Update CodeLens display in current buffer."
  (when-let* ((server (eglot-current-server))
              (codelens-list (eglot-codelens--fetch-codelens server (current-buffer))))
    (eglot-codelens--cleanup-overlays)
    (setq eglot-codelens--cache codelens-list
          eglot-codelens--version (eglot--version-of-file (buffer-file-name)))
    (eglot-codelens--render-codelens codelens-list)))
```

#### 服务器能力检测
```lisp
(defun eglot-codelens--server-supports-codelens (&optional server)
  "Check if SERVER supports CodeLens."
  (let ((server (or server (eglot-current-server))))
    (and server
         (eglot-server-capable server :codeLensProvider))))
```
