# eglot-codelens 测试文档

本文档介绍 `eglot-codelens-test.el` 的测试内容和结构。

## 测试概览

测试套件使用 Emacs 内置的 ERT (Emacs Lisp Regression Testing) 框架编写，共包含 **37 个测试用例**，覆盖以下功能模块：

| 模块             | 测试数量 | 说明                                     |
|------------------|----------|------------------------------------------|
| 缓存管理         | 5        | CodeLens 缓存的构建、分组和排序          |
| UI 显示          | 6        | 文本格式化、图标转换、显示字符串         |
| Overlay 管理     | 3        | Overlay 创建、属性设置、清理             |
| 渲染系统         | 5        | Overlay 创建/复用/删除、可见范围渲染     |
| CodeLens 解析    | 3        | 解析队列的处理和清理                     |
| 交互处理         | 2        | CodeLens 命令执行                        |
| 辅助函数         | 2        | 可见范围计算和边界检查                   |
| 行数增量优化     | 8        | 基于光标位置的 overlay 复用优化测试      |
| Mock 功能        | 3        | Mock 辅助函数验证                        |

## 运行测试

### 使用 Eask (推荐)

```bash
eask test ert tests/eglot-codelens-test.el
```

### 手动运行

```bash
emacs -batch -l eglot-codelens.el \
    -l tests/eglot-codelens-test.el \
    -f ert-run-tests-batch-and-exit
```

### GUI 模式下运行

```bash
# 在 Emacs 中
M-x ert
# 选择测试或运行所有测试
```

## 测试场景

### 1. 缓存管理测试

测试 `eglot-codelens--build-cache` 函数：

```elisp
(ert-deftest eglot-codelens-test-build-cache-multiple-same-line ()
  "Test building cache with multiple CodeLens on same line."
  ;; 同一行有多个 CodeLens 时，验证:
  ;; - 按行正确分组
  ;; - 保持 LSP 返回顺序
  ;; - 正确设置索引
  )
```

**使用场景**: 模拟 A.java 第 1 行有 2 个 CodeLens (references + implementations)

### 2. 渲染系统测试

测试 `eglot-codelens--render-codelens` 函数，需要 mock `eglot-codelens--visible-range`：

```elisp
(ert-deftest eglot-codelens-test-render-only-visible-range ()
  "Test that rendering only creates overlays in visible range."
  ;; 模拟可见范围为 (3 . 6)
  ;; 验证只渲染第 3、6 行的 CodeLens
  ;; 第 1 行的 CodeLens 不应创建 overlay
  )
```

**使用场景**:
- 模拟用户滚动窗口，只渲染可见区域
- 验证范围外 CodeLens 不被渲染

### 3. Overlay 复用测试

测试 overlay 的复用逻辑：

```elisp
(ert-deftest eglot-codelens-test-render-reuse-existing-overlays ()
  "Test that rendering reuses existing overlays."
  ;; 第一次渲染: 创建 overlays
  ;; 第二次渲染: 复用已有 overlays，更新 docver
  ;; 验证 overlay 数量保持不变
  )
```

**使用场景**: 文档变化后重新获取 CodeLens，复用已有 overlay

### 4. 过期 Overlay 删除测试

```elisp
(ert-deftest eglot-codelens-test-render-delete-stale-overlays ()
  "Test that stale overlays (not in new cache) are deleted."
  ;; 第一次渲染: 第 1、3 行有 CodeLens
  ;; 第二次渲染: 只有第 1 行有 CodeLens (使用不同 docver)
  ;; 验证第 3 行的 overlay 被删除
  )
```

**使用场景**: CodeLens 被移除后，对应的 overlay 应该被删除

### 5. 可见范围扩展测试

```elisp
(ert-deftest eglot-codelens-test-visible-range-extended ()
  "Test visible range with extension."
  ;; 测试可见范围的扩展功能:
  ;; - 扩展范围的起始行 <= 正常范围的起始行 (向前扩展)
  ;; - 扩展范围的结束行 >= 正常范围的结束行 (向后扩展)
  ;; - 扩展范围的跨度 >= 正常范围的跨度
  ;; - 扩展范围不超出缓冲区边界 (>= 1, <= buffer-max-line)
  )
```

**重要**: 此测试在 GUI 模式下运行，在 batch mode 下自动跳过。

### 6. 行数增量优化测试

测试基于光标位置的 overlay 复用优化逻辑：

```elisp
(ert-deftest eglot-codelens-test-render-cursor-line-no-delta ()
  "Test that CodeLens at cursor line is not adjusted by delta."
  ;; 验证修复: (>= line cursor-line) -> (> line cursor-line)
  ;; 光标所在行不应用 delta 调整，只有光标之后的行才调整
  ;;
  ;; 场景: 光标在 line 5，插入 2 行后 (delta = +2)
  ;; - Line 5 (光标行): 查找 line 5 (不调整) ✓ 找到匹配
  ;; - Line 7 (光标后): 查找 line 5 (7-2=5) ✗ 已被占用
  ;; 结果: 一个复用，一个新建，总数仍为 2
  )
```

**使用场景**:
- 文档编辑时插入/删除行，通过计算行数增量来优化 overlay 查找
- 光标之前的行不受影响，光标之后的行按增量调整查找
- 避免全量重建 overlay，提升性能

相关测试用例：
- `eglot-codelens-test-prev-line-count-initialization` - 初始化行数计数
- `eglot-codelens-test-prev-line-count-update` - 更新行数计数
- `eglot-codelens-test-render-reuse-with-line-insertion` - 行插入时的复用
- `eglot-codelens-test-render-reuse-with-line-deletion` - 行删除时的复用
- `eglot-codelens-test-render-cursor-boundary-lookup` - 光标边界查找
- `eglot-codelens-test-render-no-delta-same-line-count` - 无增量时直接复用
- `eglot-codelens-test-render-cursor-line-no-delta` - 光标行不调整
- `eglot-codelens-test-cleanup-buffer-clears-prev-line-count` - 清理时重置计数

## Mock 辅助函数

### `eglot-codelens-test--mock-codelens`

创建模拟的 CodeLens 数据结构：

```elisp
(eglot-codelens-test--mock-codelens line title &optional command)
;; line: Emacs 行号 (1-based)
;; title: 显示文本
;; command: 可选的命令 plist
```

**示例**:
```elisp
;; 创建一个带命令的 CodeLens
(eglot-codelens-test--mock-codelens 3 "5 references")

;; 创建一个未解析的 CodeLens (无命令)
(eglot-codelens-test--mock-codelens-unresolved 3)
```

### `eglot-codelens-test--setup-test-buffer`

创建模拟 A.java 结构的测试缓冲区：

```elisp
 Line 1: "public class A {\n"
 Line 2: "\n"
 Line 3: "    class B {\n"
 Line 4: "    }\n"
 Line 5: "\n"
 Line 6: "    public void test() {\n"
 ...
```

### `eglot-codelens-test--get-overlay-at-line`

获取指定行的第一个 CodeLens overlay。注意：此函数使用 `overlays-in` 检查整行而非仅行首位置，确保能正确找到所有相关 overlay。

## 测试数据

测试使用 `tests/A.java` 作为参考数据：

```java
public class A {          // Line 1: 2 CodeLens (X references, Y implementations)

    class B {             // Line 3: 2 CodeLens
    }

    public void test() {  // Line 6: 2 CodeLens
        // var b = new B();
    }
}
```

## 行号处理

**重要**: LSP 使用 0-based 行号，Emacs 使用 1-based 行号。

```elisp
;; Mock 函数中正确转换
(list :start (list :line (1- line) :character 0)  ; LSP 0-based
      ...)
```

## 运行模式差异

### Batch Mode 测试

在 batch mode 下运行时：
1. **窗口相关测试**: `visible-range` 测试被跳过（因为 `noninteractive` 为 `t`）
2. **Mock `eglot-codelens--visible-range`**: 渲染测试中模拟可见范围
3. **Buffer-local 变量**: 使用 `setq` 而非 `let` 绑定
4. **防止自动解析**: Mock `eglot-codelens--resolve-schedule`

### GUI 模式测试

在 GUI 模式下运行时：
1. **窗口创建**: 使用 `save-window-excursion` 保存窗口配置
2. **临时窗口**: 使用 `set-window-buffer` 将临时缓冲区显示在窗口中
3. **测试完成后恢复**: `save-window-excursion` 自动恢复原窗口配置

## 常见问题

### Q: 为什么可见范围测试在 GUI 和 batch 模式下表现不同？

A: `with-temp-buffer` 创建的缓冲区没有关联窗口。GUI 模式下需要手动创建临时窗口来测试窗口相关函数。测试使用 `(unless noninteractive ...)` 来区分两种模式。

### Q: 为什么扩展范围测试检查相对关系而非绝对值？

A: 由于 `line-number-at-pos (point-max)` 的行为（在 30 行内容后返回 31），测试比较扩展前后的相对变化，验证扩展逻辑正确性而非具体的行号。

### Q: 为什么需要 mock `eglot-codelens--visible-range`？

A: 在 batch mode 下，窗口不存在，`window-start`/`window-end` 会返回意外值。Mock 可以精确控制测试的可见范围。
渲染测试中需要精确控制可见范围以测试边界条件（如部分渲染、范围外 overlay 复用等）。

### Q: 为什么测试中使用不同的 docver？

A: `eglot-codelens--render-codelens` 使用 `docver` 和 `usever` 的比较来删除过期 overlay。使用不同 docver 可以触发这个逻辑。

### Q: 如何添加新的测试？

A: 按照 ERT 格式添加测试：

```elisp
(ert-deftest eglot-codelens-test-your-test-name ()
  "Brief description of what this tests."
  ;; 测试代码
  (should (some-condition))
  (should (= expected actual)))
```

对于需要窗口的测试，使用 `save-window-excursion` 和 `set-window-buffer`：

```elisp
(ert-deftest eglot-codelens-test-window-function ()
  "Test window-dependent function."
  (unless noninteractive  ; Skip in batch mode
    (save-window-excursion   ; Preserve window config
      (with-temp-buffer
        ;; Create test content
        (set-window-buffer (selected-window) (current-buffer))
        ;; Run test
        )))))
```

## 相关文件

- `eglot-codelens.el` - 主程序文件
- `tests/eglot-codelens-test.el` - 测试文件
- `Eask` - Eask 配置文件
