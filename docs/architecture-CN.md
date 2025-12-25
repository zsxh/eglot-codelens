# Eglot-CodeLens 架构文档

**eglot-codelens: v0.3版本**

## 目录

- [架构概览](#架构概览)
- [核心数据结构](#核心数据结构)
- [模块架构](#模块架构)
  - [1. LSP 协议处理模块](#1-lsp-协议处理模块)
  - [2. UI 显示系统](#2-ui-显示系统)
  - [3. 交互处理模块](#3-交互处理模块)
  - [4. Eglot 集成模块](#4-eglot-集成模块)
- [性能优化策略](#性能优化策略)
- [数据流图](#数据流图)

---

## 架构概览

eglot-codelens 采用分层模块化架构，将功能划分为五个核心模块：

```
┌─────────────────────────────────────────────────────────────┐
│                     用户交互层                                │
│  - 鼠标点击执行                                               │
│  - 键盘命令执行                                               │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     UI 显示层                                │
│  - Overlay 创建与复用                                        │
│  - 按行分组的缓存管理                                         │
│  - 可见区域增量更新                                           │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   业务逻辑层                                  │
│  - CodeLens 解析队列管理                                      │
│  - 文档版本追踪                                              │
│  - 延迟更新调度                                              │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   LSP 协议层                                 │
│  - textDocument/codeLens 请求                                │
│  - codeLens/resolve 请求                                     │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   Eglot 集成层                               │
│  - 文档变更钩子                                              │
│  - 窗口滚动/配置变更处理                                      │
└─────────────────────────────────────────────────────────────┘
```

---

## 核心数据结构

### 1. 缓存数据结构

#### 行式分组哈希表
```lisp
(defvar-local eglot-codelens--cache nil
  "CodeLens 对象按行分组的缓存。
哈希表结构：
  key: 行号 (整数)
  value: 已排序的 CODELENS-OVERLAY-CELL 列表
         CODELENS-OVERLAY-CELL = (CODELENS . OVERLAY)
         按 LSP 返回顺序排序（通过 index 索引）")
```

**设计理由**：
- O(1) 按行查找，支持快速执行命令
- 保持 LSP 返回顺序，确保显示顺序正确
- Cell 结构支持 overlay 复用，提升性能

#### 文档版本追踪
```lisp
(defvar-local eglot-codelens--version nil
  "文档版本号，用于判断缓存有效性。
当文档版本变化时，缓存失效。")
```

#### 待处理行追踪
```lisp
(defvar-local eglot-codelens--pending-lines nil
  "待渲染的行号列表。
用于增量更新，仅处理可见区域内的行。")
```

### 2. 队列数据结构

#### 解析队列
```lisp
(defvar-local eglot-codelens--resolve-queue nil
  "待解析的 CodeLens 队列。
元素格式：(DOCVER . CODELENS-CELL)
用于批量解析请求，配合速率限制使用。")
```

### 3. Overlay 属性

每个 CodeLens overlay 包含以下属性：

| 属性 | 用途 |
|------|------|
| `eglot-codelens` | 标识符，用于清理时识别 |
| `eglot-codelens-docver` | 文档版本，验证显示数据是否过期 |
| `eglot-codelens-usever` | 缓存版本，用于标记 overlay 是否仍在当前缓存中引用 |
| `eglot-codelens-command` | 缓存的命令信息，用于执行 |
| `priority` | 显示优先级（0, 1, 2...），匹配 LSP 返回顺序 |

---

## 模块架构

### 1. LSP 协议处理模块

#### 1.1 CodeLens 获取

**函数**: `eglot-codelens--fetch-codelens`

**流程**:
```
开始
  │
  ▼
检查服务器和文档版本 ──► 不满足 ──► 结束
  │ 满足
  ▼
发送 textDocument/codeLens 请求
  │
  ▼
等待服务器响应
  │
  ▼
构建行式分组缓存 (build-cache)
  │
  ▼
获取可见区域范围
  │
  ▼
调用 render-codelens 渲染
  │
  ▼
更新缓存、版本、待处理行
  │
  ▼
结束
```

**关键实现** (eglot-codelens.el:191-222):
```lisp
(defun eglot-codelens--fetch-codelens ()
  "从服务器获取 CodeLens 并更新显示。"
  (eglot--async-request
   server :textDocument/codeLens ...
   :success-fn (lambda (codelens-list)
                 ;; 构建新缓存
                 (let ((new-cache (eglot-codelens--build-cache codelens-list))
                       (range (eglot-codelens--visible-range)))
                   ;; 增量渲染
                   (eglot-codelens--render-codelens
                    new-cache docver all-lines old-cache range)))))
```

#### 1.2 CodeLens 解析队列

**函数**: `eglot-codelens--resolve-schedule`

**速率限制机制**:
```
解析队列处理
  │
  ▼
立即处理队列首项
  │
  ▼
设置定时器（延迟 = eglot-codelens-resolve-delay）
  │
  ▼
定时器触发 ──► 检查队列 ──► 非空 ──► 递归调度
  │                                │
  │                                ▼
  │                             处理下一项
  │
  ▼
为空 ──► 取消定时器，结束
```

**关键实现** (eglot-codelens.el:157-176):
- 使用 `run-with-timer` 实现速率限制
- 防止重复调度（检查 timer 是否已存在）
- 自动清理过期的队列项（通过 docver 版本检查）

---

### 2. UI 显示系统

#### 2.1 缓存构建

**函数**: `eglot-codelens--build-cache`

**算法**:
```lisp
输入: CODELENS-LIST (LSP 返回的向量)
输出: 行式分组哈希表

步骤:
1. 创建空哈希表 line-groups
2. 遍历 CODELENS-LIST:
   a. 提取行号 (range.start.line + 1)
   b. 构建 cell (CODELENS . nil)
   c. 添加到对应行的列表 (push 到表头)
3. 反转每行的列表 (恢复 LSP 顺序)
4. 返回 line-groups
```

#### 2.2 Overlay 创建

**函数**: `eglot-codelens--make-overlay`

**显示字符串构建**:
```lisp
;; 单行多个 CodeLens 的显示格式
indentation + title + separator

;; 示例: 行首缩进 + "3 references" + " | " + "Run Tests" + 换行
"    3 references | Run Tests\n"
```

**优先级设置**:
- 第一个 CodeLens: priority = 0
- 第二个 CodeLens: priority = 1
- ...以此类推
- 确保 Emacs 按正确顺序渲染 overlay

#### 2.3 增量渲染引擎

**函数**: `eglot-codelens--render-codelens`

**四步渲染流程**:

```
Step 1: 处理待处理行
  │
  ├─ 遍历 pending-lines
  │  │
  │  ├─ 在 new-cache 中查找该行的 CodeLens 列表
  │  │
  │  ├─ 计算行首位置
  │  │
  │  ├─ 判断是否在可见范围 (range)
  │  │
  │  └─ 对每个 CodeLens (按索引):
  │     │
  │     ├─ 在范围内:
  │     │  ├─ 复用 new-cache 中的 overlay (更新 docver 和显示)
  │     │  ├─ 复用 old-cache 中的 overlay (更新 docver、usever 和显示)
  │     │  └─ 创建新 overlay
  │     │
  │     └─ 在范围外:
  │        └─ 复用 old-cache 中的 overlay (仅更新 usever)
  │
  ▼
Step 2: 清理过期 overlay
  │
  └─ 遍历所有 overlay，删除 usever != docver 的 overlay
  │
  ▼
Step 3: 更新 pending-lines
  │
  └─ 从 pending-lines 中移除已处理的行
  │
  ▼
Step 4: 调度解析队列
  │
  └─ 将需要解析的 CodeLens 加入队列并启动调度
```

**Overlay 复用策略**:

| 场景                           | 处理方式                       |
|--------------------------------|--------------------------------|
| 在范围内，new-cache 有 overlay | 更新显示字符串和 docver        |
| 在范围内，old-cache 有 overlay | 复用，更新显示、docver、usever |
| 在范围内，无现有 overlay       | 创建新 overlay                 |
| 在范围外，old-cache 有 overlay | 复用，仅更新 usever            |
| 在范围外，无现有 overlay       | 不创建 overlay                 |

**性能优势**:
- 避免重复创建/删除 overlay
- 仅更新可见区域，减少渲染开销
- 使用 usever 机制高效清理过期 overlay

#### 2.4 可见区域刷新

**函数**: `eglot-codelens--refresh-visible-area`

**用途**: 处理窗口滚动/配置变更时的高效刷新

**流程**:
```
用户滚动/调整窗口
  │
  ▼
触发 window-scroll-functions / window-configuration-change-hook
  │
  ▼
调度延迟刷新 (debounce)
  │
  ▼
获取新的可见区域范围
  │
  ▼
过滤 pending-lines，仅保留可见区域内的行
  │
  ▼
调用 render-codelens (使用现有缓存，range 参数)
  │
  ▼
仅更新可见区域内的 overlay
```

---

### 3. 交互处理模块

#### 3.1 命令执行

**函数**: `eglot-codelens-execute`

**执行逻辑**:
```
用户点击 CodeLens
  │
  ▼
检查命令是否已解析
  │
  ├─ 是 ──► 执行命令 (eglot-execute)
  │
  └─ 否 ──► 检查服务器是否支持 resolve
            │
            ├─ 是 ──► 触发解析 ──► "Resolving..."
            │
            └─ 否 ──► "CodeLens command not available"
```

#### 3.2 行级执行

**函数**: `eglot-codelens-execute-at-line`

**交互流程**:
```
用户执行命令
  │
  ▼
获取当前行号
  │
  ▼
在 cache 中查找该行的 CodeLens 列表
  │
  ├─ 找到 1 个 ──► 直接执行
  │
  └─ 找到多个 ──► 显示选择菜单 (completing-read)
                  │
                  └─ 用户选择后执行
```

---

### 4. Eglot 集成模块

#### 4.1 缓冲区设置

**函数**: `eglot-codelens--setup-buffer`

**初始化内容**:
- 重置缓存和版本号
- 添加文档变更钩子
- 添加窗口滚动钩子
- 添加窗口配置变更钩子

#### 4.2 文档变更处理

**函数**: `eglot-codelens--on-document-change`

**防抖机制**:
```
文档变更
  │
  ▼
检查是否有定时器
  │
  ├─ 是 ──► 重置定时器时间 (timer-set-time)
  │
  └─ 否 ──► 创建新定时器
             │
             └─ 延迟后执行 fetch-codelens
```

**延迟配置**: `eglot-codelens-update-delay` (默认 0.5 秒)

#### 4.3 可见区域刷新调度

**函数**: `eglot-codelens--schedule-visible-refresh`

**防抖机制**:
- 与文档变更处理相同
- 使用 `timer-set-time` 重置现有定时器，避免取消/重建
- 延迟配置: `eglot-codelens-visible-refresh-delay` (默认 0.25 秒)

#### 4.4 Minor Mode

**函数**: `eglot-codelens-mode`

**启用条件**:
- `eglot-managed-mode` 激活
- 服务器支持 `codeLensProvider` 能力

---

## 性能优化策略

### 1. 增量渲染

- **仅渲染可见区域**: 减少初始渲染开销
- **Overlay 复用**: 避免重复创建/删除，提升性能
- **按需解析**: 未显示的 CodeLens 不解析

### 2. 防抖/节流

| 场景          | 配置项                                 | 默认值 | 作用           |
|---------------|----------------------------------------|--------|----------------|
| 文档变更      | `eglot-codelens-update-delay`          | 0.5s   | 减少频繁更新   |
| 窗口滚动      | `eglot-codelens-visible-refresh-delay` | 0.25s  | 快速响应用户   |
| CodeLens 解析 | `eglot-codelens-resolve-delay`         | 0.25s  | 限制服务器压力 |

### 3. 缓存机制

- **行式分组哈希表**: O(1) 查找性能
- **版本追踪**: 避免无效渲染
- **命令缓存**: overlay 存储命令信息，减少重复解析

### 4. 异步处理

- **LSP 请求**: 使用 `eglot--async-request`
- **队列管理**: 解析请求排队，避免阻塞 UI
- **缓冲区检查**: 回调中验证缓冲区存活状态

---

## 数据流图

### 完整数据流

```
用户操作 / 事件
      │
      ▼
┌─────────────────────────────────────────────────────────┐
│ Eglot 文档变更 / 窗口滚动 / 配置变更                        │
└─────────────────────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────────────────────┐
│ 防抖延迟 (timer-set-time / run-with-idle-timer)          │
└─────────────────────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────────────────────┐
│ fetch-codelens / refresh-visible-area                   │
└─────────────────────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────────────────────┐
│ LSP 服务器响应 (CodeLens 列表)                            │
└─────────────────────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────────────────────┐
│ build-cache (构建行式分组哈希表)                           │
└─────────────────────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────────────────────┐
│ render-codelens (增量渲染)                               │
│  - 复用现有 overlay                                      │
│  - 创建新 overlay                                        │
│  - 清理过期 overlay                                      │
└─────────────────────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────────────────────┐
│ resolve-schedule (解析未解析的 CodeLens)                  │
│  - 加入队列                                              │
│  - 速率限制处理                                           │
└─────────────────────────────────────────────────────────┘
      │
      ▼
┌─────────────────────────────────────────────────────────┐
│ update-resolved-codelens (更新已解析的 CodeLens)          │
│  - 更新 overlay 显示字符串                                │
│  - 缓存命令信息                                           │
└─────────────────────────────────────────────────────────┘
      │
      ▼
用户交互 (点击/键盘执行)
```

### Overlay 生命周期

```
创建 (make-overlay)
  │
  ▼
初始状态 (docver = X, usever = X, 显示 "Loading...")
  │
  ▼
需要解析 ──► 加入解析队列 ──► 解析完成 ──► 更新显示
  │
  ▼
已解析状态 (docver = X, usever = X, 显示实际命令)
  │
  ▼
文档变更 ──► docver ≠ 当前版本
  │
  ▼
新渲染周期
  │
  ├─ 在可见范围 ──► 更新 docver, usever, 显示
  │
  └─ 不在可见范围 ──► 仅更新 usever
  │
  ▼
下一个渲染周期
  │
  ├─ usever = 当前版本 ──► 保持存活
  │
  └─ usever ≠ 当前版本 ──► 删除 (delete-overlay)
```

