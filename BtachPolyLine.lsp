;;; -*- coding: gb18030 -*-
;;;    Copyright 2025~2026 李靖康
;;; 
;;;    Licensed under the Apache License, Version 2.0 (the "License");
;;;    you may not use this file except in compliance with the License.
;;;    You may obtain a copy of the License at
;;; 
;;;        http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;;    Unless required by applicable law or agreed to in writing, software
;;;    distributed under the License is distributed on an "AS IS" BASIS,
;;;    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;    See the License for the specific language governing permissions and
;;;    limitations under the License.

(princ "\n[提示] 已加载 BatchPolyLine 插件 (版本 1.0.0)\n")
(princ "Copyright 2025~2026 李靖康 All rights reserved.\n")
(princ "运行 \"BATPL\" 命令以开始绘制闭合线框\n\n")

(vl-load-com)

(setq *batpl-maxerr* 20)     ; 无效行最多逐条显示多少条
(setq *batpl-maxlen* 60)     ; 回显原始行时最多显示多少个字符

(setq *batpl-seps* (strcat "," ";" (chr 9) (chr 13) " " (chr 160)
                            "，" "；" "　" (chr 65279)))

(setq *batpl-sys*  nil
      *batpl-port* nil
      *batpl-undo* nil)

(defun batpl-blanks (n / s)
  (setq s "")
  (repeat (if (< n 1) 0 n) (setq s (strcat s " ")))
  s)

(setq *batpl-gap* (batpl-blanks (strlen *batpl-seps*)))

(defun batpl-normalize (s)
  (vl-string-trim " " (vl-string-translate *batpl-seps* *batpl-gap* s)))

(defun batpl-tokens (s / i n ch cur out)
  (setq n (strlen s) i 0 cur "" out nil)
  (while (< i n)
    (setq ch (substr s (1+ i) 1))
    (if (= ch " ")
      (if (/= cur "") (setq out (cons cur out) cur ""))
      (setq cur (strcat cur ch)))
    (setq i (1+ i)))
  (if (/= cur "") (setq out (cons cur out)))
  (reverse out))

(defun batpl-decimal (tok / i n ch ok dots digits)
  (setq n (strlen tok) i 0 ok T dots 0 digits 0)
  (if (< n 1)
    nil
    (progn
      (while (and ok (< i n))
        (setq ch (substr tok (1+ i) 1))
        (cond
          ((= ch ".") (setq dots (1+ dots)) (if (> dots 1) (setq ok nil)))
          ((member ch '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
           (setq digits (1+ digits)))
          ((and (= i 0) (or (= ch "+") (= ch "-"))) nil)
          (T (setq ok nil)))
        (setq i (1+ i)))
      (if (and ok (> digits 0)) (distof tok 1) nil))))

(defun batpl-parse-line (raw / s toks a b)
  (setq s (batpl-normalize raw))
  (setq toks (batpl-tokens s))
  (cond
    ((= (length toks) 0) (list 'SKIP))
    ((= (length toks) 1) (list 'BAD "只有 1 个数值"))
    ((> (length toks) 2)
     (list 'BAD (strcat "有 " (itoa (length toks)) " 个数值（只支持两列：X Y）")))
    (T
     (setq a (batpl-decimal (car toks))
           b (batpl-decimal (cadr toks)))
     (cond
       ((null a) (list 'BAD (strcat "\"" (car toks) "\" 不是十进制数值")))
       ((null b) (list 'BAD (strcat "\"" (cadr toks) "\" 不是十进制数值")))
       (T (list 'OK b a))))))

(defun batpl-parse-lines (lines / n r pts ok bad errs)
  (setq n 0 pts nil ok 0 bad 0 errs nil)
  (foreach ln lines
    (setq n (1+ n))
    (setq r (batpl-parse-line ln))
    (cond
      ((= (car r) 'SKIP) nil)
      ((= (car r) 'OK)
       (setq ok (1+ ok)
             pts (cons (list (cadr r) (caddr r)) pts)))
      (T
       (setq bad (1+ bad))
       (if (<= bad *batpl-maxerr*)
         (setq errs (cons (list n ln (cadr r)) errs))))))
  (list (reverse pts) ok bad n (reverse errs)))

(defun batpl-parse-file (f / ln lines)
  (setq lines nil)
  (while (setq ln (read-line f))
    (setq lines (cons ln lines)))
  (batpl-parse-lines (reverse lines)))

(defun batpl-same-pt (a b)
  (and (= (car a) (car b)) (= (cadr a) (cadr b))))

(defun batpl-dedupe (pts / out n)
  (setq out nil n 0)
  (foreach p pts
    (if (or (null out) (not (batpl-same-pt p (car out))))
      (setq out (cons p out))
      (setq n (1+ n))))
  (setq out (reverse out))
  (if (and (> (length out) 1) (batpl-same-pt (car out) (last out)))
    (progn
      (setq out (reverse (cdr (reverse out))))
      (setq n (1+ n))))
  (list out n))

(defun batpl-sys-freeze (names / lst)
  (setq lst nil)
  (foreach nm names
    (setq lst (cons (cons nm (getvar nm)) lst)))
  (setq *batpl-sys* lst)
  (foreach nm names
    (setvar nm 0))
  (princ))

(defun batpl-sys-thaw ()
  (foreach itm *batpl-sys*
    (setvar (car itm) (cdr itm)))
  (setq *batpl-sys* nil)
  (princ))

(defun batpl-restore ()
  (batpl-sys-thaw)
  (if *batpl-port*
    (progn (close *batpl-port*) (setq *batpl-port* nil)))
  (if *batpl-undo*
    (progn (setq *batpl-undo* nil)
           (command "_.undo" "_end")))
  (princ))

(defun batpl-abort (msg)
  (batpl-restore)
  (if (and msg (/= msg ""))
    (princ (strcat "\n[已中断] " msg))
    (princ "\n[已取消]"))
  (princ))

(defun batpl-print-errors (errs / itm txt)
  (foreach itm errs
    (setq txt (cadr itm))
    (if (> (strlen txt) *batpl-maxlen*)
      (setq txt (strcat (substr txt 1 *batpl-maxlen*) "...")))
    (princ (strcat "\n[错误] 第 " (itoa (car itm)) " 行无效: \"" txt "\""
                   "  (原因: " (caddr itm) ")"))))

(defun batpl-draw (pts / newent)
  (batpl-sys-freeze '("OSMODE" "ORTHOMODE" "SNAPMODE" "CMDECHO"))
  (command "_.undo" "_begin")
  (setq *batpl-undo* T)
  (if (entmake (append (list '(0 . "LWPOLYLINE")
                             '(100 . "AcDbEntity")
                             '(100 . "AcDbPolyline")
                             (cons 90 (length pts))
                             '(70 . 1)
                             '(210 0.0 0.0 1.0)
                             '(38 . 0.0))
                       (mapcar '(lambda (q) (cons 10 q)) pts)))
    (progn
      (setq newent (entlast))
      (command "_.zoom" "_o" newent "")
      (if *batpl-undo*
        (progn (command "_.undo" "_end")
               (setq *batpl-undo* nil)))
      T)
    (progn
      (if *batpl-undo*
        (progn (command "_.undo" "_end")
               (setq *batpl-undo* nil)))
      nil)))

(defun batpl-run (file / f res pts ok bad n npts ded ndup result)
  (setq f (open file "r"))
  (if (null f)
    (progn
      (princ (strcat "\n[失败] 无法打开文件：" file))
      nil)
    (progn
      (setq *batpl-port* f)
      (setq res (batpl-parse-file f))
      (close f)
      (setq *batpl-port* nil
            f nil)
      (setq pts (car res) ok (cadr res) bad (caddr res) n (cadddr res))
      (batpl-print-errors (nth 4 res))
      (if (> bad *batpl-maxerr*)
        (princ (strcat "\n[提示] 另有 " (itoa (- bad *batpl-maxerr*))
                       " 处无效行未逐条显示。")))
      (setq ded (batpl-dedupe pts)
            pts (car ded)
            ndup (cadr ded))
      (if (> ndup 0)
        (princ (strcat "\n[提示] 已合并重合的重复坐标 " (itoa ndup)
                       " 个：有效坐标 " (itoa ok) " → " (itoa (length pts)) " 个。")))
      (setq npts (length pts))
      (setq result
        (cond
          ((< npts 3)
           (princ (strcat "\n[提示] 有效坐标只有 " (itoa npts)
                          " 个，闭合线框至少需要 3 个，未绘制。"
                          "\n        有效 " (itoa ok) " 行，无效 " (itoa bad)
                          " 行，共 " (itoa n) " 行。"))
           (if (and (= ok 0) (> bad 0))
             (princ "\n[提示] 所有行都没解析出来：请检查文件编码（UTF-8）与分隔符（逗号/空格/Tab/分号）。"))
           nil)
          ((batpl-draw pts)
           (princ (strcat "\n[完成] 已绘制闭合线框：顶点 " (itoa npts)
                          " 个。有效 " (itoa ok) " 行，无效 " (itoa bad)
                          " 行，共 " (itoa n) " 行。"))
           T)
          (T
           (princ (strcat "\n[失败] entmake 创建多段线失败（当前图层 = " (getvar "CLAYER")
                          "），图纸未改动。"))
           nil)))
      (batpl-restore)
      result)))

(defun batpl-getfiled (title default ext flags / saved res)
  (setq saved (getvar "FILEDIA"))
  (setq res (getfiled title default ext flags))
  (setvar "FILEDIA" saved)
  res)

(defun c:BATPL (/ *error* file)
  (setq *error* batpl-abort)
  (princ "\n[BatchPolyLine] 坐标文件：每行一个点，两列十进制数值")
  (princ "\n                第一列 = X（对应 CAD 的 Y），第二列 = Y（对应 CAD 的 X）")
  (princ "\n                分隔符：逗号 / 空格 / Tab / 分号，全角亦可；空行自动跳过\n")
  (setq file (batpl-getfiled "选择坐标文件（X Y）" "" "txt" 0))
  (if (null file)
    (princ "\n[取消] 未选择文件，命令未执行。")
    (batpl-run file))
  (batpl-restore)
  (princ))
(princ)
