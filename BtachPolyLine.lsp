;;; Copyright 2025 李靖康 All rights reserved.
(princ "\n[提示] 已加载 BatchPoyLine 插件 (版本 0.1.0)\n")
(princ "Copyright 2025 李靖康 All rights reserved.\n")
(princ "运行 \"BATPL\" 命令以开始绘制闭合线框\n\n")

(defun c:BATPL (/ file f line points coords) 
  (alert "请打开保存有坐标（X、Y）集合的.txt文件\n\n格式要求：\n1. 每行一个坐标对\n2. 坐标间用西文逗号或空格分隔\n3. 文件编码为不带 BOM 的 UTF-8")  
  (command "ucs" "w") 
  (vl-load-com)
  (setq file (getfiled "选择坐标文件（UTF-8无BOM格式）" "" "txt" 0))
  (if (setq f (open file "r"))
    (progn
      (while (setq line (read-line f))
        (if (> (strlen line) 0)
          (progn
            (setq line (vl-string-trim " \t\r\n" line))
            (setq line (vl-string-subst " " "," line))
            (setq coords (read (strcat "(" line ")"))) ; 原代码此处缺少右括号
            (if (and (listp coords)
                     (= (length coords) 2)
                     (numberp (car coords))
                     (numberp (cadr coords)))
              (setq points (cons (list (cadr coords) (car coords)) points))
              (princ 
                (strcat 
                  "\n[错误] 无效行: \"" line "\"\n"
                  "      要求: X、Y需通过西文逗号或空格分隔，坐标之间需换行\n"
                  "      示例: 100,200 或 300 400\n"
                )
              )
            )
          )
        )
      )
      (close f) 
      (setq points (reverse points))
      (if points
        (progn
          (command "pline")
          (foreach pt points (command pt))
          (command "c")
          (command "zoom" "e")
          (command "zoom" "0.8x")
          (alert "线框绘制完成！")
        )
        (alert 
          (strcat 
            "未找到有效坐标！\n\n"
            "可能原因：\n"
            "1. 文件内容为空\n"
            "2. 所有行格式错误\n"
            "3. 分隔符使用中文标点\n"
            "解决方案：检查文件并确保符合格式要求"
          )
        )
      )
    )
    (alert 
      (strcat 
        "文件打开失败！\n\n"
        "常见解决方法：\n"
        "1. 检查文件路径是否包含特殊字符\n"
        "2. 确认文件未在其他程序中打开\n"
        "3. 确保文件编码为不带 BOM 的 UTF-8\n"
        "   （可用记事本另存为时选择编码）"
      )
    )
  )
  (princ) 
)