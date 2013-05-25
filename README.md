seognil
=======

seognil -- the reverse of lingoes -- a pure emacs lisp program to query words in massaged dictionaries

Explanation in Chinese
======================
发信人: Madsen (format t "A walker"), 信区: Emacs
标  题: 弄了个算是纯elisp的查本地某格式词典的程序
发信站: 水木社区 (Sat Dec  8 09:55:16 2012), 转信

之所以说是“算是纯elisp”是因为程序里调用了w3m来渲染查出来的结果。
（在windows上可以用MinGW编译一个w3m来做这事儿）

不能直接用某格式的词典，而是要先对其预处理一番。预处理的流程写在了
程序的注释里。

速度嘛，不快，不过我自己觉得能接受（4G内存的PC上）。
