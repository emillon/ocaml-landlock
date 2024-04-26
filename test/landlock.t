  $ alias landlock=../bin/demo.exe
  $ mkdir ro-dir
  $ touch ro-dir/contents
  $ landlock --rx /usr --ro ro-dir ls ro-dir
  contents
  $ landlock --rx /usr --ro ro-dir ls /tmp
  ls: cannot open directory '/tmp': Permission denied
  [2]
