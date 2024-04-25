  $ landlock
  can read /usr/include/paths.h: true
  can read /bin/bash: true
  can write /tmp/x: true

  $ landlock --restrict
  can read /usr/include/paths.h: true
  can read /bin/bash: true
  can write /tmp/x: false
