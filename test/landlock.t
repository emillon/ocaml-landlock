  $ landlock
  can read /usr/include/paths.h: true
  can read /etc/hosts: true
  can write /tmp/x: true

  $ landlock --restrict
  can read /usr/include/paths.h: true
  can read /etc/hosts: false
  can write /tmp/x: false
