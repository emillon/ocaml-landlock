#define _GNU_SOURCE
#include <caml/fail.h>
#include <caml/memory.h>
#include <fcntl.h>
#include <linux/landlock.h>
#include <sys/syscall.h>
#include <unistd.h>

#ifndef landlock_add_rule
static inline int landlock_add_rule(const int ruleset_fd,
                                    const enum landlock_rule_type rule_type,
                                    const void *const rule_attr,
                                    const __u32 flags) {
  return syscall(__NR_landlock_add_rule, ruleset_fd, rule_type, rule_attr,
                 flags);
}
#endif

value setup_landlock(value v_ruleset_fd) {
  CAMLparam1(v_ruleset_fd);
  int ruleset_fd = Int_val(v_ruleset_fd);

  int err;
  struct landlock_path_beneath_attr path_beneath = {
      .allowed_access = LANDLOCK_ACCESS_FS_EXECUTE |
                        LANDLOCK_ACCESS_FS_READ_FILE |
                        LANDLOCK_ACCESS_FS_READ_DIR,
  };

  path_beneath.parent_fd = open("/usr", O_PATH | O_CLOEXEC);
  if (path_beneath.parent_fd < 0) {
    caml_failwith("open");
  }
  err = landlock_add_rule(ruleset_fd, LANDLOCK_RULE_PATH_BENEATH, &path_beneath,
                          0);
  close(path_beneath.parent_fd);
  if (err) {
    caml_failwith("landlock_add_rule");
  }
  CAMLreturn(Val_unit);
}
