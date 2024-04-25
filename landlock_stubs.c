#define _GNU_SOURCE
#include <caml/memory.h>
#include <fcntl.h>
#include <linux/landlock.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <unistd.h>

#ifndef landlock_create_ruleset
static inline int
landlock_create_ruleset(const struct landlock_ruleset_attr *const attr,
                        const size_t size, const __u32 flags) {
  return syscall(__NR_landlock_create_ruleset, attr, size, flags);
}
#endif

#ifndef landlock_add_rule
static inline int landlock_add_rule(const int ruleset_fd,
                                    const enum landlock_rule_type rule_type,
                                    const void *const rule_attr,
                                    const __u32 flags) {
  return syscall(__NR_landlock_add_rule, ruleset_fd, rule_type, rule_attr,
                 flags);
}
#endif

#ifndef landlock_restrict_self
static inline int landlock_restrict_self(const int ruleset_fd,
                                         const __u32 flags) {
  return syscall(__NR_landlock_restrict_self, ruleset_fd, flags);
}
#endif

value setup_landlock(value v_p_ruleset_attr) {
  CAMLparam1(v_p_ruleset_attr);
  struct landlock_ruleset_attr *p_ruleset_attr =
      (struct landlock_ruleset_attr *)Nativeint_val(v_p_ruleset_attr);
  struct landlock_ruleset_attr ruleset_attr = *p_ruleset_attr;
  int ruleset_fd;

  ruleset_fd = landlock_create_ruleset(&ruleset_attr, sizeof(ruleset_attr), 0);
  if (ruleset_fd < 0) {
    perror("Failed to create a ruleset");
    return 1;
  }
  int err;
  struct landlock_path_beneath_attr path_beneath = {
      .allowed_access = LANDLOCK_ACCESS_FS_EXECUTE |
                        LANDLOCK_ACCESS_FS_READ_FILE |
                        LANDLOCK_ACCESS_FS_READ_DIR,
  };

  path_beneath.parent_fd = open("/usr", O_PATH | O_CLOEXEC);
  if (path_beneath.parent_fd < 0) {
    perror("Failed to open file");
    close(ruleset_fd);
    return 1;
  }
  err = landlock_add_rule(ruleset_fd, LANDLOCK_RULE_PATH_BENEATH, &path_beneath,
                          0);
  close(path_beneath.parent_fd);
  if (err) {
    perror("Failed to update ruleset");
    close(ruleset_fd);
    return 1;
  }
  if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
    perror("Failed to restrict privileges");
    close(ruleset_fd);
    return 1;
  }
  if (landlock_restrict_self(ruleset_fd, 0)) {
    perror("Failed to enforce ruleset");
    close(ruleset_fd);
    return 1;
  }
  close(ruleset_fd);
  CAMLreturn(Val_unit);
}
