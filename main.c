#include <stdio.h>
#include <stdlib.h>
#include "emacs-module.h"

int plugin_is_GPL_compatible;

static void bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  emacs_value args[] = { Qsym, Sfun };

  env->funcall(env, Qfset, 2, args);
}

static emacs_value call(emacs_env *env, const char *name, ptrdiff_t nargs, emacs_value args[])
{
  emacs_value Qname = env->intern(env, name);
  return env->funcall(env, Qname, nargs, args);
}

static void provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value args[] = { Qfeat };

  call(env, "provide", 1, args);
}

static emacs_value gc(emacs_env *env)
{
  emacs_value args[] = {};
  return env->funcall(env, env->intern(env, "garbage-collect"), 0, args);
}

static emacs_value print(emacs_env *env, emacs_value v)
{
  emacs_value args[] = { v };
  return call(env, "print", 1, args);
}

static emacs_value crash(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  emacs_value *v = malloc(2 * sizeof * v);
  v[0] = env->make_string(env, "1", 1);
  v[1] = env->make_string(env, "2", 1);

  gc(env);

  emacs_value list = call(env, "list", 2, v);

  // Segfault here.
  print(env, list);
  free(v);
  return list;
}

// Use stack-allocated array.
static emacs_value ok_1(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  emacs_value v[2];
  v[0] = env->make_string(env, "1", 1);
  v[1] = env->make_string(env, "2", 1);

  gc(env);

  emacs_value list = call(env, "list", 2, v);

  print(env, list);
  return list;
}

// Construct the returned list before calling GC.
static emacs_value ok_2(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  emacs_value *v = malloc(2 * sizeof * v);
  v[0] = env->make_string(env, "1", 1);
  v[1] = env->make_string(env, "2", 1);

  emacs_value list = call(env, "list", 2, v);

  gc(env);

  print(env, list);
  free(v);
  return list;
}

// Pass the received emacs_value's into Lisp again before calling GC.
static emacs_value ok_3(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  emacs_value *v = malloc(2 * sizeof * v);
  v[0] = env->make_string(env, "1", 1);
  v[1] = env->make_string(env, "2", 1);

  print(env, v[0]);
  print(env, v[1]);

  gc(env);

  emacs_value list = call(env, "list", 2, v);

  print(env, list);
  free(v);
  return list;
}

// Pass one received emacs_value's into Lisp, printf the other, before calling GC.
static emacs_value ok_4(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  emacs_value *v = malloc(2 * sizeof * v);
  v[0] = env->make_string(env, "1", 1);
  v[1] = env->make_string(env, "2", 1);

  // If the next 2 lines are switched, the later 'print(env, list)' call crashes.
  print(env, v[0]);
  printf("-> %x\n", v[1]);

  gc(env);

  emacs_value list = call(env, "list", 2, v);

  print(env, list);
  free(v);
  return list;
}

int emacs_module_init(struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);

  bind_function(env, "testing/crash", env->make_function(env, 0, 0, crash, "", NULL));
  bind_function(env, "testing/ok-1", env->make_function(env, 0, 0, ok_1, "", NULL));
  bind_function(env, "testing/ok-2", env->make_function(env, 0, 0, ok_2, "", NULL));
  bind_function(env, "testing/ok-3", env->make_function(env, 0, 0, ok_3, "", NULL));
  bind_function(env, "testing/ok-4", env->make_function(env, 0, 0, ok_4, "", NULL));
  provide(env, "testing");
  return 0;
}
