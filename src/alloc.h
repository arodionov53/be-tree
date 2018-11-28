#pragma once

#ifdef NIF
#define bmalloc enif_alloc
#define bcalloc enif_alloc
#define brealloc enif_realloc
#define bfree enif_free
#else
#define bmalloc malloc
#define bcalloc(x) calloc(x, 1)
#define brealloc realloc
#define bfree free
#endif

#include <stdarg.h>

char* bstrdup(const char *s1);
int bvasprintf(char **buf, const char *format, va_list va);
int basprintf(char **buf, const char *format, ...);

