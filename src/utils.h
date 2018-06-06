#pragma once

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

bool bmin(bool a, bool b);
bool bmax(bool a, bool b);
int64_t d64min(int64_t a, int64_t b);
int64_t d64max(int64_t a, int64_t b);
uint64_t u64min(uint64_t a, uint64_t b);
uint64_t u64max(uint64_t a, uint64_t b);
size_t smin(size_t a, size_t b);
size_t smax(size_t a, size_t b);

int64_t random_in_range(int64_t min, int64_t max);
bool random_bool();

bool feq(double a, double b);
bool fne(double a, double b);
void switch_default_error(const char* str);
// char* strdup(const char* str);
int asprintf(char** buf, const char* format, ...);
int vasprintf(char** buf, const char* format, va_list va);

void betree_assert(bool test, const char* message);
