#ifndef HASHTABLES_DEFS_H
#define HASHTABLES_DEFS_H

#include <stdint.h>
#include <strings.h>

typedef uintptr_t full_hash_t;
typedef uint16_t small_hash_t;

void prefetch_cacheline_write(small_hash_t* line, int start);
void prefetch_cacheline_read(small_hash_t* line, int start);

int forward_search_2(small_hash_t* array,
                     int start,
                     int end,
                     small_hash_t x1,
                     small_hash_t x2);
int forward_search_3(small_hash_t* array,
                     int start,
                     int end,
                     small_hash_t x1,
                     small_hash_t x2,
                     small_hash_t x3);

int line_search(small_hash_t* array, int start, small_hash_t x1);
int line_search_2(small_hash_t* array, int start, small_hash_t x1,
                  small_hash_t x2);
int line_search_3(small_hash_t* array, int start, small_hash_t x1,
                  small_hash_t x2, small_hash_t x3);
void suicide(volatile int* check, int i);

void CHECK(int actual, int expected, char* what);
void check_impl_specific();

#endif  /* HASHTABLES_DEFS_H */
