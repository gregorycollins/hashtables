#include "defs.h"

#ifdef WIN32
#include <windows.h>
#else
#include <stdlib.h>
#include <unistd.h>
#endif

#include <stdio.h>

void suicide(volatile int* check, int t) {
    int secs = (3*t + 999999) / 1000000;
    if (secs < 1) secs = 1;
#ifdef WIN32
    Sleep(secs * 1000);
#else
    sleep(secs);
#endif
    if (*check) {
        printf("timeout expired, dying!!\n");
        exit(1);
    }
}

#if defined(__GNUC__)
#define PREFETCH_READ(x) (__builtin_prefetch(x, 0, 3))
#define PREFETCH_WRITE(x) (__builtin_prefetch(x, 1, 3))
#else
#define PREFETCH_READ(x)
#define PREFETCH_WRITE(x)
#endif

void prefetch_cacheline_read(small_hash_t* line, int start)
{
    PREFETCH_READ((void*)(&line[start]));
}

void prefetch_cacheline_write(small_hash_t* line, int start)
{
    PREFETCH_WRITE((void*)(&line[start]));
}

int forward_search_2(small_hash_t* array, int start, int end,
                     small_hash_t x1, small_hash_t x2) {
    small_hash_t* ep = array + end;
    small_hash_t* p = array + start;
    int wrapped = 0;
    while (1) {
        if (p >= ep) {
            if (wrapped) return -1;
            ep = array + start;
            p = array;
            wrapped = 1;
            continue;
        }
        if (*p == x1 || *p == x2) return p - array;
        ++p;
    }
}

int forward_search_3(small_hash_t* array, int start, int end,
                     small_hash_t x1, small_hash_t x2, small_hash_t x3) {
    small_hash_t* ep = array + end;
    small_hash_t* p = array + start;
    int wrapped = 0;
    while (1) {
        if (p >= ep) {
            if (wrapped) return -1;
            ep = array + start;
            p = array;
            wrapped = 1;
            continue;
        }
        if (*p == x1 || *p == x2 || *p == x3) return p - array;
        ++p;
    }
}

