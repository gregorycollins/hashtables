#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

#if defined(USE_SSE_4_1)
#include <smmintrin.h>
#endif


#if defined(__GNUC__)
#define PREFETCH_READ(x) (__builtin_prefetch(x, 0, 3))
#define PREFETCH_WRITE(x) (__builtin_prefetch(x, 1, 3))
#else
#define PREFETCH_READ(x)
#define PREFETCH_WRITE(x)
#endif

void prefetchCacheLine32_write(uint32_t* line, int start)
{
    PREFETCH_WRITE((void*)(&line[start]));
}


void prefetchCacheLine64_write(uint64_t* line, int start)
{
    PREFETCH_WRITE((void*)(&line[start]));
}


void prefetchCacheLine32_read(uint32_t* line, int start)
{
    PREFETCH_READ((void*)(&line[start]));
}


void prefetchCacheLine64_read(uint64_t* line, int start)
{
    PREFETCH_READ((void*)(&line[start]));
}


int forwardSearch32_2(uint32_t* array, int start, int end,
                      uint32_t x1, uint32_t x2) {
    uint32_t* ep = array + end;
    uint32_t* p = array + start;
    while (1) {
        if (p == ep) p = array;
        if (*p == x1 || *p == x2) return p - array;
        ++p;
    }
}


int forwardSearch32_3(uint32_t* array, int start, int end,
                      uint32_t x1, uint32_t x2, uint32_t x3) {
    uint32_t* ep = array + end;
    uint32_t* p = array + start;
    while (1) {
        if (p == ep) p = array;
        if (*p == x1 || *p == x2 || *p == x3) return p - array;
        ++p;
    }
}


int forwardSearch64_2(uint64_t* array, int start, int end,
                      uint64_t x1, uint64_t x2) {
    uint64_t* ep = array + end;
    uint64_t* p = array + start;
    while (1) {
        if (p == ep) p = array;
        if (*p == x1 || *p == x2) return p - array;
        ++p;
    }
}


int forwardSearch64_3(uint64_t* array, int start, int end,
                      uint64_t x1, uint64_t x2, uint64_t x3) {
    uint64_t* ep = array + end;
    uint64_t* p = array + start;
    while (1) {
        if (p == ep) p = array;
        if (*p == x1 || *p == x2 || *p == x3) return p - array;
        ++p;
    }
}


//----------------------------------------------------------------------------
// cache line search functions
// First: 32 bit

inline int mask(int a, int b) { return -(a == b); }


uint8_t deBruijnBitPositions[] = {
    0,   1, 28,  2, 29, 14, 24,  3, 30, 22, 20, 15, 25, 17,  4,  8,
    31, 27, 13, 23, 21, 19, 16,  7, 26, 12, 18,  6, 11,  5, 10,  9
};


int firstBitSet(int a) {
    int zeroCase = mask(0, a);
    uint32_t x = (uint32_t) (a & -a);
    x *= 0x077CB531;
    x >>= 27;
    return zeroCase | deBruijnBitPositions[x];
}


int32_t lineResult32(int m, int start) {
    int p = firstBitSet(m);
    int32_t mm = mask(p, -1);
    return mm | (~mm & (start + p));
}


uint32_t lineMask32(uint32_t* array, int start, uint32_t value) {
    uint32_t* p = array + start;
    uint32_t m = 0;
    int offset = start & 0xf;

    switch (offset) {
    case 0:  m |= mask(*p++, value) & 0x1;
    case 1:  m |= mask(*p++, value) & 0x2;
    case 2:  m |= mask(*p++, value) & 0x4;
    case 3:  m |= mask(*p++, value) & 0x8;
    case 4:  m |= mask(*p++, value) & 0x10;
    case 5:  m |= mask(*p++, value) & 0x20;
    case 6:  m |= mask(*p++, value) & 0x40;
    case 7:  m |= mask(*p++, value) & 0x80;
    case 8:  m |= mask(*p++, value) & 0x100;
    case 9:  m |= mask(*p++, value) & 0x200;
    case 10: m |= mask(*p++, value) & 0x400;
    case 11: m |= mask(*p++, value) & 0x800;
    case 12: m |= mask(*p++, value) & 0x1000;
    case 13: m |= mask(*p++, value) & 0x2000;
    case 14: m |= mask(*p++, value) & 0x4000;
    case 15: m |= mask(*p++, value) & 0x8000;
    }

    return m >> offset;
}


int lineSearch32(uint32_t* array, int start, uint32_t value) {
    uint32_t m = lineMask32(array, start, value);
    return lineResult32((int)m, start);
}


uint32_t lineMask32_2(uint32_t* array, int start, uint32_t x1, uint32_t x2) {
    uint32_t* p = array + start;
    uint32_t m = 0;
    int offset = start & 0xf;

    switch (offset) {
    case 0:  m |= (mask(*p, x1) | mask(*p, x2)) & 0x1;    ++p;
    case 1:  m |= (mask(*p, x1) | mask(*p, x2)) & 0x2;    ++p;
    case 2:  m |= (mask(*p, x1) | mask(*p, x2)) & 0x4;    ++p;
    case 3:  m |= (mask(*p, x1) | mask(*p, x2)) & 0x8;    ++p;
    case 4:  m |= (mask(*p, x1) | mask(*p, x2)) & 0x10;   ++p;
    case 5:  m |= (mask(*p, x1) | mask(*p, x2)) & 0x20;   ++p;
    case 6:  m |= (mask(*p, x1) | mask(*p, x2)) & 0x40;   ++p;
    case 7:  m |= (mask(*p, x1) | mask(*p, x2)) & 0x80;   ++p;
    case 8:  m |= (mask(*p, x1) | mask(*p, x2)) & 0x100;  ++p;
    case 9:  m |= (mask(*p, x1) | mask(*p, x2)) & 0x200;  ++p;
    case 10: m |= (mask(*p, x1) | mask(*p, x2)) & 0x400;  ++p;
    case 11: m |= (mask(*p, x1) | mask(*p, x2)) & 0x800;  ++p;
    case 12: m |= (mask(*p, x1) | mask(*p, x2)) & 0x1000; ++p;
    case 13: m |= (mask(*p, x1) | mask(*p, x2)) & 0x2000; ++p;
    case 14: m |= (mask(*p, x1) | mask(*p, x2)) & 0x4000; ++p;
    case 15: m |= (mask(*p, x1) | mask(*p, x2)) & 0x8000; ++p;
    }

    return m >> offset;
}


int lineSearch32_2(uint32_t* array, int start, uint32_t x1, uint32_t x2) {
    uint32_t m = lineMask32_2(array, start, x1, x2);
    return lineResult32((int)m, start);
}


uint32_t lineMask32_3(uint32_t* array, int start,
                      uint32_t x1, uint32_t x2, uint32_t x3) {
    uint32_t* p = array + start;
    uint32_t m = 0;
    int offset = start & 0xf;

    switch (offset) {
    case 0:  m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x1;    ++p;
    case 1:  m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x2;    ++p;
    case 2:  m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x4;    ++p;
    case 3:  m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x8;    ++p;
    case 4:  m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x10;   ++p;
    case 5:  m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x20;   ++p;
    case 6:  m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x40;   ++p;
    case 7:  m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x80;   ++p;
    case 8:  m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x100;  ++p;
    case 9:  m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x200;  ++p;
    case 10: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x400;  ++p;
    case 11: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x800;  ++p;
    case 12: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x1000; ++p;
    case 13: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x2000; ++p;
    case 14: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x4000; ++p;
    case 15: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x8000; ++p;
    }
    
    return m >> offset;
}


int lineSearch32_3(uint32_t* array, int start,
                   uint32_t x1, uint32_t x2, uint32_t x3) {
    uint32_t m = lineMask32_3(array, start, x1, x2, x3);
    return lineResult32((int)m, start);
}


//----------------------------------------------------------------------------
// Now: 64-bit. If USE_SSE_4_1 is on, we will use SSE4.1 SIMD instructions to
// search the cache line super-efficiently.

#if defined(USE_SSE_4_1)

inline uint64_t mask_to_mask2(__m128i m) {
    int mask16 = _mm_movemask_epi8(m);
    // output of _mm_movemask_epi8 is a 16-bit word where bit i is 1 iff the
    // most significant bit of byte i of the mask is 1
    int m1 = mask16 & 0x1;
    int m2 = (mask16 & 0x100) >> 7;
    return (uint64_t) (m1 | m2);
}


inline uint64_t cmp_and_mask(__m128i val, __m128i x0) {
    __m128i mask1 = _mm_cmpeq_epi64(val, x0);
    return mask_to_mask2(mask1);
}


inline uint64_t cmp_and_mask_2(__m128i val, __m128i x0, __m128i x1) {
    __m128i mask1 = _mm_cmpeq_epi64(val, x0);
    __m128i mask2 = _mm_cmpeq_epi64(val, x1);
    mask1 = _mm_or_si128(mask1, mask2);
    return mask_to_mask2(mask1);
}


inline uint64_t cmp_and_mask_3(__m128i val, __m128i x0, __m128i x1,
                               __m128i x2) {
    __m128i mask1 = _mm_cmpeq_epi64(val, x0);
    __m128i mask2 = _mm_cmpeq_epi64(val, x1);
    __m128i mask3 = _mm_cmpeq_epi64(val, x2);
    mask1 = _mm_or_si128(mask1, mask2);
    mask1 = _mm_or_si128(mask1, mask3);
    return mask_to_mask2(mask1);
}


uint64_t lineMask64(uint64_t* array, int start0, uint64_t v1) {
    int offset = start0 & 0x7;
    int start  = start0 & ~0x7;
    
    __m128i* p = (__m128i*) (&array[start]);
    __m128i x1 = _mm_cvtsi32_si128(0);
    x1 = _mm_insert_epi64(x1, v1, 0);
    x1 = _mm_insert_epi64(x1, v1, 1);
    uint64_t dest_mask = 0;

    // x1 contains two 64-bit copies of the value to look for
    
    // words 0, 1
    __m128i x = _mm_load_si128(p);
    dest_mask = cmp_and_mask(x, x1);
    p = (__m128i*) (&array[start+2]);

    // words 2, 3
    x = _mm_load_si128(p);
    dest_mask |= (cmp_and_mask(x, x1) << 2);
    p = (__m128i*) (&array[start+4]);

    // words 4, 5
    x = _mm_load_si128(p);
    dest_mask |= (cmp_and_mask(x, x1) << 4);
    p = (__m128i*) (&array[start+6]);
    
    // words 6, 7
    x = _mm_load_si128(p);
    dest_mask |= (cmp_and_mask(x, x1) << 6);

    return dest_mask >> offset;
}


uint64_t lineMask64_2(uint64_t* array, int start0, uint64_t v1, uint64_t v2) {
    int offset = start0 & 0x7;
    int start  = start0 & ~0x7;
    
    __m128i* p = (__m128i*) (&array[start]);
    __m128i x1 = _mm_cvtsi32_si128(0);
    x1 = _mm_insert_epi64(x1, v1, 0);
    x1 = _mm_insert_epi64(x1, v1, 1);

    __m128i x2 = _mm_cvtsi32_si128(0);
    x2 = _mm_insert_epi64(x2, v2, 0);
    x2 = _mm_insert_epi64(x2, v2, 1);

    uint64_t dest_mask = 0;

    // words 0, 1
    __m128i x = _mm_load_si128(p);
    dest_mask = cmp_and_mask_2(x, x1, x2);
    p = (__m128i*) (&array[start+2]);

    // words 2, 3
    x = _mm_load_si128(p);
    dest_mask |= (cmp_and_mask_2(x, x1, x2) << 2);
    p = (__m128i*) (&array[start+4]);

    // words 4, 5
    x = _mm_load_si128(p);
    dest_mask |= (cmp_and_mask_2(x, x1, x2) << 4);
    p = (__m128i*) (&array[start+6]);
    
    // words 6, 7
    x = _mm_load_si128(p);
    dest_mask |= (cmp_and_mask_2(x, x1, x2) << 6);

    return dest_mask >> offset;
}


uint64_t lineMask64_3(uint64_t* array, int start0,
                      uint64_t v1, uint64_t v2, uint64_t v3) {
    int offset = start0 & 0x7;
    int start  = start0 & ~0x7;
    
    __m128i* p = (__m128i*) (&array[start]);
    __m128i x1 = _mm_cvtsi32_si128(0);
    x1 = _mm_insert_epi64(x1, v1, 0);
    x1 = _mm_insert_epi64(x1, v1, 1);

    __m128i x2 = _mm_cvtsi32_si128(0);
    x2 = _mm_insert_epi64(x2, v2, 0);
    x2 = _mm_insert_epi64(x2, v2, 1);

    __m128i x3 = _mm_cvtsi32_si128(0);
    x3 = _mm_insert_epi64(x3, v3, 0);
    x3 = _mm_insert_epi64(x3, v3, 1);

    uint64_t dest_mask = 0;

    // words 0, 1
    __m128i x = _mm_load_si128(p);
    dest_mask = cmp_and_mask_3(x, x1, x2, x3);
    p = (__m128i*) (&array[start+2]);

    // words 2, 3
    x = _mm_load_si128(p);
    dest_mask |= (cmp_and_mask_3(x, x1, x2, x3) << 2);
    p = (__m128i*) (&array[start+4]);

    // words 4, 5
    x = _mm_load_si128(p);
    dest_mask |= (cmp_and_mask_3(x, x1, x2, x3) << 4);
    p = (__m128i*) (&array[start+6]);
    
    // words 6, 7
    x = _mm_load_si128(p);
    dest_mask |= (cmp_and_mask_3(x, x1, x2, x3) << 6);

    return dest_mask >> offset;
}


#else



uint64_t lineMask64(uint64_t* array, int start, uint64_t value) {
    uint64_t* p = array + start;
    uint64_t m = 0;
    int offset = start & 0x7;

    switch (offset) {
    case 0: m |= mask(*p++, value) & 0x1;
    case 1: m |= mask(*p++, value) & 0x2;
    case 2: m |= mask(*p++, value) & 0x4;
    case 3: m |= mask(*p++, value) & 0x8;
    case 4: m |= mask(*p++, value) & 0x10;
    case 5: m |= mask(*p++, value) & 0x20;
    case 6: m |= mask(*p++, value) & 0x40;
    case 7: m |= mask(*p++, value) & 0x80;
    }

    return m >> offset;
}


uint64_t lineMask64_2(uint64_t* array, int start, uint64_t x1, uint64_t x2) {
    uint64_t* p = array + start;
    uint64_t m = 0;
    int offset = start & 0x7;

    switch (offset) {
    case 0: m |= (mask(*p, x1) | mask(*p, x2)) & 0x1;  ++p;
    case 1: m |= (mask(*p, x1) | mask(*p, x2)) & 0x2;  ++p;
    case 2: m |= (mask(*p, x1) | mask(*p, x2)) & 0x4;  ++p;
    case 3: m |= (mask(*p, x1) | mask(*p, x2)) & 0x8;  ++p;
    case 4: m |= (mask(*p, x1) | mask(*p, x2)) & 0x10; ++p;
    case 5: m |= (mask(*p, x1) | mask(*p, x2)) & 0x20; ++p;
    case 6: m |= (mask(*p, x1) | mask(*p, x2)) & 0x40; ++p;
    case 7: m |= (mask(*p, x1) | mask(*p, x2)) & 0x80; ++p;
    }

    return m >> offset;
}


uint64_t lineMask64_3(uint64_t* array, int start,
                      uint64_t x1, uint64_t x2, uint64_t x3) {
    uint64_t* p = array + start;
    uint64_t m = 0;
    int offset = start & 0x7;

    switch (offset) {
    case 0: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x1;  ++p;
    case 1: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x2;  ++p;
    case 2: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x4;  ++p;
    case 3: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x8;  ++p;
    case 4: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x10; ++p;
    case 5: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x20; ++p;
    case 6: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x40; ++p;
    case 7: m |= (mask(*p, x1) | mask(*p, x2) | mask(*p, x3)) & 0x80; ++p;
    }
    
    return m >> offset;
}


#endif   // USE_SSE_4_1


int64_t lineResult64(int64_t m, int64_t start) {
    int p = firstBitSet((int)m);
    int64_t mm = (int64_t) mask(p, -1);
    return mm | (~mm & (start + p));
}


int lineSearch64(uint64_t* array, int start, uint64_t value) {
    uint64_t m = lineMask64(array, start, value);
    return lineResult64((int)m, start);
}


int lineSearch64_2(uint64_t* array, int start, uint64_t x1, uint64_t x2) {
    uint64_t m = lineMask64_2(array, start, x1, x2);
    return lineResult64((int)m, start);
}


int lineSearch64_3(uint64_t* array, int start,
                   uint64_t x1, uint64_t x2, uint64_t x3) {
    uint64_t m = lineMask64_3(array, start, x1, x2, x3);
    return lineResult64((int)m, start);
}

void suicide(volatile int* check, int t) {
    int secs = (3*t + 999999) / 1000000;
    if (secs < 1) secs = 1;

    sleep(secs);
    if (*check) {
        printf("timeout expired, dying!!\n");
        raise(SIGKILL);
    }
}
