#include "defs.h"

#include <smmintrin.h>
#include <stdio.h>

/* Straight-line branchless SSE 4.2 code for searching an array of uint16_t
   hash codes. */

static inline int32_t mask(int32_t a, int32_t b) { return -(a == b); }

#if defined(__GNUC__)
static inline int32_t first_bit_set(int32_t a) {
    return __builtin_ffs(a) - 1;
}
#else
static uint8_t de_bruijn_table[] = {
    0,   1, 28,  2, 29, 14, 24,  3, 30, 22, 20, 15, 25, 17,  4,  8,
    31, 27, 13, 23, 21, 19, 16,  7, 26, 12, 18,  6, 11,  5, 10,  9
};

static inline int32_t first_bit_set(int32_t a) {
    int32_t zero_case = mask(0, a);
    uint32_t x = (uint32_t) (a & -a);
    x *= 0x077CB531;
    x >>= 27;
    return zero_case | de_bruijn_table[x];
}
#endif

static inline __m128i fill(small_hash_t v) {
    int32_t v1 = (((int)v) << 16) | v;
    __m128i x = _mm_cvtsi32_si128(0);
    x = _mm_insert_epi32(x, v1, 0);
    return _mm_shuffle_epi32(x, _MM_SHUFFLE(0,0,0,0));
}

#ifndef SIDD_UWORD_OPS
#define SIDD_UWORD_OPS _SIDD_UWORD_OPS
#endif

#ifndef SIDD_CMP_EQUAL_EACH
#define SIDD_CMP_EQUAL_EACH _SIDD_CMP_EQUAL_EACH
#endif

#ifndef SIDD_BIT_MASK
#define SIDD_BIT_MASK _SIDD_BIT_MASK
#endif

#define _MODE (SIDD_UWORD_OPS | SIDD_CMP_EQUAL_EACH)

static inline __m128i cmp_mask(__m128i a, __m128i b) {
    return _mm_cmpistrm(a, b, _MODE | SIDD_BIT_MASK);
}

static inline int32_t line_result(uint32_t m, int start) {
    int32_t p  = first_bit_set((int32_t) m);
    int32_t mm = mask(p, -1);
    return mm | (start + p);
}

#define DUMP(xval) do {                                       \
    uint16_t xval##_x0 = _mm_extract_epi16(xval, 0);          \
    uint16_t xval##_x1 = _mm_extract_epi16(xval, 1);          \
    uint16_t xval##_x2 = _mm_extract_epi16(xval, 2);          \
    uint16_t xval##_x3 = _mm_extract_epi16(xval, 3);          \
    uint16_t xval##_x4 = _mm_extract_epi16(xval, 4);          \
    uint16_t xval##_x5 = _mm_extract_epi16(xval, 5);          \
    uint16_t xval##_x6 = _mm_extract_epi16(xval, 6);          \
    uint16_t xval##_x7 = _mm_extract_epi16(xval, 7);          \
    printf("  % 10s: %04x-%04x-%04x-%04x-%04x-%04x-%04x-%04x\n", \
           #xval, xval##_x0, xval##_x1, xval##_x2, xval##_x3, \
           xval##_x4, xval##_x5, xval##_x6, xval##_x7);       \
  } while(0);


int line_search(small_hash_t* array, int start0, small_hash_t v1) {
    int offset = start0 & 31;
    int start  = start0 & ~31;
    __m128i* p = (__m128i*) &array[start];
    __m128i x1, val1, val2, val3, val4;
    __m128i m1, m2, m3, m4, dmask;

    x1 = fill(v1);

    val1 = *p++;
    m1 = cmp_mask(x1, val1);
    val2 = *p++;
    m2 = _mm_slli_si128(cmp_mask(x1, val2), 1);
    val3 = *p++;
    m3 = _mm_slli_si128(cmp_mask(x1, val3), 2);
    val4 = *p;
    m4 = _mm_slli_si128(cmp_mask(x1, val4), 3);

    dmask = _mm_or_si128(_mm_or_si128(m1, m2),
                         _mm_or_si128(m3, m4));
    uint32_t imask = _mm_extract_epi32(dmask, 0);

    const uint32_t p2 = 1 << offset;
    const uint32_t dest_mask = imask & ~(p2 - 1);

    return line_result(dest_mask, start);
}

int line_search_2(small_hash_t* array, int start0, small_hash_t v1,
                  small_hash_t v2) {
    int offset = start0 & 31;
    int start  = start0 & ~31;
    __m128i* p = (__m128i*) &array[start];
    __m128i x1, x2, val1, val2, val3, val4;
    __m128i m1, m2, m3, m4, dmask;

    x1 = fill(v1);
    x2 = fill(v2);

#define M(v) _mm_or_si128(cmp_mask(x1,(v)), \
                          cmp_mask(x2,(v)))
    val1 = *p++;
    m1 = M(val1);
    val2 = *p++;
    m2 = _mm_slli_si128(M(val2), 1);
    val3 = *p++;
    m3 = _mm_slli_si128(M(val3), 2);
    val4 = *p;
    m4 = _mm_slli_si128(M(val4), 3);
#undef M

    dmask = _mm_or_si128(_mm_or_si128(m1, m2),
                         _mm_or_si128(m3, m4));
    uint32_t imask = _mm_extract_epi32(dmask, 0);

    const uint32_t p2 = 1 << offset;
    const uint32_t dest_mask = imask & ~(p2 - 1);

    return line_result(dest_mask, start);
}

int line_search_3(small_hash_t* array, int start0, small_hash_t v1,
                  small_hash_t v2, small_hash_t v3) {
    int offset = start0 & 31;
    int start  = start0 & ~31;
    __m128i* p = (__m128i*) &array[start];
    __m128i x1, x2, x3, val1, val2, val3, val4;
    __m128i m1, m2, m3, m4, dmask;

    x1 = fill(v1);
    x2 = fill(v2);
    x3 = fill(v3);

#define M(v) _mm_or_si128(                  \
        cmp_mask(x1,(v)),                   \
        _mm_or_si128(cmp_mask(x2,(v)),      \
                     cmp_mask(x3,(v))))
    val1 = *p++;
    m1 = M(val1);
    val2 = *p++;
    m2 = _mm_slli_si128(M(val2), 1);
    val3 = *p++;
    m3 = _mm_slli_si128(M(val3), 2);
    val4 = *p;
    m4 = _mm_slli_si128(M(val4), 3);
#undef M

    dmask = _mm_or_si128(_mm_or_si128(m1, m2),
                         _mm_or_si128(m3, m4));
    uint32_t imask = _mm_extract_epi32(dmask, 0);

    const uint32_t p2 = 1 << offset;
    const uint32_t dest_mask = imask & ~(p2 - 1);

    return line_result(dest_mask, start);
}
