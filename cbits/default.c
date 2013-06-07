// Specialized i686 versions of the cache line search functions.

#include "defs.h"

inline int32_t mask(int32_t a, int32_t b) { return -(a == b); }

#if defined(__GNUC__)
inline int32_t first_bit_set(int32_t a) {
    return __builtin_ffs(a) - 1;
}
#else
static uint8_t de_bruijn_table[] = {
    0,   1, 28,  2, 29, 14, 24,  3, 30, 22, 20, 15, 25, 17,  4,  8,
    31, 27, 13, 23, 21, 19, 16,  7, 26, 12, 18,  6, 11,  5, 10,  9
};

inline int32_t first_bit_set(int32_t a) {
    int32_t zero_case = mask(0, a);
    uint32_t x = (uint32_t) (a & -a);
    x *= 0x077CB531;
    x >>= 27;
    return zero_case | de_bruijn_table[x];
}
#endif

inline uint32_t line_mask(small_hash_t* array, int start, small_hash_t x1) {
    small_hash_t* p      = array + start;
    uint32_t      m1     = 0;
    uint32_t      m2     = 0;
    uint32_t      m3     = 0;
    int           offset = start & 0x1f;

#define M (mask(*p, x1))

    switch (offset) {
    case 0:  m1 |= M & 0x1;        ++p;
    case 1:  m2 |= M & 0x2;        ++p;
    case 2:  m3 |= M & 0x4;        ++p;
    case 3:  m1 |= M & 0x8;        ++p;
    case 4:  m2 |= M & 0x10;       ++p;
    case 5:  m3 |= M & 0x20;       ++p;
    case 6:  m1 |= M & 0x40;       ++p;
    case 7:  m2 |= M & 0x80;       ++p;
    case 8:  m3 |= M & 0x100;      ++p;
    case 9:  m1 |= M & 0x200;      ++p;
    case 10: m2 |= M & 0x400;      ++p;
    case 11: m3 |= M & 0x800;      ++p;
    case 12: m1 |= M & 0x1000;     ++p;
    case 13: m2 |= M & 0x2000;     ++p;
    case 14: m3 |= M & 0x4000;     ++p;
    case 15: m1 |= M & 0x8000;     ++p;
    case 16: m2 |= M & 0x10000;    ++p;
    case 17: m3 |= M & 0x20000;    ++p;
    case 18: m1 |= M & 0x40000;    ++p;
    case 19: m2 |= M & 0x80000;    ++p;
    case 20: m3 |= M & 0x100000;   ++p;
    case 21: m1 |= M & 0x200000;   ++p;
    case 22: m2 |= M & 0x400000;   ++p;
    case 23: m3 |= M & 0x800000;   ++p;
    case 24: m1 |= M & 0x1000000;  ++p;
    case 25: m2 |= M & 0x2000000;  ++p;
    case 26: m3 |= M & 0x4000000;  ++p;
    case 27: m1 |= M & 0x8000000;  ++p;
    case 28: m2 |= M & 0x10000000; ++p;
    case 29: m3 |= M & 0x20000000; ++p;
    case 30: m1 |= M & 0x40000000; ++p;
    case 31: m2 |= M & 0x80000000; ++p;
    }

#undef M

    return (m1 | m2 | m3) >> offset;
}

inline uint32_t line_mask_2(small_hash_t* array, int start,
                            small_hash_t x1, small_hash_t x2) {
    small_hash_t* p      = array + start;
    uint32_t      m1     = 0;
    uint32_t      m2     = 0;
    uint32_t      m3     = 0;
    int           offset = start & 0x1f;

#define M (mask(*p, x1) | mask(*p, x2))

    switch (offset) {
    case 0:  m1 |= M & 0x1;        ++p;
    case 1:  m2 |= M & 0x2;        ++p;
    case 2:  m3 |= M & 0x4;        ++p;
    case 3:  m1 |= M & 0x8;        ++p;
    case 4:  m2 |= M & 0x10;       ++p;
    case 5:  m3 |= M & 0x20;       ++p;
    case 6:  m1 |= M & 0x40;       ++p;
    case 7:  m2 |= M & 0x80;       ++p;
    case 8:  m3 |= M & 0x100;      ++p;
    case 9:  m1 |= M & 0x200;      ++p;
    case 10: m2 |= M & 0x400;      ++p;
    case 11: m3 |= M & 0x800;      ++p;
    case 12: m1 |= M & 0x1000;     ++p;
    case 13: m2 |= M & 0x2000;     ++p;
    case 14: m3 |= M & 0x4000;     ++p;
    case 15: m1 |= M & 0x8000;     ++p;
    case 16: m2 |= M & 0x10000;    ++p;
    case 17: m3 |= M & 0x20000;    ++p;
    case 18: m1 |= M & 0x40000;    ++p;
    case 19: m2 |= M & 0x80000;    ++p;
    case 20: m3 |= M & 0x100000;   ++p;
    case 21: m1 |= M & 0x200000;   ++p;
    case 22: m2 |= M & 0x400000;   ++p;
    case 23: m3 |= M & 0x800000;   ++p;
    case 24: m1 |= M & 0x1000000;  ++p;
    case 25: m2 |= M & 0x2000000;  ++p;
    case 26: m3 |= M & 0x4000000;  ++p;
    case 27: m1 |= M & 0x8000000;  ++p;
    case 28: m2 |= M & 0x10000000; ++p;
    case 29: m3 |= M & 0x20000000; ++p;
    case 30: m1 |= M & 0x40000000; ++p;
    case 31: m2 |= M & 0x80000000; ++p;
    }

#undef M

    return (m1 | m2 | m3) >> offset;
}

inline uint32_t line_mask_3(small_hash_t* array, int start,
                            small_hash_t x1, small_hash_t x2,
                            small_hash_t x3) {
    small_hash_t* p      = array + start;
    uint32_t      m1     = 0;
    uint32_t      m2     = 0;
    uint32_t      m3     = 0;
    int           offset = start & 0x1f;

#define M (mask(*p, x1) | mask(*p, x2) | mask(*p, x3))

    switch (offset) {
    case 0:  m1 |= M & 0x1;        ++p;
    case 1:  m2 |= M & 0x2;        ++p;
    case 2:  m3 |= M & 0x4;        ++p;
    case 3:  m1 |= M & 0x8;        ++p;
    case 4:  m2 |= M & 0x10;       ++p;
    case 5:  m3 |= M & 0x20;       ++p;
    case 6:  m1 |= M & 0x40;       ++p;
    case 7:  m2 |= M & 0x80;       ++p;
    case 8:  m3 |= M & 0x100;      ++p;
    case 9:  m1 |= M & 0x200;      ++p;
    case 10: m2 |= M & 0x400;      ++p;
    case 11: m3 |= M & 0x800;      ++p;
    case 12: m1 |= M & 0x1000;     ++p;
    case 13: m2 |= M & 0x2000;     ++p;
    case 14: m3 |= M & 0x4000;     ++p;
    case 15: m1 |= M & 0x8000;     ++p;
    case 16: m2 |= M & 0x10000;    ++p;
    case 17: m3 |= M & 0x20000;    ++p;
    case 18: m1 |= M & 0x40000;    ++p;
    case 19: m2 |= M & 0x80000;    ++p;
    case 20: m3 |= M & 0x100000;   ++p;
    case 21: m1 |= M & 0x200000;   ++p;
    case 22: m2 |= M & 0x400000;   ++p;
    case 23: m3 |= M & 0x800000;   ++p;
    case 24: m1 |= M & 0x1000000;  ++p;
    case 25: m2 |= M & 0x2000000;  ++p;
    case 26: m3 |= M & 0x4000000;  ++p;
    case 27: m1 |= M & 0x8000000;  ++p;
    case 28: m2 |= M & 0x10000000; ++p;
    case 29: m3 |= M & 0x20000000; ++p;
    case 30: m1 |= M & 0x40000000; ++p;
    case 31: m2 |= M & 0x80000000; ++p;
    }
#undef M

    return (m1 | m2 | m3) >> offset;
}


inline int32_t line_result(uint32_t m, int start) {
    int32_t p  = first_bit_set((int32_t) m);
    int32_t mm = mask(p, -1);
    return mm | (start + p);
}


int line_search(small_hash_t* array, int start, small_hash_t x1) {
    uint32_t m = line_mask(array, start, x1);
    return line_result(m, start);
}

int line_search_2(small_hash_t* array, int start, small_hash_t x1,
                  small_hash_t x2) {
    uint32_t m = line_mask_2(array, start, x1, x2);
    return line_result(m, start);
}

int line_search_3(small_hash_t* array, int start, small_hash_t x1,
                  small_hash_t x2, small_hash_t x3) {
    uint32_t m = line_mask_3(array, start, x1, x2, x3);
    return line_result(m, start);
}

void check_impl_specific(int* num_tests, int* num_errors) {

}
