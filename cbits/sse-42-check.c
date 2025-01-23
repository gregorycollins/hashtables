#include "defs.h"

#include <smmintrin.h>
#include <stdio.h>

extern __m128i fill(small_hash_t v);

static void check_fill(small_hash_t v) {
    int i;
    char buf[256];
    small_hash_t v2;

    __m128i x = fill(v);

#define F(i) do {                                       \
        v2 = _mm_extract_epi16(x, i);                   \
        sprintf(buf, "fill-%x-%d-of-8", (int) v, i+1);  \
        CHECK(v2, v, buf);                              \
    } while(0);

    F(0);
    F(1);
    F(2);
    F(3);
    F(4);
    F(5);
    F(6);
    F(7);
#undef F
}

void check_impl_specific(int* num_tests, int* num_errors) {
    check_fill(0);
    check_fill((small_hash_t) (-1));
    check_fill((small_hash_t) (-5));
    check_fill(7);
    check_fill(0xff);
}
