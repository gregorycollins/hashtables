#include <stdlib.h>
#include <stdio.h>
#include "defs.h"

static const int NUMH = 64 / sizeof(small_hash_t);

static small_hash_t t_sevens[32] =
    { 7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7 };

static small_hash_t t_zeroes[32] =
    { 0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0 };

static small_hash_t t_mixed[32] =
    { 7,1,7,7,2,7,7,7,
      7,7,3,7,7,7,7,7,
      7,7,7,7,1,7,7,7,
      7,7,7,7,3,7,7,9 };

static int num_tests = 0;
static int num_errors = 0;

void CHECK(int actual, int expected, char* what) {
    ++num_tests;
    if (actual != expected) {
        fprintf(stderr, "%s: expected %d, got %d\n", what, expected, actual);

        ++num_errors;
    }
}


void check_forward_search_2() {
    /* forward_search_2 */
    /*   - offset zero  */
    CHECK(forward_search_2(t_sevens, 0, NUMH, 0, 7   ),  0, "fs2-sevens-ok-1"  );
    CHECK(forward_search_2(t_sevens, 0, NUMH, 7, 0   ),  0, "fs2-sevens-ok-2"  );
    CHECK(forward_search_2(t_sevens, 0, NUMH, 7, 7   ),  0, "fs2-sevens-ok-3"  );
    CHECK(forward_search_2(t_sevens, 0, NUMH, 3, 0   ), -1, "fs2-sevens-fail-1");
    CHECK(forward_search_2(t_zeroes, 0, NUMH, 0, 1   ),  0, "fs2-zeroes-ok-1"  );
    CHECK(forward_search_2(t_zeroes, 0, NUMH, 2, 0   ),  0, "fs2-zeroes-ok-2"  );
    CHECK(forward_search_2(t_zeroes, 0, NUMH, 2, 0xf0), -1, "fs2-zeroes-fail-1");

    /*   - offset 5     */
    CHECK(forward_search_2(t_sevens, 5, NUMH, 0,    7),  5, "fs2-o-sevens-ok-1"  );
    CHECK(forward_search_2(t_sevens, 5, NUMH, 7,    0),  5, "fs2-o-sevens-ok-2"  );
    CHECK(forward_search_2(t_sevens, 5, NUMH, 7,    7),  5, "fs2-o-sevens-ok-3"  );
    CHECK(forward_search_2(t_sevens, 5, NUMH, 3,    0), -1, "fs2-o-sevens-fail-1");
    CHECK(forward_search_2(t_zeroes, 5, NUMH, 0,    1),  5, "fs2-o-zeroes-ok-1"  );
    CHECK(forward_search_2(t_zeroes, 5, NUMH, 2,    0),  5, "fs2-o-zeroes-ok-2"  );
    CHECK(forward_search_2(t_zeroes, 5, NUMH, 2, 0xf0), -1, "fs2-o-zeroes-fail-1");

    /*   - mixed, offset zero */
    CHECK(forward_search_2(t_mixed, 0, NUMH, 2, 0xf0),  4, "fs2-mixed-ok-1"  );
    CHECK(forward_search_2(t_mixed, 0, NUMH, 4, 0xf0), -1, "fs2-mixed-fail-1");
    CHECK(forward_search_2(t_mixed, 0, NUMH, 2,    1),  1, "fs2-mixed-ok-2"  );
    CHECK(forward_search_2(t_mixed, 0, NUMH, 2,    7),  0, "fs2-mixed-ok-3"  );
    CHECK(forward_search_2(t_mixed, 0, NUMH, 2,    3),  4, "fs2-mixed-ok-4"  );
    CHECK(forward_search_2(t_mixed, 0, NUMH, 9,    3), 10, "fs2-mixed-ok-5"  );
    CHECK(forward_search_2(t_mixed, 0, NUMH, 3,    9), 10, "fs2-mixed-ok-5"  );
    CHECK(forward_search_2(t_mixed, 0, NUMH, 8,    9), 31, "fs2-mixed-ok-6"  );

    /*   - mixed, offset 16 */
    CHECK(forward_search_2(t_mixed, 16, NUMH, 2, 0xf0),  4, "fs2-o-mixed-ok-1"  );
    CHECK(forward_search_2(t_mixed, 16, NUMH, 4, 0xf0), -1, "fs2-o-mixed-fail-1");
    CHECK(forward_search_2(t_mixed, 16, NUMH, 2,    1), 20, "fs2-o-mixed-ok-2"  );
    CHECK(forward_search_2(t_mixed, 16, NUMH, 2,    7), 16, "fs2-o-mixed-ok-3"  );
    CHECK(forward_search_2(t_mixed, 16, NUMH, 2,    3), 28, "fs2-o-mixed-ok-4"  );
    CHECK(forward_search_2(t_mixed, 16, NUMH, 9,    3), 28, "fs2-o-mixed-ok-5"  );
    CHECK(forward_search_2(t_mixed, 16, NUMH, 3,    9), 28, "fs2-o-mixed-ok-5"  );
    CHECK(forward_search_2(t_mixed, 16, NUMH, 8,    9), 31, "fs2-o-mixed-ok-6"  );
}

void check_forward_search_3() {
    /* forward_search_3 */
    /*   - offset zero  */
    CHECK(forward_search_3(t_sevens, 0, NUMH, 0,  7, 88),  0, "fs3-sevens-ok-1"  );
    CHECK(forward_search_3(t_sevens, 0, NUMH, 7,  0, 88),  0, "fs3-sevens-ok-2"  );
    CHECK(forward_search_3(t_sevens, 0, NUMH, 7,  7, 88),  0, "fs3-sevens-ok-3"  );
    CHECK(forward_search_3(t_sevens, 0, NUMH, 3,  0, 88), -1, "fs3-sevens-fail-1");
    CHECK(forward_search_3(t_zeroes, 0, NUMH, 0,  1, 88),  0, "fs3-zeroes-ok-1"  );
    CHECK(forward_search_3(t_zeroes, 0, NUMH, 2,  0, 88),  0, "fs3-zeroes-ok-2"  );
    CHECK(forward_search_3(t_zeroes, 0, NUMH, 2, 11, 0 ),  0, "fs3-zeroes-ok-3"  );
    CHECK(forward_search_3(t_zeroes, 0, NUMH, 2, 32, 88), -1, "fs3-zeroes-fail-1");

    /*   - offset 5     */
    CHECK(forward_search_3(t_sevens, 5, NUMH, 0,    7, 7 ),  5, "fs3-o-sevens-ok-1"  );
    CHECK(forward_search_3(t_sevens, 5, NUMH, 7,    0, 21),  5, "fs3-o-sevens-ok-2"  );
    CHECK(forward_search_3(t_sevens, 5, NUMH, 7,    7, 21),  5, "fs3-o-sevens-ok-3"  );
    CHECK(forward_search_3(t_sevens, 5, NUMH, 3,    0, 21), -1, "fs3-o-sevens-fail-1");
    CHECK(forward_search_3(t_zeroes, 5, NUMH, 0,    1, 21),  5, "fs3-o-zeroes-ok-1"  );
    CHECK(forward_search_3(t_zeroes, 5, NUMH, 2,    0, 21),  5, "fs3-o-zeroes-ok-2"  );
    CHECK(forward_search_3(t_zeroes, 5, NUMH, 2, 0xf0, 21), -1, "fs3-o-zeroes-fail-1");

    /*   - mixed, offset zero */
    CHECK(forward_search_3(t_mixed, 0, NUMH, 2, 0xf0, -1),  4, "fs3-mixed-ok-1"  );
    CHECK(forward_search_3(t_mixed, 0, NUMH, 4, 0xf0, -1), -1, "fs3-mixed-fail-1");
    CHECK(forward_search_3(t_mixed, 0, NUMH, 2,    1, -1),  1, "fs3-mixed-ok-2"  );
    CHECK(forward_search_3(t_mixed, 0, NUMH, 2,    7, -1),  0, "fs3-mixed-ok-3"  );
    CHECK(forward_search_3(t_mixed, 0, NUMH, 2,    3, -1),  4, "fs3-mixed-ok-4"  );
    CHECK(forward_search_3(t_mixed, 0, NUMH, 9,    3, -1), 10, "fs3-mixed-ok-5"  );
    CHECK(forward_search_3(t_mixed, 0, NUMH, 3,    9, -1), 10, "fs3-mixed-ok-5"  );
    CHECK(forward_search_3(t_mixed, 0, NUMH, 8,    9, -1), 31, "fs3-mixed-ok-6"  );

    /*   - mixed, offset 16 */
    CHECK(forward_search_3(t_mixed, 16, NUMH, 2, 96, 33),  4, "fs3-o-mixed-ok-1"  );
    CHECK(forward_search_3(t_mixed, 16, NUMH, 4, 96, 33), -1, "fs3-o-mixed-fail-1");
    CHECK(forward_search_3(t_mixed, 16, NUMH, 2,  1, 33), 20, "fs3-o-mixed-ok-2"  );
    CHECK(forward_search_3(t_mixed, 16, NUMH, 2,  7, 33), 16, "fs3-o-mixed-ok-3"  );
    CHECK(forward_search_3(t_mixed, 16, NUMH, 2,  3, 33), 28, "fs3-o-mixed-ok-4"  );
    CHECK(forward_search_3(t_mixed, 16, NUMH, 9,  3, 33), 28, "fs3-o-mixed-ok-5"  );
    CHECK(forward_search_3(t_mixed, 16, NUMH, 3,  9, 33), 28, "fs3-o-mixed-ok-5"  );
    CHECK(forward_search_3(t_mixed, 16, NUMH, 8,  9, 33), 31, "fs3-o-mixed-ok-6"  );
    CHECK(forward_search_3(t_mixed, 16, NUMH, 8, 33, 9 ), 31, "fs3-o-mixed-ok-7"  );
}

void check_line_search() {
    CHECK(line_search(t_sevens,  0, 7),  0, "ls-7s-ok-1");
    CHECK(line_search(t_sevens,  5, 7),  5, "ls-7s-ok-2");
    CHECK(line_search(t_sevens, 31, 7), 31, "ls-7s-ok-3");
    CHECK(line_search(t_sevens,  0, 1), -1, "ls-7s-fail-1");
    CHECK(line_search(t_sevens, 31, 1), -1, "ls-7s-fail-2");

    CHECK(line_search(t_mixed, 0, 7),  0, "ls-m-ok-1");
    CHECK(line_search(t_mixed, 0, 1),  1, "ls-m-ok-2");
    CHECK(line_search(t_mixed, 1, 7),  2, "ls-m-ok-3");
    CHECK(line_search(t_mixed, 0, 9), 31, "ls-m-ok-4");
    CHECK(line_search(t_mixed, 0, 8), -1, "ls-m-fail-1");

    CHECK(line_search(t_mixed, 16, 1), 20, "ls-m-ok-5");
}

void check_line_search_2() {
    CHECK(line_search_2(t_sevens,  0, 7, 3),  0, "ls2-7s-ok-1");
    CHECK(line_search_2(t_sevens,  5, 7, 9),  5, "ls2-7s-ok-2");
    CHECK(line_search_2(t_sevens, 31, 0, 7), 31, "ls2-7s-ok-3");
    CHECK(line_search_2(t_sevens,  0, 1, 3), -1, "ls2-7s-fail-1");
    CHECK(line_search_2(t_sevens, 31, 6, 1), -1, "ls2-7s-fail-2");

    CHECK(line_search_2(t_mixed, 0, 7, 9),  0, "ls2-m-ok-1");
    CHECK(line_search_2(t_mixed, 0, 9, 1),  1, "ls2-m-ok-2");
    CHECK(line_search_2(t_mixed, 1, 7, 9),  2, "ls2-m-ok-3");
    CHECK(line_search_2(t_mixed, 0, 8, 9), 31, "ls2-m-ok-4");
    CHECK(line_search_2(t_mixed, 0, 8, 4), -1, "ls2-m-fail-1");

    CHECK(line_search_2(t_mixed, 16, 3, 1), 20, "ls2-m-ok-5");
}

void check_line_search_3() {
    CHECK(line_search_3(t_sevens,  0, 4, 7, 3),  0, "ls2-7s-ok-1");
    CHECK(line_search_3(t_sevens,  5, 7, 4, 9),  5, "ls2-7s-ok-2");
    CHECK(line_search_3(t_sevens, 31, 0, 7, 4), 31, "ls2-7s-ok-3");
    CHECK(line_search_3(t_sevens,  0, 1, 4, 3), -1, "ls2-7s-fail-1");
    CHECK(line_search_3(t_sevens, 31, 4, 6, 1), -1, "ls2-7s-fail-2");

    CHECK(line_search_3(t_mixed, 0, 4, 7, 9),  0, "ls2-m-ok-1");
    CHECK(line_search_3(t_mixed, 0, 9, 4, 1),  1, "ls2-m-ok-2");
    CHECK(line_search_3(t_mixed, 1, 7, 9, 4),  2, "ls2-m-ok-3");
    CHECK(line_search_3(t_mixed, 0, 8, 4, 9), 31, "ls2-m-ok-4");
    CHECK(line_search_3(t_mixed, 0, 8, 4, 6), -1, "ls2-m-fail-1");

    CHECK(line_search_3(t_mixed, 16, 3, 1, 6), 20, "ls2-m-ok-5");
}


int main() {
    check_forward_search_2();
    check_forward_search_3();
    check_line_search();
    check_line_search_2();
    check_line_search_3();
    check_impl_specific();

    if (num_errors > 0) {
        printf("\n*** %d/%d tests failed.\n", num_errors, num_tests);
    } else {
        printf("All %d tests passed.\n", num_tests);
    }
    exit(num_errors < 255 ? num_errors : 255);
}
