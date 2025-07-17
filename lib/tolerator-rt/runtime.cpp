
#include <cstdint>
#include <cstdio>
#include <cstdlib>

extern "C" {


// This macro allows us to prefix strings so that they are less likely to
// conflict with existing symbol names in the examined programs.
// e.g. TOLERATE(entry) yields ToLeRaToR_entry
#define TOLERATE(X) ToLeRaToR_##X

void
TOLERATE(helloworld)() {
  printf("==============================\n"
         "\tHello, World!\n"
         "==============================\n");
}

void
TOLERATE(goodbyeworld)() {
  printf("==============================\n"
         "\tGoodbye, World!\n"
         "==============================\n");
}

// Task 1
void report_invalid_read() {
  std::fprintf(stderr, "FOUND: Invalid read from memory\n");
  std::exit(-1);
}

void report_invalid_write() {
  std::fprintf(stderr, "FOUND: Invalid write to memory\n");
  std::exit(-1);
}

void report_invalid_free() {
  std::fprintf(stderr, "FOUND: Invalid free of memory\n");
  std::exit(-1);
}

void report_division_by_zero() {
  std::fprintf(stderr, "FOUND: Division by zero\n");
  std::exit(-1);
}


}
