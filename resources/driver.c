#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include "printer.h" 
#include "gc.h"

int64_t bird_main(int64_t, int64_t) asm("bird_main");

int main(int argc, char** argv) {
  int64_t size = 2000;
  int64_t memory = malloc(sizeof(int64_t)*size);
  int64_t result = bird_main(memory, memory+size*8);
  printValue(result);
  return 0;
}
