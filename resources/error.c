#include <stdlib.h>
#include <inttypes.h>

#include "error.h"

void stopWithError(int64_t type) {
  switch (type) {
    case 1:
      printf("Expected an int.\n");
      break;
    case 2: 
      printf("Expected a Boolean.\n");
      break;
    case 3:
      printf("Expected a Tuple.\n");
      break;
    case 4:
      printf("Index out of bounds.\n");
      break;
    case 5:
      printf("Expected a Closure.\n");
      break;
    case 7:
      printf("Heap out of memory.\n");
    case 0:
      break ;
    /* TODO: put your other error cases here */
    default:
      printf("Unknown error %"PRId64" occurred.\n", type);
      break;
  }
  exit(type);
}
