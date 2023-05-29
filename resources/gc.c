#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gc.h"
#include "error.h"

// These extern declarations allow us to refer to the variables you made global
// in your generated assembly file.

extern uint64_t* start_of_stack;
extern uint64_t* end_of_stack;
extern uint64_t* start_of_heap;
extern uint64_t* end_of_heap;
extern uint64_t* heap_cursor;

/*
  The following macros allow you to use "debugf" in place of "printf" for
  formatted debugging.  For instance, you can write

      debugf("Pointer %p changed to pointer %p.\n", old, new);

  to print in the same way you would use printf.  The advantage is that,
  depending upon which of the two macros you are using, the debugf call either
  turns into a printf call or is erased entirely.  Use debugf to print-debug
  your garbage collector and then disable printing when you need to perform your
  unit tests.
*/

// This macro disables all debugf statements.  (They become no-ops.)
// #define debugf(fmt, ...) ;

// This macro enables all debugf statements.  (They become printf statements.)
#define debugf(fmt, ...) printf(fmt, ##__VA_ARGS__); fflush(stdout)


/**
 * This helper function will show the entire contents of your heap.  This output
 * can get very large if your heap is too big!
 */
void dump_heap() {
  debugf("HEAP:\n");
  int c = 0;
  for (uint64_t* p = (uint64_t*)((uint64_t)start_of_heap & 0xFFFFFFFFFFFFFFFC);
       p < end_of_heap; p += 1) {
    if (c==0) {
      debugf("%016"PRIx64":", p);
    }
    if (p >= start_of_heap) {
      debugf("    %016"PRIx64, *p);
    } else {
      debugf("            ");
    }
    c++;
    if (c==4) {
      debugf("\n");
      c=0;
    }
  }
  if (c!=0) {
    debugf("\n");
  }
}

bool is_pointer (uint64_t* address){ 
  uint64_t pointer = * address;
  if (pointer < start_of_heap){
    return false;
  }
  if (pointer >= heap_cursor){
    return false; 
  }
  if ((pointer & 0x0000000000000001) == 0){
    return false;
  } else if ((pointer & 0x0000000000000002)==0x0000000000000002) { 
    return false;
  } else {
    return true;
  }
}

bool is_dead(uint64_t* address){ // works for bird pointers
  uint64_t* pointer = (uint64_t*)(*address -1);
  if (pointer[1]==0) {
    return true;
  }else { 
    return false;
  }
}

int size_of_pointer(uint64_t* pointer){ // machine pointer
  int64_t size = *pointer;
  if ((size & 0x8000000000000000) == 0) {
    // this is a tuple 
    return (size+2);
  }
  else{
    // this is a closure
    size ^= 0x8000000000000000 ; 
    return (size+4) ;
  }
}

void recursive_marking (uint64_t* address, uint64_t* end) {
  debugf("Address: %p and its value: %x\n", address, *address);
  if (address > end) {
    return;
  }
  else if (is_pointer(address) && is_dead(address)){
    // * (uint64_t*) ((uint64_t) address - 1 + 8 ) = 1;
    uint64_t* new_address = (uint64_t*) (*address - 1); // dereference address, subtract 1
    
    debugf(
    "Address:  %p, new_address: %p, start_of_heap: %p, end_of_heap: %p\n",
    address, new_address, start_of_heap, end_of_heap);

    new_address[1] = 1;
    // * (new_address + 1) = 1; // make alive
    recursive_marking ((new_address + 1), new_address + size_of_pointer(new_address) - 1); // maybe -2
  }

  recursive_marking(address + 1, end);
}



void gc(int64_t desired_free) {
  // TODO: replace the following line with an implementation of the mark-compact
  //       garbage collection algorithm.  (You *will* need helper functions!)

  // To be reviewed : the use of +8 vs +1
  debugf("before Step 1\n");
  dump_heap();
  debugf("end of stack: %p, start of stack: %p\n", end_of_stack, start_of_stack);

  // Step 1 : Marking (any heap object which still has a zero GC word after the mark phase is unreachable and can be destroyed.)
  recursive_marking (end_of_stack, start_of_stack);

  debugf("before Step 2\n");
  dump_heap();
  // Step 2 : Forward (each reachable heap object’s GC word will contain a new heap address)
  uint64_t* next_heap_object = start_of_heap ; 
  uint64_t* next_live_destination = start_of_heap ;
  int size;  
  bool alive ; 
  while (next_heap_object < heap_cursor) {
    size = size_of_pointer(next_heap_object) ;
    alive = next_heap_object[1]; 
    if (alive) {
      next_heap_object[1] = next_live_destination; 
      next_live_destination += size;

      debugf("next_live_destination %p\n", next_live_destination);
    }
    next_heap_object += size;
  }

  debugf("before Step 3\n");
  dump_heap();
  // Step 3 : Update ( stack pointers to reflect the new address)
  uint64_t* current_stack = start_of_stack;
  while (current_stack> end_of_stack) { 
    if (is_pointer(current_stack)){
    // update the location 
    uint64_t* gc_address = (uint64_t*) (*current_stack - 1 + 8);
    *current_stack = gc_address[0]+1;
    // *current_stack = * (uint64_t*) ((uint64_t)current_stack - 1 + 8 ) + 1 ; 
    
    }
    current_stack -= 1; 

  }
  uint64_t* current_heap = start_of_heap; 
  while (current_heap < heap_cursor) { 
    if (is_pointer(current_heap)){
    // update the location 
    uint64_t* gc_address = (uint64_t*) (*current_heap - 1 + 8);
    *current_heap = gc_address[0]+1;
    // *current_heap =  * (uint64_t*) ((uint64_t)current_heap-1 + 8 ) + 1 ; 
    }
    current_heap += 1; 

  }
  
  debugf("before Step 4\n");
  dump_heap();
  //step 4 : Compact ( actually copying the objects to their new locations)
  uint64_t* start_copy; 
  current_heap = start_of_heap; 
  while (current_heap < heap_cursor){
    size = size_of_pointer(current_heap); 
    
    debugf("size of current heap %d\n", size);
    debugf("current heap address %016" PRIx64 "\n", current_heap);
    
    if (current_heap[1]){ // DOES THIS RETURN TRUE???
     start_copy = current_heap[1]; // the gc address
      for (int i=0 ; i<size; i++) {
       start_copy[i]= current_heap[i];
    }
    }
    current_heap += size;
  }

  debugf("before unmarking\n");
  
  heap_cursor = next_live_destination; // the heapcursor needs to change at some point 

  debugf("before Step 5\n");
  dump_heap();
  // step 5 : Unmark 
  
  // current_stack = start_of_stack;
  // while (current_stack> end_of_stack) { 
  //   if (is_pointer(current_stack)){
  //   // update the location
  //   uint64_t* gc_address = (uint64_t*) (*current_stack - 1 + 8);
  //   *gc_address = 0;
  //   // * (uint64_t*) ((uint64_t)current_stack-1+8) = 0; 
  //   }
  //   current_stack -= 1; 

  // }
  next_heap_object = start_of_heap; 
  while (next_heap_object < heap_cursor) {
    size = size_of_pointer(next_heap_object) ;
    alive = next_heap_object[1]; 
    if (alive!=0) {
      next_heap_object[1] = 0;
    }
    next_heap_object += size;
  }
  debugf("after step 5\n");
  dump_heap();

  // tidying up 
  if (((uint64_t) end_of_heap - (uint64_t) heap_cursor) < desired_free){
    dump_heap();
    stopWithError(7);
  }
}
