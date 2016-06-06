/*
 * SECD-like bytecode interpreter in C
 * Pedro Vasconcelos, 2013
 *
 * Adapted by Pedro Paredes
 */
#include <stdint.h>

/* opcodes */
#define HALT    0
#define LDC     1
#define LD      2
#define ADD     3
#define SUB     4
#define MUL     5
#define SEL     6
#define LDF     7
#define LDRF    8
#define AP      9
#define RTN     10
#define JOIN    11
#define PAIR    12
#define FST     13
#define SND     14
#define CONS    15
#define MATCH   16
#define RECORD  17
#define SELECT  18
#define LST     19
#define GTT     20
#define ROUT    21

/* values: 
   either an int or a pointer
 */
typedef intptr_t value_t;

/* environments:
   single linked list of values 
*/
typedef struct _env_node_t {
  value_t elm;
  struct _env_node_t *next;
} env_node_t;

typedef env_node_t *env_t;

/* closures 
*/
typedef struct {
  int pc;      // program counter
  env_t env;   // environment
} closure_t;

/* constructor
*/
typedef struct {
  env_t env;   // environment
} cons_t;

/* dump entry
   pair of program counter and environment pointer
 */
typedef struct {
  int pc;
  env_t env;
} dump_t;

/* global segment sizes */
#define CODE_MAX  1000
#define STACK_MAX 1000
#define DUMP_MAX  1000

