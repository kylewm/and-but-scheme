#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "bstrlib.h"

typedef enum {
  THE_EMPTY_LIST, FIXNUM, SYMBOL, PAIR, BOOLEAN, PRIMITIVE, NON_PRIMITIVE
} object_type;

typedef struct object {
  object_type type;
  void* data;
} object;

typedef struct fixnum {
  long value;
} fixnum;

typedef struct boolean {
  char value;
} boolean;

typedef struct symbol {
  bstring value; 
} symbol;

typedef struct pair {
  object* car;
  object* cdr;
} pair;

typedef object*(*primitive_fn)(object*,object*);

typedef struct primitive {
  primitive_fn fn;
} primitive;

typedef struct non_primitive {
  object* table;
  object* formals;
  object* body;
} non_primitive;

object* globals;
object* the_empty_list;
object* true;
object* false;

void write(FILE* outs, object* sexp);

void* safe_malloc(size_t size) {
  void* result;
  if((result = malloc(size)) == NULL) {
    fprintf(stderr, "Out of memory.");
    exit(1);
  }
  return result;
}

object* make_fixnum(long value) {
  object* result;
  fixnum* data;

  data = safe_malloc(sizeof(fixnum));
  data->value = value;

  result = safe_malloc(sizeof(object));
  result->type = FIXNUM;
  result->data = data;
  
  return result;
}

object* make_symbol(bstring value) {
  object* result;
  symbol* data;

  data = safe_malloc(sizeof(symbol));
  data->value = value;

  result = safe_malloc(sizeof(object));
  result->type = SYMBOL;
  result->data = data;
  
  return result;
}

object* make_boolean(char value) {
  object* result;
  boolean* data;

  data = safe_malloc(sizeof(boolean));
  data->value = value;

  result = safe_malloc(sizeof(object));
  result->type = BOOLEAN;
  result->data = data;
  
  return result;
}

object* make_primitive(primitive_fn fn) {
  object* result;
  primitive* data;

  data =safe_malloc(sizeof(primitive));
  data->fn = fn;

  result = safe_malloc(sizeof(object));
  result->type = PRIMITIVE;
  result->data = data;

  return result;
}

object* make_non_primitive(object* table, object* formals, object* body) {
  object* result;
  non_primitive* data;

  data =safe_malloc(sizeof(non_primitive));
  data->table = table;
  data->formals = formals;
  data->body = body;

  result = safe_malloc(sizeof(object));
  result->type = NON_PRIMITIVE;
  result->data = data;

  return result;
}

char is_true(object* obj) {
  return obj == true;
}

char is_false(object* obj) {
  return !is_true(obj);
}

object* cons(object* car_obj, object* cdr_obj) {
  object* result;
  pair* data;

  data = safe_malloc(sizeof(pair));
  data->car = car_obj;
  data->cdr = cdr_obj;

  result = safe_malloc(sizeof(object));
  result->type = PAIR;
  result->data = data;

  return result;
}

object* car(object* obj) {
  return ((pair*) obj->data)->car;
}

object* cdr(object* obj) {
  return ((pair*) obj->data)->cdr;
}

#define cadr(x) car(cdr(x))
#define cddr(x) cdr(cdr(x))
#define cdddr(x) cdr(cddr(x))
#define caddr(x) car(cddr(x))
#define cadddr(x) car(cdddr(x))

char is_the_empty_list(object* pair) {
return pair == the_empty_list;
}

object* is_null_proc(object* arguments, object* table) {
  object* arg = car(arguments);
  return is_the_empty_list(arg) ? true : false;
}


object* add_proc(object* arguments, object* table) {
  long result = 0;
  while(!is_the_empty_list(arguments)) {
    object* head = car(arguments);
    result += ((fixnum*) head->data)->value;
    arguments = cdr(arguments);
  }
  return make_fixnum(result);
}

object* sub_proc(object* arguments, object* table) {
  long result = 0;
  object* head;
  head = car(arguments);
  arguments = cdr(arguments);
  result = ((fixnum*) head->data)->value;

  while(!is_the_empty_list(arguments)) {
    head = car(arguments);
    result -= ((fixnum*) head->data)->value;
    arguments = cdr(arguments);
  }
  return make_fixnum(result);
}

object* mult_proc(object* arguments, object* table) {
  long num1 = ((fixnum*) car(arguments)->data)->value;
  long num2 = ((fixnum*) cadr(arguments)->data)->value;
  return make_fixnum(num1 * num2);
}

object* div_proc(object* arguments, object* table) {
  long num1 = ((fixnum*) car(arguments)->data)->value;
  long num2 = ((fixnum*) cadr(arguments)->data)->value;
  return make_fixnum(num1 / num2);
}

object* is_eq_number_proc(object* arguments, object* table) {
  long initial;
  object* head;

  head = car(arguments);
  arguments = cdr(arguments);
  initial = ((fixnum*) head->data)->value;

  while(!is_the_empty_list(arguments)) {
    long value;
    head = car(arguments);
    value = ((fixnum*) head->data)->value;
    if(initial != value) 
      return false;
    arguments = cdr(arguments);
  }
  return true;
}

object* is_zero_proc(object* arguments, object* table) {
  object* head = car(arguments);
  long value = ((fixnum*) head->data)->value;
  return value == 0 ? true : false;
}

object* is_lt_proc(object* arguments, object* table) {
  object* a = car(arguments);
  object* b = cadr(arguments);
  long va = ((fixnum*) a->data)->value;
  long vb = ((fixnum*) b->data)->value;
  return va < vb ? true : false;
}

object* is_gt_proc(object* arguments, object* table) {
  object* a = car(arguments);
  object* b = cadr(arguments);
  long va = ((fixnum*) a->data)->value;
  long vb = ((fixnum*) b->data)->value;
  return va > vb ? true : false;
}

object* is_eq_proc(object* arguments, object* table) {
  object* a = car(arguments);
  object* b = cadr(arguments);

  if(a->type == SYMBOL && b->type == SYMBOL) {
    bstring a_symb = ((symbol*) a->data)->value;
    bstring b_symb = ((symbol*) b->data)->value;
    return biseq(a_symb, b_symb) ? true : false;
  }
  else if(a->type == FIXNUM && b->type == FIXNUM) {
    long a_value = ((fixnum*) a->data)->value;
    long b_value = ((fixnum*) b->data)->value;
    return a_value == b_value ? true : false;
  }
  else {
    return false;
  }
}

void add_global_binding(object* identifier, object* value) {
  object* frame = car(globals);
  object* new_ids = cons(identifier, car(frame));
  object* new_vals = cons(value, cadr(frame));
  ((pair*) frame->data)->car = new_ids;
  ((pair*) frame->data)->cdr = cons(new_vals, the_empty_list);
}

object* make_lambda(object* arguments, object* table) {
  /* lambda represented as ((table) (formals) (body))*/
  object* formals;
  object* body;
  
  formals = car(arguments);
  body = cadr(arguments);
  return make_non_primitive(table, formals, body);
}

object* build_table_entry(object* identifiers, object* values) {
  return cons(identifiers, 
              cons(values, the_empty_list));
}

object* extend_table(object* table, object* formals, object* arguments) {
  return cons(build_table_entry(formals, arguments), 
              table);
}

object* lookup_in_table(object* table, object* identifier) {
  object* entry;
  object* identifiers;
  object* values;

  if(is_the_empty_list(table)) {
    fprintf(stderr, "Could not find value for identifier: ");
    write(stderr, identifier);
    fprintf(stderr, "\n");
    exit(1);
  }

  entry = car(table);
  identifiers = car(entry);
  values = cadr(entry);

  while(!is_the_empty_list(identifiers)) {

    object* candidate = car(identifiers);
    object* candidate_value = car(values);

    if(identifier->type == SYMBOL && candidate->type == SYMBOL &&
       biseq(((symbol*) identifier->data)->value, ((symbol*) candidate->data)->value))
      return candidate_value;

    identifiers = cdr(identifiers);
    values = cdr(values);
  }

  return lookup_in_table(cdr(table), identifier);
}

object* eval(object* sexp, object* table);

object* eval_if(object* arguments, object* table) {
  object* condition = eval(car(arguments), table);
  object* if_true = cadr(arguments);
  object* if_false = is_the_empty_list(cddr(arguments)) ? false : caddr(arguments);

  return eval(is_true(condition) ? if_true : if_false, table);
}


object* apply_closure(object* table, object* formals, object* body, object* arguments) {
  object* new_table = extend_table(table, formals, arguments);
  return eval(body, new_table);
}

object* apply(object* operator, object* arguments, object* table) {
  if(operator->type == PRIMITIVE) {
    primitive_fn fn = ((primitive*) operator->data)->fn;
    return (*fn)(arguments, table);
  }
  else if(operator->type == NON_PRIMITIVE) {
    non_primitive* nonprim = ((non_primitive*) operator->data);
    return apply_closure(nonprim->table, nonprim->formals, nonprim->body, arguments);
  }
  else if(operator->type == SYMBOL) {
    /* check known primitives */
    bstring symb = ((symbol*) operator->data)->value;
    fprintf(stderr, "unknown operator %s\n", symb->data);
    exit(1);
  }
  else {
    fprintf(stderr, "unknown operator of unrecognized type: %d\n", operator->type);
    exit(1);
  }

}

object* eval_list_of_args(object* sexp, object* table) {
  if(is_the_empty_list(sexp)) {
    return the_empty_list;
  }
  else {
    object* first = car(sexp);
    object* rest = cdr(sexp);
    return cons(eval(first, table), 
                eval_list_of_args(rest, table));
  }
}

object* eval_pair(object* operator, object* arguments, object* table) {
  if(operator->type == SYMBOL) {
    bstring symb = ((symbol*) operator->data)->value;
    if(biseqcstr(symb, "lambda")) {
      return make_lambda(arguments, table);
    }
    else if(biseqcstr(symb, "quote")) {
      return car(arguments);
    }
    else if(biseqcstr(symb, "define")) {
      object* identifier = car(arguments);
      object* value = eval(cadr(arguments), table);
      add_global_binding(identifier, value);
      return value;
    }
    else if(biseqcstr(symb, "if")) {
      return eval_if(arguments, table);
    }
  }
  return apply(eval(operator, table), eval_list_of_args(arguments, table), table);
}

object* eval(object* sexp, object* table) {
  switch(sexp->type) {
  case THE_EMPTY_LIST:
  case FIXNUM:
  case BOOLEAN:
  case PRIMITIVE:
  case NON_PRIMITIVE:
    return sexp;
  case SYMBOL:
    return eval(lookup_in_table(table, sexp), table);
  case PAIR: {
    object* operator = car(sexp);
    object* arguments = cdr(sexp);
    return eval_pair(operator, arguments, table);
  }
  }

  return sexp;
}


void register_procedure(bstring property, primitive_fn fn) {
  add_global_binding(make_symbol(property), make_primitive(fn));
}

void init() {
  the_empty_list = safe_malloc(sizeof(object));
  the_empty_list->type = THE_EMPTY_LIST;
  the_empty_list->data = NULL;

  globals = cons(build_table_entry(the_empty_list, the_empty_list),
                 the_empty_list);

  register_procedure(bfromcstr("+"), add_proc);
  register_procedure(bfromcstr("-"), sub_proc);
  register_procedure(bfromcstr("*"), mult_proc);
  register_procedure(bfromcstr("/"), div_proc);
  register_procedure(bfromcstr("zero?"), is_zero_proc);
  register_procedure(bfromcstr("null?"), is_null_proc);
  register_procedure(bfromcstr("eq?"), is_eq_proc);
  register_procedure(bfromcstr("<"), is_lt_proc);
  register_procedure(bfromcstr(">"), is_gt_proc);
  register_procedure(bfromcstr("="), is_eq_number_proc);

  false = make_boolean(0);
  true = make_boolean(1);
}

/* Reader and Writer */

object* parse_fixnum(bstring str) {
  long value;

  value = atol((const char*) str->data);
  return make_fixnum(value);
}

char is_delimiter(int c) {
  return isspace(c) || c == '(' || c == ')';
}

char is_symbol_initial(int c) {
  return isalpha(c) || c == '_' || c == '+' || c == '-' || c == '*' || c == '<' 
    || c == '>' || c == '?' || c == '/' || c == '=';
}

char is_symbol_allowed(int c) {
  return is_symbol_initial(c) || isdigit(c);
}

int peek(FILE* ins) {
  int c = getc(ins);
  ungetc(c, ins);
  return c;
}

void chomp_whitespace(FILE* ins) {
  int c;
  while((c = getc(ins)) != EOF) {
    if(!isspace(c)) {
      ungetc(c, ins);
      break;
    }
  }
}

object* read(FILE* ins);

object* read_pair(FILE* ins) {
  int c;
  object* car_obj;
  object* cdr_obj;

  /* we've already read the open ( */
  chomp_whitespace(ins);

  c = getc(ins);
  if(c == ')') {
    return the_empty_list;
  }
  ungetc(c, ins);

  car_obj = read(ins);
  cdr_obj = read_pair(ins);
  return cons(car_obj, cdr_obj);
}

object* read(FILE* ins) {
  int c;
  
  chomp_whitespace(ins);
  c = getc(ins);

  if(c == '(') {
    return read_pair(ins);
  }
  else if(c == '#') {
    c = getc(ins);
    if(c == 't' && is_delimiter(peek(ins))) {
      return true;
    }
    else if(c == 'f' && is_delimiter(peek(ins))) {
      return false;
    }
    else {
      fprintf(stderr, "Unrecognized literal starting with: #%c", c);
      exit(1);
    }
  }
  else if(isdigit(c) || (c == '-' && isdigit(peek(ins)))) {
    bstring buffer = bfromcstralloc(5, "");
    bconchar(buffer, c);
    while(isdigit(c = getc(ins))) {
      bconchar(buffer, c);
    }
    if(is_delimiter(c)) {
      object* result;
      ungetc(c, ins);
      result = parse_fixnum(buffer);
      bdestroy(buffer);
      return result;
    }
    else {
      fprintf(stderr, "Number '%s' followed by non-delimiter character '%c'\n", buffer->data, c);
      exit(1);
    }
  }
  else if(is_symbol_initial(c)) {
    bstring buffer = bfromcstralloc(5, "");
    bconchar(buffer, c);
    while(is_symbol_allowed(c = getc(ins))) {
      bconchar(buffer, c);
    }
    if(is_delimiter(c)) {
      ungetc(c, ins);
      return make_symbol(buffer);
    }
    else {
      fprintf(stderr, "Symbol '%s' contains disallowed character '%c'\n", buffer->data, c);
      exit(1); 
    }
  }
  else {
    fprintf(stderr, "Unrecognized token starting with '%c'\n", c);
    exit(1);
  }
  
  fprintf(stderr, "Illegal state\n");
  exit(1);
}

void write(FILE* outs, object* sexp) {
  switch(sexp->type){
  case FIXNUM: {
    fixnum* num = (fixnum*) sexp->data;
    fprintf(outs, "%ld", num->value);
    break;
  }
  case BOOLEAN: {
    fprintf(outs, sexp == true ? "#t" : "#f");
    break;
  }
  case SYMBOL: {
    symbol* sym = (symbol*) sexp->data;
    fprintf(outs, "%s", sym->value->data);
    break;
  }
  case PAIR: {
    object* car_obj;
    object* cdr_obj;
    cdr_obj = sexp;

    fprintf(outs, "(");
    while(!is_the_empty_list(cdr_obj)) {
      car_obj = car(cdr_obj);
      cdr_obj = cdr(cdr_obj);

      write(outs, car_obj);
      if(!is_the_empty_list(cdr_obj))
        fprintf(outs, " ");
    }
    fprintf(outs, ")");
    break;
  }
  case THE_EMPTY_LIST: {
    fprintf(outs, "()");
    break;
  }
  case PRIMITIVE: {
    fprintf(outs, "<primitive>");
    break;
  }
  case NON_PRIMITIVE: {
    fprintf(outs, "<non-primitive>");
    break;
  }
  default: {
    fprintf(outs, "<unknown>");
    break;
  }
  }
}

int main(int argc, char** argv) {
  object* sexp;
  object* result;

  printf("and but scheme.\n");

  init();

  for(;;) {
    printf("> ");
    sexp = read(stdin);
    if(sexp == NULL)
      break;
    result = eval(sexp, globals);
    write(stdout, result);
    printf("\n");
  }

  return 0;
}
