// regular struct
struct some_nums {
  int a;
  long b;
  float c;
};

// typedef'd anonymous struct
typedef struct {
  char a;
  char b;
  char c;
} some_characters;

// nested struct
struct outer {
  void* x;
  struct {
    void* y;
  } inner;
};

// deeply nested struct
struct son {
  struct {
    struct {
      int a;      
    } maternal_grandma;
    struct {
      int b;
    } maternal_grandpa;
  } mom;
  struct {
    struct {
      int c;      
    } paternal_grandma;
    struct {
      int d;
    } paternal_grandpa;
  } dad;
};
