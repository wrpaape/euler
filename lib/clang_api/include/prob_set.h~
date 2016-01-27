/************************************************************************************
 *                                    prob_set.h                                    *
 *                                                                                  *
 * Defines types 'ProbType' and 'SetType' that compose the problem set structure.   *
 ************************************************************************************/
/************************************************************************************
 *                               INITIAL DECLARATIONS                               *
 ************************************************************************************/
typedef struct ProbNode {
  static unsigned int num;    /* problem number identifier */
  struct *ProbNode next_ptr;  /* points to next problem in set */
  extern char *(*func)(void); /* pointer to problem function */
} ProbType;

typedef struct SetNode {
  static unsigned int num;    /* problem set number identifier */
  struct *SetNode next_ptr;   /* points to next problem set */
  struct *ProbNode prob_ptr;  /* points to first problem in set */
} SetType;
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
extern SetType *init_prob_set(void);
extern ProbType *init_prob(const unsigned int prob_num, ProbType *next_prob_ptr, char *(*prob_func)(void));
extern SetType *init_set(const unsigned int set_num, SetType *next_set_ptr, ProbType *head_prob_ptr);



