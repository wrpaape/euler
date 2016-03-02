/************************************************************************************
 *                                    set_4.h                                       *
 *                                                                                  *
 * Houses preprocessor directives, constant declarations, and function prototypes   *
 * needed in module 'set_4.c'.                                                      *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
/************************************************************************************
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
struct Fraction {
	int num;
	int den;
};

struct MultNode {
	int mult;
	struct MultNode *next;
};
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void problem_33(char *result_buffer);
struct MultNode **init_mult_map(void);
int **init_digs_map(void);
int attempt_reduce(struct Fraction *frac, struct MultNode **mult_map);
