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
bool attempt_reduce(int *ini_num, int *ini_den, struct MultNode **mult_map);
bool is_non_trivial(int ini_num, int ini_den,
		    int red_num, int red_den, int **digs_map);
int greatest_common_divisor(int num, int den);
