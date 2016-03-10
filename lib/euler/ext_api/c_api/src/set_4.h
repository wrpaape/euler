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
struct DivNode {
	int div;
	struct DivNode *next;
};
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void problem_33(char *result_buffer);
void problem_34(char *result_buffer);
void problem_35(char *result_buffer);
void problem_36(char *result_buffer);
struct DivNode **init_mult_map(void);
bool is_bin_palindrome(int n);
bool is_dec_palindrome(int n, int *dig_buff);
int **init_digits_map(void);
struct DivNode **init_divisors_map(void);
int greatest_common_divisor(int num, int den, struct DivNode **mult_map);
bool is_curious(int num, int den, int base_num, int base_den, int **digs_map);
static inline bool mult_match(int base_num, int match_num,
			      int base_den, int match_den);
size_t num_prime_buckets(int base);
size_t hash_digits_cycle(struct IntNode *dig_cyc, const int num_digs);
