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
void init_odd_dec_pals(int *pal);
void add_mid_digs(int odd_base, int lil_delta, int big_delta, int **pal);
struct DivNode **init_mult_map(void);
bool is_bin_palindrome(const int n);
int **init_digits_map(void);
struct DivNode **init_divisors_map(void);
int greatest_common_divisor(int num, int den, struct DivNode **mult_map);
bool is_curious(int num, int den, int base_num, int base_den, int **digs_map);
static inline bool mult_match(int base_num, int match_num,
			      int base_den, int match_den);
size_t num_prime_buckets(int base);
size_t hash_digits_cycle(struct IntNode *dig_cyc, const int num_digs);
