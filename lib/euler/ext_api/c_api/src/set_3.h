/************************************************************************************
 *                                    set_3.h                                       *
 *                                                                                  *
 * Houses preprocessor directives, constant declarations, and function prototypes   *
 * needed in module 'set_3.c'.                                                      *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#define SAFE_LEN_NAMES 6000
#define SIZE_NAME_BUF 16
#define NAMES_FILENAME "data/set_3-prob_22-data.txt"
#define MAX_ENTRIES 2e4
/************************************************************************************
 *                               INTIAL DECLARATIONS                                *
 ************************************************************************************/
struct NameNode {
	char name[SIZE_NAME_BUF];
	int score;
	struct NameNode *next_ptr;
};

struct SortParams {
	struct NameNode **interval;
	int span;
};
/************************************************************************************
 *                               FUNCTION PROTOTYPES                                *
 ************************************************************************************/
void problem_22(char *result_buffer);
void problem_24(char *result_buffer);
void problem_29(char *result_buffer);
void problem_30(char *result_buffer);
struct NameNode **load_buckets(void);
void *sort_buckets(void *arg);
void do_permute(int num_rem_digs,
		char *rem_digs,
		char *dig_buff,
		long *perm_count);
void do_next_digit(const long num,
		   const long sum,
		   const long nxt_pos,
		   const long *POW_MAP,
		   long *sol_sum);
