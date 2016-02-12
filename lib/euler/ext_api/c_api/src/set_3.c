/************************************************************************************
 *                                     set_3.c                                      *
 *                                                                                  *
 * Module housing solutions to problems 21-30                                       *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include "sets.h"
#include "set_3.h"
/************************************************************************************
 *                               TOP LEVEL FUNCTIONS                                *
 ************************************************************************************/
/************************************************************************************
 *                                  - problem_22 -                                  *
 * 																																									*
 * Using names.txt (right click and 'Save Link/Target As...'), a 46K text file      *
 * containing over five-thousand first names, begin by sorting it into alphabetical *
 * order. Then working out the alphabetical value for each name, multiply this      *
 * value by its alphabetical position in the list to obtain a name char_score.           *
 * 																																									*
 * For example, when the list is sorted into alphabetical order, COLIN, which is    *
 * worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would  *
 * obtain a char_score of 938 × 53 = 49714.                                              *
 * 																																									*
 * What is the total of all the name char_scores in the file?                            *
 ************************************************************************************/
void problem_22(char *result_buffer)
{
  /* load names from txt file into an array of lists sorted by starting name char */
  struct NameNode **buckets = load_buckets();


  while (buckets[25] != NULL) {
    printf("buckets[25] -> name:       %s\n", buckets[25] -> name);
    printf("buckets[25] -> char_score: %d\n", buckets[25] -> char_score);
    printf("buckets[25] -> sort_score: %d\n", buckets[25] -> sort_score);
    buckets[25] = buckets[25] -> next_ptr;
  }


  sprintf(result_buffer, "%d", 42); /* copy char_score total to buffer */
}
/************************************************************************************
 *                                HELPER FUNCTIONS                                  *
 ************************************************************************************/
struct NameNode **load_buckets(void)
{
  FILE *names_file;            /* pointer to raw data file object */
  struct NameNode **buckets;   /* 26 length array of NameNode lists */
  struct NameNode *node_ptr;   /* points to next free space of NameNode pool */
  struct NameNode **head_dptr; /* points to head of NameNode list in bucket */
  char scan_char;              /* holds next char scanned from file */
  int *char_score_ptr;         /* points to accumulating 'char_score' of NameNode */
  int *sort_score_ptr;         /* points to accumulating 'sort_score' of NameNode */
  int last_max_val;            /* points to accumulating 'sort_score' of NameNode */
  char *char_ptr;              /* points to current char of NameNode's 'name' */

  /* open the names data txt file in read mode */
  names_file   = handle_fopen(NAMES_FILENAME, "r");
  /* allocate memory for 'buckets', initialize head pointers to 'NULL' */
  buckets      = handle_calloc(26, sizeof(struct NameNode *));
  /* allocate sufficient pool of memory for NameNodes */
  node_ptr     = handle_malloc(sizeof(struct NameNode) * SAFE_LEN_NAMES);

  fseek(names_file, 1, SEEK_SET); /* skip first opening quotation mark */
  scan_char = fgetc(names_file);  /* scan in first 'name' char */
  /* while there are remaining names... */
  while (scan_char != EOF) {
    /* link the next NameNode to the bucket corresponding to its name's first char */
    head_dptr = &buckets[scan_char - 'A']; /* point to 'head_ptr' of proper bucket */
    node_ptr -> next_ptr = *head_dptr;     /* point NameNode to old head of bucket */
    *head_dptr           = node_ptr;       /* set NameNode as new head of bucket */
    
    /* assign temporary pointers to minimize refs/derefs */
    char_score_ptr = &(node_ptr -> char_score); /* point to 'char_score' */
    sort_score_ptr = &(node_ptr -> sort_score); /* point to 'sort_score' */
    char_ptr  = node_ptr -> name;               /* point to start of 'name' buffer */


    last_max_val = '@';
    /* until next closing quotation mark is found... */
    while (scan_char != '\"') {
      *char_score_ptr += (scan_char - '@'); /* add char value of 'scan_char' */
      *sort_score_ptr += (scan_char + last_max_val);
      last_max_val    += 'Z';
      *char_ptr   = scan_char;         /* set next 'name' char */
      ++char_ptr;                      /* point to next empty 'name' char */
      scan_char   = fgetc(names_file); /* scan in next 'scan_char' */
    }

    *char_ptr = '\0';               /* terminate completed 'name' string */
    ++node_ptr;                     /* point to next free node in NameNode pool */
    fseek(names_file, 2, SEEK_CUR); /* skip comma delim and next opening '"' */
    scan_char = fgetc(names_file);  /* scan in next 'name' char */
  }

  fclose(names_file); /* close file */

  return buckets;     /* return scanned data */
}
