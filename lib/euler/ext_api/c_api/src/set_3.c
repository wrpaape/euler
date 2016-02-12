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
 * value by its alphabetical position in the list to obtain a name score.           *
 * 																																									*
 * For example, when the list is sorted into alphabetical order, COLIN, which is    *
 * worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would  *
 * obtain a score of 938 × 53 = 49714.                                              *
 * 																																									*
 * What is the total of all the name scores in the file?                            *
 ************************************************************************************/
void problem_22(char *result_buffer)
{
  struct NameNode **buckets = load_buckets();       /* load names from txt file */
  /* struct NameNode *name_tups = buckets -> name_tups; /1* set pointer to 'name_tups' *1/ */
  /* const size_t num_names    = buckets -> num_names;  /1* set total count of names *1/ */


  printf("buckets[12] -> name:  %s\n",   buckets[12] -> name);
  printf("buckets[12] -> score: %lld\n", buckets[12] -> score);


  /* printf("first name:  %s\n",  name_tups[0].name); */
  /* printf("first score: %hu\n", name_tups[0].score); */
  /* printf("last name:   %s\n",  name_tups[num_names - 1].name); */
  /* printf("last score:  %hu\n", name_tups[num_names - 1].score); */
  /* printf("num_names:   %lu\n", num_names); */ 

  sprintf(result_buffer, "%d", 42); /* copy score total to buffer */
}
/************************************************************************************
 *                                HELPER FUNCTIONS                                  *
 ************************************************************************************/
struct NameNode **load_buckets(void)
{
  FILE *names_file;                /* pointer to raw data file object */
  struct NameNode **buckets; /* 26 length array of NameNode lists */
  struct NameNode *node_ptr;       /* points to next free space of NameNode pool */
  struct NameNode **head_dptr;  /* points to head of NameNode list in bucket */
  /* size_t name_i;                   /1* running counter of scanned names *1/ */
  /* char char_slot; */
  char scan_char;                  /* holds next char scanned from file */
  long long *score_ptr;  /* points to accumulating 'score' of current NameNode */
  char *char_ptr;        /* points to current char of current NameNode 'name' */

  /* conservative estimate of required memory to safely load in data */
  /* const size_t SIZE_NODE_POOL = sizeof(struct NameNode) * SAFE_LEN_NAMES; */
  /* const size_t SIZE_BUCKETS   = sizeof(struct NameBucket) * 26; */
  
  /* open the names data txt file in read mode */
  names_file   = handle_fopen(NAMES_FILENAME, "r");
  /* allocate memory for 'buckets', initialize head pointers to 'NULL' */
  buckets      = handle_calloc(26, sizeof(struct NameNode *));
  /* allocate sufficient pool of memory for NameNodes */
  node_ptr     = handle_malloc(sizeof(struct NameNode) * SAFE_LEN_NAMES);

  fseek(names_file, 1, SEEK_SET); /* skip first opening quotation mark */
  scan_char = fgetc(names_file);  /* scan in first 'name' char */
  /* name_i    = 0;                  /1* initialize name counter *1/ */
  /* while there are remaining names... */
  while (scan_char != EOF) {

    /* link the next NameNode to the bucket corresponding to its name's first char */
    head_dptr = &buckets[scan_char - 'A'];

    node_ptr -> next_ptr = *head_dptr; /* point NameNode to old head of bucket */
    *head_dptr            = node_ptr;  /* set NameNode as new head of bucket */
    
    score_ptr = &(node_ptr -> score); /* point to 'score' of current NameNode */
    char_ptr  = node_ptr -> name;   /* point to start of 'name' buffer */

    /* until next closing quotation mark is found... */
    while (scan_char != '\"') {
      *score_ptr += (scan_char - '@'); /* add char value of 'scan_char' to score */
      *char_ptr   = scan_char;         /* set next 'name' char */
      ++char_ptr;                      /* point to next empty 'name' char */
      scan_char   = fgetc(names_file); /* scan in next 'scan_char' */
    }

    *char_ptr = '\0';               /* terminate completed 'name' string */
    /* ++name_i;                       /1* increment name counter *1/ */
    ++node_ptr;                     /* point to next free node in NameNode pool */
    fseek(names_file, 2, SEEK_CUR); /* skip comma delim and next opening '"' */
    scan_char = fgetc(names_file);  /* scan in next 'name' char */

    break; /* test */
  }

  fclose(names_file); /* close file */

  return buckets;     /* return scanned data */
}
