/************************************************************************************
 *                                     set_3.c                                      *
 *                                                                                  *
 * Module housing solutions to problems 21-30                                       *
 ************************************************************************************/
/************************************************************************************
 *                             PREPROCESSOR DIRECTIVES                              *
 ************************************************************************************/
#include <unistd.h>
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
  struct SortParams sort_params_arr[4] = {
    [0 ... 1] = { .span = 7 },
    [2 ... 3] = { .span = 6 },
  };
  pthread_t sort_threads[4];
  int q_i;
  int offset;
  int spans[4] = {7, 7, 6, 6};


  /* load names from txt file into an array of lists sorted by starting name char */
  struct NameNode **buckets;

  buckets = load_buckets();

  for (q_i = 0, offset = 0; q_i < 1; ++q_i, offset += spans[q_i]) {
    sort_params_arr[q_i].interval = &buckets[offset];

    handle_pthread_create(&sort_threads[q_i],
                          NULL,
                          sort_buckets,
                          (void *) &sort_params_arr[q_i]);
  }

  /* await threads */
  for (q_i = 0; q_i < 1; ++q_i) {
    handle_pthread_join(sort_threads[q_i], NULL);
  }


  sprintf(result_buffer, "%d", 42); /* copy score total to buffer */
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
  int *score_ptr;             /* points to accumulating 'score' of NameNode */
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
    head_dptr = &buckets[scan_char - 'A']; /* point to 'new_head_ptr' of proper bucket */
    node_ptr -> next_ptr = *head_dptr;     /* point NameNode to old head of bucket */
    *head_dptr           = node_ptr;       /* set NameNode as new head of bucket */
    
    /* assign temporary pointers to minimize refs/derefs */
    score_ptr = &(node_ptr -> score); /* point to 'score' */
    char_ptr  = node_ptr -> name;               /* point to start of 'name' buffer */


    /* until next closing quotation mark is found... */
    while (scan_char != '\"') {
      *score_ptr += (scan_char - '@'); /* add char value of 'scan_char' */
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



void *sort_buckets(void *params_ptr)
{
  struct SortParams *params;     /* pthread arg struct */
  struct NameNode **bucket_ptr;  /* points to current head of unsorted bucket list */
  struct NameNode *old_head_ptr; /* head of unsorted bucket */
  struct NameNode *new_head_ptr; /* head of sorted list */
  struct NameNode *this_ptr;     /* node from bucket to be inserted into sorted list */
  struct NameNode *that_ptr;     /* comparison node when traversing sorted list */
  struct NameNode *prev_ptr;     /* prev node in sorted list, points to 'that_ptr' */
  int rem_buckets;               /* indicates remaining unsorted buckets in interval */

  params      = (struct SortParams *) params_ptr;
  bucket_ptr  = params -> interval;

  /* for all consecutive NameNode buckets this thread is responsible for... */
  for (rem_buckets = params -> span; rem_buckets > 0; ++bucket_ptr, --rem_buckets) {
    /* if there are no nodes in the bucket, continue to the next one */
    new_head_ptr = *bucket_ptr;
    if (new_head_ptr == NULL) {
      continue;
    }

    old_head_ptr = new_head_ptr -> next_ptr; /* set next in line to second node */
    new_head_ptr -> next_ptr = NULL;         /* init sorted list as head node */

    /* while there are nodes left in the unsorted bucket... */
    while (old_head_ptr != NULL) {
      /* pop next head from bucket to be compared and inserted into sorted list */
      this_ptr     = old_head_ptr;
      old_head_ptr = old_head_ptr -> next_ptr;

      /* if 'this_ptr''s  name comes before 'new_head_ptr''s name... */
      if (strcmp(this_ptr     -> name,
                 new_head_ptr -> name) < 0) {

        /* make 'this_ptr' the new head of sorted list and continue */
        this_ptr -> next_ptr = new_head_ptr;
        new_head_ptr = this_ptr;
        continue;
      }

      /* otherwise traverse the sorted list until a home for 'this_ptr' is found */
      prev_ptr = new_head_ptr;
      that_ptr = prev_ptr -> next_ptr;

      /* while there are nodes remaining, and 'this_ptr''s name comes after 'that_ptr''s name...*/
      while (that_ptr != NULL &&
             (strcmp(this_ptr -> name,
                     that_ptr -> name) > 0)) {

        /* advance the comparison node 'that_ptr' and its prev node */
        prev_ptr = that_ptr;
        that_ptr = that_ptr -> next_ptr;
      }

      /* insert 'this_ptr' between 'prev_ptr' and 'that_ptr' and continue */
      prev_ptr -> next_ptr = this_ptr;
      this_ptr -> next_ptr = that_ptr;
    }

    *bucket_ptr = new_head_ptr; /* point bucket at new sorted list */
  }

  return NULL; /* must return something for pthread routine */
}
