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


  while (buckets[25] != NULL) {
    printf("buckets[25] -> name:  %s\n", buckets[25] -> name);
    printf("buckets[25] -> score: %d\n", buckets[25] -> score);
    buckets[25] = buckets[25] -> next_ptr;
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
    head_dptr = &buckets[scan_char - 'A']; /* point to 'head_ptr' of proper bucket */
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
  struct SortParams *params = (struct SortParams *)params_ptr;
  struct NameNode **buckets = params -> interval;
  const int span            = params -> span;

  struct NameNode **head_anchor;
  struct NameNode **that_anchor;
  struct NameNode *prev_this_ptr;
  struct NameNode *this_ptr;
  struct NameNode *that_ptr;

  struct NameNode *ptr;
  int i;

  /* for (int bucket_i = 0; bucket_i < span; ++bucket_i) { */
  for (int bucket_i = 25; bucket_i < 26; ++bucket_i) {
    prev_this_ptr = buckets[bucket_i];
    if (prev_this_ptr == NULL) {
      continue;
    }

    head_anchor = &prev_this_ptr;

    this_ptr = prev_this_ptr -> next_ptr;

    while (this_ptr != NULL && i < 100) {
      ptr = buckets[25];
      i   = 0;
      while (ptr != NULL) {
        ++i;
        printf("%d. %s |", i, ptr -> name);
        ptr = ptr -> next_ptr;
      }

      puts("\nthis_ptr not NULL, plucking from list:");
      printf("  this_ptr -> name:          %s\n", this_ptr -> name);
      printf("  prev_this_ptr -> name:     %s\n", prev_this_ptr -> name);
      printf("  (*head_anchor) -> name:    %s\n", (*head_anchor) -> name);
      printf("  prev_this_ptr:             %p\n", prev_this_ptr);
      printf("  this_ptr:                  %p\n", this_ptr);
      printf("  prev_this_ptr -> next_ptr: %p\n", prev_this_ptr -> next_ptr);
      printf("  head_anchor:               %p\n", head_anchor);
      printf("  *head_anchor:              %p\n", *head_anchor);

      prev_this_ptr -> next_ptr = this_ptr -> next_ptr;
      prev_this_ptr = prev_this_ptr -> next_ptr;

      that_anchor = head_anchor;

      puts("plucked from list, bridged gap, and advanced prev_this_ptr!");
      printf("  this_ptr -> name:          %s\n", this_ptr -> name);
      printf("  prev_this_ptr -> name:     %s\n", prev_this_ptr -> name);
      printf("  (*head_anchor) -> name:    %s\n", (*head_anchor) -> name);
      printf("  prev_this_ptr:             %p\n", prev_this_ptr);
      printf("  this_ptr:                  %p\n", this_ptr);
      printf("  prev_this_ptr -> next_ptr: %p\n", prev_this_ptr -> next_ptr);
      printf("  head_anchor:               %p\n", head_anchor);
      printf("  *head_anchor:              %p\n", *head_anchor);
      sleep(1);

      while (strcmp((*that_ptr) -> name,
                    this_ptr -> name) < 0) {
        puts("\nadvancing that!");
        printf("  (*that_ptr) -> name: %s\n", (*that_ptr) -> name);
        printf("  this_ptr -> name:    %s\n", this_ptr -> name);
        sleep(1);

        that_anchor = ((*that_anchor) -> next_ptr);
        that_ptr    = that_ptr -> next_ptr;

        if (that_ptr == NULL) {
          puts("end of list, breaking!");
          sleep(1);
          break;
        }
      }

      puts("\ninserting this_ptr in front of that_anchor:");
      printf("  (*that_anchor) -> name:     %s\n", (*that_anchor) -> name);
      printf("  this_ptr -> name:           %s\n", this_ptr -> name);
      printf("  this_ptr:                   %p\n", this_ptr);
      printf("  that_ptr:                   %p\n", that_ptr);
      printf("  (*that_anchor) -> next_ptr: %p\n", (*that_anchor) -> next_ptr);
      sleep(1);

      (*that_anchor) -> next_ptr = this_ptr;
      this_ptr       -> next_ptr = that_ptr;

      puts("insertion complete!");
      printf("  (*that_anchor) -> name:     %s\n", (*that_anchor) -> name);
      printf("  this_ptr -> name:           %s\n", this_ptr -> name);
      printf("  this_ptr:                   %p\n", this_ptr);
      printf("  that_ptr:                   %p\n", that_ptr);
      printf("  (*that_anchor) -> next_ptr: %p\n", (*that_anchor) -> next_ptr);
      sleep(1);


      puts("\nadvancing this_ptr:");
      this_ptr = prev_this_ptr -> next_ptr; /* set next 'this_ptr' */
    }
  }

  printf("buckets: %p\n",  buckets);
  printf("span:     %d\n", span);
  printf("buckets[0] -> head_ptr -> name: %s\n", buckets[0] -> name);
  fflush(stdout);

  return NULL;
}
