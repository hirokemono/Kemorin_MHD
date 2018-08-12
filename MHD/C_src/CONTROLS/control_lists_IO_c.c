/*
//  control_lists_IO_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "control_lists_IO_c.h"


void init_chara3_ctl_list(struct chara3_ctl_list *head){
    int i;
    head->mlen3 = (struct maxlen_3 *) malloc(sizeof(struct maxlen_3));
    for(i=0; i<3; i++) {head->mlen3->mlen[i] = 0;};
	
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

void clear_chara3_ctl_list(struct chara3_ctl_list *head){
	free(head->mlen3);
	
    head = head->_next;
    while (head != NULL) {
        dealloc_chara3_ctl_item_c(head->c3_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

struct chara3_ctl_list *add_chara3_ctl_list(struct chara3_ctl_list *current){
    struct chara3_ctl_list *added;
    struct chara3_ctl_list *old_next;
    
    if ((added = (struct chara3_ctl_list *) malloc(sizeof(struct chara3_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->c3_item = (struct chara3_ctl_item *) malloc(sizeof(struct chara3_ctl_item))) == NULL) {
        printf("malloc error for c3_item\n");
        exit(0);
    }
	alloc_chara3_ctl_item_c(added->c3_item);
	added->mlen3 = current->mlen3;
    
    /* replace from  current -> p2　to current -> p1 -> p2 */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

void delete_chara3_ctl_list(struct chara3_ctl_list *current){
    struct chara3_ctl_list *old_prev = current->_prev;
    struct chara3_ctl_list *old_next = current->_next;
    
    dealloc_chara3_ctl_item_c(current->c3_item);
    free(current->c3_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

int count_chara3_ctl_list(struct chara3_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        if((int) strlen(head->c3_item->c1_tbl) > head->mlen3->mlen[1]){
            head->mlen3->mlen[1] = (int) strlen(head->c3_item->c1_tbl);
        };
        if((int) strlen(head->c3_item->c2_tbl) > head->mlen3->mlen[2]){ 
            head->mlen3->mlen[2] = (int) strlen(head->c3_item->c2_tbl);
        };
        
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct chara3_ctl_list *set_chara3_ctl_list_pointer(int index, struct chara3_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        if(num == index) break;
        head = head->_next;
        num = num + 1;
    };
    if(head == NULL) printf("array does not exist at index %d of %d.\n", index, num);
    return head;
};

int read_chara3_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara3_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_chara3_ctl_list(head);
        iflag = read_chara3_ctl_item_c(fp, buf, label, head->c3_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

int write_chara3_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara3_ctl_list *head){
    
    int num = count_chara3_ctl_list(head);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    head->mlen3->mlen[0] = (int) strlen(label);
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_chara3_ctl_item_c(fp, level, head->mlen3->mlen,
                                     label, head->c3_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


void init_real3_ctl_list(struct real3_ctl_list *head){
    head->mlen1 = (struct maxlen_1 *) malloc(sizeof(struct maxlen_1));
    head->mlen1->mlen = 0;
	
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

void clear_real3_ctl_list(struct real3_ctl_list *head){
	free(head->mlen1);
	
    head = head->_next;
    while (head != NULL) {
        free(head);
        head = head->_next;
	}
	
    return;
};

struct real3_ctl_list *add_real3_ctl_list(struct real3_ctl_list *current){
    struct real3_ctl_list *added;
    struct real3_ctl_list *old_next;
    
    if ((added = (struct real3_ctl_list *) malloc(sizeof(struct real3_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->r3_item = (struct real3_ctl_item *) malloc(sizeof(struct real3_ctl_item))) == NULL) {
        printf("malloc error for r3_item\n");
        exit(0);
    }
	init_real3_ctl_item_c(added->r3_item);
	added->mlen1 = current->mlen1;
    
    /* replace from  current -> p2　to current -> p1 -> p2 */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

void delete_real3_ctl_list(struct real3_ctl_list *current){
    struct real3_ctl_list *old_prev = current->_prev;
    struct real3_ctl_list *old_next = current->_next;
    
    free(current->r3_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

int count_real3_ctl_list(struct real3_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct real3_ctl_list *set_real3_ctl_list_pointer(int index, struct real3_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        if(num == index) break;
        head = head->_next;
        num = num + 1;
    };
    if(head == NULL) printf("array does not exist at index %d of %d.\n", index, num);
    return head;
};

int read_real3_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real3_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_real3_ctl_list(head);
        iflag = read_real3_ctl_item_c(fp, buf, label, head->r3_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

int write_real3_ctl_list(FILE *fp, int level, const char *label, 
                       struct real3_ctl_list *head){
    
    int num = count_real3_ctl_list(head);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    head->mlen1->mlen = (int) strlen(label);
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_real3_ctl_item_c(fp, level, head->mlen1->mlen,
                                     label, head->r3_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


void init_c2r_ctl_list(struct chara2_real_ctl_list *head){
    int i;
    head->mlen3 = (struct maxlen_3 *) malloc(sizeof(struct maxlen_3));
    for(i=0; i<3; i++) {head->mlen3->mlen[i] = 0;};
	
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

void clear_c2r_ctl_list(struct chara2_real_ctl_list *head){
	free(head->mlen3);
	
    head = head->_next;
    while (head != NULL) {
        dealloc_c2r_ctl_item_c(head->c2r_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

struct chara2_real_ctl_list *add_c2r_ctl_list(struct chara2_real_ctl_list *current){
    struct chara2_real_ctl_list *added;
    struct chara2_real_ctl_list *old_next;
    
    if ((added = (struct chara2_real_ctl_list *) malloc(sizeof(struct chara2_real_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->c2r_item = (struct chara2_real_ctl_item *) malloc(sizeof(struct chara2_real_ctl_item))) == NULL) {
        printf("malloc error for c2r_item\n");
        exit(0);
    }
	alloc_c2r_ctl_item_c(added->c2r_item);
	added->mlen3 = current->mlen3;
    
    /* replace from  current -> p2　to current -> p1 -> p2 */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

void delete_c2r_ctl_list(struct chara2_real_ctl_list *current){
    struct chara2_real_ctl_list *old_prev = current->_prev;
    struct chara2_real_ctl_list *old_next = current->_next;
    
    dealloc_c2r_ctl_item_c(current->c2r_item);
    free(current->c2r_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

int count_c2r_ctl_list(struct chara2_real_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        if((int) strlen(head->c2r_item->c1_tbl) > head->mlen3->mlen[1]){
            head->mlen3->mlen[1] = (int) strlen(head->c2r_item->c1_tbl);
        };
        if((int) strlen(head->c2r_item->c2_tbl) > head->mlen3->mlen[2]){ 
            head->mlen3->mlen[2] = (int) strlen(head->c2r_item->c2_tbl);
        };
        
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct chara2_real_ctl_list *set_c2r_ctl_list_pointer(int index, struct chara2_real_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        if(num == index) break;
        head = head->_next;
        num = num + 1;
    };
    if(head == NULL) printf("array does not exist at index %d of %d.\n", index, num);
    return head;
};

int read_c2r_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara2_real_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_c2r_ctl_list(head);
        iflag = read_c2r_ctl_item_c(fp, buf, label, head->c2r_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

int write_c2r_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara2_real_ctl_list *head){
    
    int num = count_c2r_ctl_list(head);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    head->mlen3->mlen[0] = (int) strlen(label);
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_c2r_ctl_item_c(fp, level, head->mlen3->mlen,
                                     label, head->c2r_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};
