/*
//  control_lists_IO_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "control_lists_IO_c.h"


void alloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item){
	c2r_item->c1_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c2r_item->c2_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c2r_item->r_data = 0.0;
	c2r_item->iflag = 0;
    return;
};

void dealloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item){
    free(c2r_item->c1_tbl);
    free(c2r_item->c2_tbl);
	c2r_item->iflag = 0;
    return;
};

int read_c2r_ctl_item_c(FILE *fp, char buf[LENGTHBUF], 
                          const char *label, struct chara2_real_ctl_item *c2r_item){
	char header_chara[KCHARA_C];
	
	if(c2r_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %s %lf", header_chara, 
					c2r_item->c1_tbl, c2r_item->c2_tbl, &c2r_item->r_data);
		strip_cautation_marks(c2r_item->c1_tbl);
		strip_cautation_marks(c2r_item->c2_tbl);
		c2r_item->iflag = 1;
	};
	return 1;
};

int write_c2r_ctl_item_c(FILE *fp, int level, int maxlen[3], 
                           const char *label, struct chara2_real_ctl_item *c2r_item){
    
	if(c2r_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], c2r_item->c1_tbl);
	write_one_label_cont_c(fp, maxlen[2], c2r_item->c2_tbl);
	fprintf(fp, "%.12e\n", c2r_item->r_data);
    return level;
};


void init_c2r_ctl_list(struct chara2_real_ctl_list *head){
    int i;
    head->mlen3 = (struct maxlen_3 *) malloc(sizeof(struct maxlen_3));
    for (i=0; i<3; i++) {head->mlen3->mlen[i] = 0;};
	
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
