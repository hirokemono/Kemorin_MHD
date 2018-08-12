/*
//  t_control_chara_int_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara_int_IO.h"


void alloc_chara_int_ctl_item_c(struct chara_int_ctl_item *ci_item){
	ci_item->c_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	ci_item->i_data = 0;
	ci_item->iflag = 0;
    return;
};

void dealloc_chara_int_ctl_item_c(struct chara_int_ctl_item *ci_item){
    free(ci_item->c_tbl);
	ci_item->i_data = 0;
	ci_item->iflag = 0;
    return;
};

int read_chara_int_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                              struct chara_int_ctl_item *ci_item){
	char header_chara[KCHARA_C];
	
	if(ci_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %d", header_chara, 
					ci_item->c_tbl, &ci_item->i_data);
		strip_cautation_marks(ci_item->c_tbl);
		ci_item->iflag = 1;
	};
	return 1;
};

int write_chara_int_ctl_item_c(FILE *fp, int level, int maxlen[2], 
                           const char *label, struct chara_int_ctl_item *ci_item){
    
	if(ci_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], ci_item->c_tbl);
	fprintf(fp, "%d\n", ci_item->i_data);
    return level;
};



void init_chara_int_ctl_list(struct chara_int_ctl_list *head){
    int i;
    head->mlen2 = (struct maxlen_2 *) malloc(sizeof(struct maxlen_2));
    for(i=0; i<2; i++) {head->mlen2->mlen[i] = 0;};
	
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

void clear_chara_int_ctl_list(struct chara_int_ctl_list *head){
	free(head->mlen2);
	
    head = head->_next;
    while (head != NULL) {
        dealloc_chara_int_ctl_item_c(head->ci_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

struct chara_int_ctl_list *add_chara_int_ctl_list(struct chara_int_ctl_list *current){
    struct chara_int_ctl_list *added;
    struct chara_int_ctl_list *old_next;
    
    if ((added = (struct chara_int_ctl_list *) malloc(sizeof(struct chara_int_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->ci_item = (struct chara_int_ctl_item *) malloc(sizeof(struct chara_int_ctl_item))) == NULL) {
        printf("malloc error for ci_item\n");
        exit(0);
    }
	alloc_chara_int_ctl_item_c(added->ci_item);
	added->mlen2 = current->mlen2;
    
    /* replace from  current -> p2　to current -> p1 -> p2 */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

void delete_chara_int_ctl_list(struct chara_int_ctl_list *current){
    struct chara_int_ctl_list *old_prev = current->_prev;
    struct chara_int_ctl_list *old_next = current->_next;
    
    dealloc_chara_int_ctl_item_c(current->ci_item);
    free(current->ci_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

int count_chara_int_ctl_list(struct chara_int_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        if((int) strlen(head->ci_item->c_tbl) > head->mlen2->mlen[1]){
            head->mlen2->mlen[1] = (int) strlen(head->ci_item->c_tbl);
        };
        
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct chara_int_ctl_list *set_chara_int_ctl_list_pointer(int index, struct chara_int_ctl_list *head){
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

int read_chara_int_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_int_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_chara_int_ctl_list(head);
        iflag = read_chara_int_ctl_item_c(buf, label, head->ci_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

int write_chara_int_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara_int_ctl_list *head){
    
    int num = count_chara_int_ctl_list(head);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    head->mlen2->mlen[0] = (int) strlen(label);
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_chara_int_ctl_item_c(fp, level, head->mlen2->mlen,
                                     label, head->ci_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};

