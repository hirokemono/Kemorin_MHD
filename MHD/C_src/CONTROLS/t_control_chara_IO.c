/*
//  t_control_chara_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara_IO.h"


void alloc_chara_ctl_item_c(struct chara_ctl_item *c_item){
	c_item->c_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	c_item->iflag = 0;
    return;
};

void dealloc_chara_ctl_item_c(struct chara_ctl_item *c_item){
    free(c_item->c_tbl);
	c_item->iflag = 0;
    return;
};

int read_chara_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                               struct chara_ctl_item *c_item){
	char header_chara[KCHARA_C];
	
	if(c_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s", header_chara, c_item->c_tbl);
		strip_cautation_marks(c_item->c_tbl);
		c_item->iflag = 1;
	};
	return 1;
};

int write_chara_ctl_item_c(FILE *fp, int level, int maxlen, 
                           const char *label, struct chara_ctl_item *c_item){
    
	if(c_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	write_one_label_w_lf_c(fp, c_item->c_tbl);
    return level;
};


int find_boolean_from_chara_ctl_item(struct chara_ctl_item *c_item){
    int iflag = 0;
    iflag = cmp_no_case_c(c_item->c_tbl, "ON");
    if(iflag == 0) iflag = cmp_no_case_c(c_item->c_tbl, "YES");
    return iflag;
};

void set_boolean_by_chara_ctl_item(int iflag, struct chara_ctl_item *c_item){
    if(iflag > 0){
        c_item->c_tbl = "On";
    } else {
        c_item->c_tbl = "Off";
    };
    return;
};



void init_chara_ctl_list(struct chara_ctl_list *head){
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

void clear_chara_ctl_list(struct chara_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
        dealloc_chara_ctl_item_c(head->c_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

struct chara_ctl_list *add_chara_ctl_list(struct chara_ctl_list *current){
    struct chara_ctl_list *added;
    struct chara_ctl_list *old_next;
    
    if ((added = (struct chara_ctl_list *) malloc(sizeof(struct chara_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->c_item = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item))) == NULL) {
        printf("malloc error for c_item\n");
        exit(0);
    }
	alloc_chara_ctl_item_c(added->c_item);
    
    /* replace from  current -> p2　to current -> p1 -> p2 */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

void delete_chara_ctl_list(struct chara_ctl_list *current){
    struct chara_ctl_list *old_prev = current->_prev;
    struct chara_ctl_list *old_next = current->_next;
    
    dealloc_chara_ctl_item_c(current->c_item);
    free(current->c_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

int count_chara_ctl_list(struct chara_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct chara_ctl_list *set_chara_ctl_list_pointer(int index, struct chara_ctl_list *head){
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

int read_chara_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_chara_ctl_list(head);
        iflag = read_chara_ctl_item_c(buf, label, head->c_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

int write_chara_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara_ctl_list *head){
    
    int num = count_chara_ctl_list(head);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_chara_ctl_item_c(fp, level, (int) strlen(label),
                                     label, head->c_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};

