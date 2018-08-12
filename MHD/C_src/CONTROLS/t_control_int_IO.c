/*
//  t_control_int_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_int_IO.h"


void init_int_ctl_item_c(struct int_ctl_item *i_item){
	i_item->i_data = 0;
	i_item->iflag = 0;
    return;
};

int read_integer_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct int_ctl_item *i_item){
	char header_chara[KCHARA_C];
	
	if(i_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %d", header_chara, &i_item->i_data);
		i_item->iflag = 1;
	};
	return 1;
};

int write_integer_ctl_item_c(FILE *fp, int level, int maxlen, 
                           const char *label, struct int_ctl_item *i_item){
    
	if(i_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen, label);
	fprintf(fp,  "%d\n", i_item->i_data);
    return level;
};



void init_int_ctl_list(struct int_ctl_list *head){
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

void clear_int_ctl_list(struct int_ctl_list *head){
    head = head->_next;
    while (head != NULL) {
        init_int_ctl_item_c(head->i_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

struct int_ctl_list *add_int_ctl_list(struct int_ctl_list *current){
    struct int_ctl_list *added;
    struct int_ctl_list *old_next;
    
    if ((added = (struct int_ctl_list *) malloc(sizeof(struct int_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->i_item = (struct int_ctl_item *) malloc(sizeof(struct int_ctl_item))) == NULL) {
        printf("malloc error for i_item\n");
        exit(0);
    }
	init_int_ctl_item_c(added->i_item);
    
    /* replace from  current -> p2　to current -> p1 -> p2 */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

void delete_int_ctl_list(struct int_ctl_list *current){
    struct int_ctl_list *old_prev = current->_prev;
    struct int_ctl_list *old_next = current->_next;
    
    init_int_ctl_item_c(current->i_item);
    free(current->i_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

int count_int_ctl_list(struct int_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct int_ctl_list *set_int_ctl_list_pointer(int index, struct int_ctl_list *head){
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

int read_int_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct int_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_int_ctl_list(head);
        iflag = read_integer_ctl_item_c(buf, label, head->i_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

int write_int_ctl_list(FILE *fp, int level, const char *label, 
                       struct int_ctl_list *head){
    
    int num = count_int_ctl_list(head);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_integer_ctl_item_c(fp, level, (int) strlen(label),
                                     label, head->i_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};

