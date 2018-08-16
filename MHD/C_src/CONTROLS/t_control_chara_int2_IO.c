/*
//  t_control_chara_int2_IO.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#include "t_control_chara_int2_IO.h"


void alloc_chara_int2_ctl_item_c(struct chara_int2_ctl_item *ci2_item){
	ci2_item->c_tbl = (char *)calloc(KCHARA_C, sizeof(char));
	ci2_item->i_data[0] = 0;
	ci2_item->i_data[1] = 0;
	ci2_item->iflag = 0;
    return;
};

void dealloc_chara_int2_ctl_item_c(struct chara_int2_ctl_item *ci2_item){
    free(ci2_item->c_tbl);
	ci2_item->i_data[0] = 0;
	ci2_item->i_data[1] = 0;
	ci2_item->iflag = 0;
    return;
};

int read_chara_int2_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                              struct chara_int2_ctl_item *ci2_item){
	char header_chara[KCHARA_C];
	
	if(ci2_item->iflag > 0) return 0;
	
	sscanf(buf, "%s", header_chara);
	if(cmp_no_case_c(header_chara, label) > 0){
		sscanf(buf, "%s %s %d %d", header_chara, 
					ci2_item->c_tbl, &ci2_item->i_data[0], &ci2_item->i_data[1]);
		strip_cautation_marks(ci2_item->c_tbl);
		ci2_item->iflag = 1;
	};
	return 1;
};

int write_chara_int2_ctl_item_c(FILE *fp, int level, int maxlen[2], 
                           const char *label, struct chara_int2_ctl_item *ci2_item){
    
	if(ci2_item->iflag == 0) return level;
	write_space_4_parse_c(fp, level);
	write_one_label_cont_c(fp, maxlen[0], label);
	write_one_label_cont_c(fp, maxlen[1], ci2_item->c_tbl);
	fprintf(fp, "%d %d\n", ci2_item->i_data[0], ci2_item->i_data[1]);
    return level;
};

void update_chara_int2_ctl_item_c(char *c_in, int i1_in, int i2_in,  
                              struct chara_int2_ctl_item *ci2_item){
	ci2_item->iflag = 1;
	sprintf(ci2_item->c_tbl,"%s", c_in);
	ci2_item->i_data[0] = i1_in;
	ci2_item->i_data[1] = i2_in;
    return;
};
void set_from_chara_int2_ctl_item_c( struct chara_int2_ctl_item *ci2_item,
                              char *c_out, int *i1_out, int *i2_out){
	if(ci2_item->iflag == 0) return;
	sprintf(c_out,"%s", ci2_item->c_tbl);
	*i1_out = ci2_item->i_data[0];
	*i2_out = ci2_item->i_data[1];
    return;
};


void init_chara_int2_ctl_list(struct chara_int2_ctl_list *head){
    int i;
    head->mlen2 = (struct maxlen_2 *) malloc(sizeof(struct maxlen_2));
    for(i=0; i<2; i++) {head->mlen2->mlen[i] = 0;};
	
    head->_prev = NULL;
    head->_next = NULL;
    return;
};

void clear_chara_int2_ctl_list(struct chara_int2_ctl_list *head){
	free(head->mlen2);
	
    head = head->_next;
    while (head != NULL) {
        dealloc_chara_int2_ctl_item_c(head->ci2_item);
        free(head);
        head = head->_next;
	}
	
    return;
};

struct chara_int2_ctl_list *add_chara_int2_ctl_list(struct chara_int2_ctl_list *current){
    struct chara_int2_ctl_list *added;
    struct chara_int2_ctl_list *old_next;
    
    if ((added = (struct chara_int2_ctl_list *) malloc(sizeof(struct chara_int2_ctl_list))) == NULL) {
        printf("malloc error\n");
        exit(0);
    }
    if ((added->ci2_item = (struct chara_int2_ctl_item *) malloc(sizeof(struct chara_int2_ctl_item))) == NULL) {
        printf("malloc error for ci2_item\n");
        exit(0);
    }
	alloc_chara_int2_ctl_item_c(added->ci2_item);
	added->mlen2 = current->mlen2;
    
    /* replace from  current -> p2　to current -> p1 -> p2 */
    old_next= current->_next;
    current->_next = added;
    added->_next = old_next;
    if (old_next != NULL) old_next->_prev = added;
    added->_prev = current;
    
    return added;
};

void delete_chara_int2_ctl_list(struct chara_int2_ctl_list *current){
    struct chara_int2_ctl_list *old_prev = current->_prev;
    struct chara_int2_ctl_list *old_next = current->_next;
    
    dealloc_chara_int2_ctl_item_c(current->ci2_item);
    free(current->ci2_item);
    free(current);
    
    old_prev->_next = old_next;
    if (old_next != NULL) old_next->_prev = old_prev;
    return;
};

int count_chara_int2_ctl_list(struct chara_int2_ctl_list *head){
    int num = 0;
    head = head->_next;
    while (head != NULL){
        if((int) strlen(head->ci2_item->c_tbl) > head->mlen2->mlen[1]){
            head->mlen2->mlen[1] = (int) strlen(head->ci2_item->c_tbl);
        };
        
        head = head->_next;
		num = num + 1;
    };
    return num;
};

struct chara_int2_ctl_list *find_ci2_ctl_list_item_by_index(int index, struct chara_int2_ctl_list *head){
    int i;
    if(index < 0 || index > count_chara_int2_ctl_list(head)) return NULL;
    for(i=0;i<index;i++){head = head->_next;};
    return head;
};
struct chara_int2_ctl_list *find_ci2_ctl_list_item_by_c_tbl(char *ref, struct chara_int2_ctl_list *head){
    head = head->_next;
    while (head != NULL){
		if(cmp_no_case_c(head->ci2_item->c_tbl, ref)) break;
        head = head->_next;
    };
    if(head == NULL) printf("array item %s does not exist.\n", ref);
    return head;
};

int read_chara_int2_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_int2_ctl_list *head){
    int iflag = 0;
    int icou = 0;
    int num_array = 0;
    
    iflag = find_control_array_flag_c(buf, label, &num_array);
    if(iflag == 0) return iflag;
    
    skip_comment_read_line(fp, buf);
    while(find_control_end_array_flag_c(buf, label, num_array, icou) == 0){
        head = add_chara_int2_ctl_list(head);
        iflag = read_chara_int2_ctl_item_c(buf, label, head->ci2_item);
        icou = icou + iflag;
        skip_comment_read_line(fp, buf);
    };
    
    if(num_array /= icou+1){
        printf("Number of %s does not match.: %d %d\n", label, num_array, icou);
    };
    return icou;
};

int write_chara_int2_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara_int2_ctl_list *head){
    
    int num = count_chara_int2_ctl_list(head);
    
    if(num == 0) return level;
    
    fprintf(fp, "!\n");
    head->mlen2->mlen[0] = (int) strlen(label);
    level = write_array_flag_for_ctl_c(fp, level, label, num);
    head = head->_next;
    
    while (head != NULL) {    /* Go through null pointer*/
        level = write_chara_int2_ctl_item_c(fp, level, head->mlen2->mlen,
                                     label, head->ci2_item);
        head = head->_next;
    }
    level = write_end_array_flag_for_ctl_c(fp, level, label);
    return level;
};


void append_chara_int2_ctl_list(char *c_in, int i1_in, int i2_in,
                      struct chara_int2_ctl_list *head){
	int num = count_chara_int2_ctl_list(head);
	head = find_ci2_ctl_list_item_by_index(num, head);
	head = add_chara_int2_ctl_list(head);
	update_chara_int2_ctl_item_c(c_in, i1_in, i2_in, head->ci2_item);
    return;
};

void del_chara_int2_ctl_list_by_index(int index, struct chara_int2_ctl_list *head){
	head = find_ci2_ctl_list_item_by_index(index, head);
	if(head != NULL) delete_chara_int2_ctl_list(head);
	return;
};

void update_chara_int2_ctl_list_by_index(int index, char *c_in, int i1_in, int i2_in,
			struct chara_int2_ctl_list *head){
	head = find_ci2_ctl_list_item_by_index(index, head);
	if(head != NULL) update_chara_int2_ctl_item_c(c_in, i1_in, i2_in, head->ci2_item);
	return;
};

void set_from_chara_int2_ctl_list_at_index(int index, struct chara_int2_ctl_list *head,
			char *c_out, int *i1_out, int *i2_out){
	head = find_ci2_ctl_list_item_by_index(index, head);
	if(head != NULL) set_from_chara_int2_ctl_item_c(head->ci2_item, c_out, i1_out, i2_out);
	return;
};


void del_chara_int2_ctl_list_by_c_tbl(char *ref, struct chara_int2_ctl_list *head){
	head = find_ci2_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) delete_chara_int2_ctl_list(head);
	return;
};

void update_chara_int2_ctl_list_by_c_tbl(char *ref, char *c_in, int i1_in, int i2_in,
			struct chara_int2_ctl_list *head){
	head = find_ci2_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) update_chara_int2_ctl_item_c(c_in, i1_in, i2_in, head->ci2_item);
	return;
};

void set_from_chara_int2_ctl_list_at_c_tbl(char *ref, struct chara_int2_ctl_list *head,
			char *c_out, int *i1_out, int *i2_out){
	head = find_ci2_ctl_list_item_by_c_tbl(ref, head);
	if(head != NULL) set_from_chara_int2_ctl_item_c(head->ci2_item, c_out, i1_out, i2_out);
	return;
};

