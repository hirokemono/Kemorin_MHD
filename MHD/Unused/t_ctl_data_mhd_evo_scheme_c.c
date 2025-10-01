/*
//  t_ctl_data_mhd_evo_scheme_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/05/28.
*/

#include "t_ctl_data_mhd_evo_scheme_c.h"

#define NLBL_MHD_REATART_CTL      1
#define NLBL_MHD_EVO_SCHEME_CTL  24

const char label_mhd_restart_control[NLBL_MHD_REATART_CTL][KCHARA_C] = {
    /*[ 0]*/    {"rst_ctl"},
};

const char label_mhd_evo_scheme_control[NLBL_MHD_EVO_SCHEME_CTL][KCHARA_C] = {
    /*[ 0]*/    {"coef_imp_v_ctl"},
    /*[ 1]*/    {"coef_imp_t_ctl"},
    /*[ 2]*/    {"coef_imp_b_ctl"},
    /*[ 3]*/    {"coef_imp_c_ctl"},
    
    /*[ 4]*/    {"iflag_supg_ctl"},
    /*[ 5]*/    {"iflag_supg_v_ctl"},
    /*[ 6]*/    {"iflag_supg_t_ctl"},
    /*[ 7]*/    {"iflag_supg_b_ctl"},
    /*[ 8]*/    {"iflag_supg_c_ctl"},
    
    /*[ 9]*/    {"num_multi_pass_ctl"},
    /*[10]*/    {"maxiter_ctl"},
    
    /*[11]*/    {"eps_4_velo_ctl"},
    /*[12]*/    {"eps_4_magne_ctl"},
    
    /*[13]*/    {"eps_crank_ctl"},
    /*[14]*/    {"eps_B_solver_ctl"},
    
    /*[15]*/    {"scheme_ctl"},
    /*[16]*/    {"diffuse_correct_ctl"},
    
    /*[17]*/    {"method_4_velo_ctl"},
    /*[18]*/    {"precond_4_crank_ctl"},
    
    /*[19]*/    {"Legendre_trans_loop_ctl"},
    /*[20]*/    {"FFT_library_ctl"},
    /*[21]*/    {"import_table_mode_ctl"},
    /*[22]*/    {"send_recv_routine_ctl"},
    
    /*[23]*/    {"Legendre_vector_length_ctl"},
};


void get_label_mhd_restart_control(int index, char *label){
    if(index < NLBL_MHD_REATART_CTL) strngcopy(label, label_mhd_restart_control[index]);
    return;
};
void get_label_mhd_evo_scheme_control(int index, char *label){
    if(index < NLBL_MHD_EVO_SCHEME_CTL) strngcopy(label, label_mhd_evo_scheme_control[index]);
    return;
};


struct mhd_restart_control_c * init_mhd_restart_control_c(){
    int i;
    struct mhd_restart_control_c *mrst_ctl;
    if((mrst_ctl = (struct mhd_restart_control_c *) malloc(sizeof(struct mhd_restart_control_c))) == NULL) {
        printf("malloc error for mhd_restart_control_c \n");
        exit(0);
    }
    
    mrst_ctl->iflag_use = 0;
    mrst_ctl->maxlen = 0;
    for (i=0;i<NLBL_MHD_REATART_CTL;i++){
        if(strlen(label_mhd_restart_control[i]) > mrst_ctl->maxlen){
            mrst_ctl->maxlen = (int) strlen(label_mhd_restart_control[i]);
        };
	};
	mrst_ctl->restart_flag_c = init_chara_ctl_item_c();
	
	return mrst_ctl;
}
void dealloc_mhd_restart_control_c(struct mhd_restart_control_c *mrst_ctl){
	
    dealloc_chara_ctl_item_c(mrst_ctl->restart_flag_c);
    free(mrst_ctl);
	
	return;
}
void read_mhd_restart_control_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct mhd_restart_control_c *mrst_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_mhd_restart_control[0], mrst_ctl->restart_flag_c);
	};
    mrst_ctl->iflag_use = 1;
    return;
}

int write_mhd_restart_control_c(FILE *fp, int level, const char *label, 
                                struct mhd_restart_control_c *mrst_ctl){
    if(mrst_ctl->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	write_chara_ctl_item_c(fp, level, mrst_ctl->maxlen, 
				label_mhd_restart_control[0], mrst_ctl->restart_flag_c);
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

struct mhd_evo_scheme_control_c * init_mhd_evo_scheme_control_c(){
    int i;
    struct mhd_evo_scheme_control_c *mevo_ctl;
    if((mevo_ctl = (struct mhd_evo_scheme_control_c *) malloc(sizeof(struct mhd_evo_scheme_control_c))) == NULL) {
        printf("malloc error for mhd_evo_scheme_control_c \n");
        exit(0);
    }
    
    mevo_ctl->iflag_use = 0;
    mevo_ctl->maxlen = 0;
    for (i=0;i<NLBL_MHD_EVO_SCHEME_CTL;i++){
        if(strlen(label_mhd_evo_scheme_control[i]) > mevo_ctl->maxlen){
            mevo_ctl->maxlen = (int) strlen(label_mhd_evo_scheme_control[i]);
        };
	};
	
	mevo_ctl->coef_imp_v_c = init_chara_ctl_item_c();
	mevo_ctl->coef_imp_t_c = init_chara_ctl_item_c();
	mevo_ctl->coef_imp_b_c = init_chara_ctl_item_c();
	mevo_ctl->coef_imp_c_c = init_chara_ctl_item_c();
	
	mevo_ctl->iflag_supg_c =   init_chara_ctl_item_c();
	mevo_ctl->iflag_supg_v_c = init_chara_ctl_item_c();
	mevo_ctl->iflag_supg_t_c = init_chara_ctl_item_c();
	mevo_ctl->iflag_supg_b_c = init_chara_ctl_item_c();
	mevo_ctl->iflag_supg_c_c = init_chara_ctl_item_c();
	
	mevo_ctl->num_multi_pass_c = init_int_ctl_item_c();
	mevo_ctl->maxiter_c = init_int_ctl_item_c();
	
    mevo_ctl->eps_4_velo_c =  init_real_ctl_item_c();
    mevo_ctl->eps_4_magne_c = init_real_ctl_item_c();
	
    mevo_ctl->eps_crank_c =   init_real_ctl_item_c();
    mevo_ctl->eps_B_crank_c = init_real_ctl_item_c();
	
	mevo_ctl->scheme_c =          init_chara_ctl_item_c();
	mevo_ctl->diffuse_correct_c = init_chara_ctl_item_c();
	
	mevo_ctl->method_4_CN_c =  init_chara_ctl_item_c();
	mevo_ctl->precond_4_CN_c = init_chara_ctl_item_c();
	
	mevo_ctl->Legendre_trans_type_c = init_chara_ctl_item_c();
	mevo_ctl->FFT_library_c = init_chara_ctl_item_c();
	mevo_ctl->import_mode_c = init_chara_ctl_item_c();
	mevo_ctl->SR_routine_c =  init_chara_ctl_item_c();
	
	mevo_ctl->leg_vector_len_c = init_int_ctl_item_c();
	return mevo_ctl;
}

void dealloc_mhd_evo_scheme_control_c(struct mhd_evo_scheme_control_c *mevo_ctl){
	
    dealloc_chara_ctl_item_c(mevo_ctl->coef_imp_v_c);
    dealloc_chara_ctl_item_c(mevo_ctl->coef_imp_t_c);
    dealloc_chara_ctl_item_c(mevo_ctl->coef_imp_b_c);
    dealloc_chara_ctl_item_c(mevo_ctl->coef_imp_c_c);
	
    dealloc_chara_ctl_item_c(mevo_ctl->iflag_supg_c);
    dealloc_chara_ctl_item_c(mevo_ctl->iflag_supg_v_c);
    dealloc_chara_ctl_item_c(mevo_ctl->iflag_supg_t_c);
    dealloc_chara_ctl_item_c(mevo_ctl->iflag_supg_b_c);
    dealloc_chara_ctl_item_c(mevo_ctl->iflag_supg_c_c);
	
	free(mevo_ctl->num_multi_pass_c);
	free(mevo_ctl->maxiter_c);
	
	free(mevo_ctl->eps_4_velo_c);
	free(mevo_ctl->eps_4_magne_c);
	
	free(mevo_ctl->eps_crank_c);
	free(mevo_ctl->eps_B_crank_c);
	
    dealloc_chara_ctl_item_c(mevo_ctl->scheme_c);
	free(mevo_ctl->scheme_c);
    dealloc_chara_ctl_item_c(mevo_ctl->diffuse_correct_c);
	free(mevo_ctl->diffuse_correct_c);
	
    dealloc_chara_ctl_item_c(mevo_ctl->method_4_CN_c);
	free(mevo_ctl->method_4_CN_c);
    dealloc_chara_ctl_item_c(mevo_ctl->precond_4_CN_c);
	free(mevo_ctl->precond_4_CN_c);
	
    dealloc_chara_ctl_item_c(mevo_ctl->Legendre_trans_type_c);
	free(mevo_ctl->Legendre_trans_type_c);
    dealloc_chara_ctl_item_c(mevo_ctl->FFT_library_c);
	free(mevo_ctl->FFT_library_c);
    dealloc_chara_ctl_item_c(mevo_ctl->import_mode_c);
	free(mevo_ctl->import_mode_c);
    dealloc_chara_ctl_item_c(mevo_ctl->SR_routine_c);
	free(mevo_ctl->SR_routine_c);
	
	free(mevo_ctl->leg_vector_len_c);

    free(mevo_ctl);
    return;
}
void read_mhd_evo_scheme_control_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct mhd_evo_scheme_control_c *mevo_ctl){
	while(find_control_end_flag_c(buf, label) == 0){
		skip_comment_read_line(fp, buf);
		
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[15], mevo_ctl->scheme_c);
		
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[0], mevo_ctl->coef_imp_v_c);
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[1], mevo_ctl->coef_imp_t_c);
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[2], mevo_ctl->coef_imp_b_c);
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[3], mevo_ctl->coef_imp_c_c);
		
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[4], mevo_ctl->iflag_supg_c);
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[5], mevo_ctl->iflag_supg_v_c);
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[6], mevo_ctl->iflag_supg_t_c);
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[7], mevo_ctl->iflag_supg_b_c);
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[8], mevo_ctl->iflag_supg_c_c);
		
		read_integer_ctl_item_c(buf, label_mhd_evo_scheme_control[9], mevo_ctl->num_multi_pass_c);
		read_integer_ctl_item_c(buf, label_mhd_evo_scheme_control[10], mevo_ctl->maxiter_c);
		
		read_real_ctl_item_c(buf, label_mhd_evo_scheme_control[11], mevo_ctl->eps_4_velo_c);
		read_real_ctl_item_c(buf, label_mhd_evo_scheme_control[12], mevo_ctl->eps_4_magne_c);
		
		read_real_ctl_item_c(buf, label_mhd_evo_scheme_control[13], mevo_ctl->eps_crank_c);
		read_real_ctl_item_c(buf, label_mhd_evo_scheme_control[14], mevo_ctl->eps_B_crank_c);
		
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[16], mevo_ctl->diffuse_correct_c);
		
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[17], mevo_ctl->method_4_CN_c);
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[18], mevo_ctl->precond_4_CN_c);
		
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[19], mevo_ctl->Legendre_trans_type_c);
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[20], mevo_ctl->FFT_library_c);
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[21], mevo_ctl->import_mode_c);
		read_chara_ctl_item_c(buf, label_mhd_evo_scheme_control[22], mevo_ctl->SR_routine_c);
		
		read_integer_ctl_item_c(buf, label_mhd_evo_scheme_control[23], mevo_ctl->leg_vector_len_c);
	};
    mevo_ctl->iflag_use = 1;
	return;
}
int write_mhd_evo_scheme_control_c(FILE *fp, int level, const char *label, 
                                   struct mhd_evo_scheme_control_c *mevo_ctl){
    if(mevo_ctl->iflag_use == 0) return level;
    
    fprintf(fp, "!\n");
	level = write_begin_flag_for_ctl_c(fp, level, label);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[15], mevo_ctl->scheme_c);
	
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[0], mevo_ctl->coef_imp_v_c);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[1], mevo_ctl->coef_imp_t_c);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[2], mevo_ctl->coef_imp_b_c);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[3], mevo_ctl->coef_imp_c_c);
	
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[4], mevo_ctl->iflag_supg_c);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[5], mevo_ctl->iflag_supg_v_c);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[6], mevo_ctl->iflag_supg_t_c);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[7], mevo_ctl->iflag_supg_b_c);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[8], mevo_ctl->iflag_supg_c_c);
	
	write_integer_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[9], mevo_ctl->num_multi_pass_c);
	write_integer_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[10], mevo_ctl->maxiter_c);
	
	write_real_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[11], mevo_ctl->eps_4_velo_c);
	write_real_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[12], mevo_ctl->eps_4_magne_c);
	
	write_real_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[13], mevo_ctl->eps_crank_c);
	write_real_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[14], mevo_ctl->eps_B_crank_c);
	
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[16], mevo_ctl->diffuse_correct_c);
	
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[17], mevo_ctl->method_4_CN_c);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[18], mevo_ctl->precond_4_CN_c);
	
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[19], mevo_ctl->Legendre_trans_type_c);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[20], mevo_ctl->FFT_library_c);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[21], mevo_ctl->import_mode_c);
	write_chara_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[22], mevo_ctl->SR_routine_c);
	
	write_integer_ctl_item_c(fp, level, mevo_ctl->maxlen, 
				label_mhd_evo_scheme_control[23], mevo_ctl->leg_vector_len_c);
	level = write_end_flag_for_ctl_c(fp, level, label);
	return level;
};

