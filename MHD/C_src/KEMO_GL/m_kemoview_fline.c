/*
//  m_kemoview_fline.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include "m_kemoview_fline.h"

struct kemoview_fline * init_kemoview_fline(void){
	struct kemoview_fline *kemo_fline
			= (struct kemoview_fline *) malloc(sizeof(struct kemoview_fline));
	if(kemo_fline == NULL){
		printf("malloc error for kemoview_fline\n");
		exit(0);
	}
	
    kemo_fline->fline_d =   init_fline_data();
    kemo_fline->fline_dir = init_fline_directions();
    kemo_fline->fline_m =   init_psf_menu_val();
	return kemo_fline;
};

void dealloc_kemoview_fline(struct kemoview_fline *kemo_fline){
    free(kemo_fline->fline_d);
	free(kemo_fline->fline_m);
	free(kemo_fline);
	return;
};

void close_fieldline_view(struct kemoview_fline *kemo_fline){
	kemo_fline->fline_m->iflag_draw_viz = IZERO;
    dealloc_draw_psf_flags(kemo_fline->fline_d->nfield,
                           kemo_fline->fline_d->ncomptot,
                           kemo_fline->fline_m);

    dealloc_fline_direction_data(kemo_fline->fline_dir);
	deallc_all_fline_data(kemo_fline->fline_d);
    return;
}

void init_draw_fline(struct kemoview_fline *kemo_fline, struct psf_data *ucd_tmp,
			int iformat_ucd_file, int istep, const char *ucd_header){
    kemo_fline->fline_m->viz_prefix_c = alloc_kvstring();
    alloc_copy_string(ucd_header, kemo_fline->fline_m->viz_prefix_c);
	kemo_fline->fline_m->viz_step_c = istep;
	kemo_fline->fline_m->iformat_viz_file = iformat_ucd_file;
    
	if(kemo_fline->fline_m->iflag_draw_viz > 0) close_fieldline_view(kemo_fline);

	set_kemoview_fline_data(ucd_tmp,
                            kemo_fline->fline_d,
                            kemo_fline->fline_dir,
                            kemo_fline->fline_m);
    return;
};


int evolution_fline_viewer(struct kemoview_fline *kemo_fline,
			struct psf_data *psf_ucd_tmp, int istep_sync){
	int ierr = 0;
	if (kemo_fline->fline_m->iflag_draw_viz > 0) {
		kemo_fline->fline_m->viz_step_c = istep_sync;
		ierr = refresh_FLINE_data(psf_ucd_tmp,
                                  kemo_fline->fline_d,
                                  kemo_fline->fline_dir,
                                  kemo_fline->fline_m);
	}
	return ierr;
}


void set_fline_parameters(int selected, int input, struct kemoview_fline *kemo_fline){
	if(selected == DRAW_SWITCH){
        set_draw_psf_solid(input, kemo_fline->fline_m);
	};
	return;
}

int get_fline_parameters(struct kemoview_fline *kemo_fline, int selected){
	int output = 0;
	if(selected == DRAW_SWITCH){
		output =  send_draw_psf_solid(kemo_fline->fline_m);
	};
	return output;
}

void set_fline_field_param(int selected, int input, struct kemoview_fline *kemo_fline){
	if(selected == FIELD_SEL_FLAG){
        set_PSF_field(input,
                      kemo_fline->fline_d->data_name[selected],
                      kemo_fline->fline_d->istack_comp,
                      kemo_fline->fline_m);
	}else if(selected == COMPONENT_SEL_FLAG){
        int i_field = kemo_fline->fline_m->if_draw_viz;
        set_PSF_component(input,
                          kemo_fline->fline_d->data_name[i_field],
                          kemo_fline->fline_d->istack_comp,
                          kemo_fline->fline_m);
	}else if(selected == LINETYPE_FLAG){
		set_fline_type(kemo_fline->fline_m, (long) input);
	};
	return;
};

long get_fline_field_param(int selected, struct kemoview_fline *kemo_fline){
    long output = 0;
	
	if(selected == NUM_FIELD_FLAG){
		output = get_fline_color_num_field(kemo_fline->fline_d);
	}else if(selected == NTOT_COMPONENT_FLAG){
		output = get_fline_color_ncomptot(kemo_fline->fline_d);
	}else if(selected == FIELD_SEL_FLAG){
		output = send_field_draw_each_psf(kemo_fline->fline_m);
	}else if(selected == COMPONENT_SEL_FLAG){
		output = send_draw_comp_id_psf(kemo_fline->fline_m);
	}else if(selected == DRAW_ADDRESS_FLAG){
		output = send_draw_component_psf(kemo_fline->fline_m);
	}else if(selected == LINETYPE_FLAG){
		output = get_fline_type(kemo_fline->fline_m);
    }else if(selected == COORDINATE_FLAG){
        output = send_coordinate_id_fline(kemo_fline->fline_d,
                                          kemo_fline->fline_m);
	};
	return output;
};

void set_fline_color_param(int selected, int input, struct kemoview_fline *kemo_fline){
	if(selected == PSFSOLID_TOGGLE){
	}else if(selected == ISET_COLORMAP){
        set_psf_patch_color_mode(kemo_fline->fline_m, input);
	}else if(selected == ISET_VECTOR_COLOR){
	};
	return;
};

int get_fline_color_param(int selected, struct kemoview_fline *kemo_fline){
	int iflag = 0;
	if(selected == PSFSOLID_TOGGLE){
	}else if(selected == ISET_COLORMAP){
		iflag = send_each_psf_patch_color_mode(kemo_fline->fline_m);
	}else if(selected == ISET_NUM_COLOR){
		iflag = get_each_PSF_color_table_num(kemo_fline->fline_m);
	}else if(selected == ISET_NUM_OPACITY){
		iflag = get_each_PSF_opacity_table_num(kemo_fline->fline_m);
	}else if(selected == ISET_VECTOR_COLOR){
	};
	return iflag;
};

void set_fline_color_w_exp(int selected, double value, int i_digit, 
						   struct kemoview_fline *kemo_fline){
	double data = const_from_digit_order(value, i_digit);
	if(selected == ISET_WIDTH){
        set_each_isoline_width(data, kemo_fline->fline_m);
	};
	return;
};

void get_fline_color_w_exp(int selected, struct kemoview_fline *kemo_fline,
							   double *value, int *i_digit){
	double data = 0.0;
	if(selected == ISET_COLOR_MIN){
		data = get_each_PSF_color_table_min(kemo_fline->fline_m);
	}else if(selected == ISET_COLOR_MAX){
		data = get_each_PSF_color_table_max(kemo_fline->fline_m);
	}else if(selected == ISET_WIDTH){
		data = send_isoline_width(kemo_fline->fline_m);
	};
	find_order_digit(data, value, i_digit);
	return;
};

double get_fline_data_range(int selected, int icomp, struct kemoview_fline *kemo_fline){
	double value = 0.0;
	if(selected == ISET_COLOR_MIN){
		value = get_fline_data_min(kemo_fline->fline_d, icomp);
	}else if(selected == ISET_COLOR_MAX){
		value = get_fline_data_max(kemo_fline->fline_d, icomp);
	};
	return value;
}

double get_fline_colormap_range(int selected, struct kemoview_fline *kemo_fline){
	double value = 0.0;
	if(selected == ISET_OPACITY_MIN){
		value = get_each_PSF_minimum_opacity(kemo_fline->fline_m);
	}else if(selected == ISET_OPACITY_MAX){
		value = get_each_PSF_maximum_opacity(kemo_fline->fline_m);
	};
	return value;
};
