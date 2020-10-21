
/* m_kemoview_psf.c */

#include "m_kemoview_psf.h"

struct kemoview_psf * init_kemoview_psf(void){
	int i;
	struct kemoview_psf *kemo_psf
			= (struct kemoview_psf *) malloc(sizeof(struct kemoview_psf));
	if(kemo_psf == NULL){
		printf("malloc error for kemoview_psf\n");
		exit(0);
	}
	
	kemo_psf->psf_a =  (struct kemo_array_control *) malloc(sizeof(struct kemo_array_control));
	set_max_psf_loading(NMAX_PSF, kemo_psf->psf_a);
	
	kemo_psf->psf_d =  (struct psf_data **)     malloc(NMAX_PSF*sizeof(struct psf_data *));
	kemo_psf->psf_m =  (struct psf_menu_val **) malloc(NMAX_PSF*sizeof(struct psf_menu_val *));
	for(i=0;i<kemo_psf->psf_a->nlimit_loaded;i++){
		kemo_psf->psf_d[i] =   (struct psf_data *) malloc(sizeof(struct psf_data));
		kemo_psf->psf_m[i] =   (struct psf_menu_val *) malloc(sizeof(struct psf_menu_val));
		init_psf_parameters(kemo_psf->psf_m[i]);
	}
	return kemo_psf;
};

void dealloc_kemoview_psf(struct kemoview_psf *kemo_psf){
	int i;
	
	for(i=0;i<kemo_psf->psf_a->nlimit_loaded;i++){
		free(kemo_psf->psf_d[i]);
		free(kemo_psf->psf_m[i]);
	}
	free(kemo_psf->psf_a);
	free(kemo_psf->psf_d);
	free(kemo_psf->psf_m);
	free(kemo_psf);
	return;
};

static void set_avail_time_flag(struct kemoview_psf *kemo_psf){
	int id_load;
    kemo_psf->psf_a->iflag_avail_time = 0;
    kemo_psf->psf_a->iflag_avail_file_step = 0;
    for(id_load=0; id_load<kemo_psf->psf_a->num_loaded; id_load++){
        if(kemo_psf->psf_a->iflag_loaded[id_load] > 0){
            if(kemo_psf->psf_m[id_load]->iflag_draw_time > 0){
                kemo_psf->psf_a->iflag_avail_time = 1;
                kemo_psf->psf_a->iflag_avail_file_step = 1;
                kemo_psf->psf_a->time_disp = kemo_psf->psf_m[id_load]->time;
                kemo_psf->psf_a->file_step_disp = kemo_psf->psf_m[id_load]->psf_step;
                break;
            };
        };
	};
	return;
}

static void set_avail_file_step_flag(struct kemoview_psf *kemo_psf){
	int id_load;
    if(kemo_psf->psf_a->iflag_avail_time != 0) return;
	
	for(id_load=0; id_load<kemo_psf->psf_a->num_loaded; id_load++){
        if(kemo_psf->psf_a->iflag_loaded[id_load] > 0){
			kemo_psf->psf_a->iflag_avail_file_step = 1;
			kemo_psf->psf_a->file_step_disp = kemo_psf->psf_m[id_load]->psf_step;
			break;
        };
	};
	return;
}

void init_draw_psf(struct kemoview_psf *kemo_psf, struct psf_data *ucd_tmp,
			int iflag_fileformat, int istep, double time, const char *ucd_header){
    int id_load;
    id_load = add_new_kemoview_array(kemo_psf->psf_a);

    kemo_psf->psf_m[id_load]->psf_header = alloc_kvstring();
    alloc_copy_string(ucd_header, kemo_psf->psf_m[id_load]->psf_header);
	kemo_psf->psf_m[id_load]->psf_step = istep;
	kemo_psf->psf_m[id_load]->iflag_psf_file = iflag_fileformat;
	
    set_iflag_draw_time(time, kemo_psf->psf_m[id_load]);
    
	if(kemo_psf->psf_a->num_loaded == kemo_psf->psf_a->nlimit_loaded){
		dealloc_draw_psf_flags(kemo_psf->psf_d[id_load], kemo_psf->psf_m[id_load]);
		deallc_all_psf_data(kemo_psf->psf_d[id_load]);
	};
	
	set_kemoview_psf_data(kemo_psf->psf_d[id_load], ucd_tmp, kemo_psf->psf_m[id_load]);
    set_avail_time_flag(kemo_psf);
    set_avail_file_step_flag(kemo_psf);
    return;
};

void close_PSF_view(struct kemoview_psf *kemo_psf){
	dealloc_draw_psf_flags(kemo_psf->psf_d[kemo_psf->psf_a->id_current],
                           kemo_psf->psf_m[kemo_psf->psf_a->id_current]);
	deallc_all_psf_data(kemo_psf->psf_d[kemo_psf->psf_a->id_current]);
	
	set_close_current_kemoview_array(kemo_psf->psf_a);
    set_avail_time_flag(kemo_psf);
    set_avail_file_step_flag(kemo_psf);
	return;
}

void evolution_psf_viewer(struct psf_data *psf_ucd_tmp, struct kemoview_psf *kemo_psf){
	int id_load;
	printf("Loading PSF %d \n",kemo_psf->psf_a->nmax_loaded);
	for(id_load=0; id_load<kemo_psf->psf_a->nmax_loaded; id_load++){
		if(kemo_psf->psf_a->iflag_loaded[id_load] > 0){
			printf("Loaded PSF file %d %d %s\n", id_load, 
						kemo_psf->psf_m[id_load]->iflag_psf_file,
						kemo_psf->psf_m[id_load]->psf_header->string);
			kemo_psf->psf_m[id_load]->psf_step = kemo_psf->psf_a->istep_sync;
			evolution_PSF_data(kemo_psf->psf_d[id_load], psf_ucd_tmp, 
							   kemo_psf->psf_m[id_load]);
		};
    };
	set_avail_time_flag(kemo_psf);
	set_avail_file_step_flag(kemo_psf);
	return;
}

void set_PSF_loaded_params(int selected, int input, struct kemoview_psf *kemo_psf){
	if(selected == NUM_LOADED){
		set_PSF_num_loaded(input, kemo_psf->psf_a);
	}else if(selected == MAX_LOADED){
		set_PSF_max_loaded(input, kemo_psf->psf_a);
	}else if(selected == SET_CURRENT){
		set_current_PSF_to_menu(input, kemo_psf->psf_a);
	};
	return;
}

int get_PSF_loaded_params(struct kemoview_psf *kemo_psf, int selected){
	int output = 0;
	if(selected == NUM_LOADED){
		output= get_PSF_num_loaded(kemo_psf->psf_a);
	}else if(selected == MAX_LOADED){
		output = get_PSF_max_loaded(kemo_psf->psf_a);
	}else if(selected == SET_CURRENT){
		output = get_curent_PSF_ID(kemo_psf->psf_a);
	}else if(selected == DRAW_SWITCH){
		output = get_PSF_draw_switch(kemo_psf->psf_a);
	};
	return output;
}

void set_each_PSF_field_param(int selected, int input, struct kemoview_psf *kemo_psf){
	int i_current = kemo_psf->psf_a->id_current;
	if(selected == FIELD_SEL_FLAG){
		set_PSF_field(input, kemo_psf->psf_d[i_current], kemo_psf->psf_m[i_current]);
	}else if(selected == COMPONENT_SEL_FLAG){
		set_PSF_component(input, kemo_psf->psf_d[i_current], kemo_psf->psf_m[i_current]);
	};
	return;
};

int get_each_PSF_field_param(int selected, struct kemoview_psf *kemo_psf){
	int output = 0;
	int i_current = kemo_psf->psf_a->id_current;
	
	if(selected == NUM_FIELD_FLAG){
		output = send_nfield_each_psf(kemo_psf->psf_d[i_current]);
	} else if(selected == NTOT_COMPONENT_FLAG){
		output = send_ncomptot_each_psf(kemo_psf->psf_d[i_current]);
	} else if(selected == FIELD_SEL_FLAG){
		output =  send_field_draw_each_psf(kemo_psf->psf_m[i_current]);
	} else if(selected == COMPONENT_SEL_FLAG){
		output =  send_draw_comp_id_psf(kemo_psf->psf_m[i_current]);
	} else if(selected == DRAW_ADDRESS_FLAG){
		output =  send_draw_component_psf(kemo_psf->psf_m[i_current]);
	} else if(selected == COORDINATE_FLAG){
		output =  send_coordinate_id_psf(kemo_psf->psf_d[i_current], kemo_psf->psf_m[i_current]);
	};
	return output;
};

int toggle_each_PSF_draw_switch(int selected, struct kemoview_psf *kemo_psf){
	int i_current = kemo_psf->psf_a->id_current;
	int toggle = 0;
	
	if      (selected == PSFSOLID_TOGGLE){
		return toggle_draw_psf_solid(kemo_psf->psf_m[i_current]);
	} else if (selected == PSFGRID_TOGGLE){
		return toggle_draw_psf_grid(kemo_psf->psf_m[i_current]);
	} else if (selected == ZEROGRID_TOGGLE){
		return toggle_draw_psf_zero(kemo_psf->psf_m[i_current]);
	} else if (selected == COLORBAR_TOGGLE){
		return toggle_draw_psf_cbar(kemo_psf->psf_m[i_current]);
	} else if (selected == PSF_POLYGON_SWITCH){
		return toggle_each_psf_polygon_mode(kemo_psf->psf_m[i_current]);
	} else if (selected == PSFVECT_TOGGLE){
		return toggle_draw_psf_vect(kemo_psf->psf_m[i_current]);
	} else if (selected == PSFREFV_TOGGLE){
		return toggle_draw_psf_refv(kemo_psf->psf_m[i_current]);
	} else if (selected == PSFTANVEC_TOGGLE){
		return toggle_each_psf_vector_mode(kemo_psf->psf_m[i_current]);
	};
	return toggle;
}

int get_each_PSF_draw_switch(int selected, struct kemoview_psf *kemo_psf){
	int i_current = kemo_psf->psf_a->id_current;
	int iflag = 0;
	if      (selected == PSFSOLID_TOGGLE){
		iflag = send_draw_psf_solid(kemo_psf->psf_m[i_current]);
	} else if (selected == PSFGRID_TOGGLE){
		iflag = send_draw_psf_grid(kemo_psf->psf_m[i_current]);
	} else if (selected == ZEROGRID_TOGGLE){
		iflag = send_draw_psf_zero(kemo_psf->psf_m[i_current]);
	} else if (selected == COLORBAR_TOGGLE){
		iflag = send_draw_psf_cbar(kemo_psf->psf_m[i_current]);
	} else if (selected == PSF_POLYGON_SWITCH){
		iflag = send_each_psf_polygon_mode(kemo_psf->psf_m[i_current]);
	} else if (selected == PSFVECT_TOGGLE){
		iflag = send_draw_psf_vect(kemo_psf->psf_m[i_current]);
	} else if (selected == PSFTANVEC_TOGGLE){
		iflag = send_each_psf_vector_mode(kemo_psf->psf_m[i_current]);
	};
	return iflag;
}

void set_each_PSF_color_param(int selected, int input, struct kemoview_psf *kemo_psf){
	int i_current = kemo_psf->psf_a->id_current;
	if(selected == PSFSOLID_TOGGLE){
		set_psf_patch_color_mode(kemo_psf->psf_m[i_current], input);
	}else if(selected == PSFGRID_TOGGLE){
		set_each_isoline_color(kemo_psf->psf_m[i_current], input);
	}else if(selected == ISET_NLINE){
		set_each_n_isoline(kemo_psf->psf_m[i_current], input);
	}else if(selected == ISET_COLORMAP){
		set_PSF_colormap_id(kemo_psf->psf_m[i_current], input);
	}else if(selected == ISET_VECTOR_COLOR){
		set_each_vector_patch_color(kemo_psf->psf_m[i_current], input);
	};
	return;
};

int get_each_PSF_color_param(int selected, struct kemoview_psf *kemo_psf){
	int iflag = 0;
	int i_current = kemo_psf->psf_a->id_current;
	if(selected == PSFSOLID_TOGGLE){
		iflag = send_each_psf_patch_color(kemo_psf->psf_m[i_current]);
	}else if(selected == PSFGRID_TOGGLE){
		iflag = send_each_isoline_color(kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_NLINE){
		iflag = send_num_isoline(kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_COLORMAP){
		iflag = send_PSF_colormap_id(kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_NUM_COLOR){
		iflag = send_each_PSF_color_table_num(kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_NUM_OPACITY){
		iflag = send_each_PSF_opacity_table_num(kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_VECTOR_COLOR){
		iflag = send_each_vector_patch_color(kemo_psf->psf_m[i_current]);
	};
	return iflag;
};

void set_each_PSF_color_w_exp(int selected, double value, int i_digit, 
							  struct kemoview_psf *kemo_psf){
	double data = const_from_digit_order(value, i_digit);
	int i_current = kemo_psf->psf_a->id_current;
	if(selected == ISET_WIDTH){
		set_each_isoline_width(data, kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_PSF_REFVECT){
		set_each_scale_vect(data, kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_PSF_V_THICK){
		set_each_vector_thick(data, kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_VECTOR_INC){
		set_each_increment_vect((int) data, kemo_psf->psf_m[i_current]);
	};
	return;
};
void get_each_PSF_color_w_exp(int selected, struct kemoview_psf *kemo_psf,
							   double *value, int *i_digit){
	double data = 0.0;
	int i_current = kemo_psf->psf_a->id_current;
	if(selected == ISET_COLOR_MIN){
		data = send_each_PSF_color_table_min(kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_COLOR_MAX){
		data = send_each_PSF_color_table_max(kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_WIDTH){
		data = send_isoline_width(kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_PSF_REFVECT){
		data = send_scale_vector(kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_PSF_V_THICK){
		data = send_vector_thick(kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_VECTOR_INC){
		data = (double) send_each_increment_vect(kemo_psf->psf_m[i_current]);
	};
	find_order_digit(data, value, i_digit);
	return;
};

double get_each_PSF_data_range(int selected, int icomp, struct kemoview_psf *kemo_psf){
	double value = 0.0;
	int i_current = kemo_psf->psf_a->id_current;
	if(selected == ISET_COLOR_MIN){
		value = send_psf_data_min(kemo_psf->psf_d[i_current], icomp);
	}else if(selected == ISET_COLOR_MAX){
		value = send_psf_data_max(kemo_psf->psf_d[i_current], icomp);
	};
	return value;
}

double get_each_PSF_colormap_range(int selected, struct kemoview_psf *kemo_psf){
	double value = 0.0;
	int i_current = kemo_psf->psf_a->id_current;
	if(selected == ISET_OPACITY_MIN){
		value = send_each_PSF_minimum_opacity(kemo_psf->psf_m[i_current]);
	}else if(selected == ISET_OPACITY_MAX){
		value = send_each_PSF_maximum_opacity(kemo_psf->psf_m[i_current]);
	};
	return value;
};


void set_draw_time_flag(int iflag, struct kemoview_psf *kemo_psf){
    kemo_psf->psf_a->iflag_draw_time = iflag;
};
int toggle_draw_time_flag(struct kemoview_psf *kemo_psf){
    return kemo_psf->psf_a->iflag_draw_time = toggle_value_c(kemo_psf->psf_a->iflag_draw_time);
};
int get_draw_time_flag(struct kemoview_psf *kemo_psf){
    return kemo_psf->psf_a->iflag_draw_time;
};
int get_avail_time_flag(struct kemoview_psf *kemo_psf){
    return kemo_psf->psf_a->iflag_avail_time;
};

void set_draw_file_step_flag(int iflag, struct kemoview_psf *kemo_psf){
    kemo_psf->psf_a->iflag_draw_file_step = iflag;
};
int toggle_draw_file_step_flag(struct kemoview_psf *kemo_psf){
    return kemo_psf->psf_a->iflag_draw_file_step = toggle_value_c(kemo_psf->psf_a->iflag_draw_file_step);
};
int get_draw_file_step_flag(struct kemoview_psf *kemo_psf){
    return kemo_psf->psf_a->iflag_draw_file_step;
};
int get_avail_file_step_flag(struct kemoview_psf *kemo_psf){
    return kemo_psf->psf_a->iflag_avail_file_step;
};
