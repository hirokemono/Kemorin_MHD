
/* m_kemoview_psf.c */

#include "m_kemoview_psf.h"

struct kemoview_mul_psf * init_kemoview_mul_psf(void){
	int i;
	struct kemoview_mul_psf *kemo_mul_psf
			= (struct kemoview_mul_psf *) malloc(sizeof(struct kemoview_mul_psf));
	if(kemo_mul_psf == NULL){
		printf("malloc error for kemoview_mul_psf\n");
		exit(0);
	}
	
    kemo_mul_psf->psf_a =  (struct kemo_array_control *) malloc(sizeof(struct kemo_array_control));
	set_max_psf_loading(NMAX_PSF, kemo_mul_psf->psf_a);
	
    kemo_mul_psf->psf_d =  (struct psf_data **)     malloc(NMAX_PSF*sizeof(struct psf_data *));
    if(kemo_mul_psf->psf_d == NULL){
        printf("malloc error for kemo_mul_psf->psf_d\n");
        exit(0);
    }
    kemo_mul_psf->psf_n =  (struct psf_normals **)     malloc(NMAX_PSF*sizeof(struct psf_normals *));
    if(kemo_mul_psf->psf_n == NULL){
        printf("malloc error for kemo_mul_psf->psf_n\n");
        exit(0);
    }
    kemo_mul_psf->psf_m =  (struct psf_menu_val **) malloc(NMAX_PSF*sizeof(struct psf_menu_val *));
    if(kemo_mul_psf->psf_m == NULL){
        printf("malloc error for kemo_mul_psf->psf_m\n");
        exit(0);
    }
	for(i=0;i<kemo_mul_psf->psf_a->nlimit_loaded;i++){
        kemo_mul_psf->psf_d[i] = init_psf_data();
        kemo_mul_psf->psf_n[i] = init_psf_normals();
        kemo_mul_psf->psf_m[i] = init_psf_menu_val();

		init_psf_parameters(kemo_mul_psf->psf_m[i]);
	}
	return kemo_mul_psf;
};

void dealloc_kemoview_mul_psf(struct kemoview_mul_psf *kemo_mul_psf){
	int i;
	
	for(i=0;i<kemo_mul_psf->psf_a->nlimit_loaded;i++){
		free(kemo_mul_psf->psf_d[i]);
		free(kemo_mul_psf->psf_m[i]);
	}
	free(kemo_mul_psf->psf_a);
	free(kemo_mul_psf->psf_d);
	free(kemo_mul_psf->psf_m);
	free(kemo_mul_psf);
	return;
};

static void set_avail_time_flag(struct kemoview_mul_psf *kemo_mul_psf){
	int id_load;
    kemo_mul_psf->psf_a->iflag_avail_time = 0;
    kemo_mul_psf->psf_a->iflag_avail_file_step = 0;
    for(id_load=0; id_load<kemo_mul_psf->psf_a->num_loaded; id_load++){
        if(kemo_mul_psf->psf_a->iflag_loaded[id_load] > 0){
            if(kemo_mul_psf->psf_m[id_load]->iflag_draw_time > 0){
                kemo_mul_psf->psf_a->iflag_avail_time = 1;
                kemo_mul_psf->psf_a->iflag_avail_file_step = 1;
                kemo_mul_psf->psf_a->time_disp = kemo_mul_psf->psf_m[id_load]->time;
                kemo_mul_psf->psf_a->file_step_disp = kemo_mul_psf->psf_m[id_load]->viz_step_c;
                break;
            };
        };
	};
	return;
}

static void set_avail_file_step_flag(struct kemoview_mul_psf *kemo_mul_psf){
	int id_load;
    if(kemo_mul_psf->psf_a->iflag_avail_time != 0) return;
	
	for(id_load=0; id_load<kemo_mul_psf->psf_a->num_loaded; id_load++){
        if(kemo_mul_psf->psf_a->iflag_loaded[id_load] > 0){
            kemo_mul_psf->psf_a->iflag_avail_file_step = 1;
            kemo_mul_psf->psf_a->file_step_disp = kemo_mul_psf->psf_m[id_load]->viz_step_c;
			break;
        };
	};
	return;
}

void init_draw_mul_psf(struct kemoview_mul_psf *kemo_mul_psf, struct psf_data *ucd_tmp,
                       int iflag_fileformat, int istep, double time, const char *ucd_header){
    int id_load;
    id_load = add_new_kemoview_array(kemo_mul_psf->psf_a);

    kemo_mul_psf->psf_m[id_load]->viz_prefix_c = alloc_kvstring();
    alloc_copy_string(ucd_header, kemo_mul_psf->psf_m[id_load]->viz_prefix_c);
    kemo_mul_psf->psf_m[id_load]->viz_step_c = istep;
    kemo_mul_psf->psf_m[id_load]->iformat_viz_file = iflag_fileformat;
	
    set_iflag_draw_time(time, kemo_mul_psf->psf_m[id_load]);
    
	if(kemo_mul_psf->psf_a->num_loaded == kemo_mul_psf->psf_a->nlimit_loaded){
        dealloc_psf_cutting_4_map(kemo_mul_psf->psf_m[id_load]->map_itp);
		dealloc_draw_psf_flags(kemo_mul_psf->psf_d[id_load]->nfield,
                               kemo_mul_psf->psf_d[id_load]->ncomptot,
                               kemo_mul_psf->psf_m[id_load]);
        dealloc_edge_data_4_psf(kemo_mul_psf->psf_d[id_load]->nele_viz,
                                kemo_mul_psf->psf_n[id_load]->psf_edge);
        dealloc_psf_norm_s(kemo_mul_psf->psf_n[id_load]);
		deallc_all_psf_data(kemo_mul_psf->psf_d[id_load]);
	};
	
    kemo_mul_psf->psf_m[id_load]->map_itp = alloc_psf_cutting_4_map();
    kemo_mul_psf->psf_m[id_load]->nadded_for_phi0 
        = set_psf_data_by_UCD(kemo_mul_psf->psf_m[id_load]->map_itp,
                              kemo_mul_psf->psf_d[id_load],
                              kemo_mul_psf->psf_n[id_load], ucd_tmp);

	set_kemoview_psf_data(kemo_mul_psf->psf_d[id_load],
                          kemo_mul_psf->psf_m[id_load]);
    set_avail_time_flag(kemo_mul_psf);
    set_avail_file_step_flag(kemo_mul_psf);
    return;
};

void close_PSF_view(struct kemoview_mul_psf *kemo_mul_psf){
    int i_current = kemo_mul_psf->psf_a->id_current;
    dealloc_psf_cutting_4_map(kemo_mul_psf->psf_m[i_current]->map_itp);
	dealloc_draw_psf_flags(kemo_mul_psf->psf_d[i_current]->nfield,
                           kemo_mul_psf->psf_d[i_current]->ncomptot,
                           kemo_mul_psf->psf_m[i_current]);
    dealloc_edge_data_4_psf(kemo_mul_psf->psf_d[i_current]->nele_viz,
                            kemo_mul_psf->psf_n[i_current]->psf_edge);
    dealloc_psf_norm_s(kemo_mul_psf->psf_n[i_current]);
	deallc_all_psf_data(kemo_mul_psf->psf_d[i_current]);
	
	set_close_current_kemoview_array(kemo_mul_psf->psf_a);
    set_avail_time_flag(kemo_mul_psf);
    set_avail_file_step_flag(kemo_mul_psf);
	return;
}

void evolution_psf_viewer(struct psf_data *psf_ucd_tmp,
                          struct kemoview_mul_psf *kemo_mul_psf){
	int id_load;
	printf("Loading PSF %d \n",kemo_mul_psf->psf_a->nmax_loaded);
	for(id_load=0; id_load<kemo_mul_psf->psf_a->nmax_loaded; id_load++){
		if(kemo_mul_psf->psf_a->iflag_loaded[id_load] > 0){
			printf("Loaded PSF file %d %d %s\n", id_load, 
                   kemo_mul_psf->psf_m[id_load]->iformat_viz_file,
                   kemo_mul_psf->psf_m[id_load]->viz_prefix_c->string);
            kemo_mul_psf->psf_m[id_load]->viz_step_c = kemo_mul_psf->psf_a->istep_sync;
			evolution_PSF_data(kemo_mul_psf->psf_d[id_load],
                               kemo_mul_psf->psf_n[id_load],
                               psf_ucd_tmp, kemo_mul_psf->psf_m[id_load]);
		};
    };
	set_avail_time_flag(kemo_mul_psf);
	set_avail_file_step_flag(kemo_mul_psf);
	return;
}

void set_PSF_loaded_params(int selected, int input,
                           struct kemoview_mul_psf *kemo_mul_psf){
	if(selected == NUM_LOADED){
		set_PSF_num_loaded(input, kemo_mul_psf->psf_a);
	}else if(selected == MAX_LOADED){
		set_PSF_max_loaded(input, kemo_mul_psf->psf_a);
	}else if(selected == SET_CURRENT){
		set_current_PSF_to_menu(input, kemo_mul_psf->psf_a);
	};
	return;
}

int get_PSF_loaded_params(struct kemoview_mul_psf *kemo_mul_psf,
                          int selected){
	int output = 0;
	if(selected == NUM_LOADED){
		output= get_PSF_num_loaded(kemo_mul_psf->psf_a);
	}else if(selected == MAX_LOADED){
		output = get_PSF_max_loaded(kemo_mul_psf->psf_a);
	}else if(selected == SET_CURRENT){
		output = get_curent_PSF_ID(kemo_mul_psf->psf_a);
	}else if(selected == DRAW_SWITCH){
		output = get_PSF_draw_switch(kemo_mul_psf->psf_a);
	};
	return output;
}

void set_each_PSF_field_param(int selected, int input,
                              struct psf_data *viz_data,
                              struct psf_menu_val *viz_menu){
	if(selected == FIELD_SEL_FLAG){
        set_VIZ_field(input, viz_data->data_name[selected],
                      viz_data->istack_comp, viz_menu);
	}else if(selected == COMPONENT_SEL_FLAG){
        int i_field = viz_menu->if_draw_viz;
        set_VIZ_component(input, viz_data->data_name[i_field],
                          viz_data->istack_comp, viz_menu);
	};
	return;
};

long get_VIZ_field_param(int selected,
                         struct psf_data *viz_data,
                         struct psf_menu_val *viz_menu){
    long output = 0;
    
    if(selected == NUM_FIELD_FLAG){
        output = send_nfield_each_VIZ(viz_data);
    }else if(selected == NTOT_COMPONENT_FLAG){
        output = send_ncomptot_each_VIZ(viz_data);
    }else if(selected == FIELD_SEL_FLAG){
        output = send_field_draw_each_VIZ(viz_menu);
    }else if(selected == COMPONENT_SEL_FLAG){
        output = send_draw_comp_id_VIZ(viz_menu);
    }else if(selected == DRAW_ADDRESS_FLAG){
        output = send_draw_component_VIZ(viz_menu);
    }else if(selected == COORDINATE_FLAG){
        output = send_coordinate_id_VIZ(viz_data, viz_menu);
    };
    return output;
};

void set_each_PSF_draw_switch(int selected, int iflag, struct kemoview_mul_psf *kemo_mul_psf){
    int i_current = kemo_mul_psf->psf_a->id_current;
    if      (selected == PSFGRID_TOGGLE){
        set_draw_psf_grid(iflag, kemo_mul_psf->psf_m[i_current]);
    } else if (selected == ZEROGRID_TOGGLE){
        set_draw_psf_zero(iflag, kemo_mul_psf->psf_m[i_current]);
    }
    return;
}

int get_each_PSF_draw_switch(int selected, struct kemoview_mul_psf *kemo_mul_psf){
	int i_current = kemo_mul_psf->psf_a->id_current;
	int iflag = 0;
	if      (selected == PSFGRID_TOGGLE){
		iflag = send_draw_psf_grid(kemo_mul_psf->psf_m[i_current]);
	} else if (selected == ZEROGRID_TOGGLE){
		iflag = send_draw_psf_zero(kemo_mul_psf->psf_m[i_current]);
	} else if (selected == PSF_POLYGON_SWITCH){
		iflag = send_each_psf_polygon_mode(kemo_mul_psf->psf_m[i_current]);
	} else if (selected == PSFTANVEC_TOGGLE){
		iflag = send_VIZ_vector_mode(kemo_mul_psf->psf_m[i_current]);
	};
	return iflag;
}

void update_PSF_textured_id(struct kemoview_mul_psf *kemo_mul_psf){
    int i;
    int i_current = kemo_mul_psf->psf_a->id_current;
    for(i=0;i<kemo_mul_psf->psf_a->nmax_loaded;i++){
        if(kemo_mul_psf->psf_a->iflag_loaded[i] != 0
           && kemo_mul_psf->psf_m[i]->viz_color_mode == TEXTURED_SURFACE
           && i != i_current){
            kemo_mul_psf->psf_m[i]->viz_color_mode = COLORED_BY_DATA;
        };
    };
    kemo_mul_psf->psf_a->ipsf_texured = i_current;
    release_texture_4_psf(kemo_mul_psf->psf_a);
};

void set_each_PSF_color_param(int selected, int input,
                              struct kemoview_mul_psf *kemo_mul_psf){
    int i_current = kemo_mul_psf->psf_a->id_current;
    if(selected == PSFGRID_TOGGLE){
		set_each_isoline_color(kemo_mul_psf->psf_m[i_current], input);
	}else if(selected == ISET_NLINE){
		set_each_n_isoline(kemo_mul_psf->psf_m[i_current], input);
	}else if(selected == ISET_VECTOR_COLOR){
		set_each_vector_patch_color(kemo_mul_psf->psf_m[i_current], input);
	};
	return;
};

void set_viz_colormap_param(int selected, int input,
                            struct psf_menu_val *viz_menu){
    if(selected == ISET_COLORMAP){
        set_viz_colormap_id(viz_menu, input);
    }
}

int get_viz_colormap_param(int selected,
                           struct psf_menu_val *viz_menu){
    int iflag = 0;
    if(selected == ISET_COLORMAP){
        iflag = get_PSF_colormap_id(viz_menu);
    }else if(selected == ISET_NUM_COLOR){
        iflag = get_VIZ_color_table_num(viz_menu);
    }else if(selected == ISET_NUM_OPACITY){
        iflag = get_VIZ_opacity_table_num(viz_menu);
    }
    return iflag;
}


int get_each_PSF_color_param(int selected, struct kemoview_mul_psf *kemo_mul_psf){
	int iflag = 0;
	int i_current = kemo_mul_psf->psf_a->id_current;
	if(selected == PSFGRID_TOGGLE){
		iflag = send_each_isoline_color(kemo_mul_psf->psf_m[i_current]);
	}else if(selected == ISET_NLINE){
		iflag = send_num_isoline(kemo_mul_psf->psf_m[i_current]);
	}else if(selected == ISET_COLORMAP){
		iflag = get_PSF_colormap_id(kemo_mul_psf->psf_m[i_current]);
	}else if(selected == ISET_VECTOR_COLOR){
		iflag = send_each_vector_patch_color(kemo_mul_psf->psf_m[i_current]);
	};
	return iflag;
};

void set_VIZ_vector_w_exp(int selected, double value, int i_digit, 
                          struct psf_menu_val *viz_menu){
	double data = const_from_digit_order(value, i_digit);
	if(selected == ISET_PSF_REFVECT){
		set_each_scale_vect(data, viz_menu);
	}else if(selected == ISET_PSF_V_THICK){
		set_each_vector_thick(data, viz_menu);
	}else if(selected == ISET_VECTOR_INC){
		set_each_increment_vect((int) data, viz_menu);
	};
	return;
};

void get_VIZ_vector_w_exp(int selected, struct psf_menu_val *viz_menu,
                          double *value, int *i_digit){
    double data = 0.0;
    if(selected == ISET_PSF_REFVECT){
        data = send_scale_vector(viz_menu);
    }else if(selected == ISET_PSF_V_THICK){
        data = send_vector_thick(viz_menu);
    }else if(selected == ISET_VECTOR_INC){
        data = (double) send_each_increment_vect(viz_menu);
    };
    find_order_digit(data, value, i_digit);
    return;
};

void get_VIZ_color_w_exp(int selected, struct psf_menu_val *viz_menu,
                         double *value, int *i_digit){
    double data = 0.0;
	if(selected == ISET_COLOR_MIN){
		data = get_VIZ_color_table_min(viz_menu);
	}else if(selected == ISET_COLOR_MAX){
		data = get_each_PSF_color_table_max(viz_menu);
    }else if(selected == ISET_WIDTH){
        data = get_VIZ_line_width(viz_menu);
    };
    find_order_digit(data, value, i_digit);
    return;
};

double get_VIZ_data_range(int selected, int icomp,
                          struct psf_data *viz_d){
	double value = 0.0;
	if(selected == ISET_COLOR_MIN){
		value = send_VIZ_data_min(viz_d, icomp);
	}else if(selected == ISET_COLOR_MAX){
		value = send_VIZ_data_max(viz_d, icomp);
	};
	return value;
}


double get_VIZ_opacity_range(int selected, struct psf_menu_val *viz_menu){
    double value = 0.0;
    if(selected == ISET_OPACITY_MIN){
        value = get_VIZ_minimum_opacity(viz_menu);
    }else if(selected == ISET_OPACITY_MAX){
        value = get_VIZ_maximum_opacity(viz_menu);
    };
    return value;
};

double get_each_PSF_colormap_range(int selected, struct kemoview_mul_psf *kemo_mul_psf){
	double value = 0.0;
	int i_current = kemo_mul_psf->psf_a->id_current;
	if(selected == ISET_OPACITY_MIN){
		value = get_VIZ_minimum_opacity(kemo_mul_psf->psf_m[i_current]);
	}else if(selected == ISET_OPACITY_MAX){
		value = get_VIZ_maximum_opacity(kemo_mul_psf->psf_m[i_current]);
	};
	return value;
};

void set_PSF_linear_colormap(double minvalue, int i_min_digit,
                             double maxvalue, int i_max_digit,
                             struct kemoview_mul_psf *kemo_mul_psf){
    int i_current = kemo_mul_psf->psf_a->id_current;
    set_VIZ_linear_colormap(minvalue, i_min_digit, maxvalue, i_max_digit,
                                 kemo_mul_psf->psf_m[i_current]);
}


void set_draw_time_flag(int iflag, struct kemoview_mul_psf *kemo_mul_psf){
    kemo_mul_psf->psf_a->iflag_draw_time = iflag;
};
int toggle_draw_time_flag(struct kemoview_mul_psf *kemo_mul_psf){
    return kemo_mul_psf->psf_a->iflag_draw_time
            = toggle_value_c(kemo_mul_psf->psf_a->iflag_draw_time);
};
int get_draw_time_flag(struct kemoview_mul_psf *kemo_mul_psf){
    return kemo_mul_psf->psf_a->iflag_draw_time;
};
int get_avail_time_flag(struct kemoview_mul_psf *kemo_mul_psf){
    return kemo_mul_psf->psf_a->iflag_avail_time;
};

void set_draw_file_step_flag(int iflag, struct kemoview_mul_psf *kemo_mul_psf){
    kemo_mul_psf->psf_a->iflag_draw_file_step = iflag;
};
int toggle_draw_file_step_flag(struct kemoview_mul_psf *kemo_mul_psf){
    return kemo_mul_psf->psf_a->iflag_draw_file_step
        = toggle_value_c(kemo_mul_psf->psf_a->iflag_draw_file_step);
};
int get_draw_file_step_flag(struct kemoview_mul_psf *kemo_mul_psf){
    return kemo_mul_psf->psf_a->iflag_draw_file_step;
};
int get_avail_file_step_flag(struct kemoview_mul_psf *kemo_mul_psf){
    return kemo_mul_psf->psf_a->iflag_avail_file_step;
};


void set_PSF_polygon_mode(int iflag, struct kemoview_mul_psf *kemo_mul_psf){
    int i_current = kemo_mul_psf->psf_a->id_current;
    set_each_psf_polygon_mode(kemo_mul_psf->psf_m[i_current], iflag);
};
void set_PSF_tangential_vec_mode(int iflag, struct kemoview_mul_psf *kemo_mul_psf){
    int i_current = kemo_mul_psf->psf_a->id_current;
    set_each_psf_vector_mode(kemo_mul_psf->psf_m[i_current], iflag);
};
int get_PSF_draw_refv(struct kemoview_mul_psf *kemo_mul_psf){
    int i_current = kemo_mul_psf->psf_a->id_current;
    return send_draw_each_psf_refv(kemo_mul_psf->psf_m[i_current]);
};

void write_PSF_colormap_file(struct kv_string *filename, int iflag_draw_axis,
                             struct psf_menu_val *viz_menu){
    write_VIZ_colormap_control_file(filename, iflag_draw_axis, viz_menu);
}



int send_psf_file_dir_prefix(struct kemoview_mul_psf *kemo_mul_psf,
                             struct kv_string *stripped_dir,
                             struct kv_string *stripped_filehead){
    int i_current = kemo_mul_psf->psf_a->id_current;
    return send_each_psf_file_dir_prefix(kemo_mul_psf->psf_m[i_current],
                                         stripped_dir, stripped_filehead);
};

struct colormap_params * link_active_colormap_param(struct psf_menu_val *viz_menu){
    long icomp =    send_draw_component_VIZ(viz_menu);
    return viz_menu->cmap_viz_comp[icomp];
}

