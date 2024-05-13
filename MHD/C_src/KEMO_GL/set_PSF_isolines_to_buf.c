/*
// set_PSF_isolines_to_buf.c
*/

#include "set_PSF_isolines_to_buf.h"

static double black[4] =   {BLACK_R,BLACK_G,BLACK_B,BLACK_A};
static double white[4] =   {WHITE_R,WHITE_G,WHITE_B,WHITE_A};

double cal_isoline_value(int j, int n_isoline, struct colormap_params *cmap_s){
	double v_line, range_min, range_max;
	double v1, v2;
	int num = count_real2_clist(cmap_s->colormap);
	set_from_real2_clist_at_index(0,     cmap_s->colormap, &range_min, &v1);
	set_from_real2_clist_at_index(num-1, cmap_s->colormap, &range_max, &v2);
	
	v_line = range_min + (range_max - range_min)
			* ((double) j) / ((double) (n_isoline-1));
	
	return v_line;
}

int find_start_positive_lines(int n_isoline, struct colormap_params *cmap_s){
	int ist_positive_line = 0;
    int j;
    double pre_value, current_value, range_min, range_max;
	double v1, v2;
	int num = count_real2_clist(cmap_s->colormap);
	set_from_real2_clist_at_index(0,     cmap_s->colormap, &range_min, &v1);
	set_from_real2_clist_at_index(num-1, cmap_s->colormap, &range_max, &v2);
    
    if(range_min >= ZERO) ist_positive_line = 0;
    else if(range_max <= ZERO){
        ist_positive_line = n_isoline;
    } else {
        ist_positive_line = 0;
        current_value = range_min;
        for (j = 1; j < n_isoline; j++){
            pre_value = current_value;
            current_value = cal_isoline_value(j, n_isoline, cmap_s);
            if((current_value*pre_value) <= ZERO){return j;};
        };
    };
    
    return ist_positive_line;
}

static long add_num_PSF_isolines(long ist_patch, const int nthreads, int ist, int ied,
                                 struct psf_data *psf_s, struct psf_menu_val *psf_m,
                                 long *istack_smp_psf_iso){
	int j;
	double v_line;
	
	long num_patch = ist_patch;
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m->n_isoline,
								   psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
        num_patch = sel_add_each_isoline_npatch_pthread(num_patch, nthreads, v_line,
                                                        psf_m->icomp_draw_psf, psf_s,
                                                        &istack_smp_psf_iso[j*nthreads]);
        printf("count num_patch-ist_patch: %d %d\n", j, num_patch-ist_patch);
	};
	return num_patch;
}

static long set_PSF_isolines_to_buf(const long ist_patch, int ist, int ied,
                                    const int nthreads, long *istack_smp_psf_iso,
                                    struct psf_data *psf_s, struct psf_menu_val *psf_m, 
                                    struct gl_strided_buffer *psf_buf){
	int j, nd;
	double v_line;
	double f_color[4];
	
    struct colormap_params *cmap_s = psf_m->cmap_psf_comp[psf_m->icomp_draw_psf];
    struct colormap_array *cmap_array = init_colormap_from_list(cmap_s->colormap);
    struct colormap_array *omap_array = init_colormap_from_list(cmap_s->opacitymap);
	if (psf_m->isoline_color == BLACK_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = black[nd];}
	} else if(psf_m->isoline_color == WHITE_LINE){
		for(nd=0;nd<4;nd++) {f_color[nd] = white[nd];}
	};
	
	long inum_patch = ist_patch;
	for (j = ist; j < ied; j++){
		v_line = cal_isoline_value(j, psf_m->n_isoline, 
								   psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		
		if (psf_m->isoline_color == RAINBOW_LINE){
			set_rainbow_color_code(cmap_array, omap_array, cmap_s->id_color_mode,
                                   v_line, f_color);
		};
        
        inum_patch = sel_each_isoline_to_buf_pthread(inum_patch, nthreads,
                                                     &istack_smp_psf_iso[j*nthreads],
                                                     psf_m->isoline_width, v_line,
                                                     psf_m->icomp_draw_psf, f_color,
                                                     psf_s, psf_buf);
        
        printf("inum_patch-ist_patch: %d %d\n", j, inum_patch-ist_patch);
	};
    dealloc_colormap_array(omap_array);
    dealloc_colormap_array(cmap_array);
	return inum_patch;
}


long add_PSF_all_isolines_num(const long ist_patch, const int nthreads,
                              struct psf_data *psf_s, struct psf_menu_val *psf_m,
                              long *istack_smp_psf_iso){
	long num_patch = ist_patch;
	if(psf_m->draw_psf_grid  != 0){
		psf_m->ist_positive_line = find_start_positive_lines(psf_m->n_isoline,
								psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		if(psf_m->ist_positive_line > 1){
			num_patch = add_num_PSF_isolines(num_patch, nthreads,
                                             IZERO, psf_m->ist_positive_line,
											 psf_s, psf_m, istack_smp_psf_iso);
		};
        if(psf_m->ist_positive_line < psf_m->n_isoline){
            num_patch = add_num_PSF_isolines(num_patch, nthreads,
                                             psf_m->ist_positive_line, psf_m->n_isoline,
											 psf_s, psf_m, istack_smp_psf_iso);
        };
    };
	if(psf_m->draw_psf_zero != 0){
        num_patch = sel_add_each_isoline_npatch_pthread(num_patch, nthreads, ZERO,
                                                        psf_m->icomp_draw_psf, psf_s,
                                                        &istack_smp_psf_iso[psf_m->n_isoline*nthreads]);
    };
	return num_patch;
}

long set_PSF_all_isolines_to_buf(const long ist_patch,
                                 const int nthreads, long *istack_smp_psf_iso,
                                 struct psf_data *psf_s, struct psf_menu_val *psf_m,
                                 struct gl_strided_buffer *psf_buf){
	double dub_r;
	long inum_patch = ist_patch;
	if(psf_m->draw_psf_grid  != 0){
		psf_m->ist_positive_line = find_start_positive_lines(psf_m->n_isoline,
								psf_m->cmap_psf_comp[psf_m->icomp_draw_psf]);
		if(psf_m->ist_positive_line > 1){
			inum_patch = set_PSF_isolines_to_buf(inum_patch, IZERO, psf_m->ist_positive_line,
                                                 nthreads, istack_smp_psf_iso,
                                                 psf_s, psf_m, psf_buf);
		};
		if(psf_m->ist_positive_line < psf_m->n_isoline){
			inum_patch = set_PSF_isolines_to_buf(inum_patch, psf_m->ist_positive_line, psf_m->n_isoline,
                                                 nthreads, istack_smp_psf_iso,
                                                 psf_s, psf_m, psf_buf);
        };
    };
    
    
	if(psf_m->draw_psf_zero  != 0){
        dub_r = 2.0 * psf_m->isoline_width;
        
        struct isoline_mesh_work *wk_iso_mesh
                = (struct isoline_mesh_work *) malloc(sizeof(struct isoline_mesh_work));
        if(wk_iso_mesh == NULL){
            printf("failed allocation for isoline_mesh_work\n");
            exit(1);
        }
        wk_iso_mesh->num_edge = psf_s->psf_edge->nedge_viewer;
        wk_iso_mesh->inum_line =  (long *) malloc(2*wk_iso_mesh->num_edge * sizeof(long));
        if(wk_iso_mesh->inum_line == NULL){
            printf("failed allocation for wk_iso_mesh->inum_line\n");
            exit(1);
        }
        wk_iso_mesh->ineib_edge = (long *) malloc(2*wk_iso_mesh->num_edge * sizeof(long));
        if(wk_iso_mesh->ineib_edge == NULL){
            printf("failed allocation for wk_iso_mesh->ineib_edge\n");
            exit(1);
        }
        wk_iso_mesh->xyzw_edge = (double *) calloc(4*wk_iso_mesh->num_edge, sizeof(double));
        if(wk_iso_mesh->xyzw_edge == NULL){
            printf("failed allocation for wk_iso_mesh->xyzw_edge\n");
            exit(1);
        }
        
        struct isoline_line_work *wk_iso_line
                = (struct isoline_line_work *) malloc(sizeof(struct isoline_line_work));
        if(wk_iso_line == NULL){
            printf("failed allocation for isoline_line_work\n");
            exit(1);
        }
        wk_iso_line->num_line = istack_smp_psf_iso[(psf_m->n_isoline+1)*nthreads]
                                - istack_smp_psf_iso[psf_m->n_isoline*nthreads];
        wk_iso_line->num_line = wk_iso_line->num_line / 12;
        
        wk_iso_line->iedge_itp = (long *) calloc(2*wk_iso_line->num_line, sizeof(long));
        if(wk_iso_line->iedge_itp == NULL){
            printf("failed allocation for wk_iso_line->iedge_itp\n");
            exit(1);
        }
        wk_iso_line->xyzw_line = (double *) calloc(8*wk_iso_line->num_line, sizeof(double));
        if(wk_iso_line->xyzw_line == NULL){
            printf("failed allocation for wk_iso_line->xyzw_line\n");
            exit(1);
        }
        wk_iso_line->dir_line = (double *) calloc(8*wk_iso_line->num_line, sizeof(double));
        if(wk_iso_line->dir_line == NULL){
            printf("failed allocation for wk_iso_line->dir_line\n");
            exit(1);
        }
        wk_iso_line->norm_line = (double *) calloc(8*wk_iso_line->num_line, sizeof(double));
        if(wk_iso_line->norm_line == NULL){
            printf("failed allocation for wk_iso_line->norm_line\n");
            exit(1);
        }
        wk_iso_line->color_line = (double *) calloc(8*wk_iso_line->num_line, sizeof(double));
        if(wk_iso_line->color_line == NULL){
            printf("failed allocation for wk_iso_line->color_line\n");
            exit(1);
        }
        
        
        
        long ntmp = 0;
        long nend;
        nend = set_each_isoline_test(ntmp, IZERO, psf_s->nele_viz,
                                     dub_r, ZERO, psf_m->icomp_draw_psf, black,
                                     psf_s, wk_iso_line->iedge_itp, wk_iso_line->xyzw_line);
        

        long j;
        long iedge;
        for(j=0;j<2*wk_iso_mesh->num_edge;j++){
            wk_iso_mesh->inum_line[j] = -1;
            wk_iso_mesh->ineib_edge[j] = -1;
        }
        long iedge1, iedge2;
        for(j=0;j<(nend-ntmp);j++){
            iedge1 = labs(wk_iso_line->iedge_itp[2*j  ]) - 1;
            iedge2 = labs(wk_iso_line->iedge_itp[2*j+1]) - 1;
            if(wk_iso_mesh->inum_line[2*iedge1] < 0){
                wk_iso_mesh->inum_line[2*iedge1] = 2*j;
                wk_iso_mesh->ineib_edge[2*iedge1] = iedge2;
            }else if(wk_iso_mesh->inum_line[2*iedge1] >= 0){
                wk_iso_mesh->inum_line[2*iedge1+1] = 2*j;
                wk_iso_mesh->ineib_edge[2*iedge1+1] = iedge2;
            }

            if(wk_iso_mesh->inum_line[2*iedge2] < 0){
                wk_iso_mesh->inum_line[2*iedge2] = 2*j+1;
                wk_iso_mesh->ineib_edge[2*iedge2] = iedge1;
            }else if(wk_iso_mesh->inum_line[2*iedge2] >= 0){
                wk_iso_mesh->inum_line[2*iedge2+1] = 2*j+1;
                wk_iso_mesh->ineib_edge[2*iedge2+1] = iedge1;
            }

        }
        
        
        long j1, j2;
        long ineib1, ineib2;
        long k1, k2;
        long in2;
        
        for(iedge=0;iedge<wk_iso_mesh->num_edge;iedge++){
            j1 = wk_iso_mesh->inum_line[2*iedge  ];
            if(j1 < 0) continue;
            
            wk_iso_mesh->xyzw_edge[4*iedge  ] = wk_iso_line->xyzw_line[4*j1  ];
            wk_iso_mesh->xyzw_edge[4*iedge+1] = wk_iso_line->xyzw_line[4*j1+1];
            wk_iso_mesh->xyzw_edge[4*iedge+2] = wk_iso_line->xyzw_line[4*j1+2];
        };
        
        for(j=0;j<2*(nend-ntmp);j++){
            iedge = labs(wk_iso_line->iedge_itp[j]) - 1;
            j1 = wk_iso_mesh->inum_line[2*iedge  ];
            if(j1 < 0) continue;
            
            ineib1 = wk_iso_mesh->ineib_edge[2*iedge  ];
            k1 = wk_iso_mesh->inum_line[2*ineib1  ];
            
            j2 = wk_iso_mesh->inum_line[2*iedge+1];
            if(j2 < 0){
                wk_iso_line->dir_line[4*j  ] = wk_iso_line->xyzw_line[4*k1  ]
                                                - wk_iso_line->xyzw_line[4*j1  ];
                wk_iso_line->dir_line[4*j+1] = wk_iso_line->xyzw_line[4*k1+1]
                                                - wk_iso_line->xyzw_line[4*j1+1];
                wk_iso_line->dir_line[4*j+2] = wk_iso_line->xyzw_line[4*k1+2]
                                                - wk_iso_line->xyzw_line[4*j1+2];
            }else{
                in2 = psf_s->psf_edge->ie_edge[iedge][1] - 1;
                ineib2 = wk_iso_mesh->ineib_edge[2*iedge+1];
                k2 = wk_iso_mesh->inum_line[2*ineib2  ];
                wk_iso_line->dir_line[4*j  ] = wk_iso_line->xyzw_line[4*k2  ]
                                                - wk_iso_line->xyzw_line[4*k1  ];
                wk_iso_line->dir_line[4*j+1] = wk_iso_line->xyzw_line[4*k2+1]
                                                - wk_iso_line->xyzw_line[4*k1+1];
                wk_iso_line->dir_line[4*j+2] = wk_iso_line->xyzw_line[4*k2+2]
                                                - wk_iso_line->xyzw_line[4*k1+2];
            }
        }
        
        double def;
        for(iedge=0;iedge<wk_iso_mesh->num_edge;iedge++){
            ineib1 = wk_iso_mesh->ineib_edge[2*iedge  ];
            ineib2 = wk_iso_mesh->ineib_edge[2*iedge+1];
            if(iedge > ineib1){
                j1 = wk_iso_mesh->inum_line[2*iedge  ];
                k1 = wk_iso_mesh->inum_line[2*ineib1  ];
                def =  wk_iso_line->dir_line[4*j1  ] * wk_iso_line->dir_line[4*k1  ]
                     + wk_iso_line->dir_line[4*j1+1] * wk_iso_line->dir_line[4*k1+1]
                     + wk_iso_line->dir_line[4*j1+2] * wk_iso_line->dir_line[4*k1+2];
                if(def < 0.0){
                    wk_iso_line->dir_line[4*j1  ] = -wk_iso_line->dir_line[4*j1  ];
                    wk_iso_line->dir_line[4*j1+1] = -wk_iso_line->dir_line[4*j1+1];
                    wk_iso_line->dir_line[4*j1+2] = -wk_iso_line->dir_line[4*j1+2];
                }
                j2 = wk_iso_mesh->inum_line[2*iedge+1];
                if(j2 > 0){
                    wk_iso_line->dir_line[4*j2  ] = wk_iso_line->dir_line[4*j1  ];
                    wk_iso_line->dir_line[4*j2+1] = wk_iso_line->dir_line[4*j1+1];
                    wk_iso_line->dir_line[4*j2+2] = wk_iso_line->dir_line[4*j1+2];
                };
                continue;
            }
            if(ineib2 < 0) continue;
            if(iedge > ineib2){
                k2 = wk_iso_mesh->inum_line[2*ineib2  ];
                def =  wk_iso_line->dir_line[4*j1  ] * wk_iso_line->dir_line[4*k2  ]
                     + wk_iso_line->dir_line[4*j1+1] * wk_iso_line->dir_line[4*k2+1]
                     + wk_iso_line->dir_line[4*j1+2] * wk_iso_line->dir_line[4*k2+2];
                if(def < 0.0){
                    wk_iso_line->dir_line[4*j1  ] = -wk_iso_line->dir_line[4*j1  ];
                    wk_iso_line->dir_line[4*j1+1] = -wk_iso_line->dir_line[4*j1+1];
                    wk_iso_line->dir_line[4*j1+2] = -wk_iso_line->dir_line[4*j1+2];
                }
                j2 = wk_iso_mesh->inum_line[2*iedge+1];
                if(j2 > 0){
                    wk_iso_line->dir_line[4*j2  ] = wk_iso_line->dir_line[4*j1  ];
                    wk_iso_line->dir_line[4*j2+1] = wk_iso_line->dir_line[4*j1+1];
                    wk_iso_line->dir_line[4*j2+2] = wk_iso_line->dir_line[4*j1+2];
                };
            }
        }
    
        
        for(j=0;j<2*(nend-ntmp);j++){
            iedge = labs(wk_iso_line->iedge_itp[j]) - 1;
            j1 = wk_iso_mesh->inum_line[2*iedge  ];
            if(j1 < 0) continue;
            
            ineib1 = wk_iso_mesh->ineib_edge[2*iedge  ];
            k1 = wk_iso_mesh->inum_line[2*ineib1  ];
            
            j2 = wk_iso_mesh->inum_line[2*iedge+1];
            if(j2 < 0){
                cal_normal_4_quad_c(&wk_iso_line->xyzw_line[4*k1  ], &wk_iso_mesh->xyzw_edge[4*iedge],
                                    &wk_iso_line->xyzw_line[4*j1  ], &psf_s->xyzw_viz[4*in2],
                                    &wk_iso_line->norm_line[4*j]);
            }else{
                in2 = psf_s->psf_edge->ie_edge[iedge][1] - 1;
                ineib2 = wk_iso_mesh->ineib_edge[2*iedge+1];
                k2 = wk_iso_mesh->inum_line[2*ineib2  ];
                cal_normal_4_quad_c(&wk_iso_line->xyzw_line[4*k2  ], &wk_iso_mesh->xyzw_edge[4*iedge],
                                    &wk_iso_line->xyzw_line[4*k1  ], &psf_s->xyzw_viz[4*in2],
                                    &wk_iso_line->norm_line[4*j]);
            }
            wk_iso_line->norm_line[4*j+3] = 1.0;
        }
        
        for(iedge=0;iedge<wk_iso_mesh->num_edge;iedge++){
            ineib1 = wk_iso_mesh->ineib_edge[2*iedge  ];
            ineib2 = wk_iso_mesh->ineib_edge[2*iedge+1];
            if(iedge > ineib1){
                j1 = wk_iso_mesh->inum_line[2*iedge  ];
                k1 = wk_iso_mesh->inum_line[2*ineib1  ];
                def =  wk_iso_line->norm_line[4*j1  ] * wk_iso_line->norm_line[4*k1  ]
                     + wk_iso_line->norm_line[4*j1+1] * wk_iso_line->norm_line[4*k1+1]
                     + wk_iso_line->norm_line[4*j1+2] * wk_iso_line->norm_line[4*k1+2];
                if(def < 0.0){
                    wk_iso_line->norm_line[4*j1  ] = -wk_iso_line->norm_line[4*j1  ];
                    wk_iso_line->norm_line[4*j1+1] = -wk_iso_line->norm_line[4*j1+1];
                    wk_iso_line->norm_line[4*j1+2] = -wk_iso_line->norm_line[4*j1+2];
                }
                j2 = wk_iso_mesh->inum_line[2*iedge+1];
                if(j2 > 0){
                    wk_iso_line->norm_line[4*j2  ] = wk_iso_line->norm_line[4*j1  ];
                    wk_iso_line->norm_line[4*j2+1] = wk_iso_line->norm_line[4*j1+1];
                    wk_iso_line->norm_line[4*j2+2] = wk_iso_line->norm_line[4*j1+2];
                };
                continue;
            }
            if(ineib2 < 0) continue;
            if(iedge > ineib2){
                k2 = wk_iso_mesh->inum_line[2*ineib2  ];
                def =  wk_iso_line->norm_line[4*j1  ] * wk_iso_line->norm_line[4*k2  ]
                     + wk_iso_line->norm_line[4*j1+1] * wk_iso_line->norm_line[4*k2+1]
                     + wk_iso_line->norm_line[4*j1+2] * wk_iso_line->norm_line[4*k2+2];
                if(def < 0.0){
                    wk_iso_line->norm_line[4*j1  ] = -wk_iso_line->norm_line[4*j1  ];
                    wk_iso_line->norm_line[4*j1+1] = -wk_iso_line->norm_line[4*j1+1];
                    wk_iso_line->norm_line[4*j1+2] = -wk_iso_line->norm_line[4*j1+2];
                }
                j2 = wk_iso_mesh->inum_line[2*iedge+1];
                if(j2 > 0){
                    wk_iso_line->norm_line[4*j2  ] = wk_iso_line->norm_line[4*j1  ];
                    wk_iso_line->norm_line[4*j2+1] = wk_iso_line->norm_line[4*j1+1];
                    wk_iso_line->norm_line[4*j2+2] = wk_iso_line->norm_line[4*j1+2];
                };
            }
        }
    
        
        
        for(j=0;j<nend-ntmp;j++){
                wk_iso_line->color_line[8*j  ] = black[0];
                wk_iso_line->color_line[8*j+1] = black[1];
                wk_iso_line->color_line[8*j+2] = black[2];
                wk_iso_line->color_line[8*j+3] = black[3];
                wk_iso_line->color_line[8*j+4] = 0.05;
                wk_iso_line->color_line[8*j+5] = 0.8;
                wk_iso_line->color_line[8*j+6] = 0.8;
                wk_iso_line->color_line[8*j+7] = 1.0;
        }
        
        free(wk_iso_mesh->inum_line);
        free(wk_iso_mesh->ineib_edge);
        free(wk_iso_mesh->xyzw_edge);
        free(wk_iso_mesh);
        
        
        inum_patch = set_each_isoline_to_buf2(inum_patch, ntmp, nend, dub_r,
                                              psf_s, wk_iso_line, psf_buf);
        /*
        inum_patch = sel_each_isoline_to_buf_pthread(inum_patch, nthreads,
                                                     &istack_smp_psf_iso[psf_m->n_isoline*nthreads],
                                                     dub_r, ZERO,
                                                     psf_m->icomp_draw_psf, black,
                                                     psf_s, psf_buf);
        */
        free(wk_iso_line->iedge_itp);
        free(wk_iso_line->xyzw_line);
        free(wk_iso_line->dir_line);
        free(wk_iso_line->norm_line);
        free(wk_iso_line->color_line);
        free(wk_iso_line);
	};
    
	return inum_patch;
}
