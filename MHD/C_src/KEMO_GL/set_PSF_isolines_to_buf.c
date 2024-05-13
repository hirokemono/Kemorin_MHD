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
        
        long ntmp = inum_patch / 12;
        long nend;
        long *inod_itp_psf = (long *) calloc(4*istack_smp_psf_iso[1+psf_m->n_isoline*nthreads], sizeof(long));
        long *iedge_itp = (long *) calloc(2*istack_smp_psf_iso[1+psf_m->n_isoline*nthreads], sizeof(long));
        long *ineib_itp = (long *) calloc(istack_smp_psf_iso[1+psf_m->n_isoline*nthreads], sizeof(long));
        double *xyzw_line = (double *) calloc(8*istack_smp_psf_iso[1+psf_m->n_isoline*nthreads], sizeof(double));
        nend = set_each_isoline_test(ntmp, IZERO, psf_s->nele_viz,
                                     dub_r, ZERO, psf_m->icomp_draw_psf, black,
                                     psf_s, inod_itp_psf, iedge_itp,
                                     xyzw_line);
        long j, jj;
        long iedge;
        long *inum_line = (long *) calloc(2*psf_s->psf_edge->nedge_viewer, sizeof(long));
        long *ineib_edge = (long *) calloc(2*psf_s->psf_edge->nedge_viewer, sizeof(long));
        long *icou_lone = (long *) calloc(2*psf_s->psf_edge->nedge_viewer, sizeof(long));
        for(j=0;j<2*psf_s->psf_edge->nedge_viewer;j++){
            inum_line[j] = -1;
            ineib_edge[j] = -1;
        }
        long iedge1, iedge2;
        for(j=0;j<(nend-ntmp);j++){
            iedge1 = labs(iedge_itp[2*j  ]) - 1;
            iedge2 = labs(iedge_itp[2*j+1]) - 1;
            icou_lone[iedge1] = icou_lone[iedge1] + 1;
            icou_lone[iedge2] = icou_lone[iedge2] + 1;
            if(inum_line[2*iedge1] < 0){
                inum_line[2*iedge1] = 2*j;
                ineib_edge[2*iedge1] = iedge2;
            }else if(inum_line[2*iedge1] >= 0){
                inum_line[2*iedge1+1] = 2*j;
                ineib_edge[2*iedge1+1] = iedge2;
            }

            if(inum_line[2*iedge2] < 0){
                inum_line[2*iedge2] = 2*j+1;
                ineib_edge[2*iedge2] = iedge1;
            }else if(inum_line[2*iedge2] >= 0){
                inum_line[2*iedge2+1] = 2*j+1;
                ineib_edge[2*iedge2+1] = iedge1;
            }

        }
/*
        long icou = 0;
        for(iedge=0;iedge<psf_s->psf_edge->nedge_viewer;iedge++){
            if(inum_line[2*iedge+1] > 0){
                icou = icou+2;
                printf("count %d %d  %d %d   %d %d\n", iedge, icou_lone[iedge],
                       inum_line[2*iedge  ], inum_line[2*iedge+1],
                       ineib_edge[2*iedge  ], ineib_edge[2*iedge+1]);
            }else if(inum_line[2*iedge  ]  > 0){
                icou = icou + 1;
                printf("count %d %d  %d %d   %d %d\n", iedge, icou_lone[iedge],
                       inum_line[2*iedge  ], inum_line[2*iedge+1],
                       ineib_edge[2*iedge  ], ineib_edge[2*iedge+1]);
            };
        }
        printf("todal %d %d\n", icou/2, (nend-ntmp));

    /*
        for(iedge=0;iedge<psf_s->psf_edge->nedge_viewer;iedge++){
            j1 = inum_line[2*iedge  ];
            j2 = inum_line[2*iedge+1];
            ineib1 = ineib_edge[2*iedge  ];
            ineib2 = ineib_edge[2*iedge+1];
            k1 = inum_line[2*ineib1  ];
            k2 = inum_line[2*ineib2  ];
            if(j2 >= 0){
                printf("edges %d    %d %d %d\n", iedge, j1, k1, k2);
            }else if(j1 >= 0){
                printf("ohpaned edges %d    %d %d   %d  %d  %d\n", iedge,
                       ineib1 , ineib2, j1, k1, k2);
            }
        }
        printf("todal %d %d\n", icou/2, (nend-ntmp));
*/
        double len;
        long j1, j2, j3, j4;
        long i11, i12, i21, i22;
        long ineib1, ineib2;
        long k1, k2, k3;
        double *xyzw_edge = (double *) calloc(4*psf_s->psf_edge->nedge_viewer, sizeof(double));
        double *dir_edge = (double *) calloc(4*psf_s->psf_edge->nedge_viewer, sizeof(double));
        double *norm_ed = (double *) calloc(4*psf_s->psf_edge->nedge_viewer, sizeof(double));
        long in1, in2;
        for(iedge=0;iedge<psf_s->psf_edge->nedge_viewer;iedge++){
            j1 = inum_line[2*iedge  ];
            j2 = inum_line[2*iedge+1];
            ineib1 = ineib_edge[2*iedge  ];
            ineib2 = ineib_edge[2*iedge+1];
            k1 = inum_line[2*ineib1  ];
            k2 = inum_line[2*ineib2  ];
            in1 = psf_s->psf_edge->ie_edge[iedge][0] - 1;
            in2 = psf_s->psf_edge->ie_edge[iedge][1] - 1;
            if(j2 >= 0){
                xyzw_edge[4*iedge  ] = xyzw_line[4*k1  ];
                xyzw_edge[4*iedge+1] = xyzw_line[4*k1+1];
                xyzw_edge[4*iedge+2] = xyzw_line[4*k1+2];

                dir_edge[4*iedge  ] = xyzw_line[4*k2  ] - xyzw_line[4*k1  ];
                dir_edge[4*iedge+1] = xyzw_line[4*k2+1] - xyzw_line[4*k1+1];
                dir_edge[4*iedge+2] = xyzw_line[4*k2+2] - xyzw_line[4*k1+2];

                len = sqrt(  (dir_edge[4*iedge  ]) * dir_edge[4*iedge  ]
                           + (dir_edge[4*iedge+1]) * dir_edge[4*iedge+1]
                           + (dir_edge[4*iedge+2]) * dir_edge[4*iedge+2]);
                /*
                if(len > 0.0){
                    dir_edge[4*iedge  ] = dir_edge[4*iedge  ] / len;
                    dir_edge[4*iedge+1] = dir_edge[4*iedge+1] / len;
                    dir_edge[4*iedge+2] = dir_edge[4*iedge+2] / len;
                }
*/

                cal_normal_4_quad_c(&xyzw_line[4*k2  ], &xyzw_edge[4*iedge],
                                    &xyzw_line[4*k1  ], &psf_s->xyzw_viz[4*in2],
                                    &norm_ed[4*iedge]);
            }else if(j1 >= 0){
                xyzw_edge[4*iedge  ] = xyzw_line[4*k1  ];
                xyzw_edge[4*iedge+1] = xyzw_line[4*k1+1];
                xyzw_edge[4*iedge+2] = xyzw_line[4*k1+2];

                dir_edge[4*iedge  ] = xyzw_line[4*k1  ] - xyzw_line[4*j1  ];
                dir_edge[4*iedge+1] = xyzw_line[4*k1+1] - xyzw_line[4*j1+1];
                dir_edge[4*iedge+2] = xyzw_line[4*k1+2] - xyzw_line[4*j1+2];

                len = sqrt(  (dir_edge[4*iedge  ]) * dir_edge[4*iedge  ]
                           + (dir_edge[4*iedge+1]) * dir_edge[4*iedge+1]
                           + (dir_edge[4*iedge+2]) * dir_edge[4*iedge+2]);
                if(len > 0.0){
                    dir_edge[4*iedge  ] = dir_edge[4*iedge  ] / len;
                    dir_edge[4*iedge+1] = dir_edge[4*iedge+1] / len;
                    dir_edge[4*iedge+2] = dir_edge[4*iedge+2] / len;
                }

                cal_normal_4_quad_c(&xyzw_line[4*k1  ], &psf_s->xyzw_viz[4*in1],
                                    &xyzw_line[4*j1  ], &psf_s->xyzw_viz[4*in2],
                                    &norm_ed[4*iedge]);
            /*
                printf("ohpaned edges %d %d  %d      %lf %lf %lf     %lf  %lf  %lf\n",
                       iedge, k1, j1,
                       xyzw_line[4*k1  ],
                       xyzw_line[4*k1+1],
                       xyzw_line[4*k1+2],
                       xyzw_line[4*j1  ],
                       xyzw_line[4*j1+1],
                       xyzw_line[4*j1+2]);
             */
            }
            norm_ed[4*iedge+3] = 1.0;
        }

        for(iedge=0;iedge<psf_s->psf_edge->nedge_viewer;iedge++){
            in1 = psf_s->psf_edge->ie_edge[iedge][0] - 1;
            in2 = psf_s->psf_edge->ie_edge[iedge][1] - 1;
            j1 = inum_line[2*iedge  ];
            j2 = inum_line[2*iedge+1];
            ineib1 = ineib_edge[2*iedge  ];
            ineib2 = ineib_edge[2*iedge+1];
            k1 = inum_line[2*ineib1  ];
            k2 = inum_line[2*ineib2  ];
            if(j2 >= 0){
                /*
                printf("edges %d    %lf %lf %lf     %lf  %lf  %lf         %lf\n", iedge,
                       dir_edge[4*iedge  ],
                       dir_edge[4*iedge+1],
                       dir_edge[4*iedge+2],
                       norm_ed[4*iedge  ],
                       norm_ed[4*iedge+1],
                       norm_ed[4*iedge+2],
                       (  (dir_edge[4*iedge  ]) * norm_ed[4*iedge  ]
                        + (dir_edge[4*iedge+1]) * norm_ed[4*iedge+1]
                        + (dir_edge[4*iedge+2]) * norm_ed[4*iedge+2]));
                (  (psf_s->xyzw_viz[4*in2  ]-psf_s->xyzw_viz[4*in1  ]) * norm_ed[4*iedge  ]
                 + (psf_s->xyzw_viz[4*in2+1]-psf_s->xyzw_viz[4*in1+1]) * norm_ed[4*iedge+1]
                 + (psf_s->xyzw_viz[4*in2+2]-psf_s->xyzw_viz[4*in1+2]) * norm_ed[4*iedge+2]));
*/
            }else if(j1 >= 0){
            /*
                printf("ohpaned edges %d     %lf %lf %lf     %lf  %lf %lf     %lf\n", iedge,
                       dir_edge[4*iedge  ],
                       dir_edge[4*iedge+1],
                       dir_edge[4*iedge+2],
                       norm_ed[4*iedge  ],
                       norm_ed[4*iedge+1],
                       norm_ed[4*iedge+2],
                       (  (psf_s->xyzw_viz[4*in2+1]-psf_s->xyzw_viz[4*in1  ]) * norm_ed[4*iedge  ]
                        + (psf_s->xyzw_viz[4*in2+1]-psf_s->xyzw_viz[4*in1+1]) * norm_ed[4*iedge+1]
                        + (psf_s->xyzw_viz[4*in2+2]-psf_s->xyzw_viz[4*in1+2]) * norm_ed[4*iedge+2]));
             */
            }
        }

        free(iedge_itp);
        free(inod_itp_psf);

        
        
        
		dub_r = 2.0 * psf_m->isoline_width;



        inum_patch = set_each_isoline_to_buf2(inum_patch, ntmp, nend,
                                              dub_r, ZERO, psf_m->icomp_draw_psf, black,
                                              psf_s, iedge_itp, xyzw_edge, dir_edge, norm_ed,
                                              psf_buf);


/*
        inum_patch = sel_each_isoline_to_buf_pthread(inum_patch, nthreads,
                                                     &istack_smp_psf_iso[psf_m->n_isoline*nthreads],
                                                     dub_r, ZERO, psf_m->icomp_draw_psf, black,
                                                     psf_s, psf_buf);
 */
 
	};
	
	return inum_patch;
}
