/*
 *  t_PSF_each_isoline_edge_list.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#include "t_PSF_each_isoline_edge_list.h"

#define NCORNER  6

void dealloc_isoline_mesh_work(struct isoline_mesh_work *wk_iso_mesh){
        free(wk_iso_mesh->inum_line);
        free(wk_iso_mesh->ineib_edge);
        free(wk_iso_mesh->xyzw_edge);
        free(wk_iso_mesh);
    return;
}

void dealloc_isoline_line_work(struct isoline_line_work *wk_iso_line){
        free(wk_iso_line->iflag_checked);
    
        free(wk_iso_line->iedge_itp);
        free(wk_iso_line->xyzw_line);
        free(wk_iso_line->dir_line);
        free(wk_iso_line);
    return;
}

struct isoline_mesh_work * init_isoline_mesh_work(struct psf_edge_data_c *psf_edge){
        struct isoline_mesh_work *wk_iso_mesh
                = (struct isoline_mesh_work *) malloc(sizeof(struct isoline_mesh_work));
        if(wk_iso_mesh == NULL){
            printf("failed allocation for isoline_mesh_work\n");
            exit(1);
        }
        wk_iso_mesh->num_edge = psf_edge->nedge_viewer;
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
    return wk_iso_mesh;
}

struct isoline_line_work * init_isoline_line_work(int nthreads, long *istack_threads){
        struct isoline_line_work *wk_iso_line
                = (struct isoline_line_work *) malloc(sizeof(struct isoline_line_work));
    if(wk_iso_line == NULL){
        printf("failed allocation for isoline_line_work\n");
        exit(1);
    }
    wk_iso_line->num_line = istack_threads[nthreads] - istack_threads[0];
    
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
    wk_iso_line->iflag_checked = (int *) malloc(2*wk_iso_line->num_line*sizeof(int));
    
    wk_iso_line->ncorner = NCORNER;
    return wk_iso_line;
}


void set_isoline_color_in_wk(double color[4],
                             struct isoline_line_work *wk_iso_line){
    for(int nd=0;nd<4;nd++){
        wk_iso_line->f_color[nd] = color[nd];
        wk_iso_line->f_color[nd+4] = color[nd];
    };
    return;
}

void set_isoline_edge_list(struct isoline_line_work *wk_iso_line, 
                           struct isoline_mesh_work *wk_iso_mesh){
        long j;
        for(j=0;j<2*wk_iso_mesh->num_edge;j++){
            wk_iso_mesh->inum_line[j] = -1;
            wk_iso_mesh->ineib_edge[j] = -1;
        }
        long iedge1, iedge2;
        for(j=0;j<wk_iso_line->num_line;j++){
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
    return;
}

void set_isoline_position_on_edge(struct isoline_line_work *wk_iso_line, 
                                  struct isoline_mesh_work *wk_iso_mesh){
    long iedge;
    long j1;
    
        for(iedge=0;iedge<wk_iso_mesh->num_edge;iedge++){
            j1 = wk_iso_mesh->inum_line[2*iedge  ];
            if(j1 < 0) continue;
            
            wk_iso_mesh->xyzw_edge[4*iedge  ] = wk_iso_line->xyzw_line[4*j1  ];
            wk_iso_mesh->xyzw_edge[4*iedge+1] = wk_iso_line->xyzw_line[4*j1+1];
            wk_iso_mesh->xyzw_edge[4*iedge+2] = wk_iso_line->xyzw_line[4*j1+2];
        };
        
    return;
}

void set_direction_for_isoline(struct psf_edge_data_c *psf_edge,
                               struct isoline_mesh_work *wk_iso_mesh, 
                               struct isoline_line_work *wk_iso_line){
    long iedge, ineib1, ineib2;
    long j, j1, j2, k1, k2;
    
        for(j=0;j<2*(wk_iso_line->num_line);j++){
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
    return;
}

void set_normal_for_isoline(double *xyzw_psf,
                            struct psf_edge_data_c *psf_edge,
                            struct isoline_mesh_work *wk_iso_mesh, 
                            struct isoline_line_work *wk_iso_line){
    long iedge, ineib1;
    long j, j1, j2, k1, in2;
    
        for(j=0;j<2*(wk_iso_line->num_line);j++){
            iedge = labs(wk_iso_line->iedge_itp[j]) - 1;
            j1 = wk_iso_mesh->inum_line[2*iedge  ];
            if(j1 < 0) continue;
            
            ineib1 = wk_iso_mesh->ineib_edge[2*iedge  ];
            k1 = wk_iso_mesh->inum_line[2*ineib1  ];
            
            j2 = wk_iso_mesh->inum_line[2*iedge+1];
            in2 = psf_edge->ie_edge[iedge][1] - 1;
        }
    return;
}

void adjust_direction_by_neighbor(struct isoline_mesh_work *wk_iso_mesh, 
                                  double *vect_line){
    long iedge, ineib1, ineib2;
    long j1, j2, k1, k2;
    
        double def;
        for(iedge=0;iedge<wk_iso_mesh->num_edge;iedge++){
            j1 = wk_iso_mesh->inum_line[2*iedge  ];
            if(j1 < 0) continue;
            
            ineib1 = wk_iso_mesh->ineib_edge[2*iedge  ];
            ineib2 = wk_iso_mesh->ineib_edge[2*iedge+1];
            if(iedge > ineib1){
                k1 = wk_iso_mesh->inum_line[2*ineib1  ];
                def =  vect_line[4*j1  ] * vect_line[4*k1  ]
                     + vect_line[4*j1+1] * vect_line[4*k1+1]
                     + vect_line[4*j1+2] * vect_line[4*k1+2];
                if(def < 0.0){
                    vect_line[4*j1  ] = -vect_line[4*j1  ];
                    vect_line[4*j1+1] = -vect_line[4*j1+1];
                    vect_line[4*j1+2] = -vect_line[4*j1+2];
                }
                j2 = wk_iso_mesh->inum_line[2*iedge+1];
                if(j2 > 0){
                    vect_line[4*j2  ] = vect_line[4*j1  ];
                    vect_line[4*j2+1] = vect_line[4*j1+1];
                    vect_line[4*j2+2] = vect_line[4*j1+2];
                };
                continue;
            }
            if(ineib2 < 0) continue;
            if(iedge > ineib2){
                k2 = wk_iso_mesh->inum_line[2*ineib2  ];
                def =  vect_line[4*j1  ] * vect_line[4*k2  ]
                     + vect_line[4*j1+1] * vect_line[4*k2+1]
                     + vect_line[4*j1+2] * vect_line[4*k2+2];
                if(def < 0.0){
                    vect_line[4*j1  ] = -vect_line[4*j1  ];
                    vect_line[4*j1+1] = -vect_line[4*j1+1];
                    vect_line[4*j1+2] = -vect_line[4*j1+2];
                }
                j2 = wk_iso_mesh->inum_line[2*iedge+1];
                if(j2 > 0){
                    vect_line[4*j2  ] = vect_line[4*j1  ];
                    vect_line[4*j2+1] = vect_line[4*j1+1];
                    vect_line[4*j2+2] = vect_line[4*j1+2];
                };
            }
        }
    return;
}





long adjust_neighboring_direction(long icou, long j_ref, long j1_line, long j2_line,
                                  int *iflag_checked, double *vect_line){
    double def =  vect_line[4*j1_line  ] * vect_line[4*j_ref  ]
                 + vect_line[4*j1_line+1] * vect_line[4*j_ref+1]
                 + vect_line[4*j1_line+2] * vect_line[4*j_ref+2];
    if(def < 0.0){
        vect_line[4*j1_line  ] = -vect_line[4*j1_line  ];
        vect_line[4*j1_line+1] = -vect_line[4*j1_line+1];
        vect_line[4*j1_line+2] = -vect_line[4*j1_line+2];
    }
    iflag_checked[j1_line] = 1;
    icou = icou + 1;
    
    if(j2_line >= 0){
        vect_line[4*j2_line  ] = vect_line[4*j1_line  ];
        vect_line[4*j2_line+1] = vect_line[4*j1_line+1];
        vect_line[4*j2_line+2] = vect_line[4*j1_line+2];
        iflag_checked[j2_line] = 1;
        icou = icou + 1;
    };
    return icou;
}

