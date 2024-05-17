/*
// m_kemoview_fline_menu.c
*/

#include "m_kemoview_fline_menu.h"


struct fline_menu_val * init_fline_menu_val(void){
    struct fline_menu_val *fline_m
            = (struct fline_menu_val *) malloc(sizeof(struct fline_menu_val));
    if(fline_m == NULL){
        printf("malloc error for fline_menu_val\n");
        exit(0);
    }
    
    fline_m->fieldline_ncorner = ISIX;
    return fline_m;
};

void alloc_draw_fline_flags(struct fline_data *fline_d, struct fline_menu_val *fline_m){
	int i;
	fline_m->cmap_fline_comp
			=  (struct colormap_params **) malloc(fline_d->ncomptot*sizeof(struct colormap_params *));
	if( fline_m->cmap_fline_comp == NULL ) {
		printf( "cmap_fline_comp cannot alloc!\n" );
		exit( 1 );
	}
	for (i=0;i<fline_d->ncomptot;i++){
		fline_m->cmap_fline_comp[i] = (struct colormap_params *) malloc( sizeof(struct colormap_params));
		if(fline_m->cmap_fline_comp[i] == NULL) {
			printf( "fline_m->cmap_fline_comp[i] cannot alloc!\n" );
			exit( 1 );
		}
		alloc_color_index_list_s(fline_m->cmap_fline_comp[i], RAINBOW_MODE);
	};
	
	fline_m->cmap_fline_fld
			=  (struct colormap_params **) malloc(fline_d->nfield*sizeof(struct colormap_params *));
	if( fline_m->cmap_fline_fld == NULL ) {
		printf( "cmap_fline_fld cannot alloc!\n" );
		exit( 1 );
	}
	for (i=0;i<fline_d->nfield;i++) {
		fline_m->cmap_fline_fld[i] = (struct colormap_params *) malloc( sizeof(struct colormap_params));
		if(fline_m->cmap_fline_fld[i] == NULL) {
			printf( "fline_m->cmap_fline_fld[i] cannot alloc!\n" );
			exit( 1 );
		}
		alloc_color_index_list_s(fline_m->cmap_fline_fld[i], RAINBOW_MODE);
	}
	
	fline_m->cmap_fline = fline_m->cmap_fline_comp[0];
	return;
}

void dealloc_draw_fline_flags(struct fline_data *fline_d, struct fline_menu_val *fline_m){
	int i;
	
	for (i=0;i<fline_d->nfield;i++){dealloc_color_index_list_s(fline_m->cmap_fline_fld[i]);};
	free(fline_m->cmap_fline_fld);
	
	for (i=0;i<fline_d->ncomptot;i++){dealloc_color_index_list_s(fline_m->cmap_fline_comp[i]);};
	free(fline_m->cmap_fline_comp);
    fline_d->nfield = 0;
    fline_d->ncomptot = 0;
	dealloc_kvstring(fline_m->fline_header);
	return;
}


void init_fline_parameters(struct fline_menu_val *fline_m){
	fline_m ->iflag_draw_fline =  IZERO;
	
	fline_m->if_draw_fline = INIT_IF_DRAW_FLINE;
	fline_m->ic_draw_fline = INIT_IC_DRAW_FLINE;
	fline_m->icomp_draw_fline = INIT_IC_DRAW_FLINE;
	
	fline_m->fieldline_color = INIT_FLDLINE_COLOR;
	fline_m->fieldline_type =  INIT_FLDLINE_TYPE;
	fline_m->fieldline_thick = INIT_FLDLINE_THICK;
	return;
}

void set_fline_color_field(int selected, struct fline_data *fline_d,
                           struct fline_menu_val *fline_m){
	fline_m->if_draw_fline = (long) selected;
	fline_m->ic_draw_fline = IZERO;
	fline_m->icomp_draw_fline = fline_d->istack_comp[fline_m->if_draw_fline];
	fline_m->cmap_fline = fline_m->cmap_fline_comp[fline_m->icomp_draw_fline];
	printf("selected 1st component of %s, %ld \n", 
           fline_d->data_name[fline_m->if_draw_fline], fline_m->if_draw_fline);
	return;
}

void set_fline_color_component(int selected, struct fline_data *fline_d,
                               struct fline_menu_val *fline_m){
	fline_m->ic_draw_fline = (long) selected;
	fline_m->icomp_draw_fline = fline_d->istack_comp[fline_m->if_draw_fline] + fline_m->ic_draw_fline;
	fline_m->cmap_fline = fline_m->cmap_fline_comp[fline_m->icomp_draw_fline];
	printf("selected %d  of %s, %ld \n", (selected+1), 
           fline_d->data_name[fline_m->if_draw_fline], fline_m->icomp_draw_fline);
	return;
}


