/*
// m_kemoview_fline_menu.c
*/

#include "m_kemoview_fline_menu.h"


void alloc_draw_fline_flags(struct psf_data *fline_s, struct fline_menu_val *fline_m){
	int i;
	fline_m->cmap_fline_comp
			=  (struct colormap_params **) malloc(fline_s->ncomptot*sizeof(struct colormap_params *));
	if( fline_m->cmap_fline_comp == NULL ) {
		printf( "cmap_fline_comp cannot alloc!\n" );
		exit( 1 );
	}
	for (i=0;i<fline_s->ncomptot;i++){
		fline_m->cmap_fline_comp[i] = (struct colormap_params *) malloc( sizeof(struct colormap_params));
		if(fline_m->cmap_fline_comp[i] == NULL) {
			printf( "fline_m->cmap_fline_comp[i] cannot alloc!\n" );
			exit( 1 );
		}
		alloc_color_index_list_s(fline_m->cmap_fline_comp[i], RAINBOW_MODE);
	};
	
	fline_m->cmap_fline_fld
			=  (struct colormap_params **) malloc(fline_s->nfield*sizeof(struct colormap_params *));
	if( fline_m->cmap_fline_fld == NULL ) {
		printf( "cmap_fline_fld cannot alloc!\n" );
		exit( 1 );
	}
	for (i=0;i<fline_s->nfield;i++) {
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

void dealloc_draw_fline_flags(struct psf_data *fline_s, struct fline_menu_val *fline_m){
	int i;
	
	for (i=0;i<fline_s->nfield;i++){dealloc_color_index_list_s(fline_m->cmap_fline_fld[i]);};
	free(fline_m->cmap_fline_fld);
	
	for (i=0;i<fline_s->ncomptot;i++){dealloc_color_index_list_s(fline_m->cmap_fline_comp[i]);};
	free(fline_m->cmap_fline_comp);
	
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

void set_fline_color_field(int selected, struct psf_data *fline_s,
			struct fline_menu_val *fline_m){
	fline_m->if_draw_fline = (long) selected;
	fline_m->ic_draw_fline = IZERO;
	fline_m->icomp_draw_fline = fline_s->istack_comp[fline_m->if_draw_fline];
	fline_m->cmap_fline = fline_m->cmap_fline_comp[fline_m->icomp_draw_fline];
	printf("selected 1st component of %s, %ld \n", 
			fline_s->data_name[fline_m->if_draw_fline], fline_m->if_draw_fline);
	return;
}

void set_fline_color_component(int selected, struct psf_data *fline_s,
			struct fline_menu_val *fline_m){
	fline_m->ic_draw_fline = (long) selected;
	fline_m->icomp_draw_fline = fline_s->istack_comp[fline_m->if_draw_fline] + fline_m->ic_draw_fline;
	fline_m->cmap_fline = fline_m->cmap_fline_comp[fline_m->icomp_draw_fline];
	printf("selected %d  of %s, %ld \n", (selected+1), 
			fline_s->data_name[fline_m->if_draw_fline], fline_m->icomp_draw_fline);
	return;
}


