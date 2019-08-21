
/* m_kemoview_psf.c */

#include "m_kemoview_psf.h"

struct kemoview_psf * init_kemoview_psf(){
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

void close_PSF_view(struct kemoview_psf *kemo_psf, 
			struct psf_data *psf_current_data, struct psf_menu_val*psf_current_menu){
	dealloc_draw_psf_flags(kemo_psf->psf_d[kemo_psf->psf_a->id_current],
                           kemo_psf->psf_m[kemo_psf->psf_a->id_current]);
	deallc_all_psf_data(kemo_psf->psf_d[kemo_psf->psf_a->id_current]);
	
	set_close_current_kemoview_array(kemo_psf->psf_a);
    
	psf_current_data = kemo_psf->psf_d[kemo_psf->psf_a->id_current];
	psf_current_menu = kemo_psf->psf_m[kemo_psf->psf_a->id_current];
    
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
			evolution_PSF_data(kemo_psf->psf_d[id_load], psf_ucd_tmp, kemo_psf->psf_m[id_load]);
		};
	}
	
	return;
}

