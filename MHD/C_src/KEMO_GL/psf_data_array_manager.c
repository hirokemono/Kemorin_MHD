
/* psf_data_array_manager.c*/

#include "psf_data_array_manager.h"

void check_array_size(struct kemo_array_control *kemo_array){
    int *iflag_tmp;
    int i;

    if(kemo_array->nmax_loaded < kemo_array->nlimit_loaded) return;

	iflag_tmp =  (int *)calloc(kemo_array->nmax_loaded,sizeof(int));
    for (i=0;i<kemo_array->nmax_loaded; i++) iflag_tmp[i] = kemo_array->iflag_loaded[i];
    
    dealloc_kemoview_array(kemo_array);
    kemo_array->nlimit_loaded = ITWO * kemo_array->nmax_loaded;
    alloc_kemoview_array(kemo_array);
    /*    printf("New array size %d\n", kemo_array->nlimit_loaded); */
    
    for (i=0;i<kemo_array->nmax_loaded; i++) kemo_array->iflag_loaded[i] = iflag_tmp[i];
    free(iflag_tmp);
    return;
};

int add_new_kemoview_array(struct kemo_array_control *kemo_array){
	int id_load;
	kemo_array->num_loaded = kemo_array->num_loaded + 1;
	for(id_load=0; id_load<kemo_array->nmax_loaded; id_load++){
		if(kemo_array->iflag_loaded[id_load] == 0) break;
	}
	kemo_array->iflag_loaded[id_load] = 1;
	kemo_array->id_current = id_load;
	if(id_load == kemo_array->nmax_loaded) kemo_array->nmax_loaded = id_load + 1;
        
	return kemo_array->id_current;
}

void set_close_current_kemoview_array(struct kemo_array_control *kemo_array){
	int id_load;

	kemo_array->iflag_loaded[kemo_array->id_current] = 0;
	kemo_array->num_loaded = kemo_array->num_loaded - 1;
	
	for(id_load=0; id_load<kemo_array->nmax_loaded; id_load++){
		if(kemo_array->iflag_loaded[id_load] == 1) break;
	}
	kemo_array->id_current = id_load;
	return;
}

