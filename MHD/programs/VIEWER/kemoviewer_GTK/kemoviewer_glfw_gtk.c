
/* kemoviewer_glfw_gtk.c */

#include "kemo_mesh_viewer_glfw_gtk.h"

int main(int argc, char *argv[]){
	int iflag_streo_shutter = SHUTTER_OFF;
	int i;
	int ierr;
	
	/*	printf("Number of arguments %d\n", argc);*/
	for (i = 0; i < argc; i++) {
/*		printf("%dth arguments: %s\n", i, argv[i]);*/
		if(strcmp(argv[i],"-help") == 0){
			printf("-stereo_shutter: Use streo monitor with shutter\n");
			return 0;
		}
	}
		
	for (i = 0; i < argc; i++) {
		if(strcmp(argv[i],"-stereo_shutter") == 0){
			printf("shutter ON\n");
			iflag_streo_shutter = SHUTTER_ON;
		}
	}
	
	ierr = draw_mesh_kemo(iflag_streo_shutter, IZERO);
	return ierr;
};


