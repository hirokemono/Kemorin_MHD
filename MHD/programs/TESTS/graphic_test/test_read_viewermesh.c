
#include <stdio.h>
#include <stdlib.h>

#include "m_surface_mesh_4_viewer_c.h"
#include "read_viewer_mesh_c.h"
#include "check_viewer_mesh_c.h"

struct viewer_mesh *tako;
struct viewer_mesh takomesh;

int main(int argc,char *argv[]){
	int i, j, k, num, jst, jed;
	tako = &takomesh;
	
	
	if(argc < 2){
		printf("set viewer mesh file header for first argument\n");
		return 1;
	}
	
	check_gzip_viewer_mesh_first(argv[1], tako);
	set_viewer_mesh(tako);
	
	check_viewer_kemo(tako);
	
	dealloc_all_mesh_4_viewer_s;
	return 0;
}
