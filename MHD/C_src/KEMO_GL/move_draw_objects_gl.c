
/* move_draw_objects_gl.c */

#include "move_draw_objects_gl.h"


void draw_objects(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
				  struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
				  struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
				  struct fline_menu_val *fline_m, struct view_element *view_s,
				  struct buffer_for_gl *gl_buf){
	int i;
	int iflag_psf = 0;
	
    /*
     glUseProgram(view_s->PhongGl2Program);
     */
    
	glDeleteLists(view_s->gl_drawID, 1);
	glNewList(view_s->gl_drawID, GL_COMPILE_AND_EXECUTE);
	
    /* Draw Solid Objects */
	
	glEnable(GL_COLOR_MATERIAL);
	if(mesh_m->iflag_draw_axis != 0){
		draw_axis(view_s, (GLfloat) mesh_m->dist_domains);
	}
	
	if(fline_m->iflag_draw_fline != 0){
		if(fline_m->fieldline_type == IFLAG_PIPE){
			draw_fieldtubes_c(fline_s, fline_m, gl_buf);
		} else {
			draw_fieldlines_c(fline_s, fline_m, gl_buf);
		};
	};
    
	for(i=0; i<psf_a->nmax_loaded; i++){
		iflag_psf = iflag_psf + psf_a->iflag_loaded[i];
		if(psf_a->iflag_loaded[i] != 0){
			if(mesh_m->iflag_view_type == VIEW_MAP) {
				draw_objects_4_map(mesh_m->shading_mode, psf_s[i], psf_m[i],
								   view_s, gl_buf, mesh_m->iflag_draw_coast,
								   mesh_m->iflag_draw_sph_grid);
			} else {
				if(psf_m[i]->cmap_psf->min_opacity >= 1.0) {
					draw_solid_patch_4_psf(mesh_m->shading_mode, 
										   psf_s[i], psf_m[i], gl_buf);
				}
				draw_solid_objects_4_psf(psf_s[i], psf_m[i], gl_buf,
                                         view_s->iflag_retina, view_s->iflag_write_ps);
			}
            
			if(psf_m[i]->draw_psf_cbar > 0) {
				draw_colorbar_gl(view_s->iflag_retina,
                                 view_s->nx_window, view_s->ny_window,
                                 mesh_m->text_color, psf_m[i]->cmap_psf);
			}
		};
	};
    
	
	if(mesh_m->iflag_draw_mesh != 0){
		draw_grids_4_domain(mesh_s, mesh_m, gl_buf);
		draw_nodes_4_domain(mesh_s, mesh_m, gl_buf);
		draw_patches_4_domain(mesh_s, mesh_m, gl_buf);
	};
    
	if(mesh_m->iflag_view_type != VIEW_MAP) {
		if(mesh_m->iflag_draw_coast != 0)   {draw_coastline(mesh_m->radius_coast, gl_buf);};
		if(mesh_m->iflag_draw_sph_grid != 0){draw_sph_flame(mesh_m->radius_coast, gl_buf,
                                                            view_s->iflag_write_ps);};
	};
	
    /* Draw Transparent Objects */
	
	for(i=0; i<psf_a->nmax_loaded; i++){
		if(psf_a->iflag_loaded[i] != 0 && mesh_m->iflag_view_type != VIEW_MAP){
			if(psf_m[i]->cmap_psf->min_opacity < 1.0){
				draw_transparent_objects_4_psf(mesh_m->shading_mode, psf_s[i], psf_m[i],
											   view_s, gl_buf);
			}
		};
	};
	
	if(mesh_m->iflag_draw_mesh != 0) draw_transparent_4_domain(mesh_s, mesh_m, view_s, gl_buf);
	
	glEnable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glDisable(GL_COLOR_MATERIAL);
	
	/* draw example cube for empty data */
	if( (mesh_m->iflag_draw_mesh+iflag_psf+fline_m->iflag_draw_fline) == 0){
		glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		drawCube_Element (0.5f);
	}
	
	glEndList();
	
	return;
}
