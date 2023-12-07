/*
// draw_colorbar_gl.c
*/

#include "draw_colorbar_gl.h"

static const float default_background[4] = { 0.9, 0.9, 0.9, 1.0 };
static const int    num_light = 2;
static const float  lightposition[4] =  { 1.5, 1.5,-10.0,0.0};
static const float  light2position[4] = {-1.5,-1.5,-10.0,0.0};
static const float  white1[4] = {0.4, 0.4, 0.4, 1.0};
static const float  white2[4] = {0.4, 0.4, 0.4, 1.0};
static const float  white3[4] = {0.2, 0.2, 0.2, 1.0};
static const float  shine[1] = {30.0};

void set_bg_color_kemoview(float bg_color[4], float text_color[4]){
    int i;
    
    for(i=0;i<3;i++){
        if(bg_color[i] < 0.5){
            text_color[i] = 0.9;
        }else{
            text_color[i] = 0.1;
        };
    }
    text_color[3] = ONE;
    return;
};
void init_bg_color_kemoview(float bg_color[4], float text_color[4]){
    int i;
    for(i=0;i<3;i++) {bg_color[i] = default_background[i];};
    return;
}

struct initial_cube_lighting * init_inital_cube_lighting(void){
    struct initial_cube_lighting *init_light
        = (struct initial_cube_lighting *) malloc(sizeof(struct initial_cube_lighting));
    if(init_light == NULL){
        printf("malloc error for initial_cube_lighting\n");
        exit(0);
    }
    init_light->num_light = num_light;
    for(int i=0;i<4;i++){
        init_light->lightposition[0][i] = lightposition[i];
        init_light->lightposition[1][i] = light2position[i];
        init_light->whitelight[0][i] = 0.2*white1[i];
        init_light->whitelight[1][i] = 0.8*white2[i];
        init_light->whitelight[2][i] = 2*white3[i];
    };
    init_light->shine[0] = 10*shine[0];

    return init_light;
};

int count_colorbar_box_buffer(int iflag_zero, int num_quad){
    int num_patch = 4*num_quad + 2*(iflag_zero + IFOUR);
    return (ITHREE * num_patch);
};

static void const_colorbar_box_buffer(int iflag_retina, int nx_win, int ny_win,
                                      float text_color[4], float bg_color[4],
                                      struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                      struct cbar_work *cbar_wk, struct gl_strided_buffer *cbar_buf){
    int i;
    int icomp;
    cbar_buf->num_nod_buf = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i] != 0 && psf_m[i]->draw_psf_cbar > 0) {
            icomp = psf_m[i]->icomp_draw_psf;
            set_colorbar_position(iflag_retina, (int) nx_win, (int) ny_win,
                                  psf_m[i]->cmap_psf_comp[icomp], cbar_wk);
    
            cbar_buf->num_nod_buf = count_colorbar_box_buffer(cbar_wk->iflag_zero, cbar_wk->num_quad);
            
            int inum_quad = 0;
            inum_quad = solid_colorbar_box_to_buf(inum_quad, psf_m[i]->cmap_psf_comp[icomp],
                                                  cbar_wk, cbar_buf);
            inum_quad = fade_colorbar_box_to_buf(inum_quad, psf_m[i]->cmap_psf_comp[icomp],
                                                 bg_color, cbar_wk, cbar_buf);
            inum_quad = colorbar_frame_to_buf(inum_quad, iflag_retina, text_color,
                                              cbar_wk, cbar_buf);
            break;
        };
    };
    return;
};

static void const_cbar_text_buffer(int iflag_retina,  float text_color[4],
                                   struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, struct cbar_work *cbar_wk,
                                   struct gl_strided_buffer *min_buf,  struct line_text_image *cbar_min_image,
                                   struct gl_strided_buffer *max_buf,  struct line_text_image *cbar_max_image,
                                   struct gl_strided_buffer *zero_buf, struct line_text_image *cbar_zero_image){
    int i;
    min_buf->num_nod_buf =  0;
    max_buf->num_nod_buf =  0;
    zero_buf->num_nod_buf = 0;
    for(i=0; i<psf_a->nmax_loaded; i++){
        if(psf_a->iflag_loaded[i] != 0 && psf_m[i]->draw_psf_cbar > 0){
            set_colorbar_text_image(text_color, cbar_wk->psf_min, cbar_min_image);
            set_colorbar_text_image(text_color, cbar_wk->psf_max, cbar_max_image);
            if(cbar_wk->iflag_zero == 1){
                set_colorbar_text_image(text_color, ZERO, cbar_zero_image);
            }

            min_buf->num_nod_buf =  (ITHREE*2);
            max_buf->num_nod_buf =  (ITHREE*2);
            if(cbar_wk->iflag_zero == 1) zero_buf->num_nod_buf = (ITHREE*2);
            colorbar_mbox_to_buf(iflag_retina, text_color, cbar_wk,
                                 min_buf, max_buf, zero_buf);
            break;
        };
    };
    return;
};


void const_timelabel_buffer(int iflag_retina, int nx_win, int ny_win,
                            float text_color[4], float bg_color[4],
                            struct kemo_array_control *psf_a,
                            struct line_text_image *tlabel_image,
                            struct gl_strided_buffer *time_buf){
    if((psf_a->iflag_draw_time + psf_a->iflag_draw_file_step) > 0){
        clear_line_text_image(tlabel_image);
        if(psf_a->iflag_draw_time > 0){
            sprintf(tlabel_image->texts,"    t = %5.4E", (float) psf_a->time_disp);
        }else if(psf_a->iflag_draw_file_step > 0){
            sprintf(tlabel_image->texts,"File index: %6d", psf_a->file_step_disp);
        };
        set_time_text_image(text_color, tlabel_image);
        time_mbox_to_buf(iflag_retina, text_color, (float) nx_win, (float) ny_win, time_buf);
        time_buf->num_nod_buf = TWO * THREE;
   }else{
        time_buf->num_nod_buf = 0;
    };
    return;
};

void const_colorbar_buffer(int iflag_retina, int nx_win, int ny_win,
                           float text_color[4], float bg_color[4],
                           struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                           struct gl_strided_buffer *min_buf,  struct line_text_image *cbar_min_image,
                           struct gl_strided_buffer *max_buf,  struct line_text_image *cbar_max_image,
                           struct gl_strided_buffer *zero_buf, struct line_text_image *cbar_zero_image,
                           struct gl_strided_buffer *cbar_buf){
    struct cbar_work *cbar_wk = (struct cbar_work *) malloc(sizeof(struct cbar_work));
    if(cbar_wk == NULL){
        printf("malloc error for cbar_work\n");
        exit(0);
    }
    
    const_colorbar_box_buffer(iflag_retina, nx_win, ny_win, text_color, bg_color,
                              psf_m, psf_a, cbar_wk, cbar_buf);
    const_cbar_text_buffer(iflag_retina, text_color, psf_m, psf_a, cbar_wk,
                           min_buf, cbar_min_image, max_buf, cbar_max_image,
                           zero_buf, cbar_zero_image);
    free(cbar_wk);
    return;
};

void const_message_buffer(int iflag_retina, int nx_win, int ny_win,
                          struct gl_strided_buffer *cbar_buf,
                          struct line_text_image *message_image){
    float xbar_max, ybar_min;
    if(message_image->text_opacity > 0.0){
        clear_line_text_image(message_image);
        xbar_max = message_xmax(nx_win);
        ybar_min = message_ymin(ny_win);
        set_windowsize_image(nx_win, ny_win, message_image);
        
        cbar_buf->num_nod_buf = ITWO * ITHREE;
        message_mbox_to_buf(iflag_retina, message_image->text_opacity,
                            xbar_max, ybar_min, cbar_buf);
    }else{
        cbar_buf->num_nod_buf = 0;
    }
    return;
}

void const_screen_buffer(int iflag_view_type, int nx_win, int ny_win,
                         struct gl_strided_buffer *screen_buf){
    screen_buf->num_nod_buf = ITWO * ITHREE;
    screen_mbox_to_buf(nx_win, ny_win, screen_buf);
    
    if(iflag_view_type == VIEW_STEREO) {
        screen_buf->num_nod_buf = TWO*THREE;
    }else{
        screen_buf->num_nod_buf = 0;
    };
    return;
}

