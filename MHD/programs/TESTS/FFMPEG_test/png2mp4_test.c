/* kemoviewer_glut.c */

#include <stdio.h>

#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libavutil/imgutils.h>
#include <libavutil/opt.h>
#include <libswscale/swscale.h>
#include <libswresample/swresample.h>

#include "read_image_2_png.h"
#include "kemoview_FFMPEG_encoder.h"

void flip_gl_bitmap(int num_x, int num_y,
                    unsigned char *glimage, unsigned char *fliped_img){
    int i, j, k, l;
        
    for (j = 0; j < num_y; j++) {
        for (i = 0; i < num_x; i++) {
            k = i + (num_y-j-1)*num_x;
            l = i + j*num_x;
            fliped_img[3*l  ] = glimage[3*k];
            fliped_img[3*l+1] = glimage[3*k+1];
            fliped_img[3*l+2] = glimage[3*k+2];
        }
    }
    return;
}

/* 
 av_cold void ff_codec_close(AVCodecContext *avctx)
 {
     int i;
  
     if (!avctx)
         return;
  
     if (avcodec_is_open(avctx)) {
         AVCodecInternal *avci = avctx->internal;
  
         if (CONFIG_FRAME_THREAD_ENCODER &&
             avci->frame_thread_encoder && avctx->thread_count > 1) {
             ff_frame_thread_encoder_free(avctx);
         }
         if (HAVE_THREADS && avci->thread_ctx)
             ff_thread_free(avctx);
         if (avci->needs_close && ffcodec(avctx->codec)->close)
             ffcodec(avctx->codec)->close(avctx);
         avci->byte_buffer_size = 0;
         av_freep(&avci->byte_buffer);
         av_frame_free(&avci->buffer_frame);
         av_packet_free(&avci->buffer_pkt);
         av_packet_free(&avci->last_pkt_props);
  
         av_packet_free(&avci->in_pkt);
         av_frame_free(&avci->in_frame);
         av_frame_free(&avci->recon_frame);
  
         ff_refstruct_unref(&avci->pool);
         ff_refstruct_pool_uninit(&avci->progress_frame_pool);
  
         ff_hwaccel_uninit(avctx);
  
         av_bsf_free(&avci->bsf);
  
 #if FF_API_DROPCHANGED
         av_channel_layout_uninit(&avci->initial_ch_layout);
 #endif
  
 #if CONFIG_LCMS2
         ff_icc_context_uninit(&avci->icc);
 #endif
  
         av_freep(&avctx->internal);
     }
  
     for (i = 0; i < avctx->nb_coded_side_data; i++)
         av_freep(&avctx->coded_side_data[i].data);
     av_freep(&avctx->coded_side_data);
     avctx->nb_coded_side_data = 0;
     av_frame_side_data_free(&avctx->decoded_side_data,
                             &avctx->nb_decoded_side_data);
  
     av_buffer_unref(&avctx->hw_frames_ctx);
     av_buffer_unref(&avctx->hw_device_ctx);
  
     if (avctx->priv_data && avctx->codec && avctx->codec->priv_class)
         av_opt_free(avctx->priv_data);
     av_opt_free(avctx);
     av_freep(&avctx->priv_data);
     if (av_codec_is_encoder(avctx->codec)) {
         av_freep(&avctx->extradata);
         avctx->extradata_size = 0;
     } else if (av_codec_is_decoder(avctx->codec))
         av_freep(&avctx->subtitle_header);
  
     avctx->codec = NULL;
     avctx->active_thread_type = 0;
 }
  
*/

void avcodec_free_context_k(AVCodecContext **pavctx)
 {
     AVCodecContext *avctx = *pavctx;
  
     if (!avctx)
         return;
  
    printf("###  avctx ###\n");
    printf("###  avctx->hw_device_ctx %p\n", avctx->hw_device_ctx);
    printf("###  avctx->hw_frames_ctx %p\n", avctx->hw_frames_ctx);
    printf("###  avctx->hw_device_ctx %p\n", avctx->hw_device_ctx);
    printf("###  avctx->priv_data %p\n", avctx->priv_data);
    printf("###  avctx->extradata_size %p\n", avctx->extradata_size);
    printf("###  avctx->subtitle_header %p\n", avctx->subtitle_header);
    printf("###  avctx->codec %p\n", avctx->codec);
    printf("###  avctx->active_thread_type %p\n", avctx->active_thread_type);
        fflush(stdout);
     avcodec_close(avctx);
  
    printf("###  extradata ###\n");
        fflush(stdout);
     av_freep(&avctx->extradata);
    printf("###  subtitle_header ###\n");
        fflush(stdout);
     av_freep(&avctx->subtitle_header);
    printf("###  intra_matrix ###\n");
        fflush(stdout);
     av_freep(&avctx->intra_matrix);
    printf("###  chroma_intra_matrix ###\n");
        fflush(stdout);
     av_freep(&avctx->chroma_intra_matrix);
    printf("###  inter_matrix ###\n");
        fflush(stdout);
     av_freep(&avctx->inter_matrix);
    printf("###  rc_override ###\n");
        fflush(stdout);
     av_freep(&avctx->rc_override);
    printf("###  ch_layout ###\n");
        fflush(stdout);
     av_channel_layout_uninit(&avctx->ch_layout);
  
    printf("###  pavctx ###\n");
     av_freep(pavctx);
 }
  

int main(int argc, char *argv[])
{
    struct FFMPEG_encoder *kemo_encode;
    
	int   width, height;
	int iflag_rgba;
	unsigned char   *cimage, *fliped_img;
	int i;
	
	if(argc < 4){
		printf("Command format is \n");
		printf("png2mp4 [input file ptrfix] [start step] [end step] [increment] \n");
		return 1;
    };
    
    int FrameRate = 30;
    int inc = 1;
    int ist = atoi(argv[2]);
    int ied = atoi(argv[3]);
    if(argc > 4) inc = atoi(argv[4]);
    if(argc > 5) FrameRate = atoi(argv[5]);
    printf("start, end, increment: %d %d %d\n", ist, ied, inc);
    printf("FrameRate: %d \n", FrameRate);
    
    
/* set memory for image  */
    
    char fname[256];
    char output[256];
    sprintf(fname,"%s.0", argv[1]);
    sprintf(output,"%s.mp4", argv[1]);
    printf("file: %s\n", fname);
    read_png_file_c(fname, &width, &height, &iflag_rgba);    
    
    cimage = (unsigned char *) malloc( (3*width*height) * sizeof(unsigned char));
    if (cimage == NULL) {exit(2);}
    copy_rgb_from_png_c(width, height, iflag_rgba, cimage);
    
    fliped_img = (unsigned char *) malloc( (3*width*height) * sizeof(unsigned char));
    if (fliped_img == NULL) {exit(2);}
    flip_gl_bitmap(width, height, cimage, fliped_img);
    
    
    kemo_encode = init_FFMPEG_encoder(width, height, FrameRate, output);
    int ret = 0;
    
    int j;
    printf("file: ");
        kemo_encode->outframe->pict_type = AV_PICTURE_TYPE_NONE;
//        kemo_encode->outframe->key_frame = 1;
    for(int i=ist;i<ied;i++){
        if((i-ist) % inc != 0) continue;
        
        sprintf(fname,"%s.%d", argv[1], i);
        printf("%s", fname);
        fflush(stdout);
        
        read_png_file_c(fname, &width, &height, &iflag_rgba);
        copy_rgb_from_png_c(width, height, iflag_rgba, cimage);
        flip_gl_bitmap(width, height, cimage, fliped_img);
        
        encode_by_FFMPEG(width, height, fliped_img, kemo_encode);
        
        for(j=0;j<strlen(fname);j++)printf("\b");
        fflush(stdout);
    }
    free(fliped_img);
    free(cimage);
    /* release memories */
    finalize_FFMPEG_encoder(kemo_encode);
    return 0;
}

