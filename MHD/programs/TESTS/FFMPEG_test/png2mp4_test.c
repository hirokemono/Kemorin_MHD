/* kemoviewer_glut.c */

#include <stdio.h>

#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libavutil/imgutils.h>
#include <libavutil/opt.h>
#include <libswscale/swscale.h>
#include <libswresample/swresample.h>

#include "read_image_2_png.h"

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
  

struct FFMPEG_encoder{
    int pts_inc;
    
    AVFormatContext *outputFmtContxt;
    const AVCodec *encoder;
    AVCodecContext *encoderContxt;
    struct SwsContext *rgb2yuv;
    enum AVPixelFormat pix_fmt;
    
    AVPacket *packet;
    AVFrame *rgbframe;
    AVFrame *outframe;
    uint8_t *outbuf;
};

struct FFMPEG_encoder * init_FFMPEG_encoder(int width, int height, int FrameRate){
    struct FFMPEG_encoder *kemo_encode
        = (struct FFMPEG_encoder *) malloc(sizeof(struct FFMPEG_encoder));
	if (kemo_encode == NULL){
		printf("malloc error for FFMPEG_encoder\n");
		exit(0);
    }
    
    kemo_encode->pix_fmt = AV_PIX_FMT_YUV420P;
    kemo_encode->pts_inc = 90000 / FrameRate;
    
    /* costruct frames */
    kemo_encode->outputFmtContxt = NULL;
    kemo_encode->encoder =         NULL;
    kemo_encode->encoderContxt =   NULL;
    
    const AVOutputFormat *MovieFmt = av_guess_format("mp4", NULL, NULL);
    avformat_alloc_output_context2(&kemo_encode->outputFmtContxt, 
                                   MovieFmt, NULL, NULL);
    printf("Frame generated\n");
    
/* encoder settings */
    AVStream *out_stream = avformat_new_stream(kemo_encode->outputFmtContxt, NULL);
    AVRational fps = av_make_q(FrameRate, 1);
    kemo_encode->encoder = avcodec_find_encoder(AV_CODEC_ID_H264);
    kemo_encode->encoderContxt = avcodec_alloc_context3(kemo_encode->encoder);
    kemo_encode->encoderContxt->width = width;
    kemo_encode->encoderContxt->height = height;
    kemo_encode->encoderContxt->pix_fmt = kemo_encode->pix_fmt;
    kemo_encode->encoderContxt->gop_size = 2000;
    kemo_encode->encoderContxt->keyint_min = 25;
    kemo_encode->encoderContxt->qmax = 10;
    kemo_encode->encoderContxt->bit_rate = 3000000;
    kemo_encode->encoderContxt->framerate = fps;
    kemo_encode->encoderContxt->time_base = av_make_q(1, 90000);
    
    av_opt_set(kemo_encode->encoderContxt->priv_data, "tune", "zerolatency", 0);
    avcodec_open2(kemo_encode->encoderContxt, kemo_encode->encoder, NULL);
    avcodec_parameters_from_context(out_stream->codecpar,
                                    kemo_encode->encoderContxt);
/*
    printf("Encoder generated %d %d\n", fps.num, fps.den);
    printf("encoderContxt->framerate %d %d\n", kemo_encode->encoderContxt->framerate.num, kemo_encode->encoderContxt->framerate.den);
    printf("encoderContxt->time_base %d %d\n", kemo_encode->encoderContxt->time_base.num, kemo_encode->encoderContxt->time_base.den);
    */
    
/*    Context to convert from RGB to YUV */
    kemo_encode->rgb2yuv = sws_getContext(width, height, AV_PIX_FMT_RGB24,
                                          width, height, kemo_encode->pix_fmt, SWS_BICUBIC,
                                          NULL, NULL, NULL);
    int ret = 0;
    
/*   prepare packet and frame  */
    kemo_encode->packet = av_packet_alloc();
    kemo_encode->packet->data = NULL;
    kemo_encode->packet->size = 0;
    
    kemo_encode->rgbframe = av_frame_alloc();
    kemo_encode->rgbframe->width = width;
    kemo_encode->rgbframe->height = height;
    kemo_encode->rgbframe->format = AV_PIX_FMT_RGB24;
//    kemo_encode->rgbframe->format = AV_PIX_FMT_RGBA 
    kemo_encode->rgbframe->duration = 1;        /* estimated duration of the frame */
    ret = av_frame_get_buffer(kemo_encode->rgbframe, 0);
    
    kemo_encode->outframe = av_frame_alloc();
    kemo_encode->outframe->width = width;
    kemo_encode->outframe->height = height;
    kemo_encode->outframe->format = kemo_encode->pix_fmt;
    kemo_encode->outframe->duration = kemo_encode->pts_inc;
    ret = av_frame_get_buffer(kemo_encode->outframe, 0);
    kemo_encode->outbuf = (uint8_t*) av_malloc(av_image_get_buffer_size(kemo_encode->pix_fmt, height, height, 1));
    ret = av_image_fill_arrays(kemo_encode->outframe->data,
                               kemo_encode->outframe->linesize, 
                               kemo_encode->outbuf, kemo_encode->pix_fmt,
                               width, height, 1);
    
/*   prepare output file  */
    av_dump_format(kemo_encode->outputFmtContxt, 0, output, 1);
    avio_open(&kemo_encode->outputFmtContxt->pb, output, AVIO_FLAG_WRITE);
    ret = avformat_write_header(kemo_encode->outputFmtContxt, NULL);
    printf("Prepare output\n");
    
    return kemo_encode;
}

void finalize_FFMPEG_encoder(struct FFMPEG_encoder *kemo_encode){
    printf("###  av_write_trailer ###\n");
    av_write_trailer(kemo_encode->outputFmtContxt);
    printf("###  kemo_encode->packet ###\n");
    av_packet_free(&kemo_encode->packet);
    printf("###  sws_freeContext ###\n");
    sws_freeContext(kemo_encode->rgb2yuv);
    printf("###  kemo_encode->rgbframe ###\n");
    av_frame_free(&kemo_encode->rgbframe);
    printf("###  kemo_encode->outframe ###\n");
         fflush(stdout);
   av_frame_free(&kemo_encode->outframe);
        fflush(stdout);
    avformat_free_context(kemo_encode->outputFmtContxt);
    printf("###  kemo_encode->outbuf ###\n");
        fflush(stdout);
    av_freep(&kemo_encode->outbuf);
    
    printf("###  encoderContxt ###\n");
        fflush(stdout);
    avcodec_free_context(&kemo_encode->encoderContxt);
    printf("###  kemo_encode->outputFmtContxt ###\n");
    free(kemo_encode);
    return;
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
    
    
    kemo_encode = init_FFMPEG_encoder(width, height, FrameRate);
    int ret = 0;
    
    int j;
    printf("file: ");
    int64_t pts = 0;
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
        
        kemo_encode->outframe->pts = pts;
        pts = pts + (uint64_t) kemo_encode->pts_inc;
        ret = av_frame_make_writable(kemo_encode->outframe);
        if (ret < 0){
            printf("frame unwritable\n");
            break;
        }
        
    /* convert image */
        ret = av_image_fill_arrays(kemo_encode->rgbframe->data,
                                   kemo_encode->rgbframe->linesize,
                                   fliped_img, 
                                   AV_PIX_FMT_RGB24, width, height, 1);
        sws_scale(kemo_encode->rgb2yuv, 
                  (const unsigned char * const*) kemo_encode->rgbframe->data,
                  kemo_encode->rgbframe->linesize, 0, height,
                  kemo_encode->outframe->data, kemo_encode->outframe->linesize);
        
    /* send the frame to the encoder */
        ret = avcodec_send_frame(kemo_encode->encoderContxt, kemo_encode->outframe);
        if (ret < 0){break;}
        while (ret >= 0){
            ret = avcodec_receive_packet(kemo_encode->encoderContxt, kemo_encode->packet);
            if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF || ret < 0){break;};
            kemo_encode->packet->pts = kemo_encode->outframe->pts;
            kemo_encode->packet->dts = kemo_encode->packet->dts;
            kemo_encode->packet->duration = kemo_encode->pts_inc;
            ret = av_interleaved_write_frame(kemo_encode->outputFmtContxt, kemo_encode->packet);
        }
        av_packet_unref(kemo_encode->packet);
        
        for(j=0;j<strlen(fname);j++)printf("\b");
        fflush(stdout);
    }
    free(fliped_img);
    free(cimage);
    /* release memories */
    finalize_FFMPEG_encoder(kemo_encode);
    return 0;
}

