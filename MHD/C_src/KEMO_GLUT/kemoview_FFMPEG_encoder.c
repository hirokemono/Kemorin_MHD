/*
//  kemoview_FFMPEG_encoder.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include "kemoview_FFMPEG_encoder.h"

struct FFMPEG_encoder * init_FFMPEG_encoder(int iflag_flip, int width, int height, 
                                            int FrameRate, char *file_prefix){
    struct FFMPEG_encoder *kemo_encode
        = (struct FFMPEG_encoder *) malloc(sizeof(struct FFMPEG_encoder));
	if (kemo_encode == NULL){
		printf("malloc error for FFMPEG_encoder\n");
		exit(0);
    }
    kemo_encode->iflag_flip = iflag_flip;
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
    char *file_name = alloc_string(strlen(file_prefix)+4);
    sprintf(file_name, "%s.mp4",file_prefix);
    
    av_dump_format(kemo_encode->outputFmtContxt, 0, file_name, 1);
    avio_open(&kemo_encode->outputFmtContxt->pb, file_name, AVIO_FLAG_WRITE);
    ret = avformat_write_header(kemo_encode->outputFmtContxt, NULL);
    printf("Prepare output\n");
    
    if(kemo_encode->iflag_flip > 0){
        kemo_encode->fliped_img = kemoview_alloc_RGB_buffer_to_bmp(width, height);
    }

    kemo_encode->loop_pts = 0;
    free(file_name);
    return kemo_encode;
}

void finalize_FFMPEG_encoder(struct FFMPEG_encoder *kemo_encode){
    
    if(kemo_encode->iflag_flip > 0){free(kemo_encode->fliped_img);};
    
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

int encode_by_FFMPEG(int width, int height, 
                     unsigned char *rgb, 
                     struct FFMPEG_encoder *kemo_encode){
    int ret = 0;
    
    kemo_encode->outframe->pts = kemo_encode->loop_pts;
    kemo_encode->loop_pts = kemo_encode->loop_pts + (uint64_t) kemo_encode->pts_inc;
    ret = av_frame_make_writable(kemo_encode->outframe);
    if (ret < 0){
        printf("frame unwritable\n");
        return ret;
    }
    
/* convert image */
    if(kemo_encode->iflag_flip > 0){
        flip_gl_bitmap(width, height, rgb, kemo_encode->fliped_img);
        ret = av_image_fill_arrays(kemo_encode->rgbframe->data,
                                   kemo_encode->rgbframe->linesize,
                                   kemo_encode->fliped_img,
                                   AV_PIX_FMT_RGB24, width, height, 1);
    }else{
        ret = av_image_fill_arrays(kemo_encode->rgbframe->data,
                                   kemo_encode->rgbframe->linesize,
                                   rgb, AV_PIX_FMT_RGB24, width, height, 1);
    }
    sws_scale(kemo_encode->rgb2yuv,
              (const unsigned char * const*) kemo_encode->rgbframe->data,
              kemo_encode->rgbframe->linesize, 0, height,
              kemo_encode->outframe->data, kemo_encode->outframe->linesize);

/* send the frame to the encoder */
    ret = avcodec_send_frame(kemo_encode->encoderContxt, kemo_encode->outframe);
    if (ret < 0){return 1;}
    while (ret >= 0){
        ret = avcodec_receive_packet(kemo_encode->encoderContxt, kemo_encode->packet);
        if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF || ret < 0){break;};
        kemo_encode->packet->pts = kemo_encode->outframe->pts;
        kemo_encode->packet->dts = kemo_encode->packet->dts;
        kemo_encode->packet->duration = kemo_encode->pts_inc;
        ret = av_interleaved_write_frame(kemo_encode->outputFmtContxt, kemo_encode->packet);
    }
    av_packet_unref(kemo_encode->packet);
    return ret;
}

