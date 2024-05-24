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


int main(int argc, char *argv[])
{
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
    
    
    
/* costruct frames */
    AVFormatContext *outputFmtContxt = NULL;
    const AVCodec *encoder = NULL;
    AVCodecContext *encoderContxt = NULL;
    int ret = 0;
    const AVOutputFormat *outFmt = av_guess_format("mp4", NULL, NULL);
    avformat_alloc_output_context2(&outputFmtContxt, outFmt, NULL, NULL);
    printf("Frame generated\n");
    
/* encoder settings */
    int pts_inc = 90000 / FrameRate;
    enum AVPixelFormat pix_fmt = AV_PIX_FMT_YUV420P;
    AVRational fps = av_make_q(FrameRate, 1);
    AVStream *out_stream = avformat_new_stream(outputFmtContxt, NULL);
    encoder = avcodec_find_encoder(AV_CODEC_ID_H264);
    encoderContxt = avcodec_alloc_context3(encoder);
    encoderContxt->width = width;
    encoderContxt->height = height;
    encoderContxt->pix_fmt = pix_fmt;
    encoderContxt->gop_size = 250;
    encoderContxt->keyint_min = 25;
    encoderContxt->qmax = 10;
    encoderContxt->bit_rate = 3000000;
    encoderContxt->framerate = fps;
    encoderContxt->time_base = av_make_q(1, pts_inc * FrameRate);
    
    av_opt_set(encoderContxt->priv_data, "tune", "zerolatency", 0);
    avcodec_open2(encoderContxt, encoder, NULL);
    avcodec_parameters_from_context(out_stream->codecpar, encoderContxt);
    printf("Encoder generated %d %d\n", fps.num, fps.den);
    printf("encoderContxt->framerate %d %d\n", encoderContxt->framerate.num, encoderContxt->framerate.den);
    printf("encoderContxt->time_base %d %d\n", encoderContxt->time_base.num, encoderContxt->time_base.den);
    
/*   prepare output file  */
    av_dump_format(outputFmtContxt, 0, output, 1);
    avio_open(&outputFmtContxt->pb, output, AVIO_FLAG_WRITE);
    ret = avformat_write_header(outputFmtContxt, NULL);
    printf("Prepare output\n");
    
/*    Context to convert from RGB to YUV */
    struct SwsContext *rgb2yuv = sws_getContext(width, height, AV_PIX_FMT_RGB24,
                                                width, height, pix_fmt, SWS_BICUBIC,
                                                NULL, NULL, NULL);
/*   prepare packet and frame  */
    AVPacket *packet = av_packet_alloc();
    packet->data = NULL;
    packet->size = 0;
    
    AVFrame *rgbframe = av_frame_alloc();
    rgbframe->width = width;
    rgbframe->height = height;
    rgbframe->format = AV_PIX_FMT_RGB24;
//    rgbframe->format = AV_PIX_FMT_RGBA 
    rgbframe->duration = 1;        /* estimated duration of the frame */
    ret = av_frame_get_buffer(rgbframe, 0);
    
    AVFrame *outframe = av_frame_alloc();
    outframe->width = width;
    outframe->height = height;
    outframe->format = pix_fmt;
    outframe->duration = pts_inc;
    ret = av_frame_get_buffer(outframe, 0);
    uint8_t *outbuf = (uint8_t*) av_malloc(av_image_get_buffer_size(pix_fmt, width, height, 1));
    ret = av_image_fill_arrays(outframe->data, outframe->linesize, 
                               outbuf, pix_fmt, width, height, 1);
    int j;
    printf("file: ");
    int64_t pts = 0;
        outframe->pict_type = AV_PICTURE_TYPE_NONE;
//        outframe->key_frame = 1;
    for(int i=ist;i<ied;i++){
        if((i-ist) % inc != 0) continue;
        
        sprintf(fname,"%s.%d", argv[1], i);
        printf("%s", fname);
        fflush(stdout);
        
        read_png_file_c(fname, &width, &height, &iflag_rgba);
        copy_rgb_from_png_c(width, height, iflag_rgba, cimage);
        flip_gl_bitmap(width, height, cimage, fliped_img);
        
        outframe->pts = pts;
        pts = pts + (uint64_t) pts_inc;
        ret = av_frame_make_writable(outframe);
        if (ret < 0){
            printf("frame unwritable\n");
            break;
        }
        
    /* convert image */
        ret = av_image_fill_arrays(rgbframe->data, rgbframe->linesize, fliped_img, 
                                   AV_PIX_FMT_RGB24, width, height, 1);
        sws_scale(rgb2yuv, (const unsigned char * const*) rgbframe->data,
                  rgbframe->linesize, 0, height,
                  outframe->data, outframe->linesize);
        
    /* send the frame to the encoder */
        ret = avcodec_send_frame(encoderContxt, outframe);
        if (ret < 0){break;}
        while (ret >= 0){
            ret = avcodec_receive_packet(encoderContxt, packet);
            if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF || ret < 0){break;};
            packet->pts = outframe->pts;
            packet->dts = packet->dts;
            packet->duration = pts_inc;
            ret = av_interleaved_write_frame(outputFmtContxt, packet);
        }
        av_packet_unref(packet);
        
        for(j=0;j<strlen(fname);j++)printf("\b");
        fflush(stdout);
    }
    free(fliped_img);
    free(cimage);
    /* release memories */
    printf("###  av_write_trailer ###\n");
    av_write_trailer(outputFmtContxt);
    printf("###  packet ###\n");
    av_packet_free(&packet);
    printf("###  rgbframe ###\n");
    av_frame_free(&rgbframe);
    printf("###  outframe ###\n");
    av_frame_free(&outframe);
    printf("###  encoderContxt ###\n");
    avcodec_free_context(&encoderContxt);
    printf("###  outputFmtContxt ###\n");
    avformat_free_context(outputFmtContxt);
    printf("###  outbuf ###\n");
    av_freep(&outbuf);
    printf("###  sws_freeContext ###\n");
    sws_freeContext(rgb2yuv);
    return 0;
}

