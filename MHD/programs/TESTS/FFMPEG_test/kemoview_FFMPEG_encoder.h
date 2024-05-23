/*
//  kemoview_FFMPEG_encoder.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#ifndef M_KEMOVIEW_OBJECT_BUFFERS_H_
#define M_KEMOVIEW_OBJECT_BUFFERS_H_

#include <stdio.h>

#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libavutil/imgutils.h>
#include <libavutil/opt.h>
#include <libswscale/swscale.h>
#include <libswresample/swresample.h>

struct FFMPEG_encoder{
    int pts_inc;
    int64_t loop_pts;
    enum AVPixelFormat pix_fmt;
    
    AVFormatContext *outputFmtContxt;
    const AVCodec *encoder;
    AVCodecContext *encoderContxt;
    struct SwsContext *rgb2yuv;
    
    AVPacket *packet;
    AVFrame *rgbframe;
    AVFrame *outframe;
    uint8_t *outbuf;
};

/*  prototypes  */

struct FFMPEG_encoder * init_FFMPEG_encoder(int width, int height, 
                                            int FrameRate, char *file_name);
void finalize_FFMPEG_encoder(struct FFMPEG_encoder *kemo_encode);

int encode_by_FFMPEG(int width, int height, 
                     unsigned const char *rgb, 
                     struct FFMPEG_encoder *kemo_encode);

#endif   /*  M_KEMOVIEW_OBJECT_BUFFERS_H_  */

