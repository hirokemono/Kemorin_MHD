//
//  KemoviewerQuickTimeMaker.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#ifndef KemoviewerQuickTimeMaker_
#define KemoviewerQuickTimeMaker_

@import Cocoa;
@import AVFoundation;
@import CoreVideo;

#import "KemoViewerMetalView.h"
#import "KemoViewerMetalViewController.h"
#import "KemoViewerObject.h"
#import "KemoviewerImageMaker.h"

#include <time.h>
#include "Kemoviewer.h"


@interface KemoviewerQuickTimeMaker : NSObject {
    IBOutlet KemoviewerImageMaker *_kemoImageMaker;
    
    IBOutlet KemoViewerMetalView * _metalView;
    IBOutlet KemoViewerMetalViewController * _metalViewController;

    AVAssetWriter *videoWriter;
    AVAssetWriterInput *writerInput;
    AVAssetWriterInputPixelBufferAdaptor *adaptor;
}

-(void) CloseKemoviewMovieFile;

-(void) OpenQTMovieFile:(NSString *)movieFileName;
-(void) AddKemoviewImageToMovie:(CMTime)frameTime
                       kemoview:(struct kemoviewer_type *) kemo_sgl;

-(int) OpenQuiltQTMovieFile:(NSString *)movieFileName
                   kemoview:(struct kemoviewer_type *) kemo_sgl;
-(void) AddKemoviewQuiltToMovie:(CMTime)frameTime
                         degree:(NSInteger) int_degree
                           axis:(NSInteger) rotationaxis
                       kemoview:(struct kemoviewer_type *) kemo_sgl;

@end

#endif  /*  KemoviewerQuickTimeMaker_  */
