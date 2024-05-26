//
//  KemoviewerImageMaker.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#ifndef KemoviewerImageMaker_
#define KemoviewerImageMaker_

@import Cocoa;
@import AVFoundation;
@import CoreVideo;

#import "KemoViewerMetalView.h"
#import "KemoViewerMetalViewController.h"
#import "KemoViewerObject.h"

#include <time.h>
#include "Kemoviewer.h"


@interface KemoviewerImageMaker : NSObject {
    IBOutlet KemoViewerMetalView * _metalView;
    IBOutlet KemoViewerMetalViewController * _metalViewController;
    IBOutlet KemoViewerObject *_kmv;
}

-(void) SendImageToClipboardAsTIFF;

-(void) SalectSaveKemoviewImageFile:(NSInteger) id_format
                         filePrefix:(NSString*) ImageFilehead;

- (NSImage *) InitMetalQuiltBitmapToImage:(NSInteger) int_degree
                                     axis:(NSInteger) rotationaxis
                                 kemoview:(struct kemoviewer_type *) kemo_sgl;
-(void) SalectSaveKemoQuiltImageFile:(NSInteger) id_format
                          filePrefix:(NSString*) ImageFilehead
                              degree:(NSInteger) int_degree
                                axis:(NSInteger) rotationaxis
                            kemoview:(struct kemoviewer_type *) kemo_sgl;
@end

#endif  /*  KemoviewerImageMaker_  */
