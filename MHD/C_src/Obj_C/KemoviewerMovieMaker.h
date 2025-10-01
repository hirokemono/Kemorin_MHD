//
//  KemoviewerMovieMaker.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#ifndef KemoviewerMovieMaker_
#define KemoviewerMovieMaker_

@import Cocoa;
@import AVFoundation;
@import CoreVideo;

#import "KemoViewerMetalView.h"
#import "KemoViewerMetalViewController.h"
#import "KemoViewerObject.h"
#import "KemoviewerImageMaker.h"
#import "KemoviewerQuickTimeMaker.h"

#include <time.h>
#include "Kemoviewer.h"


@interface KemoviewerMovieMaker : NSObject {
        IBOutlet NSWindow*  window;
    IBOutlet KemoViewerMetalView * _metalView;
    
    IBOutlet KemoViewerMetalViewController * _metalViewController;
    IBOutlet KemoviewerImageMaker *_kemoImageMaker;
    IBOutlet KemoviewerQuickTimeMaker *_KemoviewQTMaker;
    
    IBOutlet KemoViewerObject *_kmv;

    IBOutlet NSUserDefaultsController* _movie_defaults_controller;

    NSInteger MovieFormatFlag;
    NSInteger CurrentMovieFormat;
    IBOutlet id movieFormat_item;

    NSInteger FramePerSecond;

    NSString *EvolutionImageFilehead;
    NSString *EvolutionImageFilename;
    NSInteger EvolutionStartStep;
    NSInteger EvolutionEndStep;
    NSInteger EvolutionIncrement;
    
    NSInteger CurrentStep;
    
    NSInteger NumTeetRotation;
    CGFloat   SnapshotFPS;
    CGFloat   AverageFPS;

    NSString *RotateImageFilehead;
    NSString *RotateImageFilenameNoStep;
    
    NSInteger RotationAxisID;
    NSInteger RotationIncrement;
    IBOutlet id rotationAxis;
    
    NSInteger stepDisplayFlag;
    CGFloat   stepToDisplay;
}
@property NSInteger MovieFormatFlag;
@property NSInteger FramePerSecond;
@property NSInteger RotationAxisID;
@property NSInteger RotationIncrement;
@property NSInteger EvolutionStartStep;
@property NSInteger EvolutionEndStep;
@property NSInteger EvolutionIncrement;
@property NSInteger CurrentStep;
@property NSInteger NumTeetRotation;
@property CGFloat   SnapshotFPS;
@property CGFloat   AverageFPS;
@property CGFloat   stepToDisplay;
@property NSInteger stepDisplayFlag;

-(id) init;

-(void) InitEvolutionStepByPSF:(int) id_model
                      kemoview:(struct kemoviewer_type *) kemo_sgl;

- (IBAction)SendToClipAsTIFF:(id)sender;

- (IBAction)GetRotationMovieFPS:(id)sender;

- (IBAction)ShowRotationMovie:(id)sender;
- (IBAction)ShowQuiltMovie:(id)sender;
- (IBAction)ShowEvolutionMovie:(id)sender;
- (IBAction)SaveRotationMovie:(id)sender;
- (IBAction)SaveEvolutionMovie:(id)sender;

- (IBAction)SetFramePerSecond:(id)sender;
- (IBAction)getMovieFormatFlag:(id)sender;
- (IBAction)SetEvolutionSteps:(id)sender;
- (IBAction)ChooseRotateAxis:(id)sender;

@end

#endif /*  KemoviewerMovieMaker_  */
