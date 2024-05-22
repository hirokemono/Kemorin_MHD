//
//  KemoviewerMovieMaker.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

@import Cocoa;
@import AVFoundation;
@import CoreVideo;

#import "KemoViewerMetalView.h"
#import "KemoViewerMetalViewController.h"
#import "KemoViewerObject.h"

#include <time.h>
#include "Kemoviewer.h"


@interface KemoviewerMovieMaker : NSObject {
    IBOutlet NSWindow*  window;
    IBOutlet KemoViewerMetalView * _metalView;
    IBOutlet KemoViewerMetalViewController * _metalViewController;
    IBOutlet KemoViewerObject *_kmv;

    IBOutlet NSUserDefaultsController* _movie_defaults_controller;

	NSInteger MovieFormatFlag;
	NSInteger CurrentMovieFormat;
	IBOutlet id movieFormat_item;

    AVAssetWriter *videoWriter;
    AVAssetWriterInput *writerInput;
    AVAssetWriterInputPixelBufferAdaptor *adaptor;
    NSImage *SnapshotImage;
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
	IBOutlet NSProgressIndicator *rotateProgreessBar;
	IBOutlet NSProgressIndicator *evolutionProgreessBar;
	
	NSInteger RotationAxisID;
	NSInteger RotationIncrement;
	IBOutlet id rotationAxis;
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

-(id) init;
-(CVPixelBufferRef)pixelBufferFromCGImage:(CGImageRef)image;

-(void) InitEvolutionStepByPSF:(struct kemoviewer_type *) kemo_sgl;
-(void) InitEvolutionStepByFline:(struct kemoviewer_type *) kemo_sgl;

-(void) SaveKemoviewPNGFile:(NSString*)ImageFilehead;
-(void) SaveKemoviewBMPFile:(NSString*)ImageFilehead;
-(void) SaveKemoviewPDFFile:(NSString*)ImageFilehead;

-(void) SaveKemoviewQuiltPNGFile:(NSString*)ImageFilehead
                          degree:(NSInteger) int_degree
                            axis:(NSInteger)rotationaxis
                        kemoview:(struct kemoviewer_type *) kemo_sgl;
-(void) SaveKemoviewQuiltBMPFile:(NSString*)ImageFilehead
                          degree:(NSInteger)int_degree
                            axis:(NSInteger)rotationaxis
                        kemoview:(struct kemoviewer_type *) kemo_sgl;
-(void) SaveKemoviewQuiltPDFFile:(NSString*)ImageFilehead
                          degree:(NSInteger) int_degree
                            axis:(NSInteger) rotationaxis
                        kemoview:(struct kemoviewer_type *) kemo_sgl;

- (IBAction)SendToClipAsTIFF:(id)sender;

- (NSInteger) SetImageFileFormatID:(NSString *)FileExtension;

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
