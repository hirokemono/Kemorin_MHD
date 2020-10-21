//
//  KemoviewerMovieMaker.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <AVFoundation/AVFoundation.h>
#import <CoreVideo/CoreVideo.h>
#import "KemoViewerOpenGLView.h"


@interface KemoviewerMovieMaker : NSObject {
    IBOutlet NSWindow*  window;
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;
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

-(id) init;
-(CVPixelBufferRef)pixelBufferFromCGImage:(CGImageRef)image;

-(void) InitEvolutionStepByPSF;
-(void) InitEvolutionStepByFline;
-(void) SaveKemoviewPNGFile:(NSString*)ImageFilehead;
-(void) SaveKemoviewBMPFile:(NSString*)ImageFilehead;
-(void) SaveKemoviewPDFFile:(NSString*)ImageFilehead;

- (IBAction)SendToClipAsPDF:(id)sender;

- (NSInteger) SetImageFileFormatID:(NSString *)FileExtension;

- (IBAction)ShowRotationMovie:(id)sender;
- (IBAction)ShowEvolutionMovie:(id)sender;
- (IBAction)SaveRotationMovie:(id)sender;
- (IBAction)SaveEvolutionMovie:(id)sender;

- (IBAction)SetFramePerSecond:(id)sender;
- (IBAction)getMovieFormatFlag:(id)sender;
- (IBAction)SetEvolutionSteps:(id)sender;
- (IBAction)ChooseRotateAxis:(id)sender;

@end
