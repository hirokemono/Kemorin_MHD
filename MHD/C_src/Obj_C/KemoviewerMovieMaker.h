//
//  KemoviewerMovieMaker.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <QTKit/QTKit.h>
#import "KemoViewerOpenGLView.h"


@interface KemoviewerMovieMaker : NSObject {
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;
	IBOutlet NSUserDefaultsController* _movie_defaults_controller;

	NSInteger MovieFormatFlag;
	NSInteger CurrentMovieFormat;
	IBOutlet id movieFormat_item;

	QTMovie * KemoMovie;
	NSImage *SnapshotImage;
	NSInteger FramePerSecond;
	QTTime duration;
	NSDictionary *movieDict;

	NSString *EvolutionImageFilehead;
	NSString *EvolutionImageFilename;
	NSInteger EvolutionStartStep;
	NSInteger EvolutionEndStep;
	NSInteger EvolutionIncrement;
	
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

-(id) init;
-(void) InitEvolutionStepByPSF;
-(void) InitEvolutionStepByFline;

- (IBAction)ShowRotationMovie:(id)sender;
- (IBAction)ShowEvolutionMovie:(id)sender;
- (IBAction)SaveRotationMovie:(id)sender;
- (IBAction)SaveEvolutionMovie:(id)sender;

- (IBAction)SetFramePerSecond:(id)sender;
- (IBAction)getMovieFormatFlag:(id)sender;
- (IBAction)SetEvolutionSteps:(id)sender;
- (IBAction)ChooseRotateAxis:(id)sender;


@end
