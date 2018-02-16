//
//  KemoMovieControll.h
//  Kemo_Moviemaker
//
//  Created by Hiroaki Matsui on 10/10/26.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <AVFoundation/AVFoundation.h>
#import <CoreVideo/CoreVideo.h>



@interface KemoMovieControll : NSObject {

	NSInteger evolutionCurrentStep;
	NSInteger evolutionStartStep;
	NSInteger evolutionEndStep;
	NSInteger evolutionIncrement;
	NSInteger evolutionFPS;
    NSInteger imageWidth;
    NSInteger imageHight;

    AVAssetWriter *videoWriter;
    AVAssetWriterInput *writerInput;
    AVAssetWriterInputPixelBufferAdaptor *adaptor;
    NSImage *ReferenceImage;
    NSImage *SnapshotImage;
    
	IBOutlet NSProgressIndicator *progreessBar;
    IBOutlet NSButton *saveBottun;
    IBOutlet NSMenuItem *saveMenu;
    IBOutlet NSImageView *refImageView;
    
	NSString *movieFileName;
	NSString *movieFileHead;
	NSString *movieFileExt;
	
	NSString *imageFileName;
	NSString *imageFileHead;
	NSString *imageFileExt;
	NSString *imageFileHeadExStep;
	
}
@property NSInteger evolutionCurrentStep;
@property NSInteger evolutionStartStep;
@property NSInteger evolutionEndStep;
@property NSInteger evolutionIncrement;
@property NSInteger evolutionFPS;
@property NSInteger imageWidth;
@property NSInteger imageHight;

-(void) OpenReferenceImageFile:(NSString *)iFileName;
-(void) OpenKemoviewMovieFile:(NSString *)mFileName;
-(void) CloseKemoviewMovieFile;

-(IBAction) OpenReferenceImage:(id)pSender;
-(IBAction) SaveImageEvolution:(id)pSender;
-(IBAction) SetEvolutionSteps:(id)pSender;

@end
