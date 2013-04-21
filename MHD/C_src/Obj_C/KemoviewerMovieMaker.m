//
//  KemoviewerMovieMaker.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "KemoviewerMovieMaker.h"
#include "kemoviewer.h"

static unsigned char *glimage;
NSImage *SnapshotImage;
NSBitmapImageRep *bmpRep;

@implementation KemoviewerMovieMaker
@synthesize MovieFormatFlag;
@synthesize FramePerSecond;
@synthesize RotationAxisID;
@synthesize RotationIncrement;
@synthesize EvolutionStartStep;
@synthesize EvolutionEndStep;
@synthesize EvolutionIncrement;
-(id) init
{
	self.FramePerSecond = 12;
	self.RotationIncrement =  2;
	self.RotationAxisID = 3;
	self.EvolutionStartStep = 1;
	self.EvolutionEndStep =   1;
	self.EvolutionIncrement = 1;
	
	return self;
}

-(void) InitEvolutionStepByPSF;
{
	char image_head[LENGTHBUF];
	int iflag;
	int  istep = send_current_psf_full_path_header(image_head, &iflag);
	self.EvolutionStartStep = istep;
	self.EvolutionEndStep =   istep;
};

-(void) InitEvolutionStepByFline;
{
	char image_head[LENGTHBUF];
	int  istep = send_fline_file_header(image_head);
	self.EvolutionStartStep = istep;
	self.EvolutionEndStep =   istep;
}

-(void) OpenKemoviewMovieFile:(NSString *)movieFileName{
	NSError *overWriteflag = [[NSError alloc] init];

	// Create a QTMovie with a writable data reference
	NSLog(@"movieFileName: %@", movieFileName);
	KemoMovie = [[QTMovie alloc] initToWritableFile:movieFileName error:&overWriteflag];

	if(overWriteflag!= NULL ){
		NSFileManager *fman = [NSFileManager defaultManager];
        NSURL *RotateImageFileURLNoStep = [[NSURL alloc] initFileURLWithPath:RotateImageFilenameNoStep];
		[fman removeItemAtURL:RotateImageFileURLNoStep error:nil];
		KemoMovie = [[QTMovie alloc] initToWritableFile:RotateImageFilenameNoStep error:NULL];
	}
	// mark the movie as editable
	[KemoMovie setAttribute:[NSNumber numberWithBool:YES] forKey:QTMovieFlatten];
	// keep it around until we are done with it...
	[overWriteflag release];
	
	duration = QTMakeTime(IONE, self.FramePerSecond);
}

-(void) CloseKemoviewMovieFile{
	[SnapshotImage release];
	[KemoMovie release];
    [bmpRep release];
	free(glimage);
}

// ---------------------------------

- (void) allocateBitmapArray
{
    GLint XViewsize = [_kemoviewer KemoviewHorizontalViewSize];
    GLint YViewsize = [_kemoviewer KemoviewVerticalViewSize];

	glimage = (unsigned char*)calloc(3*XViewsize*XViewsize, sizeof(unsigned char));
	SnapshotImage = [[NSImage alloc] init];

    bmpRep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes:nil
                                                     pixelsWide: XViewsize
                                                     pixelsHigh: YViewsize
                                                  bitsPerSample: 8
                                                samplesPerPixel: 3
                                                       hasAlpha: NO
                                                       isPlanar: NO
                                                 colorSpaceName:NSDeviceRGBColorSpace
              //	bytesPerRow: (XpixelGLWindow*3) //pixelsWide*samplesPerPixel
              // bitsPerPixel: (8*3)   //bitsPerSample*samplesPerPixel
                                                    bytesPerRow: (XViewsize*3) //pixelsWide*samplesPerPixel
                                                   bitsPerPixel: 0  //bitsPerSample*samplesPerPixel
              ]; 
}

-(void) AddKemoviewImageToMovie
{
	// when adding images we must provide a dictionary
	// specifying the codec attributes
	NSDictionary *movieDict = nil;
	movieDict = [NSDictionary dictionaryWithObjectsAndKeys:@"mp4v",
				 QTAddImageCodecType,
				 [NSNumber numberWithLong:codecMaxQuality],
				 QTAddImageCodecQuality,
				 nil];

	// Adds an image for the specified duration to the QTMovie
    get_kemoviewer_fliped_img((int) [_kemoviewer KemoviewHorizontalViewSize],
                              (int) [_kemoviewer KemoviewVerticalViewSize],
                              glimage, [bmpRep bitmapData]);
    
	[SnapshotImage addRepresentation:bmpRep];

	[KemoMovie addImage:SnapshotImage forDuration:duration withAttributes:movieDict];
	[KemoMovie updateMovieFile];
}


// ---------------------------------

-(void) SaveQTmovieRotation{
	NSInteger istep;
	NSInteger ied_deg = 360/self.RotationIncrement;
	NSInteger int_degree;

	if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [self allocateBitmapArray];
        [self OpenKemoviewMovieFile:RotateImageFilenameNoStep];
    }

	[rotateProgreessBar setUsesThreadedAnimation:YES];
    
	for(istep = 0;istep<ied_deg;istep++){
		int_degree = (istep*self.RotationIncrement);
		[rotateProgreessBar incrementBy:(double) self.RotationIncrement];
		[rotateProgreessBar displayIfNeeded];
		
		[_kemoviewer DrawRotation:int_degree:RotationAxisID];

		if (CurrentMovieFormat == SAVE_QT_MOVIE) {
			[self AddKemoviewImageToMovie];
		} else if (CurrentMovieFormat != 0) {
            write_kemoviewer_window_step_file((int) CurrentMovieFormat, (int) istep,
                                              [RotateImageFilehead UTF8String]);
		}
	}
	[rotateProgreessBar setDoubleValue:(double) 0];
	[rotateProgreessBar displayIfNeeded];
	[rotateProgreessBar stopAnimation:self];
	
	if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [self CloseKemoviewMovieFile];
    }
}

-(void) SaveQTmovieEvolution{
	int istep;
	
	if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [self allocateBitmapArray];
        [self OpenKemoviewMovieFile:EvolutionImageFilename];
    }
	
	[evolutionProgreessBar setUsesThreadedAnimation:YES];
	[evolutionProgreessBar setMinValue:(double) self.EvolutionStartStep];
	[evolutionProgreessBar setMaxValue:(double) self.EvolutionEndStep];
	for(istep = self.EvolutionStartStep;istep<self.EvolutionEndStep+1;istep++){
		if( ((istep-self.EvolutionStartStep)%self.EvolutionIncrement) == 0) {
			[_kemoviewer DrawEvolution:istep];

			if (CurrentMovieFormat == SAVE_QT_MOVIE) {
				[self AddKemoviewImageToMovie];
			} else if (CurrentMovieFormat != 0) {
                write_kemoviewer_window_step_file((int) CurrentMovieFormat, (int) istep,
                                                  [EvolutionImageFilehead UTF8String]);
			}

			[evolutionProgreessBar incrementBy:(double) self.EvolutionIncrement];
			[evolutionProgreessBar displayIfNeeded];
		}
	}
    
	if(CurrentMovieFormat == SAVE_QT_MOVIE) [bmpRep release];
	[evolutionProgreessBar setDoubleValue:(double) self.EvolutionStartStep];
	[evolutionProgreessBar displayIfNeeded];
	[evolutionProgreessBar stopAnimation:self];
	
	if (CurrentMovieFormat == SAVE_QT_MOVIE) [self CloseKemoviewMovieFile];
}


// ---------------------------------

- (IBAction)SendToClipAsPDF:(id)sender
{
    [self allocateBitmapArray];
    get_kemoviewer_fliped_img((int) [_kemoviewer KemoviewHorizontalViewSize],
                              (int) [_kemoviewer KemoviewVerticalViewSize],
                              glimage, [bmpRep bitmapData]);
	[SnapshotImage addRepresentation:bmpRep];
    
    
    NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
	[pasteboard declareTypes:[NSArray arrayWithObjects:NSPasteboardTypeTIFF, nil] owner:nil];
	[pasteboard setData:[SnapshotImage TIFFRepresentation] forType:NSTIFFPboardType];
    
	free(glimage);
    [bmpRep release];
	[SnapshotImage release];
}

// ---------------------------------

- (IBAction)ShowRotationMovie:(id)sender;
{
	CurrentMovieFormat = 0;
	[self SaveQTmovieRotation];
}
- (IBAction)ShowEvolutionMovie:(id)sender;
{
	CurrentMovieFormat = 0;
	[self SaveQTmovieEvolution];
}

- (IBAction)SaveRotationMovie:(id)sender;
{
	NSUserDefaults* defaults = [_movie_defaults_controller defaults];
	CurrentMovieFormat = [[defaults stringForKey:@"MovieFormatID"] intValue];

	NSSavePanel *RotateImageSavePanelObj = [NSSavePanel savePanel];
	int RotateImageSaveInt = [RotateImageSavePanelObj runModal];
	if(RotateImageSaveInt == NSOKButton){
		NSString * RotateImageFilename = [[ RotateImageSavePanelObj URL] path];
		NSString * RotateImageFileext =   [RotateImageFilename pathExtension];
		RotateImageFilehead = [RotateImageFilename stringByDeletingPathExtension];

		if ([RotateImageFileext isEqualToString:@"mov"] 
			|| [RotateImageFileext isEqualToString:@"MOV"]
			|| [RotateImageFileext isEqualToString:@"moov"]
			|| [RotateImageFileext isEqualToString:@"MOOV"]) {
			CurrentMovieFormat = SAVE_QT_MOVIE;
			RotateImageFilenameNoStep = [RotateImageFilehead stringByAppendingPathExtension:@"mov"];
		} else if ([RotateImageFileext isEqualToString:@"png"] 
		   || [RotateImageFileext isEqualToString:@"PNG"]) {
			CurrentMovieFormat = SAVE_PNG;
		} else if ([RotateImageFileext isEqualToString:@"bmp"] 
		   || [RotateImageFileext isEqualToString:@"BMP"]) {
			CurrentMovieFormat = SAVE_BMP;
		} else if ([RotateImageFileext isEqualToString:@"eps"] 
		   || [RotateImageFileext isEqualToString:@"EPS"]) {
			CurrentMovieFormat = SAVE_EPS;
		} else if ([RotateImageFileext isEqualToString:@"pdf"] 
		   || [RotateImageFileext isEqualToString:@"PDF"]) {
			CurrentMovieFormat = SAVE_PDF;
		} else if ([RotateImageFileext isEqualToString:@"ps"] 
		   || [RotateImageFileext isEqualToString:@"PS"]) {
			CurrentMovieFormat = SAVE_PS;
		} else {
			CurrentMovieFormat = [[defaults stringForKey:@"MovieFormatID"] intValue];
			RotateImageFilenameNoStep = [RotateImageFilehead stringByAppendingPathExtension:@"mov"];
		}
		
		[self SaveQTmovieRotation];
	};
}

- (IBAction)SaveEvolutionMovie:(id)sender;
{
	NSUserDefaults* defaults = [_movie_defaults_controller defaults];
	CurrentMovieFormat = [[defaults stringForKey:@"MovieFormatID"] intValue];
	
	NSSavePanel *EvolutionImageSavePanelObj = [NSSavePanel savePanel];
	int EvolutionImageSaveInt = [EvolutionImageSavePanelObj runModal];
	if(EvolutionImageSaveInt == NSOKButton){
		EvolutionImageFilename = [[ EvolutionImageSavePanelObj URL] path];
		NSString * EvolutionImageFileext =   [EvolutionImageFilename pathExtension];
		EvolutionImageFilehead = [EvolutionImageFilename stringByDeletingPathExtension];
		
		if ([EvolutionImageFileext isEqualToString:@"mov"] 
			|| [EvolutionImageFileext isEqualToString:@"MOV"]
			|| [EvolutionImageFileext isEqualToString:@"moov"]
			|| [EvolutionImageFileext isEqualToString:@"MOOV"]) {
			CurrentMovieFormat = SAVE_QT_MOVIE;
			EvolutionImageFilename = [EvolutionImageFilehead stringByAppendingPathExtension:@"mov"];
		} else if ([EvolutionImageFileext isEqualToString:@"png"] 
				   || [EvolutionImageFileext isEqualToString:@"PNG"]) {
			CurrentMovieFormat = SAVE_PNG;
		} else if ([EvolutionImageFileext isEqualToString:@"bmp"] 
				   || [EvolutionImageFileext isEqualToString:@"BMP"]) {
			CurrentMovieFormat = SAVE_BMP;
		} else if ([EvolutionImageFileext isEqualToString:@"eps"] 
				   || [EvolutionImageFileext isEqualToString:@"EPS"]) {
			CurrentMovieFormat = SAVE_EPS;
		} else if ([EvolutionImageFileext isEqualToString:@"ps"] 
				   || [EvolutionImageFileext isEqualToString:@"PS"]) {
			CurrentMovieFormat = SAVE_PS;
		} else if ([EvolutionImageFileext isEqualToString:@"pdf"] 
				   || [EvolutionImageFileext isEqualToString:@"PDF"]) {
			CurrentMovieFormat = SAVE_PDF;
		} else {
			CurrentMovieFormat = [[defaults stringForKey:@"MovieFormatID"] intValue];
			EvolutionImageFilename = [EvolutionImageFilehead stringByAppendingPathExtension:@"mov"];
		}
		
		[self SaveQTmovieEvolution];
	};
}

- (IBAction)SetFramePerSecond:(id)sender;
{
	[_kemoviewer swapbuffer_cocoa];
}

- (IBAction)getMovieFormatFlag:(id)sender
{
	NSUserDefaults* defaults = [_movie_defaults_controller defaults];
	MovieFormatFlag = [[defaults stringForKey:@"MovieFormatID"] intValue];
	MovieFormatFlag = [[movieFormat_item selectedCell] tag];
	[defaults release];
}

- (IBAction)SetEvolutionSteps:(id)sender{
	[_kemoviewer swapbuffer_cocoa];
}

- (IBAction)ChooseRotateAxis:(id)sender;
{
	RotationAxisID = [[rotationAxis selectedCell] tag];
}

@end
