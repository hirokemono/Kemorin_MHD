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

-(CVPixelBufferRef)pixelBufferFromCGImage:(CGImageRef)image
{
    NSDictionary *options = @{ (NSString *)kCVPixelBufferCGImageCompatibilityKey: @YES,
                               (NSString *)kCVPixelBufferCGBitmapContextCompatibilityKey: @YES, };
    
    CVPixelBufferRef pxbuffer = NULL;
    
    CGFloat width  = CGImageGetWidth(image);
    CGFloat height = CGImageGetHeight(image);
    CVPixelBufferCreate(kCFAllocatorDefault,
                        width,
                        height,
                        kCVPixelFormatType_32ARGB,
                        (__bridge CFDictionaryRef)options,
                        &pxbuffer);
    
    CVPixelBufferLockBaseAddress(pxbuffer, 0);
    void *pxdata = CVPixelBufferGetBaseAddress(pxbuffer);
    
    size_t bitsPerComponent       = 8;
    size_t bytesPerRow            = 4 * width;
    CGColorSpaceRef rgbColorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef context = CGBitmapContextCreate(pxdata,
                                                 width,
                                                 height,
                                                 bitsPerComponent,
                                                 bytesPerRow,
                                                 rgbColorSpace,
                                                 (CGBitmapInfo)kCGImageAlphaNoneSkipFirst);
    
    CGContextDrawImage(context, CGRectMake(0, 0, width, height), image);
    CGColorSpaceRelease(rgbColorSpace);
    CGContextRelease(context);
    
    CVPixelBufferUnlockBaseAddress(pxbuffer, 0);
    
    return pxbuffer;
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

-(void) OpenQTMovieFile:(NSString *)movieFileName{
    GLint XViewsize = [_kemoviewer KemoviewHorizontalViewSize];
    GLint YViewsize = [_kemoviewer KemoviewVerticalViewSize];
    
    // Movie setting
    NSDictionary *outputSettings = 
    @{
      AVVideoCodecKey : AVVideoCodecH264,
      AVVideoWidthKey : @(XViewsize),
      AVVideoHeightKey: @(YViewsize),
      };    
    // source pixel buffer attributes
    NSDictionary *sourcePixBufferAttributes = 
    @{
      (NSString *)kCVPixelBufferPixelFormatTypeKey: @(kCVPixelFormatType_32ARGB),
      (NSString *)kCVPixelBufferWidthKey : @(XViewsize),
      (NSString *)kCVPixelBufferHeightKey: @(YViewsize),
      };
    
    NSError *overWriteflag = [[NSError alloc] init];
    // Create a QTMovie with a writable data reference
    NSLog(@"EvolutionImageFileName: %@", movieFileName);
    NSURL *url = [NSURL fileURLWithPath:movieFileName];
    
    //   Coheck if movie file is exist
    if ([[NSFileManager defaultManager] fileExistsAtPath:movieFileName])
    {
        NSLog(@"%@ is exist!!!", movieFileName);
        NSFileManager *fman = [NSFileManager defaultManager];
        [fman removeItemAtURL:url error: nil];
    }
    videoWriter = [[AVAssetWriter alloc] initWithURL:url
                                            fileType:AVFileTypeQuickTimeMovie error:&overWriteflag];
    
    NSLog(@"%@", [overWriteflag localizedDescription]);
    if(overWriteflag!= NULL ){
        NSLog(@"AVAssetWriter Failed!!");
    }
    
    // Construct Initilize writer
    writerInput = [AVAssetWriterInput assetWriterInputWithMediaType:AVMediaTypeVideo outputSettings:outputSettings];
    [videoWriter addInput:writerInput];
    writerInput.expectsMediaDataInRealTime = YES;
    
    
    // Construct writer input pixel buffer adaptor
    adaptor = [AVAssetWriterInputPixelBufferAdaptor
               assetWriterInputPixelBufferAdaptorWithAssetWriterInput:writerInput
               sourcePixelBufferAttributes:sourcePixBufferAttributes];
    
    // Start movie generation
    if (![videoWriter startWriting]) {printf("Error!");}
    [videoWriter startSessionAtSourceTime:kCMTimeZero];
}

-(void) CloseKemoviewMovieFile{
    [writerInput markAsFinished];
    [videoWriter finishWritingWithCompletionHandler:^{
        NSLog(@"Finish writing!");
    }];
    CVPixelBufferPoolRelease(adaptor.pixelBufferPool);
}

// ---------------------------------

- (NSBitmapImageRep *) allocateBitmapArray
{
    GLint XViewsize = [_kemoviewer KemoviewHorizontalViewSize];
    GLint YViewsize = [_kemoviewer KemoviewVerticalViewSize];

	glimage = (unsigned char*)calloc(3*XViewsize*XViewsize, sizeof(unsigned char));
	SnapshotImage = [[NSImage alloc] init];

    NSBitmapImageRep *bitmapRep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes:nil
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
    return bitmapRep;
}

-(void) AddKemoviewImageToMovie:(CMTime)frameTime
{
	// Adds an image for the specified duration to the QTMovie
    NSBitmapImageRep *bmpRep = [self allocateBitmapArray];
    get_kemoviewer_fliped_img((int) [_kemoviewer KemoviewHorizontalViewSize],
                              (int) [_kemoviewer KemoviewVerticalViewSize],
                              glimage, [bmpRep bitmapData]);
    
	[SnapshotImage addRepresentation:bmpRep];
    CGImageRef CGImage = [SnapshotImage CGImageForProposedRect:nil context:nil hints:nil];
    CVPixelBufferRef buffer = [self pixelBufferFromCGImage:CGImage];

    // Append Image buffer
    if (![adaptor appendPixelBuffer:buffer withPresentationTime:frameTime]) {
        NSLog(@"Adapter Failure");
    }
    
    if (buffer) {CVBufferRelease(buffer);}
    [bmpRep release];
    [SnapshotImage release];            
}


// ---------------------------------

-(void) SaveQTmovieRotation
{
	NSInteger istep;
	NSInteger ied_deg = 360/self.RotationIncrement;
	NSInteger int_degree;

	if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [self OpenQTMovieFile:RotateImageFilenameNoStep];
    }

	[rotateProgreessBar setUsesThreadedAnimation:YES];
    
	for(istep = 0;istep<ied_deg;istep++){
		int_degree = (istep*self.RotationIncrement);
		[rotateProgreessBar incrementBy:(double) self.RotationIncrement];
		[rotateProgreessBar displayIfNeeded];
		
		[_kemoviewer DrawRotation:int_degree:RotationAxisID];

		if (CurrentMovieFormat == SAVE_QT_MOVIE) {
            CMTime frameTime = CMTimeMake((int64_t)istep, self.FramePerSecond);
            [self AddKemoviewImageToMovie:frameTime];
		} else if (CurrentMovieFormat != 0) {
            write_kemoviewer_window_step_file((int) CurrentMovieFormat, (int) istep,
                                              [RotateImageFilehead UTF8String]);
		}
	}
	[rotateProgreessBar setDoubleValue:(double) 0];
	[rotateProgreessBar stopAnimation:self];
    [rotateProgreessBar setDisplayedWhenStopped:NO];
	[rotateProgreessBar displayIfNeeded];
	
	if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [self CloseKemoviewMovieFile];
    }
}

-(void) SaveQTmovieEvolution{
	int istep, iframe;
	
	if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [self OpenQTMovieFile:EvolutionImageFilename];
    }
	
	[evolutionProgreessBar setUsesThreadedAnimation:YES];
	[evolutionProgreessBar setMinValue:(double) self.EvolutionStartStep];
	[evolutionProgreessBar setMaxValue:(double) self.EvolutionEndStep];
	for(istep = self.EvolutionStartStep;istep<self.EvolutionEndStep+1;istep++){
		if( ((istep-self.EvolutionStartStep)%self.EvolutionIncrement) == 0) {
			[_kemoviewer DrawEvolution:istep];

			if (CurrentMovieFormat == SAVE_QT_MOVIE) {
                iframe = (istep - self.EvolutionStartStep) / self.EvolutionIncrement;
                CMTime frameTime = CMTimeMake((int64_t)iframe, self.FramePerSecond);
                [self AddKemoviewImageToMovie:frameTime];
			} else if (CurrentMovieFormat != 0) {
                write_kemoviewer_window_step_file((int) CurrentMovieFormat, (int) istep,
                                                  [EvolutionImageFilehead UTF8String]);
			}

			[evolutionProgreessBar incrementBy:(double) self.EvolutionIncrement];
			[evolutionProgreessBar displayIfNeeded];
		}
	}
    
	[evolutionProgreessBar setDoubleValue:(double) self.EvolutionStartStep];
	[evolutionProgreessBar stopAnimation:self];
    [evolutionProgreessBar setDisplayedWhenStopped:NO];
	[evolutionProgreessBar displayIfNeeded];
	
	if (CurrentMovieFormat == SAVE_QT_MOVIE) [self CloseKemoviewMovieFile];
}


// ---------------------------------

- (IBAction)SendToClipAsPDF:(id)sender
{
    NSBitmapImageRep *bmpRep = [self allocateBitmapArray];
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

- (void) SelectRotationMovieFile:(NSString *)RotateImageFilename
{
    NSUserDefaults* defaults = [_movie_defaults_controller defaults];
    CurrentMovieFormat = [[defaults stringForKey:@"MovieFormatID"] intValue];

    NSString *RotateImageFileext = [RotateImageFilename pathExtension];
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
}

- (void) SelectEvolutionMovieFile:(NSString *)EvolutionMovieFilename
{
    NSUserDefaults* defaults = [_movie_defaults_controller defaults];
    CurrentMovieFormat = [[defaults stringForKey:@"MovieFormatID"] intValue];
    
    NSString * EvolutionImageFileext =   [EvolutionMovieFilename pathExtension];
    EvolutionImageFilehead = [EvolutionMovieFilename stringByDeletingPathExtension];
    
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

}

- (IBAction)SaveRotationMovie:(id)sender;
{    
    NSSavePanel *RotateImageSavePanelObj = [NSSavePanel savePanel];
    [RotateImageSavePanelObj beginSheetModalForWindow:window 
                                    completionHandler:^(NSInteger RotateImageSaveInt){
        if (RotateImageSaveInt == NSFileHandlingPanelOKButton) {
            NSString *RotateImageFilename = [[ RotateImageSavePanelObj URL] path];
            [self SelectRotationMovieFile:RotateImageFilename];
        };
                                    }];
}

- (IBAction)SaveEvolutionMovie:(id)sender;
{
	NSSavePanel *EvolutionImageSavePanelObj = [NSSavePanel savePanel];
    [EvolutionImageSavePanelObj beginSheetModalForWindow:window 
                                    completionHandler:^(NSInteger EvolutionImageSaveInt){
	if(EvolutionImageSaveInt == NSFileHandlingPanelOKButton){
		NSString *EvolutionMovieFilename = [[ EvolutionImageSavePanelObj URL] path];
        [self SelectEvolutionMovieFile:EvolutionMovieFilename];
	};
                                    }];
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
