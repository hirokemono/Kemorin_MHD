//
//  KemoviewerMovieMaker.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "KemoviewerMovieMaker.h"
#include "kemoviewer.h"

NSBitmapImageRep *SnapshotBitmapRep;
NSImage *SnapshotImage;
NSData *SnapshotData;

@implementation KemoviewerMovieMaker
@synthesize MovieFormatFlag;
@synthesize FramePerSecond;
@synthesize RotationAxisID;
@synthesize RotationIncrement;
@synthesize EvolutionStartStep;
@synthesize EvolutionEndStep;
@synthesize EvolutionIncrement;
@synthesize CurrentStep;
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
	self.CurrentStep = send_current_psf_full_path_header(image_head, &iflag);
	self.EvolutionStartStep = self.CurrentStep;
	self.EvolutionEndStep =   self.CurrentStep;
};

-(void) InitEvolutionStepByFline;
{
	char image_head[LENGTHBUF];
	self.CurrentStep = send_fline_file_header(image_head);
	self.EvolutionStartStep = self.CurrentStep;
	self.EvolutionEndStep =   self.CurrentStep;
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

- (void) SetGLBitmapToImageRep
{
    static unsigned char *glimage;
    GLint XViewsize = [_kemoviewer KemoviewHorizontalViewSize];
    GLint YViewsize = [_kemoviewer KemoviewVerticalViewSize];
    
    glimage = (unsigned char*)calloc(3*XViewsize*XViewsize, sizeof(unsigned char));
    
    SnapshotBitmapRep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes:nil
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
    
    get_kemoviewer_fliped_img((int) [_kemoviewer KemoviewHorizontalViewSize],
                              (int) [_kemoviewer KemoviewVerticalViewSize],
                              glimage, [SnapshotBitmapRep bitmapData]);
    free(glimage);
    return;
}

- (void) SetGLBitmapToImage
{
    [self SetGLBitmapToImageRep];

    [SnapshotImage addRepresentation:SnapshotBitmapRep];
    [SnapshotBitmapRep release];
    return;
}

-(void) AddKemoviewImageToMovie:(CMTime)frameTime
{
	// Adds an image for the specified duration to the QTMovie
    SnapshotImage = [[NSImage alloc] init];
    [self SetGLBitmapToImage];
    
    CGImageRef CGImage = [SnapshotImage CGImageForProposedRect:nil context:nil hints:nil];
    CVPixelBufferRef buffer = [self pixelBufferFromCGImage:CGImage];

    // Append Image buffer
    if (![adaptor appendPixelBuffer:buffer withPresentationTime:frameTime]) {
        NSLog(@"Adapter Failure");
    }
    
    if (buffer) {CVBufferRelease(buffer);}
    [SnapshotImage release];            
}


-(void) SaveKemoviewImageFile:(NSString*)filename
{
    BOOL interlaced;
    NSDictionary *properties;
    
    [self SetGLBitmapToImageRep];
    
    SnapshotImage = [[NSImage alloc] init];
    [SnapshotImage addRepresentation:SnapshotBitmapRep];
    
    properties = [NSDictionary
                  dictionaryWithObject:[NSNumber numberWithBool:interlaced]
                  forKey:NSImageInterlaced];
    SnapshotData = [SnapshotBitmapRep representationUsingType:NSPNGFileType
                                        properties:properties];
    [SnapshotData writeToFile:filename atomically:YES];
    printf("PNG file output\n");
    [SnapshotImage release];            
    [SnapshotBitmapRep release];
}


// ---------------------------------

-(void) SaveQTmovieRotation
{
	NSInteger ied_deg = 360/self.RotationIncrement;
	NSInteger int_degree;

	if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [self OpenQTMovieFile:RotateImageFilenameNoStep];
    }

    [rotateProgreessBar setHidden:NO];
	[rotateProgreessBar setUsesThreadedAnimation:YES];
    [rotateProgreessBar startAnimation:self];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]]; 
    [rotateProgreessBar displayIfNeeded];
    
	for(self.CurrentStep = 0;self.CurrentStep<ied_deg;self.CurrentStep++){
		int_degree = (self.CurrentStep * self.RotationIncrement);
		[_kemoviewer DrawRotation:int_degree:RotationAxisID];

		if (CurrentMovieFormat == SAVE_QT_MOVIE) {
            CMTime frameTime = CMTimeMake((int64_t)self.CurrentStep, self.FramePerSecond);
            [self AddKemoviewImageToMovie:frameTime];
        } else if (CurrentMovieFormat == SAVE_PNG) {
            NSString *numstring = [NSString stringWithFormat:@"%ld",self.CurrentStep];
            NSString *TmpName =  [RotateImageFilehead stringByAppendingString:numstring];
            NSString *FileName =  [TmpName stringByAppendingString:@".png"];
            [self SaveKemoviewImageFile:FileName];
		} else if (CurrentMovieFormat != 0) {
            write_kemoviewer_window_step_file((int) CurrentMovieFormat, (int) self.CurrentStep,
                                              [RotateImageFilehead UTF8String]);
		}
        
        [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]]; 
		[rotateProgreessBar incrementBy:(double) self.RotationIncrement];
		[rotateProgreessBar displayIfNeeded];
	}
	[rotateProgreessBar setDoubleValue:(double) 0];
	[rotateProgreessBar stopAnimation:self];
    [rotateProgreessBar setHidden:YES];
	[rotateProgreessBar displayIfNeeded];
	
	if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [self CloseKemoviewMovieFile];
    }
}

-(void) SaveQTmovieEvolution{
	int iframe;
	
	if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [self OpenQTMovieFile:EvolutionImageFilename];
    }
	
    [evolutionProgreessBar setHidden:NO];
	[evolutionProgreessBar setUsesThreadedAnimation:YES];
	[evolutionProgreessBar setMinValue:(double) self.EvolutionStartStep];
	[evolutionProgreessBar setMaxValue:(double) self.EvolutionEndStep];
    [evolutionProgreessBar startAnimation:self];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]]; 
    [evolutionProgreessBar displayIfNeeded];

    for(self.CurrentStep = self.EvolutionStartStep;self.CurrentStep<self.EvolutionEndStep+1;self.CurrentStep++){
		if( ((self.CurrentStep-self.EvolutionStartStep)%self.EvolutionIncrement) == 0) {
			[_kemoviewer DrawEvolution:self.CurrentStep];

			if (CurrentMovieFormat == SAVE_QT_MOVIE) {
                iframe = (self.CurrentStep - self.EvolutionStartStep) / self.EvolutionIncrement;
                CMTime frameTime = CMTimeMake((int64_t)iframe, self.FramePerSecond);
                [self AddKemoviewImageToMovie:frameTime];
            } else if (CurrentMovieFormat == SAVE_PNG) {
                NSString *numstring = [NSString stringWithFormat:@"%ld",self.CurrentStep];
                NSString *TmpName =  [RotateImageFilehead stringByAppendingString:numstring];
                NSString *FileName =  [TmpName stringByAppendingString:@".png"];
                [self SaveKemoviewImageFile:FileName];
			} else if (CurrentMovieFormat != 0) {
                write_kemoviewer_window_step_file((int) CurrentMovieFormat, (int) self.CurrentStep,
                                                  [EvolutionImageFilehead UTF8String]);
			}

			[evolutionProgreessBar incrementBy:(double) self.EvolutionIncrement];
            [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]]; 
            [evolutionProgreessBar displayIfNeeded];
		}
	}
    
	[evolutionProgreessBar setDoubleValue:(double) self.EvolutionStartStep];
	[evolutionProgreessBar stopAnimation:self];
    [evolutionProgreessBar setHidden:YES];
    [evolutionProgreessBar setDisplayedWhenStopped:NO];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]]; 
	[evolutionProgreessBar displayIfNeeded];
	
	if (CurrentMovieFormat == SAVE_QT_MOVIE) [self CloseKemoviewMovieFile];
}


// ---------------------------------

- (IBAction)SendToClipAsPDF:(id)sender
{
    SnapshotImage = [[NSImage alloc] init];
    [self SetGLBitmapToImage];
    
    
    NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
	[pasteboard declareTypes:[NSArray arrayWithObjects:NSPasteboardTypeTIFF, nil] owner:nil];
	[pasteboard setData:[SnapshotImage TIFFRepresentation] forType:NSTIFFPboardType];
    
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
            [RotateImageSavePanelObj orderOut:nil];
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
        [EvolutionImageSavePanelObj orderOut:nil];
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
}

- (IBAction)SetEvolutionSteps:(id)sender{
	[_kemoviewer swapbuffer_cocoa];
}

- (IBAction)ChooseRotateAxis:(id)sender;
{
	RotationAxisID = [[rotationAxis selectedCell] tag];
}

@end
