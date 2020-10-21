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
	int iflag;
    struct kv_string *psf_filehead = kemoview_alloc_kvstring();
    
	self.CurrentStep = kemoview_get_PSF_full_path_file_prefix(psf_filehead, &iflag);
	self.EvolutionStartStep = self.CurrentStep;
	self.EvolutionEndStep =   self.CurrentStep;
    
    kemoview_free_kvstring(psf_filehead);
};

-(void) InitEvolutionStepByFline;
{
    struct kv_string *fline_filehead = kemoview_alloc_kvstring();
	self.CurrentStep = kemoview_get_fline_file_step_prefix(fline_filehead);
	self.EvolutionStartStep = self.CurrentStep;
	self.EvolutionEndStep =   self.CurrentStep;
    kemoview_free_kvstring(fline_filehead);
}

-(void) OpenQTMovieFile:(NSString *)movieFileName{
    int XViewsize = [_kemoviewer KemoviewHorizontalViewSize];
    int YViewsize = [_kemoviewer KemoviewVerticalViewSize];
    
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
    int XViewsize = [_kemoviewer KemoviewHorizontalViewSize];
    int YViewsize = [_kemoviewer KemoviewVerticalViewSize];
    
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
    
    kemoview_get_fliped_img((int) [_kemoviewer KemoviewHorizontalViewSize],
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


-(void) SaveKemoviewPNGFile:(NSString*)ImageFilehead
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
    
    NSString *filename =  [ImageFilehead stringByAppendingString:@".png"];
    [SnapshotData writeToFile:filename atomically:YES];
    [SnapshotImage release];            
    [SnapshotBitmapRep release];
}

-(void) SaveKemoviewBMPFile:(NSString*)ImageFilehead
{
    BOOL interlaced;
    NSDictionary *properties;
    
    [self SetGLBitmapToImageRep];
    
    SnapshotImage = [[NSImage alloc] init];
    [SnapshotImage addRepresentation:SnapshotBitmapRep];
    
    properties = [NSDictionary
                  dictionaryWithObject:[NSNumber numberWithBool:interlaced]
                  forKey:NSImageInterlaced];
    SnapshotData = [SnapshotBitmapRep representationUsingType:NSBMPFileType
                                                   properties:properties];
    
    NSString *filename =  [ImageFilehead stringByAppendingString:@".bmp"];
    [SnapshotData writeToFile:filename atomically:YES];
    [SnapshotImage release];            
    [SnapshotBitmapRep release];
}

-(void) SaveKemoviewPDFFile:(NSString*)ImageFilehead
{
	NSImageView *myView;
	NSRect vFrame;
	NSData *pdfData;
	
	[self SetGLBitmapToImageRep];
	
	SnapshotImage = [[NSImage alloc] init];
	[SnapshotImage addRepresentation:SnapshotBitmapRep];
	
	vFrame = NSZeroRect;
	vFrame.size = [SnapshotImage size];
	myView = [[NSImageView alloc] initWithFrame:vFrame];
	[myView setImage:SnapshotImage];
	pdfData = [myView dataWithPDFInsideRect:vFrame];
	[pdfData retain];

	NSString *filename =  [ImageFilehead stringByAppendingString:@".pdf"];
	[pdfData writeToFile:filename atomically:YES];
	[pdfData release];            
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
        } else if (CurrentMovieFormat != 0) {
            NSString *numstring = [NSString stringWithFormat:@"%ld",self.CurrentStep];
            NSString *ImageFilehead =  [RotateImageFilehead stringByAppendingString:numstring];
            if (CurrentMovieFormat == SAVE_PNG) {
                [self SaveKemoviewPNGFile:ImageFilehead];
            } else if (CurrentMovieFormat == SAVE_BMP) {
				[self SaveKemoviewBMPFile:ImageFilehead];
			} else {
                [self SaveKemoviewPDFFile:ImageFilehead];
            }
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
			} else if (CurrentMovieFormat != 0) {
                NSString *numstring = [NSString stringWithFormat:@"%ld",self.CurrentStep];
                NSString *ImageFilehead =  [EvolutionImageFilehead stringByAppendingString:numstring];
                if (CurrentMovieFormat == SAVE_PNG) {
                    [self SaveKemoviewPNGFile:ImageFilehead];
                } else if (CurrentMovieFormat == SAVE_BMP) {
					[self SaveKemoviewBMPFile:ImageFilehead];
				} else {
                    [self SaveKemoviewPDFFile:ImageFilehead];
                }
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

- (NSInteger) SetImageFileFormatID:(NSString *)FileExtension
{
    NSInteger id_format;

    if ([FileExtension isEqualToString:@"mov"] 
        || [FileExtension isEqualToString:@"MOV"]
        || [FileExtension isEqualToString:@"moov"]
        || [FileExtension isEqualToString:@"MOOV"]) {
        id_format = SAVE_QT_MOVIE;
    } else if ([FileExtension isEqualToString:@"png"] 
               || [FileExtension isEqualToString:@"PNG"]) {
        id_format = SAVE_PNG;
    } else if ([FileExtension isEqualToString:@"bmp"] 
               || [FileExtension isEqualToString:@"BMP"]) {
        id_format = SAVE_BMP;
    } else if ([FileExtension isEqualToString:@"pdf"] 
               || [FileExtension isEqualToString:@"PDF"]) {
        id_format = SAVE_PDF;
    } else {
        id_format = SAVE_UNDEFINED;
    }
    
    return id_format;
}


- (void) SelectRotationMovieFile:(NSString *)RotateImageFilename
{
    NSUserDefaults* defaults = [_movie_defaults_controller defaults];

    NSString *RotateImageFileext = [RotateImageFilename pathExtension];
    RotateImageFilehead = [RotateImageFilename stringByDeletingPathExtension];
    
    CurrentMovieFormat = [self SetImageFileFormatID:RotateImageFileext];
    
    if(CurrentMovieFormat == SAVE_UNDEFINED){
    CurrentMovieFormat = [[defaults stringForKey:@"MovieFormatID"] intValue];
    }
    if(CurrentMovieFormat == SAVE_QT_MOVIE){
        RotateImageFilenameNoStep = [RotateImageFilehead stringByAppendingPathExtension:@"mov"];
    }
    
    [self SaveQTmovieRotation];
}

- (void) SelectEvolutionMovieFile:(NSString *)EvolutionMovieFilename
{
    NSUserDefaults* defaults = [_movie_defaults_controller defaults];
    NSString * EvolutionImageFileext =   [EvolutionMovieFilename pathExtension];
    EvolutionImageFilehead = [EvolutionMovieFilename stringByDeletingPathExtension];
    CurrentMovieFormat = [self SetImageFileFormatID:EvolutionImageFileext];
    
    if(CurrentMovieFormat == SAVE_UNDEFINED){
        CurrentMovieFormat = [[defaults stringForKey:@"MovieFormatID"] intValue];
    }
    if(CurrentMovieFormat == SAVE_QT_MOVIE){
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
    //    [_kemoviewer swapbuffer_cocoa];
    return;
}

- (IBAction)getMovieFormatFlag:(id)sender
{
	NSUserDefaults* defaults = [_movie_defaults_controller defaults];
	MovieFormatFlag = [[defaults stringForKey:@"MovieFormatID"] intValue];
	MovieFormatFlag = [[movieFormat_item selectedCell] tag];
};

- (IBAction)SetEvolutionSteps:(id)sender{
//    [_kemoviewer swapbuffer_cocoa];
    return;
};

- (IBAction)ChooseRotateAxis:(id)sender{
	RotationAxisID = [[rotationAxis selectedCell] tag];
};

@end
