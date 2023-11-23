//
//  KemoviewerMovieMaker.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "KemoviewerMovieMaker.h"
#include "kemoviewer.h"

// NSImage *SnapshotImage;
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

-(void) OpenQTMovieFileWithSize:(NSString *)movieFileName : (NSInteger) XViewsize : (NSInteger) YViewsize{
    
    // Movie setting
    NSDictionary *outputSettings = 
    @{
      AVVideoCodecKey : AVVideoCodecTypeH264,
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

-(void) OpenQTMovieFile:(NSString *)movieFileName{
    NSInteger XViewsize = [_kemoviewer KemoviewHorizontalViewSize];
    NSInteger YViewsize = [_kemoviewer KemoviewVerticalViewSize];
    [self OpenQTMovieFileWithSize:movieFileName: XViewsize : YViewsize];
}
-(int) OpenQuiltQTMovieFile:(NSString *)movieFileName{
    int XNumImage = kemoview_get_quilt_nums(ISET_QUILT_COLUMN);
    int YNumImage = kemoview_get_quilt_nums(ISET_QUILT_RAW);
    NSInteger XViewsize = XNumImage * [_kemoviewer KemoviewHorizontalViewSize];
	NSInteger YViewsize = YNumImage * [_kemoviewer KemoviewVerticalViewSize];
	
	if((XViewsize*YViewsize) > 35536896){
		float ratio = (float) (XViewsize*YViewsize) / (float) 35536896;
		char cp[80];
		sprintf(cp, "Image size is %e times larger than the limit.", ratio);
		NSString *str = [NSString stringWithCString: cp encoding:NSUTF8StringEncoding];
		NSAlert *alert = [[NSAlert alloc] init];
		[alert setMessageText:str];
		[alert setInformativeText:@"Set smaller image size"];
		[alert runModal];
		[alert release];
		return 1;
	};
	[self OpenQTMovieFileWithSize:movieFileName: XViewsize : YViewsize];
	return 0;
}

-(void) CloseKemoviewMovieFile{
    [writerInput markAsFinished];
    [videoWriter finishWritingWithCompletionHandler:^{
        NSLog(@"Finish writing!");
    }];
    CVPixelBufferPoolRelease(adaptor.pixelBufferPool);
}

// ---------------------------------

- (NSBitmapImageRep *) SetMetalBitmapToImageRep
{
    NSBitmapImageRep *SnapshotBitmapRep;
    static unsigned char *glimage;
    int XViewsize = [_metalView getHorizontalViewSize];
    int YViewsize = [_metalView getVerticalViewSize];
    
    glimage = (unsigned char*)calloc(3*XViewsize*XViewsize, sizeof(unsigned char));
    
    SnapshotBitmapRep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes:nil
                                                                          pixelsWide: XViewsize
                                                                          pixelsHigh: YViewsize
                                                                       bitsPerSample: 8
                                                                     samplesPerPixel: 3
                                                                            hasAlpha: NO
                                                                            isPlanar: NO
                                                                      colorSpaceName:NSDeviceRGBColorSpace
                                   //    bytesPerRow: (XpixelGLWindow*3) //pixelsWide*samplesPerPixel
                                   // bitsPerPixel: (8*3)   //bitsPerSample*samplesPerPixel
                                                                         bytesPerRow: (XViewsize*3) //pixelsWide*samplesPerPixel
                                                                        bitsPerPixel: 0  //bitsPerSample*samplesPerPixel
                                   ];
/*
    kemoview_get_fliped_img((int) [_kemoviewer KemoviewHorizontalViewSize],
                            (int) [_kemoviewer KemoviewVerticalViewSize],
                            glimage, [SnapshotBitmapRep bitmapData]);
 */
    free(glimage);
    return SnapshotBitmapRep;
}

- (NSBitmapImageRep *) SetGLBitmapToImageRep
{
    NSBitmapImageRep *SnapshotBitmapRep;
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
    return SnapshotBitmapRep;
}

- (NSBitmapImageRep *) SetGLQuiltBitmapToImageRep:(NSInteger) int_degree : (NSInteger)rotationaxis
{
    static unsigned char *glimage;
    NSInteger num_step = (NSInteger) kemoview_get_quilt_nums(ISET_QUILT_NUM);

    int XViewsize = [_kemoviewer KemoviewHorizontalViewSize];
    int YViewsize = [_kemoviewer KemoviewVerticalViewSize];
    int XNumImage = kemoview_get_quilt_nums(ISET_QUILT_COLUMN);
    int YNumImage = kemoview_get_quilt_nums(ISET_QUILT_RAW);

    glimage =    (unsigned char*)calloc(3*XViewsize*YViewsize, sizeof(unsigned char));

    [evolutionProgreessBar setHidden:NO];
    [evolutionProgreessBar setUsesThreadedAnimation:YES];
    [evolutionProgreessBar setMinValue:(double) 0];
    [evolutionProgreessBar setMaxValue:(double) num_step];
    [evolutionProgreessBar startAnimation:self];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    [evolutionProgreessBar displayIfNeeded];
    
    NSBitmapImageRep *SnapshotBitmapRep
                                = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes:nil
                                                                          pixelsWide: XViewsize*XNumImage
                                                                          pixelsHigh: YViewsize*YNumImage
                                                                       bitsPerSample: 8
                                                                     samplesPerPixel: 3
                                                                            hasAlpha: NO
                                                                            isPlanar: NO
                                                                      colorSpaceName:NSDeviceRGBColorSpace
                                   //    bytesPerRow: (XpixelGLWindow*3) //pixelsWide*samplesPerPixel
                                   // bitsPerPixel: (8*3)   //bitsPerSample*samplesPerPixel
                                                                         bytesPerRow: (XNumImage*XViewsize*3) //pixelsWide*samplesPerPixel
                                                                        bitsPerPixel: 0  //bitsPerSample*samplesPerPixel
                                   ];
    
    for(self.CurrentStep = 0;self.CurrentStep<num_step;self.CurrentStep++){
        kemoview_set_quilt_nums(ISET_QUILT_COUNT, (int) self.CurrentStep);
        [_metalView DrawQuilt:int_degree:rotationaxis];

        kemoview_add_fliped_quilt_img(glimage, [SnapshotBitmapRep bitmapData]);

        
        [evolutionProgreessBar incrementBy:(double) 1.0];
        [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
        [evolutionProgreessBar displayIfNeeded];
    }
    
    [evolutionProgreessBar setDoubleValue:(double) self.EvolutionStartStep];
    [evolutionProgreessBar stopAnimation:self];
    [evolutionProgreessBar setHidden:YES];
    [evolutionProgreessBar setDisplayedWhenStopped:NO];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    [evolutionProgreessBar displayIfNeeded];
    
    free(glimage);
    return SnapshotBitmapRep;
}

- (NSImage *) InitMetalBitmapToImage
{
    NSImage *SnapshotImage = [[NSImage alloc] init];

    NSBitmapImageRep *SnapshotBitmapRep = [self SetMetalBitmapToImageRep];
    [SnapshotImage addRepresentation:SnapshotBitmapRep];
    [SnapshotBitmapRep release];
    return SnapshotImage;
}

- (NSImage *) InitGLBitmapToImage
{
    NSImage *SnapshotImage = [[NSImage alloc] init];

    NSBitmapImageRep *SnapshotBitmapRep = [self SetGLBitmapToImageRep];
    [SnapshotImage addRepresentation:SnapshotBitmapRep];
    [SnapshotBitmapRep release];
    return SnapshotImage;
}

- (NSImage *) InitGLQuiltBitmapToImage:(NSInteger) int_degree : (NSInteger)rotationaxis
{
    NSImage *SnapshotImage = [[NSImage alloc] init];
    NSBitmapImageRep *SnapshotBitmapRep = [self SetGLQuiltBitmapToImageRep:int_degree:rotationaxis];

    [SnapshotImage addRepresentation:SnapshotBitmapRep];
    [SnapshotBitmapRep release];
    return SnapshotImage;
}


-(void) AddKemoviewImageToMovie:(CMTime)frameTime
{
	// Adds an image for the specified duration to the QTMovie
    NSImage *SnapshotImage = [self InitGLBitmapToImage];
    
    CGImageRef CGImage = [SnapshotImage CGImageForProposedRect:nil context:nil hints:nil];
    CVPixelBufferRef buffer = [self pixelBufferFromCGImage:CGImage];

    // Append Image buffer
    if (![adaptor appendPixelBuffer:buffer withPresentationTime:frameTime]) {
        NSLog(@"Adapter Failure");
    }
    
    if (buffer) {CVBufferRelease(buffer);}
    [SnapshotImage release];            
}

-(void) AddKemoviewQuiltToMovie:(CMTime)frameTime : (NSInteger) int_degree : (NSInteger)rotationaxis
{
    // Adds an image for the specified duration to the QTMovie
    NSImage *SnapshotImage = [self InitGLQuiltBitmapToImage:int_degree:rotationaxis];
    
    CGImageRef CGImage = [SnapshotImage CGImageForProposedRect:nil context:nil hints:nil];
    CVPixelBufferRef buffer = [self pixelBufferFromCGImage:CGImage];

    // Append Image buffer
    if (![adaptor appendPixelBuffer:buffer withPresentationTime:frameTime]) {
        NSLog(@"Adapter Failure");
    }
    
    if (buffer) {CVBufferRelease(buffer);}
    [SnapshotImage release];
}

-(void) SaveKemoviewQuiltPNGFile:(NSString*)ImageFilehead : (NSInteger) int_degree : (NSInteger)rotationaxis{
    BOOL interlaced = NO;
    NSDictionary *properties;

    NSBitmapImageRep *SnapshotBitmapRep = [self SetGLQuiltBitmapToImageRep:int_degree:rotationaxis];

    properties = [NSDictionary
                  dictionaryWithObject:[NSNumber numberWithBool:interlaced]
                  forKey:NSImageInterlaced];
    SnapshotData = [SnapshotBitmapRep representationUsingType:NSBitmapImageFileTypePNG
                                        properties:properties];
    
    NSString *filename =  [ImageFilehead stringByAppendingString:@".png"];
    [SnapshotData writeToFile:filename atomically:YES];
    [SnapshotBitmapRep release];
};

-(void) SaveKemoviewQuiltBMPFile:(NSString*)ImageFilehead : (NSInteger)int_degree : (NSInteger)rotationaxis{
    BOOL interlaced = NO;
    NSDictionary *properties;

    NSBitmapImageRep *SnapshotBitmapRep = [self SetGLQuiltBitmapToImageRep:int_degree:rotationaxis];


    NSImage *SnapshotImage = [[NSImage alloc] init];
    [SnapshotImage addRepresentation:SnapshotBitmapRep];
    
    properties = [NSDictionary
                  dictionaryWithObject:[NSNumber numberWithBool:interlaced]
                  forKey:NSImageInterlaced];
    SnapshotData = [SnapshotBitmapRep representationUsingType:NSBitmapImageFileTypePNG
                                        properties:properties];
    
    NSString *filename =  [ImageFilehead stringByAppendingString:@".png"];
    [SnapshotData writeToFile:filename atomically:YES];
    [SnapshotImage release];
    [SnapshotBitmapRep release];
};

-(void) SaveKemoviewQuiltPDFFile:(NSString*)ImageFilehead : (NSInteger) int_degree : (NSInteger)rotationaxis{
    NSImageView *myView;
    NSRect vFrame;
    NSData *pdfData;
    
    NSBitmapImageRep *SnapshotBitmapRep = [self SetGLQuiltBitmapToImageRep:int_degree:rotationaxis];

    NSImage *SnapshotImage = [[NSImage alloc] init];
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
    [SnapshotImage release];
    [SnapshotBitmapRep release];
};


-(void) SaveKemoviewPNGFile:(NSString*)ImageFilehead
{
    BOOL interlaced = NO;
    NSDictionary *properties;
    properties = [NSDictionary
                  dictionaryWithObject:[NSNumber numberWithBool:interlaced]
                  forKey:NSImageInterlaced];

    NSBitmapImageRep * imageRep = [NSBitmapImageRep alloc];
    [_metalViewController getRenderedbyMetal:imageRep];
    
    SnapshotData = [imageRep representationUsingType:NSBitmapImageFileTypePNG
                                          properties:properties];
    
    NSString *filename =  [ImageFilehead stringByAppendingString:@".png"];
    [SnapshotData writeToFile:filename atomically:YES];
    [imageRep release];
}

-(void) SaveKemoviewBMPFile:(NSString*)ImageFilehead
{
    BOOL interlaced = NO;
    NSDictionary *properties;
    properties = [NSDictionary
                  dictionaryWithObject:[NSNumber numberWithBool:interlaced]
                  forKey:NSImageInterlaced];

    NSBitmapImageRep * imageRep = [NSBitmapImageRep alloc];
    [_metalViewController getRenderedbyMetal:imageRep];
    
    SnapshotData = [imageRep representationUsingType:NSBitmapImageFileTypeBMP
                                                   properties:properties];
    
    NSString *filename =  [ImageFilehead stringByAppendingString:@".bmp"];
    [SnapshotData writeToFile:filename atomically:YES];
    [imageRep release];
}

-(void) SaveKemoviewPDFFile:(NSString*)ImageFilehead
{
	NSImageView *myView;
	NSRect vFrame;
	NSData *pdfData;
	
    NSBitmapImageRep * imageRep = [NSBitmapImageRep alloc];
    [_metalViewController getRenderedbyMetal:imageRep];
    
    NSImage * image = [[NSImage alloc] init];
    NSRect rectView = [_metalView convertRectToBacking:[_metalView bounds]];
    [image initWithSize:NSSizeFromCGSize(rectView.size)];
    [image addRepresentation:imageRep];

	vFrame = NSZeroRect;
	vFrame.size = [image size];
	myView = [[NSImageView alloc] initWithFrame:vFrame];
	[myView setImage:image];
	pdfData = [myView dataWithPDFInsideRect:vFrame];
	[pdfData retain];

	NSString *filename =  [ImageFilehead stringByAppendingString:@".pdf"];
	[pdfData writeToFile:filename atomically:YES];
	[pdfData release];            
	[imageRep release];
    [image release];
}


// ---------------------------------

-(void) SaveQTmovieRotation
{
	NSInteger ied_deg = 360/self.RotationIncrement;
	NSInteger int_degree, icount;

	if (CurrentMovieFormat == SAVE_QT_MOVIE){
        if(kemoview_get_quilt_nums(ISET_QUILT_MODE) == 1){
            if([self OpenQuiltQTMovieFile:RotateImageFilenameNoStep] != 0) return;
        }else{
            [self OpenQTMovieFile:RotateImageFilenameNoStep];
        }
    }

    [rotateProgreessBar setHidden:NO];
	[rotateProgreessBar setUsesThreadedAnimation:YES];
    [rotateProgreessBar startAnimation:self];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]]; 
    [rotateProgreessBar displayIfNeeded];
    
	for(icount = 0;icount<ied_deg;icount++){
		int_degree = (icount * self.RotationIncrement);
        self.CurrentStep = icount;
		[_metalView DrawRotation:int_degree:RotationAxisID];

        if (CurrentMovieFormat == SAVE_QT_MOVIE && kemoview_get_quilt_nums(ISET_QUILT_MODE) == 1) {
            CMTime frameTime = CMTimeMake((int64_t)icount, self.FramePerSecond);
            [self AddKemoviewQuiltToMovie:frameTime:int_degree:RotationAxisID];
       }else if(CurrentMovieFormat == SAVE_QT_MOVIE) {
            CMTime frameTime = CMTimeMake((int64_t)icount, self.FramePerSecond);
            [self AddKemoviewImageToMovie:frameTime];
        } else if (CurrentMovieFormat != 0) {
            NSString *numstring = [NSString stringWithFormat:@"%ld",icount];
            NSString *ImageFilehead =  [RotateImageFilehead stringByAppendingString:numstring];
            if (CurrentMovieFormat == SAVE_PNG && kemoview_get_quilt_nums(ISET_QUILT_MODE) == 1) {
                [self SaveKemoviewQuiltPNGFile:ImageFilehead:int_degree:RotationAxisID];
            } else if(CurrentMovieFormat == SAVE_PNG){
                [self SaveKemoviewPNGFile:ImageFilehead];
            } else if (CurrentMovieFormat == SAVE_BMP && kemoview_get_quilt_nums(ISET_QUILT_MODE) == 1) {
                [self SaveKemoviewQuiltBMPFile:ImageFilehead:int_degree:RotationAxisID];
            } else if(CurrentMovieFormat == SAVE_BMP){
				[self SaveKemoviewBMPFile:ImageFilehead];
            } else if (kemoview_get_quilt_nums(ISET_QUILT_MODE) == 1) {
                [self SaveKemoviewQuiltPDFFile:ImageFilehead:int_degree:RotationAxisID];
			} else {
                [self SaveKemoviewPDFFile:ImageFilehead];
            }
		}
        
        [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]]; 
		[rotateProgreessBar incrementBy:(double) int_degree];
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

-(void) PreviewQuiltImages
{
    NSInteger num_step = (NSInteger) kemoview_get_quilt_nums(ISET_QUILT_NUM);

    [evolutionProgreessBar setHidden:NO];
    [evolutionProgreessBar setUsesThreadedAnimation:YES];
    [evolutionProgreessBar setMinValue:(double) 0];
    [evolutionProgreessBar setMaxValue:(double) num_step];
    [evolutionProgreessBar startAnimation:self];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    [evolutionProgreessBar displayIfNeeded];
    for(self.CurrentStep = 0;self.CurrentStep<num_step;self.CurrentStep++){
        kemoview_set_quilt_nums(ISET_QUILT_COUNT, (int) self.CurrentStep);
        [_metalView DrawQuilt:IZERO:IONE];

        [evolutionProgreessBar incrementBy:(double) 1.0];
        [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
        [evolutionProgreessBar displayIfNeeded];
    }
    [evolutionProgreessBar setDoubleValue:(double) self.EvolutionStartStep];
    [evolutionProgreessBar stopAnimation:self];
    [evolutionProgreessBar setHidden:YES];
    [evolutionProgreessBar setDisplayedWhenStopped:NO];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    [evolutionProgreessBar displayIfNeeded];
}

-(void) SaveQTmovieEvolution{
	int iframe;
	
	if (CurrentMovieFormat == SAVE_QT_MOVIE){
        if(kemoview_get_quilt_nums(ISET_QUILT_MODE) == 1){
            if([self OpenQuiltQTMovieFile:EvolutionImageFilename] != 0) return;
        }else{
            [self OpenQTMovieFile:EvolutionImageFilename];
        }
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
			[_metalView DrawEvolution:self.CurrentStep];

			if (CurrentMovieFormat == SAVE_QT_MOVIE && kemoview_get_quilt_nums(ISET_QUILT_MODE) == 1) {
                iframe = (self.CurrentStep - self.EvolutionStartStep) / self.EvolutionIncrement;
                CMTime frameTime = CMTimeMake((int64_t)iframe, self.FramePerSecond);
                [self AddKemoviewQuiltToMovie:frameTime:IZERO:IONE];
            }else if(CurrentMovieFormat == SAVE_QT_MOVIE) {
                iframe = (self.CurrentStep - self.EvolutionStartStep) / self.EvolutionIncrement;
                CMTime frameTime = CMTimeMake((int64_t)iframe, self.FramePerSecond);
                [self AddKemoviewImageToMovie:frameTime];
			} else if (CurrentMovieFormat != 0) {
                NSString *numstring = [NSString stringWithFormat:@"%ld",self.CurrentStep];
                NSString *ImageFilehead =  [EvolutionImageFilehead stringByAppendingString:numstring];
                if (CurrentMovieFormat == SAVE_PNG && kemoview_get_quilt_nums(ISET_QUILT_MODE) == 1) {
                    [self SaveKemoviewQuiltPNGFile:ImageFilehead:IZERO:IONE];
                } else if(CurrentMovieFormat == SAVE_PNG){
                    [self SaveKemoviewPNGFile:ImageFilehead];
                } else if (CurrentMovieFormat == SAVE_BMP && kemoview_get_quilt_nums(ISET_QUILT_MODE) == 1) {
                    [self SaveKemoviewQuiltBMPFile:ImageFilehead:IZERO:IONE];
                } else if(CurrentMovieFormat == SAVE_BMP){
                    [self SaveKemoviewBMPFile:ImageFilehead];
                } else if (kemoview_get_quilt_nums(ISET_QUILT_MODE) == 1) {
                    [self SaveKemoviewQuiltPDFFile:ImageFilehead:IZERO:IONE];
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

- (IBAction)SendToClipAsTIFF:(id)sender
{
    NSBitmapImageRep * imageRep = [NSBitmapImageRep alloc];
    [_metalViewController getRenderedbyMetal:imageRep];
    
    NSImage * image = [[NSImage alloc] init];
    NSRect rectView = [_metalView convertRectToBacking:[_metalView bounds]];
    [image initWithSize:NSSizeFromCGSize(rectView.size)];
    [image addRepresentation:imageRep];
    [imageRep release];

    NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
	[pasteboard declareTypes:[NSArray arrayWithObjects:NSPasteboardTypeTIFF, nil] owner:nil];
	[pasteboard setData:[image TIFFRepresentation] forType:NSPasteboardTypeTIFF];
    [image release];
}

// ---------------------------------

- (IBAction)ShowRotationMovie:(id)sender;
{
	CurrentMovieFormat = 0;
	[self SaveQTmovieRotation];
}

- (IBAction)ShowQuiltMovie:(id)sender;
{
    CurrentMovieFormat = 0;
    [self PreviewQuiltImages];
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
