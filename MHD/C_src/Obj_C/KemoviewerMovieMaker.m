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
@synthesize NumTeetRotation;
@synthesize SnapshotFPS;
@synthesize AverageFPS;
@synthesize stepToDisplay;
@synthesize stepDisplayFlag;
-(id) init
{
	self.FramePerSecond = 12;
	self.RotationIncrement =  2;
	self.RotationAxisID = 3;
	self.EvolutionStartStep = 1;
	self.EvolutionEndStep =   1;
	self.EvolutionIncrement = 1;
	
    self.NumTeetRotation = 2;
    
    self.stepDisplayFlag = 0;
    self.stepToDisplay = 0.0;
	return self;
}

-(CVPixelBufferRef)pixelBufferFromCGImage:(CGImageRef)image
{
    NSDictionary *options = @{ (NSString *)kCVPixelBufferCGImageCompatibilityKey: @YES,
                               (NSString *)kCVPixelBufferCGBitmapContextCompatibilityKey: @YES, };
    
    CVPixelBufferRef pxbuffer = NULL;
    
/*    width is required producs of 16 pixels */
    CGFloat width  = 16 * (int) (1 + CGImageGetWidth(image) / 16);
/*
    CGFloat width  = CGImageGetWidth(image);
 */
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


-(void) InitEvolutionStepByPSF:(struct kemoviewer_type *) kemo_sgl
{
	int iflag;
    struct kv_string *psf_filehead = kemoview_alloc_kvstring();
    
	self.CurrentStep
        = kemoview_get_PSF_full_path_file_prefix(kemo_sgl, psf_filehead, &iflag);
	self.EvolutionStartStep = self.CurrentStep;
	self.EvolutionEndStep =   self.CurrentStep;
    
    kemoview_free_kvstring(psf_filehead);
};

-(void) InitEvolutionStepByFline:(struct kemoviewer_type *) kemo_sgl
{
    struct kv_string *fline_filehead = kemoview_alloc_kvstring();
	self.CurrentStep = kemoview_get_fline_file_step_prefix(kemo_sgl, fline_filehead);
	self.EvolutionStartStep = self.CurrentStep;
	self.EvolutionEndStep =   self.CurrentStep;
    kemoview_free_kvstring(fline_filehead);
}

-(void) OpenQTMovieFileWithSize:(NSString *)movieFileName
                          width:(NSUInteger) XViewsize
                         height:(NSUInteger) YViewsize
{
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
    [self OpenQTMovieFileWithSize:movieFileName
                            width:[_metalView getHorizontalViewSize]
                           height:[_metalView getVerticalViewSize]];
}
-(int) OpenQuiltQTMovieFile:(NSString *)movieFileName
                   kemoview:(struct kemoviewer_type *) kemo_sgl
{
    int XNumImage = kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_COLUMN);
    int YNumImage = kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_RAW);
    NSUInteger XViewsize = XNumImage * [_metalView getHorizontalViewSize];
	NSUInteger YViewsize = YNumImage * [_metalView getVerticalViewSize];
	
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
	[self OpenQTMovieFileWithSize:movieFileName
                            width:XViewsize
                           height:YViewsize];
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
- (NSBitmapImageRep *) SetMetalQuiltBitmapToImageRep:(NSInteger) int_degree
                                                axis:(NSInteger)rotationaxis
                                            kemoview:(struct kemoviewer_type *) kemo_sgl
{
    static unsigned char *glimage;
    NSInteger num_step
        = (NSInteger) kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_NUM);

    NSUInteger XViewsize = [_metalView getHorizontalViewSize];
    NSUInteger YViewsize = [_metalView getVerticalViewSize];
    NSUInteger XNumImage
        = (NSUInteger) kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_COLUMN);
    NSUInteger YNumImage
        = (NSUInteger) kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_RAW);

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
    NSUInteger pix_xy[2];
    NSUInteger pixelByte[1];
    for(self.CurrentStep = 0;self.CurrentStep<num_step;self.CurrentStep++){
        [_metalView DrawQuilt:(self.CurrentStep+1)
                       degree:int_degree
                         axis:rotationaxis
                     kemoview:kemo_sgl];

        unsigned char *bgra = [_metalViewController getRenderedbyMetalToBGRA:(NSUInteger *) pix_xy
                                                                PixelPerByte:(NSUInteger *) pixelByte
                                                                    kemoview:kemo_sgl];
        kemoview_add_bgra_to_quilt(kemo_sgl, (int) self.CurrentStep, 
                                   (int) pix_xy[0], (int) pix_xy[1], bgra, 
                                   [SnapshotBitmapRep bitmapData]);
        free(bgra);

        
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


- (NSImage *) InitMetalQuiltBitmapToImage:(NSInteger) int_degree
                                     axis:(NSInteger) rotationaxis
                                 kemoview:(struct kemoviewer_type *) kemo_sgl
{
    NSImage *SnapshotImage = [[NSImage alloc] init];
    NSBitmapImageRep *SnapshotBitmapRep
        = [self SetMetalQuiltBitmapToImageRep:int_degree
                                         axis:rotationaxis
                                     kemoview:kemo_sgl];

    [SnapshotImage addRepresentation:SnapshotBitmapRep];
    [SnapshotBitmapRep release];
    return SnapshotImage;
}


-(void) AddKemoviewImageToMovie:(CMTime)frameTime
                       kemoview:(struct kemoviewer_type *) kemo_sgl
{
	// Adds an image for the specified duration to the QTMovie
    NSImage *SnapshotImage = [[NSImage alloc] init];
    NSBitmapImageRep * imageRep = [NSBitmapImageRep alloc];
    [_metalViewController getRenderedbyMetal:imageRep
                                    kemoview:kemo_sgl];
    [SnapshotImage addRepresentation:imageRep];

    CGImageRef CGImage = [SnapshotImage CGImageForProposedRect:nil context:nil hints:nil];
    CVPixelBufferRef buffer = [self pixelBufferFromCGImage:CGImage];
    // Append Image buffer
    if (![adaptor appendPixelBuffer:buffer withPresentationTime:frameTime]) {
        NSLog(@"Adapter Failure");
    }
    if (buffer) {CVBufferRelease(buffer);};
    CGImageRelease(CGImage);
    [imageRep release];
}

-(void) AddKemoviewQuiltToMovie:(CMTime)frameTime
                         degree:(NSInteger) int_degree
                           axis:(NSInteger) rotationaxis
                       kemoview:(struct kemoviewer_type *) kemo_sgl
{
    // Adds an image for the specified duration to the QTMovie
    NSImage *SnapshotImage = [self InitMetalQuiltBitmapToImage:int_degree
                                                          axis:rotationaxis
                                                      kemoview:kemo_sgl];
    
    CGImageRef CGImage = [SnapshotImage CGImageForProposedRect:nil context:nil hints:nil];
    CVPixelBufferRef buffer = [self pixelBufferFromCGImage:CGImage];

    // Append Image buffer
    if (![adaptor appendPixelBuffer:buffer withPresentationTime:frameTime]) {
        NSLog(@"Adapter Failure");
    }
    
    if (buffer) {CVBufferRelease(buffer);}
    [SnapshotImage release];
}

-(void) SaveKemoviewQuiltPNGFile:(NSString*)ImageFilehead
                          degree:(NSInteger) int_degree
                            axis:(NSInteger)rotationaxis
                        kemoview:(struct kemoviewer_type *) kemo_sgl
{
    BOOL interlaced = NO;
    NSDictionary *properties;
    properties = [NSDictionary
                  dictionaryWithObject:[NSNumber numberWithBool:interlaced]
                  forKey:NSImageInterlaced];

    NSBitmapImageRep *SnapshotBitmapRep = [self SetMetalQuiltBitmapToImageRep:int_degree
                                                                         axis:rotationaxis
                                                                     kemoview:kemo_sgl];
    
    SnapshotData = [SnapshotBitmapRep representationUsingType:NSBitmapImageFileTypePNG
                                                   properties:properties];
    
    NSString *filename =  [ImageFilehead stringByAppendingString:@".png"];
    [SnapshotData writeToFile:filename atomically:YES];
    [SnapshotBitmapRep release];
};

-(void) SaveKemoviewQuiltBMPFile:(NSString*)ImageFilehead
                          degree:(NSInteger)int_degree
                            axis:(NSInteger)rotationaxis
                        kemoview:(struct kemoviewer_type *) kemo_sgl
{
    BOOL interlaced = NO;
    NSDictionary *properties;
    NSBitmapImageRep *SnapshotBitmapRep = [self SetMetalQuiltBitmapToImageRep:int_degree
                                                                         axis:rotationaxis
                                                                     kemoview:kemo_sgl];
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

-(void) SaveKemoviewQuiltPDFFile:(NSString*)ImageFilehead
                          degree:(NSInteger) int_degree
                            axis:(NSInteger) rotationaxis
                        kemoview:(struct kemoviewer_type *) kemo_sgl
{
    NSImageView *myView;
    NSRect vFrame;
    NSData *pdfData;
    
    NSBitmapImageRep *SnapshotBitmapRep 
        = [self SetMetalQuiltBitmapToImageRep:int_degree
                                         axis:rotationaxis
                                     kemoview:kemo_sgl];
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
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_metalViewController getRenderedbyMetal:imageRep
                                    kemoview:kemo_sgl];
    
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
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_metalViewController getRenderedbyMetal:imageRep
                                    kemoview:kemo_sgl];

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
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_metalViewController getRenderedbyMetal:imageRep
                                    kemoview:kemo_sgl];

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
-(id) setRotation:(NSInteger) int_degree
             axis:(NSInteger) rotationaxis
         kemoview:(struct kemoviewer_type *) kemo_sgl
{
    kemoview_set_view_integer(ISET_ROTATE_AXIS, (int) rotationaxis, kemo_sgl);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, (int) int_degree, kemo_sgl);
    kemoview_step_viewmatrix(IZERO, kemo_sgl);
    return self;
}

-(void) SaveQTmovieRotation:(struct kemoviewer_type *) kemo_sgl;
{
    NSInteger ied_deg = 360/self.RotationIncrement;
    NSInteger int_degree, icount;
    
    [_metalViewController refreshKemoViewTripleBuffersForRotation:kemo_sgl];
    kemoview_set_view_integer(ISET_DRAW_MODE, MOVIE_DRAW, kemo_sgl);
//    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW, kemo_sgl);

    if (CurrentMovieFormat == SAVE_QT_MOVIE){
        if(kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1){
            if([self OpenQuiltQTMovieFile:RotateImageFilenameNoStep
                                 kemoview:kemo_sgl] != 0) return;
        }else{
            [self OpenQTMovieFile:RotateImageFilenameNoStep];
        }
    }
    
    [rotateProgreessBar setHidden:NO];
    [rotateProgreessBar setUsesThreadedAnimation:YES];
    [rotateProgreessBar startAnimation:self];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    [rotateProgreessBar displayIfNeeded];
    
    if(CurrentMovieFormat != NO_SAVE_FILE) self.stepDisplayFlag = 1;
    for(icount = 0;icount<ied_deg;icount++){
        int_degree = (icount * self.RotationIncrement);
        self.CurrentStep = icount;
        self.stepToDisplay = (float) icount / (float) (ied_deg - 1);
        [self setRotation:int_degree
                     axis:RotationAxisID
                 kemoview:kemo_sgl];

        if (CurrentMovieFormat == SAVE_QT_MOVIE
            && kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1) {
            CMTime frameTime = CMTimeMake((int64_t)icount, (int) self.FramePerSecond);
            [self AddKemoviewQuiltToMovie:frameTime
                                   degree:int_degree
                                   axis:RotationAxisID
                                 kemoview:kemo_sgl];
        }else if(CurrentMovieFormat == SAVE_QT_MOVIE) {
            CMTime frameTime = CMTimeMake((int64_t)icount, (int) self.FramePerSecond);
            [self AddKemoviewImageToMovie:frameTime
                                 kemoview:kemo_sgl];
        } else if (CurrentMovieFormat != 0) {
            NSString *numstring = [NSString stringWithFormat:@".%ld",icount];
            NSString *ImageFilehead =  [RotateImageFilehead stringByAppendingString:numstring];
            if (CurrentMovieFormat == SAVE_PNG
                && kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1) {
                [self SaveKemoviewQuiltPNGFile:ImageFilehead
                                        degree:int_degree
                                          axis:RotationAxisID
                                      kemoview:kemo_sgl];
            } else if(CurrentMovieFormat == SAVE_PNG){
                [self SaveKemoviewPNGFile:ImageFilehead];
            } else if (CurrentMovieFormat == SAVE_BMP
                       && kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1) {
                [self SaveKemoviewQuiltBMPFile:ImageFilehead
                                        degree:int_degree
                                          axis:RotationAxisID
                                      kemoview:kemo_sgl];
            } else if(CurrentMovieFormat == SAVE_BMP){
                [self SaveKemoviewBMPFile:ImageFilehead];
            } else if (kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1) {
                [self SaveKemoviewQuiltPDFFile:ImageFilehead
                                        degree:int_degree
                                          axis:RotationAxisID
                                      kemoview:kemo_sgl];
            } else {
                [self SaveKemoviewPDFFile:ImageFilehead];
            }
        }else{
            [_metalView draw];
        }
        
        [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
        [rotateProgreessBar incrementBy:(double) int_degree];
        [rotateProgreessBar displayIfNeeded];
    }
    self.stepDisplayFlag = 0;

    [rotateProgreessBar setDoubleValue:(double) 0];
    [rotateProgreessBar stopAnimation:self];
    [rotateProgreessBar setHidden:YES];
    [rotateProgreessBar displayIfNeeded];
    
    if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [self CloseKemoviewMovieFile];
    }
    
    kemoview_step_viewmatrix(IZERO, kemo_sgl);
    [self setRotation:IZERO
                 axis:RotationAxisID
             kemoview:kemo_sgl];
    [_metalView setNeedsDisplay:YES];
}

-(void) ShowQTmovieRotation:(struct kemoviewer_type *) kemo_sgl
                numRotation:(NSInteger) rotCounts;
{
    struct timeval startwtime;
    struct timeval endwtime;
    double seq_time;
    double accum_time = 0.0;

    NSInteger ied_deg = rotCounts * 360/self.RotationIncrement;
    NSInteger int_degree, icount;
    
    [_metalViewController refreshKemoViewTripleBuffersForRotation:kemo_sgl];
    kemoview_set_view_integer(ISET_DRAW_MODE, MOVIE_DRAW, kemo_sgl);

    [rotateProgreessBar setHidden:NO];
    [rotateProgreessBar setUsesThreadedAnimation:YES];
    [rotateProgreessBar startAnimation:self];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    [rotateProgreessBar displayIfNeeded];
    
    for(icount = 0;icount<ied_deg;icount++){
        int_degree = (icount * self.RotationIncrement);
        self.stepToDisplay = (float) icount / (float) (ied_deg - 1);
        self.CurrentStep = icount;
        [self setRotation:int_degree
                     axis:RotationAxisID
                 kemoview:kemo_sgl];

        gettimeofday( &startwtime, NULL );
        [_metalView draw];
        gettimeofday( &endwtime, NULL );
        seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                             + endwtime.tv_sec - startwtime.tv_sec );
        accum_time = accum_time + seq_time;
        self.SnapshotFPS = 1.0 / seq_time;
        self.AverageFPS =  (double) icount / accum_time;

        [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
        [rotateProgreessBar incrementBy:(double) int_degree];
        [rotateProgreessBar displayIfNeeded];
    }
    [rotateProgreessBar setDoubleValue:(double) 0];
    [rotateProgreessBar stopAnimation:self];
    [rotateProgreessBar setHidden:YES];
    [rotateProgreessBar displayIfNeeded];
    
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW, kemo_sgl);
    kemoview_step_viewmatrix(IZERO, kemo_sgl);
    [self setRotation:IZERO
                 axis:RotationAxisID
             kemoview:kemo_sgl];
    [_metalView setNeedsDisplay:YES];
}

-(void) PreviewQuiltImages:(struct kemoviewer_type *) kemo_sgl
{
    [_metalViewController refreshKemoViewTripleBuffersForRotation:kemo_sgl];
    kemoview_set_view_integer(ISET_DRAW_MODE, MOVIE_DRAW, kemo_sgl);

    NSInteger num_step
        = (NSInteger) kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_NUM);

    [evolutionProgreessBar setHidden:NO];
    [evolutionProgreessBar setUsesThreadedAnimation:YES];
    [evolutionProgreessBar setMinValue:(double) 0];
    [evolutionProgreessBar setMaxValue:(double) num_step];
    [evolutionProgreessBar startAnimation:self];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    [evolutionProgreessBar displayIfNeeded];
    for(self.CurrentStep = 0;self.CurrentStep<num_step;self.CurrentStep++){
        [_metalView DrawQuilt:(self.CurrentStep+1)
                       degree:IZERO
                         axis:IONE
                     kemoview:kemo_sgl];

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
    
    [_metalView DrawQuilt:IZERO
                   degree:IZERO
                     axis:IONE
                 kemoview:kemo_sgl];
}

-(void) SaveQTmovieEvolution:(struct kemoviewer_type *) kemo_sgl;
{
    NSInteger iframe;
    
    if (CurrentMovieFormat == SAVE_QT_MOVIE){
        if(kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1){
            if([self OpenQuiltQTMovieFile:EvolutionImageFilename
                                 kemoview:kemo_sgl] != 0) return;
        }else{
            [self OpenQTMovieFile:EvolutionImageFilename];
        }
    }
    
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW, kemo_sgl);

    [evolutionProgreessBar setHidden:NO];
    [evolutionProgreessBar setUsesThreadedAnimation:YES];
    [evolutionProgreessBar setMinValue:(double) self.EvolutionStartStep];
    [evolutionProgreessBar setMaxValue:(double) self.EvolutionEndStep];
    [evolutionProgreessBar startAnimation:self];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    [evolutionProgreessBar displayIfNeeded];
    
    int num = self.EvolutionEndStep - self.EvolutionStartStep;
    if(CurrentMovieFormat != NO_SAVE_FILE) self.stepDisplayFlag = 1;
    for(self.CurrentStep = self.EvolutionStartStep;self.CurrentStep<self.EvolutionEndStep+1;self.CurrentStep++){
        self.stepToDisplay = (float) (self.CurrentStep - self.EvolutionStartStep) / (float) num;

        if( ((self.CurrentStep-self.EvolutionStartStep)%self.EvolutionIncrement) == 0) {
            [_metalView DrawEvolution:self.CurrentStep
                             kemoview:kemo_sgl];
            
            if (CurrentMovieFormat == SAVE_QT_MOVIE
                && kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1) {
                iframe = (self.CurrentStep - self.EvolutionStartStep) / self.EvolutionIncrement;
                CMTime frameTime = CMTimeMake((int64_t)iframe, (int) self.FramePerSecond);
                [self AddKemoviewQuiltToMovie:frameTime
                                       degree:IZERO
                                       axis:IONE
                                     kemoview:kemo_sgl];
            }else if(CurrentMovieFormat == SAVE_QT_MOVIE) {
                iframe = (self.CurrentStep - self.EvolutionStartStep) / self.EvolutionIncrement;
                CMTime frameTime = CMTimeMake((int64_t)iframe, (int) self.FramePerSecond);
                [self AddKemoviewImageToMovie:frameTime
                                     kemoview:kemo_sgl];
            } else if (CurrentMovieFormat != 0) {
                NSString *numstring = [NSString stringWithFormat:@".%ld",self.CurrentStep];
                NSString *ImageFilehead =  [EvolutionImageFilehead stringByAppendingString:numstring];
                if (CurrentMovieFormat == SAVE_PNG
                    && kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1) {
                    [self SaveKemoviewQuiltPNGFile:ImageFilehead
                                            degree:IZERO
                                              axis:IONE
                                          kemoview:kemo_sgl];
                } else if(CurrentMovieFormat == SAVE_PNG){
                    [self SaveKemoviewPNGFile:ImageFilehead];
                } else if (CurrentMovieFormat == SAVE_BMP
                           && kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1) {
                    [self SaveKemoviewQuiltBMPFile:ImageFilehead
                                            degree:IZERO
                                              axis:IONE
                                          kemoview:kemo_sgl];
                } else if(CurrentMovieFormat == SAVE_BMP){
                    [self SaveKemoviewBMPFile:ImageFilehead];
                } else if (kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1) {
                    [self SaveKemoviewQuiltPDFFile:ImageFilehead
                                            degree:IZERO
                                              axis:IONE
                                          kemoview:kemo_sgl];
                } else {
                    [self SaveKemoviewPDFFile:ImageFilehead];
                }
            }
            
            [evolutionProgreessBar incrementBy:(double) self.EvolutionIncrement];
            [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
            [evolutionProgreessBar displayIfNeeded];
        }
    }
    self.stepDisplayFlag = 0;

    [evolutionProgreessBar setDoubleValue:(double) self.EvolutionStartStep];
    [evolutionProgreessBar stopAnimation:self];
    [evolutionProgreessBar setHidden:YES];
    [evolutionProgreessBar setDisplayedWhenStopped:NO];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    [evolutionProgreessBar displayIfNeeded];
    
    if (CurrentMovieFormat == SAVE_QT_MOVIE) [self CloseKemoviewMovieFile];
        
    kemoview_step_viewmatrix(IZERO, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}


// ---------------------------------

- (IBAction)SendToClipAsTIFF:(id)sender
{
    NSBitmapImageRep * imageRep = [NSBitmapImageRep alloc];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_metalViewController getRenderedbyMetal:imageRep
                                    kemoview:kemo_sgl];
    
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

- (IBAction)GetRotationMovieFPS:(id)sender
{
    CurrentMovieFormat = 0;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self ShowQTmovieRotation:kemo_sgl
                  numRotation:self.NumTeetRotation];
}

- (IBAction)ShowRotationMovie:(id)sender;
{
	CurrentMovieFormat = 0;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self ShowQTmovieRotation:kemo_sgl
                  numRotation:1];
}

- (IBAction)ShowQuiltMovie:(id)sender;
{
    CurrentMovieFormat = NO_SAVE_FILE;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self PreviewQuiltImages:kemo_sgl];
}

- (IBAction)ShowEvolutionMovie:(id)sender;
{
	CurrentMovieFormat = NO_SAVE_FILE;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SaveQTmovieEvolution:kemo_sgl];
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
                        kemoview:(struct kemoviewer_type *) kemo_sgl
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
    [self SaveQTmovieRotation:kemo_sgl];
}

- (void) SelectEvolutionMovieFile:(NSString *)EvolutionMovieFilename
                         kemoview:(struct kemoviewer_type *) kemo_sgl;

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

    [self SaveQTmovieEvolution:kemo_sgl];

}

- (IBAction)SaveRotationMovie:(id)sender;
{    
    NSSavePanel *RotateImageSavePanelObj = [NSSavePanel savePanel];
    [RotateImageSavePanelObj beginSheetModalForWindow:window 
                                    completionHandler:^(NSInteger RotateImageSaveInt){
        if (RotateImageSaveInt == NSModalResponseOK) {
            NSString *RotateImageFilename = [[ RotateImageSavePanelObj URL] path];
            [RotateImageSavePanelObj orderOut:nil];
            struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
            [self SelectRotationMovieFile:RotateImageFilename
                                 kemoview:kemo_sgl];
        };
                                    }];
}

- (IBAction)SaveEvolutionMovie:(id)sender;
{
	NSSavePanel *EvolutionImageSavePanelObj = [NSSavePanel savePanel];
    [EvolutionImageSavePanelObj beginSheetModalForWindow:window 
                                    completionHandler:^(NSInteger EvolutionImageSaveInt){
	if(EvolutionImageSaveInt == NSModalResponseOK){
		NSString *EvolutionMovieFilename = [[ EvolutionImageSavePanelObj URL] path];
        [EvolutionImageSavePanelObj orderOut:nil];
        struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
        [self SelectEvolutionMovieFile:EvolutionMovieFilename
                              kemoview:kemo_sgl];
	};
                                    }];
}

- (IBAction)SetFramePerSecond:(id)sender;
{
    [_metalView setNeedsDisplay: YES];
    return;
}

- (IBAction)getMovieFormatFlag:(id)sender
{
	NSUserDefaults* defaults = [_movie_defaults_controller defaults];
	MovieFormatFlag = [[defaults stringForKey:@"MovieFormatID"] intValue];
	MovieFormatFlag = [[movieFormat_item selectedCell] tag];
};

- (IBAction)SetEvolutionSteps:(id)sender{
    [_metalView setNeedsDisplay: YES];
    return;
};

- (IBAction)ChooseRotateAxis:(id)sender{
	RotationAxisID = [[rotationAxis selectedCell] tag];
};

@end
