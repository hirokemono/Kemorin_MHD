//
//  KemoMovieControll.m
//  Kemo_Moviemaker
//
//  Created by Hiroaki Matsui on 10/10/26.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "KemoMovieControll.h"
#import <AVFoundation/AVFoundation.h>

@implementation KemoMovieControll;
@synthesize evolutionCurrentStep;
@synthesize evolutionStartStep;
@synthesize evolutionEndStep;
@synthesize evolutionIncrement;
@synthesize evolutionFPS;
@synthesize imageWidth;
@synthesize imageHight;
- (id)init;
{
	self.evolutionCurrentStep = 1;
	self.evolutionStartStep = 1;
	self.evolutionEndStep =   1;
	self.evolutionIncrement = 1;
	self.evolutionFPS = 12;
    
    saveBottun.enabled = NO;
    saveMenu.enabled = NO;
	return self;
}

- (CVPixelBufferRef)pixelBufferFromCGImage:(CGImageRef)image
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

-(void) OpenReferenceImageFile:(NSString *)iFileName
{
    NSUserDefaults *defalts = [NSUserDefaults standardUserDefaults];
    [defalts setObject:iFileName forKey:@"ImageFileName"];
    [defalts synchronize];
    
    SnapshotImage = [[NSImage alloc] initWithContentsOfFile:iFileName];
    CGImageRef CGImage = [SnapshotImage CGImageForProposedRect:nil context:nil hints:nil];
    NSBitmapImageRep *rep = [[NSBitmapImageRep alloc] initWithCGImage:CGImage];
    
    [refImageView setImage:SnapshotImage];
    
    self.imageWidth = [rep pixelsWide];
    self.imageHight = [rep pixelsHigh];
    //    printf("Image size %d, %d",(int)self.imageWidth, (int)self.imageHight);
    [rep release];
    [SnapshotImage release];
    
    saveBottun.enabled = YES;
    saveMenu.enabled = YES;
}

-(void) OpenKemoviewMovieFile:(NSString *)mFileName
{
    // Movie setting
    NSDictionary *outputSettings = 
    @{
      AVVideoCodecKey : AVVideoCodecH264,
      AVVideoWidthKey : @(self.imageWidth),
      AVVideoHeightKey: @(self.imageHight),
      };    
    // source pixel buffer attributes
    NSDictionary *sourcePixBufferAttributes = 
    @{
      (NSString *)kCVPixelBufferPixelFormatTypeKey: @(kCVPixelFormatType_32ARGB),
      (NSString *)kCVPixelBufferWidthKey : @(self.imageWidth),
      (NSString *)kCVPixelBufferHeightKey: @(self.imageHight),
      };

    NSError *overWriteflag = [[NSError alloc] init];
    // Create a QTMovie with a writable data reference
    NSLog(@"EvolutionImageFileName: %@", mFileName);
    NSURL *url = [NSURL fileURLWithPath:mFileName];
 
    //   Coheck if movie file is exist
    if ([[NSFileManager defaultManager] fileExistsAtPath:mFileName])
    {
        NSLog(@"%@ is exist!!!", mFileName);
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

-(IBAction) OpenReferenceImage:(id)pSender;
{
    NSOpenPanel *PsfOpenPanelObj	= [NSOpenPanel openPanel];
    [PsfOpenPanelObj setTitle:@"Choose one of image files"];
    NSInteger PsfOpenInteger	= [PsfOpenPanelObj runModal];
    if(PsfOpenInteger == NSFileHandlingPanelOKButton){
        imageFileName =  [[PsfOpenPanelObj URL] path];
    }
    else { return;};
    
    [self OpenReferenceImageFile:imageFileName];
}

-(IBAction) SaveImageEvolution:(id)pSender
{
    int i, ist, ied, inc;
	//	NSLog(@"SaveRotation received message = %@",(NSString*)[pNotification object]);
	
	NSSavePanel *evolutionImageSavePanelObj	= [NSSavePanel savePanel];
	NSInteger EvolutionSaveInteger	= [evolutionImageSavePanelObj runModal];
	[evolutionImageSavePanelObj setCanSelectHiddenExtension:YES];	

	if(EvolutionSaveInteger == NSFileHandlingPanelOKButton){
		movieFileName = [[evolutionImageSavePanelObj URL] path];
		movieFileHead = [movieFileName stringByDeletingPathExtension];
		movieFileName = [movieFileHead stringByAppendingPathExtension:@"mov"];
	}
	else { return;};
	
    NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
    imageFileName = [defaults stringForKey:@"ImageFileName"];
    imageFileExt =   [imageFileName pathExtension];
    imageFileHead =  [imageFileName stringByDeletingPathExtension];
    imageFileHeadExStep =  [imageFileHead stringByDeletingPathExtension];
    // NSLog(@"Image file header =    %@",imageFileHeadExStep);
    // NSLog(@"Image file extension =    %@",imageFileExt);

    ist = self.evolutionStartStep;
    ied = self.evolutionEndStep;
    inc = self.evolutionIncrement;
    
    [progreessBar setIndeterminate:NO];
    [progreessBar startAnimation:self];
    [progreessBar setMinValue:(double) ist];
    [progreessBar setMaxValue:(double) ied];
    [progreessBar setDoubleValue:(double) ist];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]]; 
    [progreessBar displayIfNeeded];
    
    [self OpenKemoviewMovieFile:movieFileName];    
    CVPixelBufferRef buffer;
    int frameCount = 0;
    for (i = ist;i<(ied+1);i++) {
        self.evolutionCurrentStep = i;
        if( ((i-ist)%inc) == 0) {
            CMTime frameTime = CMTimeMake((int64_t)frameCount, self.evolutionFPS);
            
            imageFileHead =  [imageFileHeadExStep stringByAppendingPathExtension:
                              [NSString stringWithFormat:@"%d",i]];
            imageFileName =  [imageFileHead stringByAppendingPathExtension:imageFileExt];

            SnapshotImage = [[NSImage alloc] initWithContentsOfFile:imageFileName];
            CGImageRef CGImage = [SnapshotImage CGImageForProposedRect:nil context:nil hints:nil];
            buffer = [self pixelBufferFromCGImage:CGImage];

            while (adaptor.assetWriterInput.readyForMoreMediaData == FALSE) {
                printf("WAit at %d\n", i);
                NSDate *maxDate = [NSDate dateWithTimeIntervalSinceNow:0.1];
                [[NSRunLoop currentRunLoop] runUntilDate:maxDate];
            }
            
            // Append Image buffer
            if (![adaptor appendPixelBuffer:buffer withPresentationTime:frameTime]) {
                NSLog(@"Adapter Failure");
            }
            [SnapshotImage release];            
            if (buffer) {CVBufferRelease(buffer);}
            frameCount = frameCount + 1;

            [progreessBar incrementBy:(double)inc];
            [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]]; 
            [progreessBar displayIfNeeded];
        };
    };

    [writerInput markAsFinished];
    [videoWriter finishWritingWithCompletionHandler:^{
        NSLog(@"Finish writing!");
    }];
    CVPixelBufferPoolRelease(adaptor.pixelBufferPool);

    [progreessBar setDoubleValue:(double)ist];
    [progreessBar stopAnimation:self];
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]]; 
    [progreessBar displayIfNeeded];
	return;
}

- (IBAction) SetEvolutionSteps:(id)pSender{
	NSLog(@"start: %d", (int) self.evolutionStartStep);
	NSLog(@"end: %d", (int) self.evolutionEndStep);
	NSLog(@"increment: %d", (int) self.evolutionIncrement);
	NSLog(@"FPS: %d", (int) self.evolutionFPS);
}

@end
