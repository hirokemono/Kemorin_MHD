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

-(void) OpenKemoviewMovieFile:(NSString *)mFileName
{
    NSError *overWriteflag = [[NSError alloc] init];
    // Create a QTMovie with a writable data reference
    NSLog(@"EvolutionImageFileName: %@", mFileName);
    NSURL *url = [NSURL fileURLWithPath:mFileName];
    
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

    // mark the movie as editable
//    [mMovie setAttribute:[NSNumber numberWithBool:YES] forKey:QTMovieFlatten];
//    duration = QTMakeTime(1, self.evolutionFPS);
}

-(void) InitMovieInput:(NSString *)iFileName
{
    SnapshotImage = [[NSImage alloc] initWithContentsOfFile:imageFileName];
    CGImageRef CGImage = [SnapshotImage CGImageForProposedRect:nil context:nil hints:nil];
    NSBitmapImageRep *rep = [[NSBitmapImageRep alloc] initWithCGImage:CGImage];
    imageWidth = [rep pixelsWide];
    imageHight = [rep pixelsHigh];
    printf("Image size %d, %d",(int)imageWidth, (int)imageHight);
    [rep release];
    [SnapshotImage release];
    
    // Movie setting
    NSDictionary *outputSettings = 
  @{
    AVVideoCodecKey : AVVideoCodecH264,
    AVVideoWidthKey : @(imageWidth),
    AVVideoHeightKey: @(imageHight),
    };    
    // Construct Initilize writer
    writerInput = [AVAssetWriterInput assetWriterInputWithMediaType:AVMediaTypeVideo outputSettings:outputSettings];
    [videoWriter addInput:writerInput];
     writerInput.expectsMediaDataInRealTime = YES;
   
    
    // source pixel buffer attributes
    NSDictionary *sourcePixBufferAttributes = 
    @{
      (NSString *)kCVPixelBufferPixelFormatTypeKey: @(kCVPixelFormatType_32ARGB),
      (NSString *)kCVPixelBufferWidthKey : @(imageWidth),
      (NSString *)kCVPixelBufferHeightKey: @(imageHight),
      };
    // Construct writer input pixel buffer adaptor
    adaptor = [AVAssetWriterInputPixelBufferAdaptor
                 assetWriterInputPixelBufferAdaptorWithAssetWriterInput:writerInput
                 sourcePixelBufferAttributes:sourcePixBufferAttributes];

    if (![videoWriter startWriting]) {
        printf("Error!");
    }

    // Start movie generation
    [videoWriter startSessionAtSourceTime:kCMTimeZero];
}

-(void) CloseKemoviewMovieFile{
    [videoWriter release];
}

-(void) ImageToMovie{
    SnapshotImage = [[NSImage alloc] initWithContentsOfFile:imageFileName];
    [SnapshotImage release];
    return;
}

-(IBAction) SaveImageEvolution:(id)pSender
{
    int i, ist, ied, inc;
	//	NSLog(@"SaveRotation received message = %@",(NSString*)[pNotification object]);
	
	NSOpenPanel *PsfOpenPanelObj	= [NSOpenPanel openPanel];
	[PsfOpenPanelObj setTitle:@"Choose one of image files"];
	NSInteger PsfOpenInteger	= [PsfOpenPanelObj runModal];
	if(PsfOpenInteger == NSFileHandlingPanelOKButton){
		imageFileName =  [[PsfOpenPanelObj URL] path];
		imageFileExt =   [imageFileName pathExtension];
		imageFileHead =  [imageFileName stringByDeletingPathExtension];
		imageFileHeadExStep =  [imageFileHead stringByDeletingPathExtension];
		// NSLog(@"PSF file name =      %@",PsfOpenFilename);
		// NSLog(@"PSF file header =    %@",PsfOpenFilehead);
	}
	else { return;};
	
	NSSavePanel *evolutionImageSavePanelObj	= [NSSavePanel savePanel];
	NSInteger EvolutionSaveInteger	= [evolutionImageSavePanelObj runModal];
	[evolutionImageSavePanelObj setCanSelectHiddenExtension:YES];	

	if(EvolutionSaveInteger == NSFileHandlingPanelOKButton){
		movieFileName = [[evolutionImageSavePanelObj URL] path];
		movieFileHead = [movieFileName stringByDeletingPathExtension];
		movieFileName = [movieFileHead stringByAppendingPathExtension:@"mov"];
	}
	else { return;};
	
    ist = self.evolutionStartStep;
    ied = self.evolutionEndStep;
    inc = self.evolutionIncrement;
    
    [progreessBar setIndeterminate:NO];
    [progreessBar startAnimation:(id)pSender];
    [self OpenKemoviewMovieFile:movieFileName];

    imageFileHead =  [imageFileHeadExStep stringByAppendingPathExtension:
                      [NSString stringWithFormat:@"%d",ist]];
    imageFileName =  [imageFileHead stringByAppendingPathExtension:imageFileExt];
    [self InitMovieInput:imageFileName];
    
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
            
            // Append Image buffer
            if (![adaptor appendPixelBuffer:buffer withPresentationTime:frameTime]) {
                NSLog(@"Adapter Failure");
            }
            [SnapshotImage release];            
            if (buffer) {CVBufferRelease(buffer);}
            frameCount = frameCount + 1;

            [progreessBar incrementBy:(double)inc];
            [progreessBar displayIfNeeded];
        };
    };

    [writerInput markAsFinished];
    [videoWriter finishWritingWithCompletionHandler:^{
        NSLog(@"Finish writing!");
    }];
    CVPixelBufferPoolRelease(adaptor.pixelBufferPool);

    [progreessBar setDoubleValue:(double)ist];
    [progreessBar displayIfNeeded];
	return;
}

- (IBAction)SetEvolutionSteps:(id)pSender{
	NSLog(@"start: %d", (int) self.evolutionStartStep);
	NSLog(@"end: %d", (int) self.evolutionEndStep);
	NSLog(@"increment: %d", (int) self.evolutionIncrement);
	NSLog(@"FPS: %d", (int) self.evolutionFPS);
}

@end
