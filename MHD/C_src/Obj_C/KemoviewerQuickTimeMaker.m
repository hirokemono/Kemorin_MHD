//
//  KemoviewerQuickTimeMaker.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "KemoviewerQuickTimeMaker.h"
#include "kemoviewer.h"

@implementation KemoviewerQuickTimeMaker
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

/*  ---------------------------------  */

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

-(void) CloseKemoviewMovieFile{
    [writerInput markAsFinished];
    [videoWriter finishWritingWithCompletionHandler:^{
        NSLog(@"Finish writing!");
    }];
    CVPixelBufferPoolRelease(adaptor.pixelBufferPool);
    [writerInput release];
}

// ---------------------------------

-(void) OpenQTMovieFile:(NSString *)movieFileName{
    [self OpenQTMovieFileWithSize:movieFileName
                            width:[_metalView getHorizontalViewSize]
                           height:[_metalView getVerticalViewSize]];
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

// ---------------------------------

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

-(void) AddKemoviewQuiltToMovie:(CMTime)frameTime
                         degree:(NSInteger) int_degree
                           axis:(NSInteger) rotationaxis
                       kemoview:(struct kemoviewer_type *) kemo_sgl
{
    // Adds an image for the specified duration to the QTMovie
    NSImage *SnapshotImage = [_kemoImageMaker InitMetalQuiltBitmapToImage:int_degree
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


@end
