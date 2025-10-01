//
//  KemoviewerImageMaker.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "KemoviewerImageMaker.h"
#include "kemoviewer.h"

// NSImage *SnapshotImage;
NSData *SnapshotData;

@implementation KemoviewerImageMaker


- (NSInteger) SetImageFileFormatID:(NSString *)FileExtension
{
    NSInteger id_format;

    if ([FileExtension isEqualToString:@"mov"]
        || [FileExtension isEqualToString:@"MOV"]
        || [FileExtension isEqualToString:@"mp4"]
        || [FileExtension isEqualToString:@"MP4"]
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


- (void)SendImageToClipboardAsTIFF
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
    NSBitmapImageRep * imageRep = [NSBitmapImageRep alloc];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_metalViewController getRenderedbyMetal:imageRep
                                    kemoview:kemo_sgl];

    NSImage * image = [[NSImage alloc] init];
    NSRect rectView = [_metalView convertRectToBacking:[_metalView bounds]];
    [image initWithSize:NSSizeFromCGSize(rectView.size)];
    [image addRepresentation:imageRep];

    NSRect vFrame = NSZeroRect;
    vFrame.size = [image size];
    NSImageView *myView = [[NSImageView alloc] initWithFrame:vFrame];
    [myView setImage:image];
    NSData *pdfData = [myView dataWithPDFInsideRect:vFrame];
    [pdfData retain];

    NSString *filename =  [ImageFilehead stringByAppendingString:@".pdf"];
    [pdfData writeToFile:filename atomically:YES];
    [pdfData release];
    [imageRep release];
    [image release];
}


-(void) SalectSaveKemoviewImageFile:(NSInteger) id_format
                         filePrefix:(NSString*) ImageFilehead
{
    if(id_format == SAVE_PNG){
        [self SaveKemoviewPNGFile:ImageFilehead];
    } else if(id_format == SAVE_BMP){
        [self SaveKemoviewBMPFile:ImageFilehead];
    } else {
        [self SaveKemoviewPDFFile:ImageFilehead];
    }
    return;
}

/*  ---------------------------------  */

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
    int istep;
    for(istep=0;istep<num_step;istep++){
        [_metalView DrawQuilt:(istep+1)
                       degree:int_degree
                         axis:rotationaxis
                     kemoview:kemo_sgl];

        unsigned char *bgra = [_metalViewController getRenderedbyMetalToBGRA:(NSUInteger *) pix_xy
                                                                PixelPerByte:(NSUInteger *) pixelByte
                                                                    kemoview:kemo_sgl];
        kemoview_add_bgra_to_quilt(kemo_sgl, istep, (int) pix_xy[0], (int) pix_xy[1],
                                   bgra, [SnapshotBitmapRep bitmapData]);
        free(bgra);
    }
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


-(void) SalectSaveKemoQuiltImageFile:(NSInteger) id_format
                          filePrefix:(NSString*) ImageFilehead
                              degree:(NSInteger) int_degree
                                axis:(NSInteger) rotationaxis
                            kemoview:(struct kemoviewer_type *) kemo_sgl
{
    if(id_format == SAVE_PNG){
        [self SaveKemoviewQuiltPNGFile:ImageFilehead
                                degree:int_degree
                                   axis:rotationaxis
                               kemoview:kemo_sgl];
    } else if(id_format == SAVE_BMP){
        [self SaveKemoviewQuiltBMPFile:ImageFilehead
                                degree:int_degree
                                  axis:rotationaxis
                              kemoview:kemo_sgl];
    } else {
        [self SaveKemoviewQuiltPDFFile:ImageFilehead
                                degree:int_degree
                                  axis:rotationaxis
                              kemoview:kemo_sgl];
    }
    return;
}

@end
