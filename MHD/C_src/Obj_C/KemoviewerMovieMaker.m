//
//  KemoviewerMovieMaker.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/01.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "KemoviewerMovieMaker.h"
#include "kemoviewer.h"

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

-(void) SaveQTMovieRotation:(struct kemoviewer_type *) kemo_sgl;
{
    NSInteger ied_deg = 360/self.RotationIncrement;
    NSInteger int_degree, icount;
    
    [_metalViewController refreshKemoViewTripleBuffersForRotation:kemo_sgl];

    if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [_KemoviewQTMaker OpenQTMovieFile:RotateImageFilenameNoStep];
    }
    
    if(CurrentMovieFormat != NO_SAVE_FILE) self.stepDisplayFlag = 1;
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    for(icount = 0;icount<ied_deg;icount++){
        int_degree = (icount * self.RotationIncrement);
        self.CurrentStep = icount;
        self.stepToDisplay = (float) icount / (float) (ied_deg - 1);
        [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
        [self setRotation:int_degree
                     axis:RotationAxisID
                 kemoview:kemo_sgl];

        if(CurrentMovieFormat == SAVE_QT_MOVIE) {
            CMTime frameTime = CMTimeMake((int64_t)icount, (int) self.FramePerSecond);
            [_KemoviewQTMaker AddKemoviewImageToMovie:frameTime
                                             kemoview:kemo_sgl];
        } else if (CurrentMovieFormat != 0) {
            NSString *numstring = [NSString stringWithFormat:@".%ld",icount];
            NSString *ImageFilehead =  [RotateImageFilehead stringByAppendingString:numstring];
            [_kemoImageMaker SalectSaveKemoviewImageFile:CurrentMovieFormat
                                              filePrefix:ImageFilehead];
        }else if(icount % 20 != 0){
            [_metalView draw];
        }
        if(icount % 20 == 0) [_metalView draw];
    }
    self.stepDisplayFlag = 0;
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];

    if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [_KemoviewQTMaker CloseKemoviewMovieFile];
    }
    
    kemoview_step_viewmatrix(IZERO, kemo_sgl);
    [self setRotation:IZERO
                 axis:RotationAxisID
             kemoview:kemo_sgl];
    [_metalView setNeedsDisplay:YES];
}

-(void) SaveQuiltMovieRotation:(struct kemoviewer_type *) kemo_sgl;
{
    NSInteger ied_deg = 360/self.RotationIncrement;
    NSInteger int_degree, icount;
    
    [_metalViewController refreshKemoViewTripleBuffersForRotation:kemo_sgl];

    if (CurrentMovieFormat == SAVE_QT_MOVIE){
        if([_KemoviewQTMaker OpenQuiltQTMovieFile:RotateImageFilenameNoStep
                                         kemoview:kemo_sgl] != 0) return;
    }
    
    if(CurrentMovieFormat != NO_SAVE_FILE) self.stepDisplayFlag = 1;
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    for(icount = 0;icount<ied_deg;icount++){
        int_degree = (icount * self.RotationIncrement);
        self.CurrentStep = icount;
        self.stepToDisplay = (float) icount / (float) (ied_deg - 1);
        [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
        [self setRotation:int_degree
                     axis:RotationAxisID
                 kemoview:kemo_sgl];

        if (CurrentMovieFormat == SAVE_QT_MOVIE){
            CMTime frameTime = CMTimeMake((int64_t)icount, (int) self.FramePerSecond);
            [_KemoviewQTMaker AddKemoviewQuiltToMovie:frameTime
                                               degree:int_degree
                                                 axis:RotationAxisID
                                             kemoview:kemo_sgl];
        } else if (CurrentMovieFormat != 0) {
            NSString *numstring = [NSString stringWithFormat:@".%ld",icount];
            NSString *ImageFilehead =  [RotateImageFilehead stringByAppendingString:numstring];
            [_kemoImageMaker SalectSaveKemoQuiltImageFile:CurrentMovieFormat
                                               filePrefix:ImageFilehead
                                                   degree:int_degree
                                                     axis:RotationAxisID
                                                 kemoview:kemo_sgl];
        }else if(icount % 20 != 0){
            [_metalView draw];
        }
        if(icount % 20 == 0) [_metalView draw];
    }
    self.stepDisplayFlag = 0;
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];

    if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [_KemoviewQTMaker CloseKemoviewMovieFile];
    }
    
    kemoview_step_viewmatrix(IZERO, kemo_sgl);
    [self setRotation:IZERO
                 axis:RotationAxisID
             kemoview:kemo_sgl];
    [_metalView setNeedsDisplay:YES];
}

-(void) ShowMetalRotation:(struct kemoviewer_type *) kemo_sgl
              numRotation:(NSInteger) rotCounts;
{
    struct timeval startwtime;
    struct timeval endwtime;
    double seq_time;
    double accum_time = 0.0;

    NSInteger ied_deg = 1 + rotCounts * 360/self.RotationIncrement;
    NSInteger int_degree, icount;
    
    [_metalViewController refreshKemoViewTripleBuffersForRotation:kemo_sgl];
    kemoview_set_view_integer(ISET_DRAW_MODE, MOVIE_DRAW, kemo_sgl);

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
    }
    
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

    for(self.CurrentStep = 0;self.CurrentStep<num_step;self.CurrentStep++){
        [_metalView DrawQuilt:(self.CurrentStep+1)
                       degree:IZERO
                         axis:IONE
                     kemoview:kemo_sgl];
    }
    
    [_metalView DrawQuilt:IZERO
                   degree:IZERO
                     axis:IONE
                 kemoview:kemo_sgl];
}

-(void) SaveQTmovieEvolution:(struct kemoviewer_type *) kemo_sgl;
{
    NSInteger iframe;
    
    if (CurrentMovieFormat == SAVE_QT_MOVIE){
            [_KemoviewQTMaker OpenQTMovieFile:EvolutionImageFilename];
    }
    
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW, kemo_sgl);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_step_viewmatrix(IZERO, kemo_sgl);

    int num = self.EvolutionEndStep - self.EvolutionStartStep;
    self.stepDisplayFlag = 1;
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    for(self.CurrentStep = self.EvolutionStartStep;self.CurrentStep<self.EvolutionEndStep+1;self.CurrentStep++){
        self.stepToDisplay = (float) (self.CurrentStep - self.EvolutionStartStep) / (float) num;
        [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];

        if( ((self.CurrentStep-self.EvolutionStartStep)%self.EvolutionIncrement) == 0) {
            kemoview_viewer_evolution((int) self.CurrentStep, kemo_sgl);
            
            if(CurrentMovieFormat == SAVE_QT_MOVIE) {
                iframe = (self.CurrentStep - self.EvolutionStartStep) / self.EvolutionIncrement;
                CMTime frameTime = CMTimeMake((int64_t)iframe, (int) self.FramePerSecond);
                [_KemoviewQTMaker AddKemoviewImageToMovie:frameTime
                                                 kemoview:kemo_sgl];
            } else if (CurrentMovieFormat != 0) {
                NSString *numstring = [NSString stringWithFormat:@".%ld",self.CurrentStep];
                NSString *ImageFilehead =  [EvolutionImageFilehead stringByAppendingString:numstring];
                [_kemoImageMaker SalectSaveKemoviewImageFile:CurrentMovieFormat
                                                  filePrefix:ImageFilehead];
            }else if(self.CurrentStep % 20 != 0){
                [_metalView draw];
            }
            if(self.CurrentStep % 20 == 0) [_metalView draw];
        }
    }
    self.stepDisplayFlag = 0;
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];

    if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [_KemoviewQTMaker CloseKemoviewMovieFile];
    }
        
    kemoview_viewer_evolution((int) self.EvolutionStartStep, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

-(void) SaveQuiltMovieEvolution:(struct kemoviewer_type *) kemo_sgl;
{
    NSInteger iframe;
    
    if (CurrentMovieFormat == SAVE_QT_MOVIE){
        if([_KemoviewQTMaker OpenQuiltQTMovieFile:EvolutionImageFilename
                                         kemoview:kemo_sgl] != 0) return;
    }
    
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW, kemo_sgl);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO, kemo_sgl);
    kemoview_step_viewmatrix(IZERO, kemo_sgl);

    int num = self.EvolutionEndStep - self.EvolutionStartStep;
    self.stepDisplayFlag = 1;
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];
    for(self.CurrentStep = self.EvolutionStartStep;self.CurrentStep<self.EvolutionEndStep+1;self.CurrentStep++){
        self.stepToDisplay = (float) (self.CurrentStep - self.EvolutionStartStep) / (float) num;
        [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];

        if( ((self.CurrentStep-self.EvolutionStartStep)%self.EvolutionIncrement) == 0) {
            kemoview_viewer_evolution((int) self.CurrentStep, kemo_sgl);

            if (CurrentMovieFormat == SAVE_QT_MOVIE == 1) {
                iframe = (self.CurrentStep - self.EvolutionStartStep) / self.EvolutionIncrement;
                CMTime frameTime = CMTimeMake((int64_t)iframe, (int) self.FramePerSecond);
                [_KemoviewQTMaker AddKemoviewQuiltToMovie:frameTime
                                                   degree:IZERO
                                                     axis:IONE
                                                 kemoview:kemo_sgl];
             } else if (CurrentMovieFormat != 0) {
                NSString *numstring = [NSString stringWithFormat:@".%ld",self.CurrentStep];
                NSString *ImageFilehead =  [EvolutionImageFilehead stringByAppendingString:numstring];
                [_kemoImageMaker SalectSaveKemoQuiltImageFile:CurrentMovieFormat
                                                   filePrefix:ImageFilehead
                                                       degree:IZERO
                                                         axis:IONE
                                                     kemoview:kemo_sgl];
             }else if(self.CurrentStep % 20 != 0){
                 [_metalView draw];
             }
             if(self.CurrentStep % 20 == 0) [_metalView draw];
        }
    }
    self.stepDisplayFlag = 0;
    [[NSRunLoop mainRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.0]];

    if (CurrentMovieFormat == SAVE_QT_MOVIE){
        [_KemoviewQTMaker CloseKemoviewMovieFile];
    }
        
    kemoview_viewer_evolution((int) self.EvolutionStartStep, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}


// ---------------------------------

- (IBAction)SendToClipAsTIFF:(id)sender
{
    [_kemoImageMaker SendImageToClipboardAsTIFF];
}

// ---------------------------------

- (IBAction)GetRotationMovieFPS:(id)sender
{
    CurrentMovieFormat = 0;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self ShowMetalRotation:kemo_sgl
                numRotation:self.NumTeetRotation];
}

- (IBAction)ShowRotationMovie:(id)sender;
{
    CurrentMovieFormat = 0;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self ShowMetalRotation:kemo_sgl
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
    
    if(kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1){
        [self SaveQuiltMovieEvolution:kemo_sgl];
    }else{
        [self SaveQTmovieEvolution:kemo_sgl];
    }
}


- (void) SelectRotationMovieFile:(NSString *)RotateImageFilename
                        kemoview:(struct kemoviewer_type *) kemo_sgl
{
    NSUserDefaults* defaults = [_movie_defaults_controller defaults];

    NSString *RotateImageFileext = [RotateImageFilename pathExtension];
    RotateImageFilehead = [RotateImageFilename stringByDeletingPathExtension];
    
    CurrentMovieFormat = [_kemoImageMaker SetImageFileFormatID:RotateImageFileext];
    
    if(CurrentMovieFormat == SAVE_UNDEFINED){
        CurrentMovieFormat = [[defaults stringForKey:@"MovieFormatID"] intValue];
    }
    if(CurrentMovieFormat == SAVE_QT_MOVIE){
        RotateImageFilenameNoStep = [RotateImageFilehead stringByAppendingPathExtension:@"mov"];
    }
    
    kemoview_set_view_integer(ISET_DRAW_MODE, MOVIE_DRAW, kemo_sgl);
    if(kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1){
        [self SaveQuiltMovieRotation:kemo_sgl];
    }else{
        [self SaveQTMovieRotation:kemo_sgl];
    }
}

- (void) SelectEvolutionMovieFile:(NSString *)EvolutionMovieFilename
                         kemoview:(struct kemoviewer_type *) kemo_sgl;

{
    NSUserDefaults* defaults = [_movie_defaults_controller defaults];
    NSString * EvolutionImageFileext =   [EvolutionMovieFilename pathExtension];
    EvolutionImageFilehead = [EvolutionMovieFilename stringByDeletingPathExtension];
    CurrentMovieFormat = [_kemoImageMaker SetImageFileFormatID:EvolutionImageFileext];
    
    if(CurrentMovieFormat == SAVE_UNDEFINED){
        CurrentMovieFormat = [[defaults stringForKey:@"MovieFormatID"] intValue];
    }
    if(CurrentMovieFormat == SAVE_QT_MOVIE){
        EvolutionImageFilename = [EvolutionImageFilehead stringByAppendingPathExtension:@"mov"];
    }

    if(kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1){
        [self SaveQuiltMovieEvolution:kemo_sgl];
    }else{
        [self SaveQTmovieEvolution:kemo_sgl];
    }
    return;
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
