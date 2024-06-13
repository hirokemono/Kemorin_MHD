//
//  TracerController.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/08/17.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "TracerController.h"
#include "kemoviewer.h"


@implementation TracerController

@synthesize currentTracerStep;
@synthesize DrawTracerFlag;
@synthesize TracerMinimumValue;
@synthesize TracerMaximumValue;
@synthesize TracerColorMinimum;
@synthesize TracerColorMaximum;
@synthesize TracerColorMinDigit;
@synthesize TracerColorMaxDigit;
@synthesize Tracertype;
@synthesize TracerRadiusFactor;
@synthesize TracerRadiusDigit;
- (id)init;
{
    self.TracerRadiusFactor = 1;
    self.TracerRadiusDigit = -2;

    FieldlineColor =      [NSNumber alloc];
    
    return self;
}

- (id)dealloc
{
    [FieldlineColor      dealloc];

    [super dealloc];
    return self;
}

-(void) awakeFromNib
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
//    self.Tracertype = kemoview_get_line_type_flag(kemo_sgl);
    return;
}

- (id) CopyTracerDisplayFlagsFromC:(struct kemoviewer_type *) kemo_sgl
{
    self.DrawTracerFlag = kemoview_get_VIZ_draw_flags(kemo_sgl,
                                                      TRACER_RENDERING);
    [_psfController SetFieldMenuItems:TRACER_RENDERING
                             kemoview:kemo_sgl
                            fieldMenu:_TracerFieldMenu];
    [_psfController SetComponentMenuItems:0
                              activeModel:TRACER_RENDERING
                                kemoview:kemo_sgl
                            componentMenu:_TracerComponentMenu];
    [self SetFlineDataRanges:0
                    kemoview:kemo_sgl];
    return self;
}

- (void) OpenTracerFile:(NSString*) TracerFilehead
               kemoview:(struct kemoviewer_type *) kemo_sgl
{
    int id_viewtype;
    
    [self CopyTracerDisplayFlagsFromC:kemo_sgl];
    
    self.currentTracerStep = [_psfController SetCurrentPSFFile:TRACER_RENDERING
                                                      kemoview:kemo_sgl
                                                      pathTree:_tracerPathControl];

    id_viewtype = kemoview_get_view_type_flag(kemo_sgl);
    [_kemoviewControl SetViewTypeMenu:id_viewtype
                             kemoview:kemo_sgl];
    [_kemoviewControl Set3DView:kemo_sgl];
};

- (IBAction) DrawTracerFile:(id)pId{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];

    NSArray *flineFileTypes = [NSArray arrayWithObjects:@"inp",@"vtk",@"gz",@"INP",@"VTK",@"GZ",nil];
    NSOpenPanel *flineOpenPanelObj    = [NSOpenPanel openPanel];
    [flineOpenPanelObj setTitle:@"Choose field line data"];
    [flineOpenPanelObj setAllowedFileTypes:flineFileTypes];
    [flineOpenPanelObj beginSheetModalForWindow:window
                              completionHandler:^(NSInteger FlineOpenInteger){

        if(FlineOpenInteger == NSModalResponseOK){
//            NSString *TracerOpenDirectory = [[flineOpenPanelObj directoryURL] path];
            NSString *OpenFilename =        [[flineOpenPanelObj URL] path];
            NSString *OpenFileExtention =   [OpenFilename pathExtension];
            NSString *OpenFilPerefix =      [OpenFilename stringByDeletingPathExtension];
            if([OpenFileExtention isEqualToString:@"gz"]
               || [OpenFileExtention isEqualToString:@"GZ"]){
                OpenFilPerefix = [OpenFilPerefix stringByDeletingPathExtension];
            };
        
            struct kv_string *filename = kemoview_init_kvstring_by_string([OpenFilename UTF8String]);
            int iflag_datatype =  kemoview_open_data(filename, kemo_sgl);
            kemoview_free_kvstring(filename);
        
            if(iflag_datatype == IFLAG_POINTS){
                [self OpenTracerFile:(NSString *)OpenFilPerefix
                            kemoview:kemo_sgl];
            }
        };
    }];
    NSInteger WindowExpandFlag = kemoview_check_all_VIZ_draw_flags(kemo_sgl);
    [_ElasticControl UpdateWindow:WindowExpandFlag];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) CloseTracerFile:(id)pId{
    int current_model = [_kemoviewControl CurrentControlModel];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_close_tracer_view(kemo_sgl);
    [self CopyTracerDisplayFlagsFromC:kemo_sgl];
    
    NSInteger WindowExpandFlag = kemoview_check_all_VIZ_draw_flags(kemo_sgl);
    [_ElasticControl UpdateWindow:WindowExpandFlag];
    [_metalView UpdateImage:kemo_sgl];
}

- (void) setSelectedTracerComponentRanges:(struct kemoviewer_type *) kemo_sgl
{
    double dataMin, dataMax;
    double cmapMinValue, cmapMaxValue;
    int cmapMinDigit, cmapMaxDigit;
    SetDataRanges(TRACER_RENDERING, kemo_sgl, &dataMin, &dataMax,
                  &cmapMinValue, &cmapMinDigit, &cmapMaxValue, &cmapMaxDigit);
    self.TracerMinimumValue = (CGFloat) dataMin;
    self.TracerMaximumValue = (CGFloat) dataMax;
	self.TracerColorMinimum =   (CGFloat) cmapMinValue;
	self.TracerColorMinDigit = (CGFloat) cmapMinDigit;
	self.TracerColorMaximum =   (CGFloat) cmapMaxValue;
	self.TracerColorMaxDigit = (CGFloat) cmapMaxDigit;
}

- (IBAction) TracerFieldAction:(id)sender
{
    NSInteger isel = [_TracerFieldMenu indexOfSelectedItem];

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_psfController SetComponentMenuItems:isel
                              activeModel:TRACER_RENDERING
                                kemoview:kemo_sgl
                            componentMenu:_TracerComponentMenu];
    [self SetFlineDataRanges:isel
                    kemoview:kemo_sgl];
    
    kemoview_set_VIZ_field_param(TRACER_RENDERING,
                                 FIELD_SEL_FLAG,
                                 (int) isel, kemo_sgl);
    
    [self setSelectedTracerComponentRanges:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) TracerComponentAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_field_param(TRACER_RENDERING,
                                 COMPONENT_SEL_FLAG,
                                 (int) [_TracerComponentMenu indexOfSelectedItem],
                                 kemo_sgl);
    [self setSelectedTracerComponentRanges:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (void) SetFlineDataRanges:(NSInteger)isel
                   kemoview:(struct kemoviewer_type *) kemo_sgl
{
    double current_thick;
    int current_digit;

    int n_field =  kemoview_get_VIZ_field_param(kemo_sgl,
                                                TRACER_RENDERING,
                                                NUM_FIELD_FLAG);
    if (n_field > 0) {
        [self setSelectedTracerComponentRanges:kemo_sgl];
    }
    
    kemoview_get_VIZ_color_w_exp(kemo_sgl,
                                 TRACER_RENDERING, ISET_WIDTH,
                                 &current_thick, &current_digit);
    self.TracerRadiusFactor = (CGFloat) current_thick;
    self.TracerRadiusDigit = (CGFloat) current_digit;

    return;
}

- (IBAction)ChooseTracerColorAction:(id)sender;
{
    NSInteger tag = [[TracerColorItem selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_patch_color_mode(TRACER_RENDERING,
                                      (int) tag, kemo_sgl);
    
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) ShowTracerRange:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_linear_colormap(self.TracerColorMinimum,
                                 (int) self.TracerColorMinDigit,
                                 self.TracerColorMaximum,
                                 (int) self.TracerColorMaxDigit,
                                 TRACER_RENDERING,
                                kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChooseFieldlineTypeAction:(id)sender;
{
    self.Tracertype = [[_flinetype_matrix selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_line_type_flag((int) self.Tracertype, kemo_sgl);
    
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetTracerRadiusAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_color_value_w_exp(TRACER_RENDERING, ISET_WIDTH,
                                       (double) self.TracerRadiusFactor,
                                       (int) self.TracerRadiusDigit,
                                       kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetFlineColorAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetPSFColorFromColorWell:kemo_sgl
                         colorwell:_tracerColorWell];
    [_metalView UpdateImage:kemo_sgl];
}

@end
