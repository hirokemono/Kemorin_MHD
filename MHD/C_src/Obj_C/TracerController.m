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

@synthesize tracerVectorMenuActive;
@synthesize DrawTracerVectorFlag;
@synthesize TracerVectorIncrement;
@synthesize TracerVectorIncDigit;
@synthesize TracerScaleVector;
@synthesize TracerScaleDigit;
@synthesize TracerColorbarSwitch;
- (id)init;
{
    self.TracerRadiusFactor = 1;
    self.TracerRadiusDigit = -2;
    self.tracerVectorMenuActive = 0;

    self.tracerVectorMenuActive = 0;

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
    
    [_controlTabView selectTabViewItemAtIndex:TRACER_RENDERING];
    [self CopyTracerDisplayFlagsFromC:kemo_sgl];
    
    self.currentTracerStep = [_kemoviewControl SetCurrentPSFFile:TRACER_RENDERING
                                                        kemoview:kemo_sgl
                                                        pathTree:_tracerPathControl];

    id_viewtype = kemoview_get_view_type_flag(kemo_sgl);
    [_kemoviewControl SetViewTypeMenu:id_viewtype
                             kemoview:kemo_sgl];
    [_kemoviewControl Set3DView:kemo_sgl];
    [_rgbaMapObject UpdateColormapView:kemo_sgl];
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
    int current_model = (int) [_kemoviewControl CurrentControlModel];
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
    
    kemoview_set_VIZ_field_param((int) isel,
                                 TRACER_RENDERING,
                                 FIELD_SEL_FLAG,
                                 kemo_sgl);
    
    [self setSelectedTracerComponentRanges:kemo_sgl];
    [_rgbaMapObject UpdateColormapView:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) TracerComponentAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_field_param((int) [_TracerComponentMenu indexOfSelectedItem],
                                 TRACER_RENDERING,
                                 COMPONENT_SEL_FLAG,
                                 kemo_sgl);
    [self setSelectedTracerComponentRanges:kemo_sgl];
    [_rgbaMapObject UpdateColormapView:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (void) SetDrawTracerVector:(struct kemoviewer_type *) kemo_sgl
{
    kemoview_set_VIZ_vector_draw_flags((int) self.DrawTracerVectorFlag,
                                       TRACER_RENDERING, kemo_sgl);
    
    if(self.DrawTracerVectorFlag == 0) {[_TracerVectorSwitchOutlet setTitle:@"Off"];}
    else{ [_TracerVectorSwitchOutlet setTitle:@"On"];};
}

- (void) SetFlineDataRanges:(NSInteger)isel
                   kemoview:(struct kemoviewer_type *) kemo_sgl
{
    double current_value;
    int current_digit;

    int n_field =  kemoview_get_VIZ_field_param(kemo_sgl,
                                                TRACER_RENDERING,
                                                NUM_FIELD_FLAG);
    long num_comp = kemoview_get_VIZ_num_component(kemo_sgl,
                                                   TRACER_RENDERING,
                                                   (int) isel);
    if(num_comp == 3){
        self.tracerVectorMenuActive = 1;
    } else {
        self.tracerVectorMenuActive = 0;
        self.DrawTracerVectorFlag = 0;
        [self SetDrawTracerVector:kemo_sgl];
    };
    
    if (n_field > 0) {
        [self setSelectedTracerComponentRanges:kemo_sgl];
    }
    
    kemoview_get_VIZ_color_w_exp(kemo_sgl,
                                 TRACER_RENDERING, ISET_WIDTH,
                                 &current_value, &current_digit);
    self.TracerRadiusFactor = (CGFloat) current_value;
    self.TracerRadiusDigit = (CGFloat) current_digit;

    
    self.DrawTracerVectorFlag = kemoview_get_VIZ_vector_draw_flags(kemo_sgl,
                                                                   TRACER_RENDERING);
    kemoview_get_VIZ_vector_w_exp(kemo_sgl,
                                  TRACER_RENDERING,
                                  ISET_PSF_REFVECT,
                                  &current_value, &current_digit);
    self.TracerScaleVector =      (CGFloat) current_value;
    self.TracerScaleDigit =       (CGFloat) current_digit;

    kemoview_get_VIZ_vector_w_exp(kemo_sgl,
                                  TRACER_RENDERING,
                                  ISET_VECTOR_INC,
                                  &current_value, &current_digit);
    self.TracerVectorIncrement = (CGFloat) current_value;
    self.TracerVectorIncDigit =  (CGFloat) current_digit;
    
    return;
}

- (int) TracerColorbarSwitchStatus
{
    return (int) self.TracerColorbarSwitch;
}
- (void) setTracerColorbarSwitchStatus:(int) isel
{
    self.TracerColorbarSwitch = isel;
}


- (IBAction)ChooseTracerColorAction:(id)sender
{
    NSInteger tag = [[TracerColorItem selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_patch_color_mode((int) tag,
                                      TRACER_RENDERING,
                                      kemo_sgl);
    
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

- (IBAction)SetTracerRadiusAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_color_value_w_exp(TRACER_RENDERING, ISET_WIDTH,
                                       (double) self.TracerRadiusFactor,
                                       (int) self.TracerRadiusDigit,
                                       kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)DrawTracerVectorAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetDrawTracerVector:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetTracerReferenceVector:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_each_VIZ_vector_w_exp(ISET_PSF_REFVECT,
                                      (double) self.TracerScaleVector,
                                      (int) self.TracerScaleDigit,
                                      TRACER_RENDERING, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetTracerVectorIncrement:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_each_VIZ_vector_w_exp(ISET_VECTOR_INC,
                                      (double) self.TracerVectorIncrement,
                                      (int) self.TracerVectorIncDigit,
                                      TRACER_RENDERING, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetFlineColorAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_psfController SetPSFColorFromColorWell:kemo_sgl
                                   colorwell:_tracerColorWell];
    [_metalView UpdateImage:kemo_sgl];
}

@end
