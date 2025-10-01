//
//  FlineController.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/08/17.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "FlineController.h"
#include "kemoviewer.h"


@implementation FlineController

@synthesize FlineWindowlabel;

@synthesize currentFlineStep;
@synthesize DrawFlineFlag;
@synthesize FlineMinimumValue;
@synthesize FlineMaximumValue;
@synthesize FlineDisplayMinimum;
@synthesize FlineDisplayMaximum;
@synthesize FlineDisplayMinDigit;
@synthesize FlineDisplayMaxDigit;
@synthesize Flinetype;
@synthesize FlineThickFactor;
@synthesize FlineThickDigit;
@synthesize FlineColorbarSwitch;
- (id)init;
{
    self.FlineThickFactor = 1;
    self.FlineThickDigit = -2;
    self.FlineWindowlabel = [NSString stringWithFormat:@"Fieldline View"];
    
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
    self.Flinetype = kemoview_get_line_type_flag(kemo_sgl);
    return;
}

- (void) setSelectedFlineComponentRanges:(struct kemoviewer_type *) kemo_sgl
{
    double dataMin, dataMax;
    double cmapMinValue, cmapMaxValue;
    int cmapMinDigit, cmapMaxDigit;
    SetDataRanges(FIELDLINE_RENDERING, kemo_sgl, &dataMin, &dataMax, 
                  &cmapMinValue, &cmapMinDigit, &cmapMaxValue, &cmapMaxDigit);
    self.FlineMinimumValue = (CGFloat) dataMin;
    self.FlineMaximumValue = (CGFloat) dataMax;
    self.FlineDisplayMinimum = (CGFloat) cmapMinValue;
    self.FlineDisplayMinDigit = (CGFloat) cmapMinDigit;
    self.FlineDisplayMaximum = (CGFloat) cmapMaxValue;
    self.FlineDisplayMaxDigit = (CGFloat) cmapMaxDigit;
}

- (void) SetFlineDataRanges:(NSInteger)isel
                   kemoview:(struct kemoviewer_type *) kemo_sgl
{
    double current_thick;
    int current_digit;
    
    int n_field =  kemoview_get_VIZ_field_param(kemo_sgl,
                                                FIELDLINE_RENDERING,
                                                NUM_FIELD_FLAG);
    if (n_field > 0) {  
        [self setSelectedFlineComponentRanges:kemo_sgl];
    }
    
    kemoview_get_VIZ_color_w_exp(kemo_sgl,
                                 FIELDLINE_RENDERING, ISET_WIDTH,
                                 &current_thick, &current_digit);
    self.FlineThickFactor = (CGFloat) current_thick;
    self.FlineThickDigit = (CGFloat) current_digit;
    
    return;
}

- (id) CopyFlineDisplayFlagsFromC:(struct kemoviewer_type *) kemo_sgl
{
    self.DrawFlineFlag = kemoview_get_VIZ_draw_flags(kemo_sgl,
                                                     FIELDLINE_RENDERING);
    [_psfController SetFieldMenuItems:FIELDLINE_RENDERING
                             kemoview:kemo_sgl
                            fieldMenu:_FlineFieldMenu];
    [_psfController SetComponentMenuItems:0
                              activeModel:FIELDLINE_RENDERING
                                 kemoview:kemo_sgl
                            componentMenu:_FlineComponentMenu];
    [self SetFlineDataRanges:0
                    kemoview:kemo_sgl];
    return self;
}

- (void) OpenFieldlineFile:(NSString*) fieldlineFilehead
                  kemoview:(struct kemoviewer_type *) kemo_sgl
{
    int id_viewtype;
    
    [_controlTabView selectTabViewItemAtIndex:FIELDLINE_RENDERING];
    self.FlineWindowlabel = [NSString stringWithFormat:@"Fieldline:%@",
                             [[fieldlineFilehead lastPathComponent] stringByDeletingPathExtension]];
    
    [self CopyFlineDisplayFlagsFromC:kemo_sgl];
    
    self.currentFlineStep = [_kemoviewControl SetCurrentPSFFile:FIELDLINE_RENDERING
                                                       kemoview:kemo_sgl
                                                       pathTree:_flinePathControl];
    
    id_viewtype = kemoview_get_view_type_flag(kemo_sgl);
    [_kemoviewControl SetViewTypeMenu:id_viewtype
                             kemoview:kemo_sgl];
    [_kemoviewControl Set3DView:kemo_sgl];
    [_rgbaMapObject UpdateColormapView:kemo_sgl];
};

- (IBAction) DrawFlineFile:(id)pId{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    
    NSArray *flineFileTypes = [NSArray arrayWithObjects:@"inp",@"vtk",@"gz",@"INP",@"VTK",@"GZ",nil];
    NSOpenPanel *flineOpenPanelObj	= [NSOpenPanel openPanel];
    [flineOpenPanelObj setTitle:@"Choose field line data"];
    [flineOpenPanelObj setAllowedFileTypes:flineFileTypes];
    [flineOpenPanelObj beginSheetModalForWindow:window 
                              completionHandler:^(NSInteger FlineOpenInteger){
        
        if(FlineOpenInteger == NSModalResponseOK){
            //        NSString *FlineOpenDirectory = [[flineOpenPanelObj directoryURL] path];
            // NSLog(@"PSF file directory = %@",FlineOpenDirectory);
            NSString *OpenFilename =  [[flineOpenPanelObj URL] path];
            NSString *OpenFileExtension = [OpenFilename pathExtension];
            NSString *OpenFilePrefix =    [OpenFilename stringByDeletingPathExtension];
            if([OpenFileExtension isEqualToString:@"gz"]
               || [OpenFileExtension isEqualToString:@"GZ"]){
                OpenFilePrefix =   [OpenFilePrefix stringByDeletingPathExtension];
            };
            
            struct kv_string *filename
            = kemoview_init_kvstring_by_string([OpenFilename UTF8String]);
            int iflag_datatype =  kemoview_open_data(filename, kemo_sgl);
            kemoview_free_kvstring(filename);
            
            if(iflag_datatype == IFLAG_LINES){
                [self OpenFieldlineFile:(NSString *)OpenFilePrefix
                               kemoview:kemo_sgl];
            }
        };
    }];
    NSInteger WindowExpandFlag = kemoview_check_all_VIZ_draw_flags(kemo_sgl);
    [_ElasticControl UpdateWindow:WindowExpandFlag];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) CloseFlineFile:(id)pId{
    int current_model = [_kemoviewControl CurrentControlModel];
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_close_fieldline_view(kemo_sgl);
    [self CopyFlineDisplayFlagsFromC:kemo_sgl];
    
    NSInteger WindowExpandFlag = kemoview_check_all_VIZ_draw_flags(kemo_sgl);
    [_ElasticControl UpdateWindow:WindowExpandFlag];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) FlineFieldAction:(id)sender
{
    NSInteger isel = [_FlineFieldMenu indexOfSelectedItem];
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_psfController SetComponentMenuItems:isel
                              activeModel:FIELDLINE_RENDERING
                                 kemoview:kemo_sgl
                            componentMenu:_FlineComponentMenu];
    [self SetFlineDataRanges:isel
                    kemoview:kemo_sgl];
    
    kemoview_set_VIZ_field_param((int) isel,
                                 FIELDLINE_RENDERING,
                                 FIELD_SEL_FLAG,
                                 kemo_sgl);
    
    [self setSelectedFlineComponentRanges:kemo_sgl];
    [_rgbaMapObject UpdateColormapView:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) FlineComponentAction:(id)sender
{	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_field_param((int) [_FlineComponentMenu indexOfSelectedItem],
                                 FIELDLINE_RENDERING,
                                 COMPONENT_SEL_FLAG,
                                 kemo_sgl);
    [self setSelectedFlineComponentRanges:kemo_sgl];
    [_rgbaMapObject UpdateColormapView:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChooseFieldlineColorAction:(id)sender;
{
    NSInteger tag = [[FieldlineColorItem selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_patch_color_mode((int) tag,
                                      FIELDLINE_RENDERING,
                                      kemo_sgl);
    
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) ShowFlineRange:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_linear_colormap(self.FlineDisplayMinimum,
                                 (int) self.FlineDisplayMinDigit,
                                 self.FlineDisplayMaximum,
                                 (int) self.FlineDisplayMaxDigit,
                                 FIELDLINE_RENDERING,
                                 kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChooseFieldlineTypeAction:(id)sender;
{
    self.Flinetype = [[_flinetype_matrix selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_line_type_flag((int) self.Flinetype, kemo_sgl);
    
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetFieldlineThicknessAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_color_value_w_exp(FIELDLINE_RENDERING, ISET_WIDTH,
                                       (double) self.FlineThickFactor,
                                       (int) self.FlineThickDigit,
                                       kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetFlineColorAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_psfController SetPSFColorFromColorWell:kemo_sgl
                                   colorwell:_flineColorWell];
    [_metalView UpdateImage:kemo_sgl];
}

- (int) FlineColorbarSwitchStatus
{
    return (int) self.FlineColorbarSwitch;
}
- (void) setFlineColorbarSwitchStatus:(int) isel
{
    self.FlineColorbarSwitch = isel;
}

@end
