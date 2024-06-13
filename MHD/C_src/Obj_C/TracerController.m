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

@synthesize FlineWindowlabel;

@synthesize DrawFlineFlag;
@synthesize TracerMinimumValue;
@synthesize TracerMaximumValue;
@synthesize TracerColorMinimum;
@synthesize TracerColorMaximum;
@synthesize TracerColorMinDigit;
@synthesize TracerColorMaxDigit;
@synthesize Flinetype;
@synthesize FlineThickFactor;
@synthesize FlineThickDigit;
- (id)init;
{
    self.FlineThickFactor = 1;
    self.FlineThickDigit = -2;
    self.FlineWindowlabel = [NSString stringWithFormat:@"Fieldline View"];

    FlineDrawFieldId =     [NSNumber alloc];
    FlineDrawComponentId = [NSNumber alloc];
    
    FieldlineColor =      [NSNumber alloc];
    
    FlineMinimumRange = [NSNumber alloc];
    FlineMaximumRange = [NSNumber alloc];
    
    return self;
}

- (id)dealloc
{
    [FlineDrawFieldId      dealloc];
    [FlineDrawComponentId  dealloc];
    
    [FieldlineColor      dealloc];

    [FlineMinimumRange dealloc];
    [FlineMaximumRange dealloc];
    
    [super dealloc];
    return self;
}

-(void) awakeFromNib
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
//    self.Flinetype = kemoview_get_line_type_flag(kemo_sgl);
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
    
    self.FlineWindowlabel = [NSString stringWithFormat:@"Fieldline:%@",
                             [[fieldlineFilehead lastPathComponent] stringByDeletingPathExtension]];

    [self CopyFlineDisplayFlagsFromC:kemo_sgl];
    
    id_viewtype = kemoview_get_view_type_flag(kemo_sgl);
    [_kemoviewControl SetViewTypeMenu:id_viewtype
                             kemoview:kemo_sgl];
    [_kemoviewControl Set3DView:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
};

- (void) ReadFlineFile:(NSString *) FlineFileName
              kemoview:(struct kemoviewer_type *) kemo_sgl
{
    FlineOpenFileext =   [FlineFileName pathExtension];
    FlineOpenFilehead =  [FlineFileName stringByDeletingPathExtension];
    // NSLog(@"PSF file name =      %@",FlineFileName);
    // NSLog(@"PSF file header =    %@",FlineOpenFilehead);
    // NSLog(@"self.FlineWindowlabel = %@",self.FlineWindowlabel);
    
    if([FlineOpenFileext isEqualToString:@"gz"] || [FlineOpenFileext isEqualToString:@"GZ"]){
        FlineOpenFileext =    [FlineOpenFilehead pathExtension];
        FlineOpenFilehead =   [FlineOpenFilehead stringByDeletingPathExtension];
    };
    
    struct kv_string *filename = kemoview_init_kvstring_by_string([FlineFileName UTF8String]);
    int iflag_datatype =  kemoview_open_data(filename, kemo_sgl);
    kemoview_free_kvstring(filename);
    
    if(iflag_datatype == IFLAG_LINES) [self OpenFieldlineFile:(NSString *)FlineOpenFilehead
                                                     kemoview:kemo_sgl];
}

- (IBAction) UpdateFieldline:(id)pId{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_metalView UpdateImage:kemo_sgl];
    return;
};

- (IBAction) DrawFlineFile:(id)pId{
    NSArray *flineFileTypes = [NSArray arrayWithObjects:@"inp",@"vtk",@"gz",@"INP",@"VTK",@"GZ",nil];
    NSOpenPanel *flineOpenPanelObj    = [NSOpenPanel openPanel];
    [flineOpenPanelObj setTitle:@"Choose field line data"];
    [flineOpenPanelObj setAllowedFileTypes:flineFileTypes];
    [flineOpenPanelObj beginSheetModalForWindow:window
                                   completionHandler:^(NSInteger FlineOpenInteger){
    if(FlineOpenInteger == NSModalResponseOK){
        FlineOpenDirectory = [[flineOpenPanelObj directoryURL] path];
        NSString *FlineOpenFilename =  [[flineOpenPanelObj URL] path];
        // NSLog(@"PSF file directory = %@",FlineOpenDirectory);
        struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
        [self ReadFlineFile:FlineOpenFilename
                   kemoview:kemo_sgl];
    };
                                   }];
}

- (IBAction) CloseFlineFile:(id)pId{
    NSInteger current_model = [_kemoviewControl CurrentControlModel];
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    
    if(current_model == FIELDLINE_RENDERING){
        kemoview_close_fieldline_view(kemo_sgl);
    }else if(current_model == TRACER_RENDERING){
        kemoview_close_tracer_view(kemo_sgl);
    };
    
    [self CopyFlineDisplayFlagsFromC:kemo_sgl];
    
    [_metalView UpdateImage:kemo_sgl];
    
}

- (void) setSelectedFlineComponentRanges:(struct kemoviewer_type *) kemo_sgl
{
    double dataMin, dataMax;
    double cmapMinValue, cmapMaxValue;
    int cmapMinDigit, cmapMaxDigit;
    SetDataRanges(FIELDLINE_RENDERING, kemo_sgl, &dataMin, &dataMax, 
                  &cmapMinValue, &cmapMinDigit, &cmapMaxValue, &cmapMaxDigit);
    self.TracerMinimumValue = (CGFloat) dataMin;
    self.TracerMaximumValue = (CGFloat) dataMax;
	self.TracerColorMinimum =   (CGFloat) cmapMinValue;
	self.TracerColorMinDigit = (CGFloat) cmapMinDigit;
	self.TracerColorMaximum =   (CGFloat) cmapMaxValue;
	self.TracerColorMaxDigit = (CGFloat) cmapMaxDigit;
}

- (IBAction) FlineFieldAction:(id)sender
{
    int i_digit;
    double value;
    NSInteger isel = [_FlineFieldMenu indexOfSelectedItem];

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_psfController SetComponentMenuItems:isel
                              activeModel:FIELDLINE_RENDERING
                                kemoview:kemo_sgl
                            componentMenu:_FlineComponentMenu];
    [self SetFlineDataRanges:isel
                    kemoview:kemo_sgl];
    
    kemoview_set_VIZ_field_param(FIELDLINE_RENDERING,
                                 FIELD_SEL_FLAG,
                                 (int) isel, kemo_sgl);
    
    [self setSelectedFlineComponentRanges:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) FlineComponentAction:(id)sender
{
    int i_digit;
    double value;
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_field_param(FIELDLINE_RENDERING,
                                 COMPONENT_SEL_FLAG,
                                 (int) [_FlineComponentMenu indexOfSelectedItem],
                                 kemo_sgl);
    [self setSelectedFlineComponentRanges:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (void) SetFlineDataRanges:(NSInteger)isel
                   kemoview:(struct kemoviewer_type *) kemo_sgl
{
    double current_thick;
    int current_digit;
    int i_digit;
    double value;

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

- (IBAction)ChooseFieldlineColorAction:(id)sender;
{
    NSInteger tag = [[FieldlineColorItem selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_patch_color_mode(FIELDLINE_RENDERING,
                                      (int) tag, kemo_sgl);
    
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) ShowFlineRange:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_linear_colormap(self.TracerColorMinimum,
                                 (int) self.TracerColorMinDigit,
                                 self.TracerColorMaximum,
                                 (int) self.TracerColorMaxDigit,
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

@end
