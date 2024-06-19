//
//  KemoviewerController.m
//
//  Created by Hiroaki Matsui on 10/09/20.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "KemoviewerController.h"
#include "kemoviewer.h"

@implementation KemoviewerController
@synthesize activeControlFlag;
@synthesize ColorLoopCount;
@synthesize NodeSizeFactor;
@synthesize NodeSizedigits;
@synthesize fInfo;
@synthesize fDrawHelp;
@synthesize StereoFlag;
@synthesize QuiltFlag;
@synthesize coastlineRadius;
@synthesize psfTexTureEnable;
@synthesize timeDisplayFlag;
@synthesize fileStepDisplayFlag;
@synthesize timeDisplayAccess;
@synthesize fileStepDisplayAccess;
@synthesize coastLineDrawFlag;
@synthesize globeGridDrawFlag;
@synthesize tangentCylinderDrawFlag;
@synthesize ICBRadius;
@synthesize axisDrawFlag;
@synthesize axisPositionFlag;
@synthesize axisDrawAccess;
@synthesize axisWidthFactor;
@synthesize axisWidthDigits;
@synthesize ThreadsCount;
@synthesize ShadingMode;
@synthesize CoastLineTubeFlag;
@synthesize TubeNumCorners;
@synthesize CoastlineWidth;
@synthesize CoastlineDigit;
- (id)init
{
	NodeSizeFactor =  1;
	NodeSizedigits = -2;
	self.ColorLoopCount =  6;
    
    self.timeDisplayAccess =     0;
    self.fileStepDisplayAccess = 0;
    self.timeDisplayFlag =     0;
    self.fileStepDisplayFlag = 0;

    self.coastLineDrawFlag = 0;
    self.globeGridDrawFlag = 0;
    self.axisDrawFlag =      0;
    self.axisDrawAccess =    1;
	
    self.QuiltFlag = [_resetview ToggleQuiltMode];
	return self;
}

-(void) awakeFromNib
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    self.coastlineRadius = kemoview_get_coastline_radius(kemo_sgl);
    self.ICBRadius = kemoview_get_inner_core_radius(kemo_sgl);
    kemoview_set_object_property_flags(TIME_LABEL_AVAIL,
                                       (int) self.timeDisplayAccess, kemo_sgl);
    kemoview_set_object_property_flags(TIME_LABEL_SWITCH,
                                       (int) self.timeDisplayFlag, kemo_sgl);
    kemoview_set_object_property_flags(FILE_STEP_LABEL_AVAIL,
                                       (int) self.fileStepDisplayAccess, kemo_sgl);
    kemoview_set_object_property_flags(FILE_STEP_LABEL_SWITCH,
                                       (int) self.fileStepDisplayFlag, kemo_sgl);
    
    NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
    self.ThreadsCount = [[defaults stringForKey:@"ThreadsCountNum"] intValue];
    if(self.ThreadsCount < 0){
        self.ThreadsCount = kemoview_get_number_of_threads(kemo_sgl);
    }else{
        kemoview_set_number_of_threads((int) self.ThreadsCount, kemo_sgl);
    }
    
    int id_default_image = [[defaults stringForKey:@"ImageFormatID"] intValue];
    kemoview_set_view_integer(IMAGE_FORMAT_FLAG, id_default_image, kemo_sgl);

    self.axisDrawFlag =     kemoview_get_object_property_flags(kemo_sgl, AXIS_TOGGLE);
    self.axisPositionFlag = kemoview_get_object_property_flags(kemo_sgl, AXIS_POSITION);
    self.ShadingMode =      kemoview_get_object_property_flags(kemo_sgl, SHADING_SWITCH);

    self.coastLineDrawFlag = kemoview_get_object_property_flags(kemo_sgl, COASTLINE_SWITCH);
    self.globeGridDrawFlag = kemoview_get_object_property_flags(kemo_sgl, SPHEREGRID_SWITCH);
    self.tangentCylinderDrawFlag
                = kemoview_get_object_property_flags(kemo_sgl, TANGENT_CYLINDER_SWITCH);

    self.CoastLineTubeFlag = kemoview_get_view_integer(kemo_sgl, COASTLINE_TUBE);
    self.TubeNumCorners =    kemoview_get_view_integer(kemo_sgl, NUM_TUBE_CORNERS_FLAG);
    int idigit;
    double value;
    kemoview_get_coastline_thickness_w_exp(kemo_sgl, &value, &idigit);
    self.CoastlineWidth = value;
    self.CoastlineDigit = idigit;

    kemoview_get_axis_thickness_w_exp(kemo_sgl, &value, &idigit);
    self.axisWidthFactor = value;
    self.axisWidthDigits = idigit;

    return;
}

- (id)dealloc
{
    [super dealloc];
	return self;
}

- (int) CurrentControlModel
{
    return (int) self.activeControlFlag;
}

- (int) SetCurrentPSFFile:(int) id_model
                 kemoview:(struct kemoviewer_type *) kemo_sgl
                 pathTree:(NSPathControl *) pathControl
{
    int i_file_step;
    struct kv_string *ucd_m;

    ucd_m = kemoview_alloc_kvstring();
    kemoview_get_full_path_file_prefix_step(kemo_sgl, id_model,
                                            ucd_m, &i_file_step);
    kemoview_free_kvstring(ucd_m);

    ucd_m = kemoview_alloc_kvstring();
    kemoview_get_full_path_file_name(kemo_sgl, id_model, ucd_m);
    NSString *str = [NSString stringWithCString:ucd_m->string encoding:NSUTF8StringEncoding];
    NSURL *urlReadPSF = [NSURL fileURLWithPath:str];
    [pathControl setURL:urlReadPSF];

    kemoview_free_kvstring(ucd_m);
    return i_file_step;
}

- (void)SetViewTypeMenu:(NSInteger) selected
               kemoview:(struct kemoviewer_type *) kemo_sgl
{
	[_viewtypeItem selectItemAtIndex:selected];
	
	[_view3dItem setState:NSControlStateValueOff];
	[_viewMapItem setState:NSControlStateValueOff];
	[_viewStereoItem setState:NSControlStateValueOff];
	[_viewXYItem setState:NSControlStateValueOff];
	[_viewYZItem setState:NSControlStateValueOff];
	[_viewZXItem setState:NSControlStateValueOff];
	
    self.axisDrawAccess = 1;
    self.QuiltFlag = (NSInteger) kemoview_get_quilt_nums(kemo_sgl,
                                                         ISET_QUILT_MODE);
	psfTexTureEnable = 1;
    
    self.StereoFlag = 0;
    if(selected == VIEW_STEREO || self.QuiltFlag > 0){self.StereoFlag = 1;};

	if(selected == VIEW_3D) {[_view3dItem setState:NSControlStateValueOn];}
	else if (selected == VIEW_MAP) {
        self.axisDrawAccess = 0;
		psfTexTureEnable = 0;
		[_viewMapItem setState:NSControlStateValueOn];
	}
	else if (selected == VIEW_STEREO) {
		[_viewStereoItem setState:NSControlStateValueOn];
	}
	else if (selected == VIEW_XY) {[_viewXYItem setState:NSControlStateValueOn];}
	else if (selected == VIEW_YZ) {[_viewYZItem setState:NSControlStateValueOn];}
	else if (selected == VIEW_XZ) {[_viewZXItem setState:NSControlStateValueOn];};
}

- (void)UpdateViewtype:(NSInteger) selected
              kemoview:(struct kemoviewer_type *) kemo_sgl
{
	[self SetViewTypeMenu:selected
                 kemoview:kemo_sgl];

    kemoview_set_viewtype((int) selected, kemo_sgl);
	[_metalView setViewerType:selected];
    kemoview_mono_viewmatrix(kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChoosePolygontypeAction:(id)sender
{
	PolygonMode = [[_polygontype_matrix selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_object_property_flags(POLYGON_SWITCH, (int) PolygonMode, kemo_sgl);
	
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChooseSurfcetypeAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_object_property_flags(SHADING_SWITCH, (int) self.ShadingMode, kemo_sgl);
    
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)AxisSwitchAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_object_property_flags(AXIS_TOGGLE, self.axisDrawFlag, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)AxisPositionAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_object_property_flags(AXIS_POSITION, self.axisPositionFlag, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}


- (IBAction)CoastSwitchAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_object_property_flags(COASTLINE_SWITCH,
                                       self.coastLineDrawFlag, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}
- (IBAction)SphGridSwitchAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_object_property_flags(SPHEREGRID_SWITCH,
                                       self.globeGridDrawFlag, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}
- (IBAction)SphRadiusAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_coastline_radius((double) self.coastlineRadius, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)TangentCylinderSwitchAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_object_property_flags(TANGENT_CYLINDER_SWITCH,
                                       self.tangentCylinderDrawFlag, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)InnerCoreRadiusAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_inner_core_radius((double) self.ICBRadius, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChooseColorModeAction:(id)sender
{
	MeshColorMode = [[_colormode_matrix selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_mesh_color_mode((int) MeshColorMode, kemo_sgl);

	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetColorLoopCount:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_num_of_color_loop((int) self.ColorLoopCount, kemo_sgl);

	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) ShowNodeSizeValue:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_node_diamater((double) NodeSizeFactor,
                               (int) NodeSizedigits,
                               kemo_sgl);

	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) ToggleQuiltSwitch:(id)sender
{
    self.QuiltFlag = [_resetview ToggleQuiltMode];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetViewTypeMenu:VIEW_3D
                 kemoview:kemo_sgl];
}

- (IBAction) SetViewtypeAction:(id)pSender{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	[self UpdateViewtype:[_viewtypeItem selectedTag]
                kemoview:kemo_sgl];
}

- (void) Set3DView:(struct kemoviewer_type *) kemo_sgl
{
    [self SetViewTypeMenu:VIEW_3D
                 kemoview:kemo_sgl];
    [_metalView setViewerType:VIEW_3D];
    kemoview_set_viewtype(VIEW_3D, kemo_sgl);
}

- (IBAction) UpdateViewByInpit:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_metalView UpdateImage:kemo_sgl];
};

- (IBAction) ResetviewAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self Set3DView:kemo_sgl];
	[_metalView Resetview];
}


-(IBAction) Toggleinfo: (id) sender
{
	fInfo = 1 - fInfo;
	[_metalView setInfo:fInfo];
}

-(IBAction) ToggleQuickhelp: (id) sender
{
	fDrawHelp = 1 - fDrawHelp;
	[_metalView setQuickHelp:fDrawHelp];
}

- (void) TimeLabelAvaiability
{
    self.timeDisplayAccess = kemoview_get_object_property_flags([_kmv KemoViewPointer],
                                                                TIME_LABEL_AVAIL);
}
- (void) FileStepLabelAvaiability
{
    self.fileStepDisplayAccess = kemoview_get_object_property_flags([_kmv KemoViewPointer],
                                                                    FILE_STEP_LABEL_AVAIL);
}

- (IBAction)TimeLabelSwitchAction:(id)sender{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_object_property_flags(TIME_LABEL_SWITCH,
                                       self.timeDisplayFlag, kemo_sgl);
    if(self.timeDisplayFlag > 0){
        self.fileStepDisplayFlag = 0;
        kemoview_set_object_property_flags(FILE_STEP_LABEL_SWITCH,
                                           (int) self.fileStepDisplayFlag, kemo_sgl);
    };
    [_metalView UpdateImage:kemo_sgl];
};

- (IBAction)FileStepLabelSwitchAction:(id)sender{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_object_property_flags(FILE_STEP_LABEL_SWITCH,
                                       self.fileStepDisplayFlag, kemo_sgl);
    if(self.fileStepDisplayFlag > 0){
        self.timeDisplayFlag = 0;
        kemoview_set_object_property_flags(TIME_LABEL_SWITCH,
                                           (int) self.timeDisplayFlag, kemo_sgl);
    };
    [_metalView UpdateImage:kemo_sgl];
};

- (IBAction)SetNnumberOfThreads:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_number_of_threads((int) self.ThreadsCount, kemo_sgl);

    NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
    [defaults setInteger:self.ThreadsCount forKey:@"ThreadsCountNum"];

    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetCoastLinETubeAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_integer(COASTLINE_TUBE,
                              (int) self.CoastLineTubeFlag,
                              kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetTubeNumCornersAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_integer(NUM_TUBE_CORNERS_FLAG,
                              (int) self.TubeNumCorners,
                              kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) SetCoastlineWidth:(id)pSender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_coastline_thickness_w_exp((double) self.CoastlineWidth,
                                           (int) self.CoastlineDigit, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
};

- (IBAction) SetAxisBarWidth:(id)pSender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_axis_thickness_w_exp((double) self.axisWidthFactor,
                                      (int) self.axisWidthDigits,
                                      kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
};


@end
