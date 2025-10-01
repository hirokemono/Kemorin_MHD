//
//  Kemoview_IO_Controller.m
//  Open_three_windows_kemo_3
//
//  Created by Hiroaki Matsui on 10/09/20.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "Kemoview_IO_Controller.h"

#include "kemoviewer.h"


@implementation Kemoview_IO_Controller
@synthesize ImageFormatFlag;
- (void) OpenKemoviewerFile:(NSString*) kemoviewOpenFilename
                   kemoview:(struct kemoviewer_type *) kemo_sgl
{
	int iflag_datatype;
	NSString *kemoviewOpenFilehead = [kemoviewOpenFilename stringByDeletingPathExtension];
	NSString *KemoviewOpenFileext =  [kemoviewOpenFilename pathExtension];
	
	if([KemoviewOpenFileext isEqualToString:@"gz"] || [KemoviewOpenFileext isEqualToString:@"GZ"]){
		KemoviewOpenFileext =    [kemoviewOpenFilehead pathExtension];
		kemoviewOpenFilehead =   [kemoviewOpenFilehead stringByDeletingPathExtension];
		if([KemoviewOpenFileext isEqualToString:@"0"]) return;
	};

    struct kv_string *filename = kemoview_init_kvstring_by_string([kemoviewOpenFilename UTF8String]);
	iflag_datatype = kemoview_open_data(filename, kemo_sgl);
    kemoview_free_kvstring(filename);
	
	if(iflag_datatype==IFLAG_MESH) {
		[_domainTableController OpenSurfaceMeshFile:kemoviewOpenFilehead
                                           kemoview:kemo_sgl];
	}
	else if(iflag_datatype==IFLAG_SURFACES) {
		[_psfController DrawPsfFile:kemoviewOpenFilehead
                           kemoview:kemo_sgl];
        [_movieMakerController InitEvolutionStepByPSF:SURFACE_RENDERING
                                             kemoview:kemo_sgl];
	}else if(iflag_datatype==IFLAG_LINES) {
        [_flineController OpenFieldlineFile:kemoviewOpenFilehead
                                   kemoview:kemo_sgl];
        [_movieMakerController InitEvolutionStepByPSF:FIELDLINE_RENDERING
                                             kemoview:kemo_sgl];
   }else if(iflag_datatype==IFLAG_POINTS){
        [_tracerController OpenTracerFile:kemoviewOpenFilehead
                                 kemoview:kemo_sgl];
        [_movieMakerController InitEvolutionStepByPSF:TRACER_RENDERING
                                             kemoview:kemo_sgl];
    };
    NSInteger WindowExpandFlag = kemoview_check_all_VIZ_draw_flags(kemo_sgl);
    [_ElasticControl UpdateWindow:WindowExpandFlag];
    [_metalView UpdateImage:kemo_sgl];	
}

- (IBAction) OpenKemoviewerFileByMenu:(id)pId;
{
	NSArray *kemoviewFileTypes = [NSArray arrayWithObjects:
                                  @"ksm",@"KSM",@"udt",@"UDT",@"inp",@"INP",
                                  @"sfm",@"SFM",@"sdt",@"SDT",
                                  @"vtk",@"VTK",@"vtd",@"VTD",@"gz",@"GZ",nil];
	NSOpenPanel *KemoviewOpenPanelObj	= [NSOpenPanel openPanel];
	[KemoviewOpenPanelObj setTitle:@"Choose data for Kemoviewer"];
    [KemoviewOpenPanelObj setAllowedFileTypes:kemoviewFileTypes];
    [KemoviewOpenPanelObj beginSheetModalForWindow:window 
                                   completionHandler:^(NSInteger KemoviewOpenInteger){
	if(KemoviewOpenInteger == NSModalResponseOK){
		NSString *kemoviewOpenFilename = [[KemoviewOpenPanelObj URL] path];
        struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
		[self OpenKemoviewerFile:kemoviewOpenFilename
                        kemoview:kemo_sgl];
	};
                                   }];
}

- (IBAction) SaveViewMatrixFile:(id)pId;
{
	NSSavePanel *ViewMatrixSavePanelObj	= [NSSavePanel savePanel];
    [ViewMatrixSavePanelObj beginSheetModalForWindow:window 
                                   completionHandler:^(NSInteger ViewMatrixSaveInt){
	if(ViewMatrixSaveInt == NSModalResponseOK){
		
		NSString * ViewMatrixFilename = [[ ViewMatrixSavePanelObj URL] path];
		NSString * ViewMatrixDirectory = [[ ViewMatrixSavePanelObj directoryURL] path];
		NSString * ViewMatrixFilehead = [ ViewMatrixFilename stringByDeletingPathExtension];
		NSLog(@" ViewMatrixFilename = %@",  ViewMatrixFilename);
		NSLog(@" ViewMatrixDirectory = %@", ViewMatrixDirectory);
		NSLog(@" ViewMatrixFilehead = %@",  ViewMatrixFilehead);

        struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
        struct kv_string *filename = kemoview_init_kvstring_by_string([ViewMatrixFilename UTF8String]);
		kemoview_write_modelview_file(filename, kemo_sgl);
        kemoview_free_kvstring(filename);
	};
                                   }];
}

- (IBAction) LoadViewMatrixFile:(id)pId;
{
/*    NSArray *MatrixFileTypes = [NSArray arrayWithObjects:@"dat",@"DAT",nil];*/
    
	NSOpenPanel *ViewMatrixOpenPanelObj	= [NSOpenPanel openPanel];
	[ViewMatrixOpenPanelObj setTitle:@"Choose View Matrix data"];
/*    [ViewMatrixOpenPanelObj setAllowedFileTypes:MatrixFileTypes];*/
    [ViewMatrixOpenPanelObj beginSheetModalForWindow:window 
                                 completionHandler:^(NSInteger ViewMatrixOpenInteger){
    if(ViewMatrixOpenInteger == NSModalResponseOK){
		
		NSString * ViewMatrixFilename = [[ ViewMatrixOpenPanelObj URL] path];
		NSString * ViewMatrixDirectory = [[ ViewMatrixOpenPanelObj directoryURL] path];
		NSString * ViewMatrixFilehead = [ ViewMatrixFilename stringByDeletingPathExtension];
		NSLog(@" ViewMatrixFilename = %@",  ViewMatrixFilename);
		NSLog(@" ViewMatrixDirectory = %@", ViewMatrixDirectory);
		NSLog(@" ViewMatrixFilehead = %@",  ViewMatrixFilehead);
        
        struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
        struct kv_string *filename = kemoview_init_kvstring_by_string([ViewMatrixFilename UTF8String]);
		kemoview_load_modelview_file(filename, kemo_sgl);
        kemoview_free_kvstring(filename);
		[_metalView UpdateImage:kemo_sgl];
	};
                                 }];
}

-(void) SaveImageByFormat:(NSString *)ImageFilename
                 kemoview:(struct kemoviewer_type *) kemo_sgl
{
    NSUserDefaults* defaults = [_user_defaults_controller defaults];
    CurrentImageFormat = [[defaults stringForKey:@"ImageFormatID"] intValue];
    
    NSString * ImageFilehead =  [ImageFilename stringByDeletingPathExtension];
    NSString * ImageFileext =   [ImageFilename pathExtension];
    NSInteger id_format = [_kemoImageMaker SetImageFileFormatID:ImageFileext];
    // NSLog(@" ImageFilename = %@",  ImageFilename);
    // NSLog(@" ImageFilehead = %@",  ImageFilehead);
    
    
    if(id_format == SAVE_UNDEFINED || id_format == SAVE_QT_MOVIE){
        id_format = (int) CurrentImageFormat;
    }
    
    if(kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE) == 1){
        [_kemoImageMaker SalectSaveKemoQuiltImageFile:id_format
                                           filePrefix:ImageFilehead
                                               degree:IZERO
                                                 axis:IONE
                                             kemoview:kemo_sgl];
    } else {
        [_kemoImageMaker SalectSaveKemoviewImageFile:id_format
                                          filePrefix:ImageFilehead];
    };
    
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) SaveImageFile:(id)pId;{
	NSSavePanel *ImageSavePanelObj	= [NSSavePanel savePanel];
	[ImageSavePanelObj setTitle:@"Save Image File"];
	[ImageSavePanelObj setCanSelectHiddenExtension:YES];
    [ImageSavePanelObj beginSheetModalForWindow:window 
                                   completionHandler:^(NSInteger ImageSaveInt){
	if(ImageSaveInt == NSModalResponseOK){
		
		NSString * ImageFilename = [[ ImageSavePanelObj URL] path];
        // NSString * ImageDirectory = [ ImageSavePanelObj directory];
        // NSLog(@" ImageDirectory = %@", ImageDirectory);
        struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
        [self SaveImageByFormat:ImageFilename
                       kemoview:kemo_sgl];
	};
                                   }];
}

- (IBAction)ChooseImageFormatAction:(id)sender;
{
	NSUserDefaults* defaults = [_user_defaults_controller defaults];
	ImageFormatFlag = [[defaults stringForKey:@"ImageFormatID"] intValue];
	ImageFormatFlag = [[_ImageFormat_item selectedCell] tag];

    struct kemoviewer_type * kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_view_integer(IMAGE_FORMAT_FLAG, (int) ImageFormatFlag, kemo_sgl);
}

- (IBAction)ColorbarSwitchAction:(id)sender;
{
    int ColorbarSwitchFlag;
    int current_model = [_kemoviewControl CurrentControlModel];
    if(current_model == TRACER_RENDERING){
        ColorbarSwitchFlag = [_tracerController TracerColorbarSwitchStatus];
    }else if(current_model == FIELDLINE_RENDERING){
        ColorbarSwitchFlag = [_flineController FlineColorbarSwitchStatus];
    }else{
        ColorbarSwitchFlag = [_psfController PSFColorbarSwitchStatus];
    }
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_colorbar_draw_flag(ColorbarSwitchFlag,
                                    current_model,
                                    kemo_sgl);

    ColorbarSwitchFlag = kemoview_get_colorbar_draw_flag(kemo_sgl, TRACER_RENDERING);
    [_tracerController setTracerColorbarSwitchStatus:ColorbarSwitchFlag];
    ColorbarSwitchFlag = kemoview_get_colorbar_draw_flag(kemo_sgl, FIELDLINE_RENDERING);
    [_flineController  setFlineColorbarSwitchStatus:ColorbarSwitchFlag];
    ColorbarSwitchFlag = kemoview_get_colorbar_draw_flag(kemo_sgl, SURFACE_RENDERING);
    [_psfController    setPSFColorbarSwitchStatus:ColorbarSwitchFlag];
    
    [_metalView UpdateImage:kemo_sgl];
}


@end
