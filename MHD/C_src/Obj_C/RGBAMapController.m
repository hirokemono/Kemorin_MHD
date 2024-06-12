//
//  RGBAMapController.m
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 11/08/23.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "RGBAMapController.h"
#include "kemoviewer.h"


@implementation RGBAMapController
@synthesize DataMaximum;
@synthesize DataMinimum;

- (void)awakeFromNib {
	self.DataMinimum = ZERO;
	self.DataMaximum = ONE;
}

- (void)updateColormapParameter:(struct kemoviewer_type *) kemo_sgl
{
    int i_comp = kemoview_get_each_PSF_field_param(kemo_sgl, DRAW_ADDRESS_FLAG);
	self.DataMinimum = kemoview_get_VIZ_data_range(kemo_sgl,
                                                   SURFACE_RENDERING,
                                                   ISET_COLOR_MIN, i_comp);
	self.DataMaximum = kemoview_get_VIZ_data_range(kemo_sgl,
                                                   SURFACE_RENDERING,
                                                   ISET_COLOR_MAX, i_comp);
}

- (void) SetColormapMinMax:(struct kemoviewer_type *) kemo_sgl
{
    [_colorMapObject SetColorTables:kemo_sgl];
    [_opacityMapObject SetOpacityTables:kemo_sgl];
}


- (IBAction) SaveColormapFile:(id)pId;{
	
	NSSavePanel *ColormapSavePanelObj	= [NSSavePanel savePanel];
    [ColormapSavePanelObj beginSheetModalForWindow:window 
                                    completionHandler:^(NSInteger ColrmapSaveInt){
	if(ColrmapSaveInt == NSModalResponseOK){
		
		NSString * ColormapFilename = [[ ColormapSavePanelObj URL] path];
		NSString * ColormapDirectory = [[ ColormapSavePanelObj directoryURL] path];
		NSString * ColormapFilehead = [ ColormapFilename stringByDeletingPathExtension];
		NSLog(@" ColormapFilename = %@",  ColormapFilename);
		NSLog(@" ColormapDirectory = %@", ColormapDirectory);
		NSLog(@" ColormapFilehead = %@",  ColormapFilehead);
		
        struct kv_string *filename = kemoview_init_kvstring_by_string([ColormapFilename UTF8String]);
        struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
        kemoview_write_colormap_file(filename, SURFACE_RENDERING,
                                     kemo_sgl);
        kemoview_free_kvstring(filename);
	};
                                    }];
}

- (IBAction) LoadColormapFile:(id)pId;{
/*    NSArray *ColormapFileTypes = [NSArray arrayWithObjects:@"dat",@"DAT",nil]; */
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    NSOpenPanel *ColormapOpenPanelObj	= [NSOpenPanel openPanel];
    [ColormapOpenPanelObj setTitle:@"Choose colormap data"];
/*    [ColormapOpenPanelObj setAllowedFileTypes:ColormapFileTypes]; */
    [ColormapOpenPanelObj beginSheetModalForWindow:window 
                                   completionHandler:^(NSInteger ColormapOpenInteger){
                                       if(ColormapOpenInteger == NSModalResponseOK){
                                           
                                           NSString * ColormapFilename = [[ ColormapOpenPanelObj URL] path];
                                           NSString * ColormapDirectory = [[ ColormapOpenPanelObj directoryURL] path];
                                           NSString * ColormapFilehead = [ ColormapFilename stringByDeletingPathExtension];
                                           NSLog(@" ColormapFilename = %@",  ColormapFilename);
                                           NSLog(@" ColormapDirectory = %@", ColormapDirectory);
                                           NSLog(@" ColormapFilehead = %@",  ColormapFilehead);
                                           
                                           struct kv_string *filename = kemoview_init_kvstring_by_string([ColormapFilename UTF8String]);
                                           kemoview_read_colormap_file(filename,
                                                                       SURFACE_RENDERING,
                                                                       kemo_sgl);
                                           kemoview_free_kvstring(filename);
                                           
                                           [_metalView UpdateImage:kemo_sgl];
                                           [_colorMapObject SetColorTables:kemo_sgl];
                                           [_opacityMapObject SetOpacityTables:kemo_sgl];
                                       };
                                   }];
    
}

@end
