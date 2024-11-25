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
- (void) UpdateColormapView:(struct kemoviewer_type *) kemo_sgl
{
    int id_model = (int) [_kemoviewControl CurrentControlModel];
    [_colorMapObject SetColorTables:id_model
                           kemoview:kemo_sgl];
    [_opacityMapObject SetOpacityTables:kemo_sgl];
    [_fillRectView UpdateColorbar];
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
        int current_model = (int) [_kemoviewControl CurrentControlModel];
        struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
        kemoview_write_colormap_file(filename, current_model, kemo_sgl);
        kemoview_free_kvstring(filename);
	};
                                    }];
}

- (IBAction) LoadColormapFile:(id)pId;{
/*    NSArray *ColormapFileTypes = [NSArray arrayWithObjects:@"dat",@"DAT",nil]; */
    int current_model = (int) [_kemoviewControl CurrentControlModel];
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
                                                                       current_model,
                                                                       kemo_sgl);
                                           kemoview_free_kvstring(filename);
                                           
                                           [_metalView UpdateImage:kemo_sgl];
                                           [_colorMapObject SetColorTables:current_model
                                                                  kemoview:kemo_sgl];
                                           [_opacityMapObject SetOpacityTables:kemo_sgl];
                                       };
                                   }];
    
}

@end
