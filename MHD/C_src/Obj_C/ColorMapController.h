/*
//
//  ColorMapController.h
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 11/08/22.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//
*/

@import Cocoa;

#import "KemoViewerMetalView.h"
#import "fillRectView.h"
#import "KemoviewerController.h"
#import "KemoViewerObject.h"
#include "Kemoviewer.h"


@interface ColorMapController : NSObject {
	IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
    IBOutlet KemoViewerMetalView *_metalView;
    IBOutlet KemoviewerController*  _kemoviewControl;
    IBOutlet KemoViewerObject *_kmv;

    IBOutlet id _colorTableView;
	IBOutlet fillRectView* _fillRectView;

	IBOutlet NSTableView * idColorTableView;
    IBOutlet NSPathControl *_cmapPathControl;

	NSInteger  NumColorTable;
	NSMutableArray *ColorTableField;
	NSMutableArray *ColorTableColor;

	IBOutlet NSColorWell *backgroundColorWell;
    NSColor *nsBackgroundColor;

	IBOutlet NSPopUpButton *_colorModeItem;
    NSInteger   colorModeTag;
}
@property (assign) NSMutableArray * ColorTableField;
@property (assign) NSMutableArray * ColorTableColor;
@property (assign) NSTableView * idColorTableView;
// @property (assign) NSPopUpButton * _colorModeItem;


- (id)init;
- (void)awakeFromNib;

- (void)SetColorTables:(int) id_model
              kemoview:(struct kemoviewer_type *) kemo_sgl;
- (IBAction)UpdateColorTables:(id)pID;

- (IBAction)addAtSelectedRow:(id)pId;
- (IBAction)deleteSelectedRow:(id)pId;

- (NSInteger)numberOfRowsInTableView:(NSTableView *)pTableViewObj;

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex;

- (IBAction)ChooseBackgroundColorAction: (id) sender;

- (IBAction)SetColorMode:(id)pId;

@end
