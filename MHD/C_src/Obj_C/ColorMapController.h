/*
//
//  ColorMapController.h
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 11/08/22.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//
*/

#import <Cocoa/Cocoa.h>
#import "KemoViewerOpenGLView.h"
#import "fillRectView.h"


@interface ColorMapController : NSObject {
	IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;
	IBOutlet id _colorTableView;
	IBOutlet fillRectView* _fillRectView;

	IBOutlet NSTableView * idColorTableView;
	
	NSInteger  NumColorTable;
	NSMutableArray *ColorTableField;
	NSMutableArray *ColorTableColor;

	IBOutlet NSColorWell *backgroundColorWell;
    NSColor *nsBackgroundColor;

	IBOutlet id ColorModeItem;	
}
@property (assign) NSMutableArray * ColorTableField;
@property (assign) NSMutableArray * ColorTableColor;
@property (assign) NSTableView * idColorTableView;


- (void)awakeFromNib;


- (IBAction)addAtSelectedRow:(id)pId;
- (IBAction)deleteSelectedRow:(id)pId;

- (int)numberOfRowsInTableView:(NSTableView *)pTableViewObj;

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex;

- (void)InitColorTables;
- (void)SetColorTables;

- (IBAction)UpdateColorTables:(id)pID;
- (IBAction)ChooseBackgroundColorAction: (id) sender;

- (IBAction)SetColorMode:(id)pId;

@end
