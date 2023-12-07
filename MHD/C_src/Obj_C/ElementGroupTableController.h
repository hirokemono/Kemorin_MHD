//
//  ElementGroupTableController.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/09/28.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

@import Cocoa;

#import "KemoViewerMetalView.h"
#import "KemoViewerObject.h"
#include "Kemoviewer.h"


@interface ElementGroupTableController : NSObject {
	
    IBOutlet KemoViewerMetalView * _metalView;
    IBOutlet KemoViewerObject *_kmv;

    IBOutlet id _elementTableView;
	
	NSInteger  NumElementGroup;
	NSMutableArray *ElementGroupDisplayNames;
	NSMutableArray *ElementGroupDisplayPatchFlags;
	NSMutableArray *ElementGroupDisplayWireFlags;
	NSMutableArray *ElementGroupDisplayNodeFlags;

	NSString *selectedElementGroupObjectType;
	
	IBOutlet id _EleGrpPatchColorItem;
	IBOutlet id _EleGrpLineColorItem;
	IBOutlet id _EleGrpNodeColorItem;

	IBOutlet NSColorWell *eleGrpPatchColorWell;
	IBOutlet NSColorWell *eleGrpGridColorWell;
	IBOutlet NSColorWell *eleGrpNodeColorWell;
    NSColor *nsEleGrpPatchColor;
    NSColor *nsEleGrpGridColor;
    NSColor *nsEleGrpNodeColor;
}

- (id) init;
- (id) dealloc;

- (void) SetElementGroupLabels;

- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView;

- (IBAction) ShowAllElementGroupAction:(id)pId;
- (IBAction) HideAllElementGroupAction:(id)pId;

- (id)tableView:(NSTableView *)aTableView
objectValueForTableColumn:(NSTableColumn *)aTableColumn
			row:(int)rowIndex;

- (void)tableView:(NSTableView *)atableView
   setObjectValue:(id)object 
   forTableColumn:(NSTableColumn *)tableColumn 
			  row:(int)rowIndex;

- (void)tableView:(NSTableView *)aTableView didClickTableColumn:(NSTableColumn *)tableColumn;

- (void) UpdateElementTable:(struct kemoviewer_type *) kemo_sgl;

- (IBAction)ChooseEleGrpPatchColorAction:(id)sender;
- (IBAction)ChooseEleGrpLineColorAction:(id)sender;
- (IBAction)ChooseEleGrpNodeColorAction:(id)sender;

- (IBAction)SetEleGrpPatchColorAction:(id)sender;
- (IBAction)SetEleGrpLineColorAction:(id)sender;
- (IBAction)SetEleGrpNodeColorAction:(id)sender;

@end
