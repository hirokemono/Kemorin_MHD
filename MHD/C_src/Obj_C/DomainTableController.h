//
//  DomainTableController.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/09/26.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "KemoViewerOpenGLView.h"
#import "ElementGroupTableController.h"
#import "NodeGroupTableController.h"
#import "SurfaceGroupTableController.h"


@interface DomainTableController : NSObject {
	
    IBOutlet NodeGroupTableController*    _nodeGrpController;
    IBOutlet ElementGroupTableController* _eleGrpController;
    IBOutlet SurfaceGroupTableController* _surfGrpController;
    
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;
    IBOutlet NSWindow*  window;
	IBOutlet NSWindow* _meshWindow;
	IBOutlet id _tableView;
	
	NSString *DomainWindowlabel;

	NSInteger DrawMeshFlag;
	NSInteger NumSubDomain;
	NSMutableArray *DomainDisplayNames;
	NSMutableArray *DomainDisplayPatchFlags;
	NSMutableArray *DomainDisplayWireFlags;
	NSMutableArray *DomainDisplayNodeFlags;
	
	NSString *selectedDomainObjectType;
	
	CGFloat objectDistance;
	
	IBOutlet id _DomainPatchColorItem;
	IBOutlet id _DomainLineColorItem;
	IBOutlet id _DomainNodeColorItem;
	
	IBOutlet NSColorWell *domainPatchColorWell;
	IBOutlet NSColorWell *domainGridColorWell;
	IBOutlet NSColorWell *domainNodeColorWell;
    NSColor *nsDomainPatchColor;
    NSColor *nsDomainGridColor;
    NSColor *nsDomainNodeColor;
}

@property NSInteger DrawMeshFlag;
@property CGFloat objectDistance;
@property(retain) NSString* DomainWindowlabel;

- (id) init;
- (id) dealloc;

- (void) OpenSurfaceMeshFile:(NSString*) MeshOpenFilehead;

- (IBAction) SelectMeshFile:(id)pId;
- (IBAction) CloseMeshFile:(id)pId;
- (IBAction) ShowAllDomainAction:(id)pId;
- (IBAction) HideAllDomainAction:(id)pId;

- (int)numberOfRowsInTableView:(NSTableView *)aTableView;

- (id)tableView:(NSTableView *)aTableView
objectValueForTableColumn:(NSTableColumn *)aTableColumn
			row:(int)rowIndex;

- (void)tableView:(NSTableView *)atableView
   setObjectValue:(id)object 
   forTableColumn:(NSTableColumn *)tableColumn 
			  row:(int)rowIndex;
- (void)tableView:(NSTableView *)aTableView didClickTableColumn:(NSTableColumn *)tableColumn;

- (IBAction) UpdateTable:(NSTableView *)aTableView;

- (IBAction)ChooseDomainPatchColorAction:(id)sender;
- (IBAction)ChooseDomainLineColorAction:(id)sender;
- (IBAction)ChooseDomainNodeColorAction:(id)sender;

- (IBAction)SetDomainPatchColorAction:(id)sender;
- (IBAction)SetDomainLineColorAction:(id)sender;
- (IBAction)SetDomainNodeColorAction:(id)sender;

- (IBAction) SetObjectDistanceAction:(id)pSender;

@end
