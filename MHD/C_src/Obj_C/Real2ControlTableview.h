/*
//  Real2ControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "kemosrc_param_c.h"
#include "t_control_real2_IO.h"
#include "t_SGS_MHD_control_c.h"

@interface Real2TableController : NSObject {
    NSString *Title1;
    NSString *Title2;
    NSInteger  NumTableItem;
    NSMutableArray *TableRealData1;
    NSMutableArray *TableRealData2;
    IBOutlet NSTableView * idReal2TableTableView;
    
    IBOutlet id _real2TableView;
}
// @property (copy) NSMutableArray * TableRealData1;
// @property (copy) NSMutableArray * TableRealData2;
@property (assign) NSTableView * idReal2TableView;
@property (copy) NSString * Title1;
@property (copy) NSString * Title2;


- (IBAction)addAtSelectedRow:(id)pId;
- (IBAction)deleteSelectedRow:(id)pId;

- (int)numberOfRowsInTableView:(NSTableView *)pTableViewObj;

- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject forTableColumn
                 :(NSTableColumn *)pTableColumn row:(int)pRowIndex;

- (void)InitReal2Tables;
- (void) SetReal2Tables;
- (IBAction) UpdateReal2Tables:(id)pID;

@end
