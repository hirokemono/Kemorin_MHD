/*
//  Chara2ControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_chara2_IO.h"
#include "c_ctl_data_SGS_MHD.h"

@interface Chara2ControlTableview : NSObject {
    NSMutableDictionary * chara2ControlDictionary;
    NSMutableArray * chara2ControlArray;
    NSTableView * chara2TableView;
    IBOutlet NSView *chara2TableViewOutlet;
    
    NSString *key1;
    NSString *key2;

    struct chara2_clist * Chara2CtlList;
}
@property(strong) NSMutableDictionary * chara2ControlDictionary;
@property(strong) NSMutableArray * chara2ControlArray;
@property(strong) NSTableView * chara2TableView;
@property(strong) NSString * key1;
@property(strong) NSString * key2;

-(void)linkToChara2clist;

- (void)awakeFromNib;
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView;
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(NSInteger)rowIndex;
- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender;
- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex;
- (IBAction)addAtSelectedRow:(id)pId;
- (IBAction)deleteSelectedRow:(id)pId;

@end
