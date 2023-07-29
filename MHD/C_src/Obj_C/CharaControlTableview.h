/*
//  CharaControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_chara_IO.h"
#include "c_ctl_data_SGS_MHD.h"

@interface CharaControlTableview : NSObject {
    NSMutableDictionary * charaControlDictionary;
    NSMutableArray * charaControlArray;
    NSTableView * charaTableView;
    IBOutlet NSView *charaTableViewOutlet;
    
    NSString *key1;

    struct chara_clist * CharaCtlList;
}
@property(strong) NSMutableDictionary * charaControlDictionary;
@property(strong) NSMutableArray * charaControlArray;
@property(strong) NSTableView * charaTableView;
@property(strong) NSString * key1;

-(void)linkToCharaclist;

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
