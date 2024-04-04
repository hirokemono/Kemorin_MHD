/*
//  CharaRealControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_chara_real_IO.h"
#include "c_ctl_data_SGS_MHD.h"

@interface CharaRealControlTableview : NSObject {
    NSMutableDictionary * charaRealControlDictionary;
    NSMutableArray * charaRealControlArray;
    NSTableView * charaRealTableView;
    NSNumberFormatter * numberFormatter;
    IBOutlet NSView *charaRealTableViewOutlet;
    
    NSString *key1;
    NSString *key2;

    struct chara_real_clist * CharaRealCtlList;
}
@property(strong) NSMutableDictionary * charaRealControlDictionary;
@property(strong) NSMutableArray * charaRealControlArray;
@property(strong) NSTableView * charaRealTableView;
@property(strong) NSString * key1;
@property(strong) NSString * key2;

-(void)linkToCharaRealclist;

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
