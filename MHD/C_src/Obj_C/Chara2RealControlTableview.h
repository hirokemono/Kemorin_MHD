/*
//  Chara2RealControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_chara2_real_IO.h"
#include "load_MHD_control_c.h"

@interface Chara2RealControlTableview : NSObject {
    NSMutableDictionary * chara2RealControlDictionary;
    NSMutableArray * chara2RealControlArray;
    NSTableView * chara2RealTableView;
    NSNumberFormatter * numberFormatter;
    IBOutlet NSView *chara2RealTableViewOutlet;
    
    NSString *key1;
    NSString *key2;
    NSString *key3;

    struct chara2_real_clist * Chara2RealCtlList;
}
@property(strong) NSMutableDictionary * chara2RealControlDictionary;
@property(strong) NSMutableArray * chara2RealControlArray;
@property(strong) NSTableView * chara2RealTableView;
@property(strong) NSString * key1;
@property(strong) NSString * key2;
@property(strong) NSString * key3;

-(void)linkToChara2Realclist;

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
