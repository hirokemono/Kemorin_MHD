/*
//  CharaIntControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_chara_int_IO.h"
#include "load_MHD_control_c.h"

@interface CharaIntControlTableview : NSObject {
    NSMutableDictionary * charaIntControlDictionary;
    NSMutableArray * charaIntControlArray;
    NSTableView * charaIntTableView;
    NSNumberFormatter * integerFormatter;
    IBOutlet NSView *charaIntTableViewOutlet;
    
    NSString *key1;
    NSString *key2;

    struct chara_int_clist * charaIntCtlList;
}
@property(strong) NSMutableDictionary * charaIntControlDictionary;
@property(strong) NSMutableArray * charaIntControlArray;
@property(strong) NSTableView * charaIntTableView;
@property(strong) NSString * key1;
@property(strong) NSString * key2;

-(void)linkToCharaIntclist;

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
