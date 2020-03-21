/*
//  IntRealControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_int_real_IO.h"
#include "load_MHD_control_c.h"

@interface IntRealControlTableview : NSObject {
    NSMutableDictionary * intRealControlDictionary;
    NSMutableArray * intRealControlArray;
    NSTableView * intRealTableView;
    NSNumberFormatter * numberFormatter;
    NSNumberFormatter * integerFormatter;
    IBOutlet NSView *intRealTableViewOutlet;
    
    NSString *key1;
    NSString *key2;
    
    struct int_real_clist * intRealCtlList;
}
@property(strong) NSMutableDictionary * intRealControlDictionary;
@property(strong) NSMutableArray * intRealControlArray;
@property(strong) NSTableView * intRealTableView;
@property(strong) NSString * key1;
@property(strong) NSString * key2;

-(void)linkToIntRealclist;

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
