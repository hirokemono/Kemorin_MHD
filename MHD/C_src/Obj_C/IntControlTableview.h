/*
//  IntControlTableview.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Cocoa/Cocoa.h>
#include "calypso_param_c.h"
#include "t_control_int_IO.h"
#include "c_ctl_data_SGS_MHD.h"

@interface IntControlTableview : NSObject {
    NSMutableDictionary * intControlDictionary;
    NSMutableArray * intControlArray;
    NSTableView * intTableView;
    NSNumberFormatter * integerFormatter;
    IBOutlet NSView *intTableViewOutlet;
    
    NSString *key1;
    
    struct int_clist * intCtlList;
}
@property(strong) NSMutableDictionary * intControlDictionary;
@property(strong) NSMutableArray * intControlArray;
@property(strong) NSTableView * intTableView;
@property(strong) NSString * key1;

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
