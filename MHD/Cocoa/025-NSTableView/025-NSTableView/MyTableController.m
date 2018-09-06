//
//  MyTableController.m
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 2018/09/05.
//  Copyright © 2018年 Hiroaki Matsui. All rights reserved.
//

#import "MyTableController.h"


@implementation MyTableController
@synthesize mutableDataArray;
@synthesize mutableDictionary;
@synthesize myTableView;

NSString *_key1 = @"key1";
NSString *_key2 = @"key2";
NSString *_key3 = @"key3";
NSString *_key4 = @"key4";
NSString *_key5 = @"key5";
NSString *_key6 = @"key6";
NSString *_key7 = @"key7";


-(void)createMutablearray
{
    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"10001",_key1,@"10002",_key2,@"10003",_key3,
                          @"10004",_key4,@"10005",_key5,@"10006",_key6,@"10007",_key7, nil];
    
    self.mutableDataArray = [[NSMutableArray alloc]init];    
    [self.mutableDataArray addObject:mutableDictionary];

    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"20001",_key1,@"20002",_key2,@"20003",_key3,
            @"20004",_key4,@"20005",_key5,@"20006",_key6,@"20007",_key7, nil];
    [self.mutableDataArray addObject:mutableDictionary];
    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"30001",_key1,@"30002",_key2,@"30003",_key3,
            @"30004",_key4,@"30005",_key5,@"30006",_key6,@"30007",_key7, nil];
    [self.mutableDataArray addObject:mutableDictionary];
    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"40001",_key1,@"40002",_key2,@"40003",_key3,
            @"40004",_key4,@"40005",_key5,@"40006",_key6,@"40007",_key7, nil];
    [self.mutableDataArray addObject:mutableDictionary];
    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"50001",_key1,@"50002",_key2,@"50003",_key3,
            @"50004",_key4,@"50005",_key5,@"50006",_key6,@"50007",_key7, nil];
    [self.mutableDataArray addObject:mutableDictionary];
    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"60001",_key1,@"60002",_key2,@"60003",_key3,
            @"60004",_key4,@"60005",_key5,@"60006",_key6,@"60007",_key7, nil];
    [self.mutableDataArray addObject:mutableDictionary];    
}


-(void)createTableView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:tableViewPlaceholderView.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.myTableView = [[NSTableView alloc] initWithFrame:tableViewPlaceholderView.bounds];
    NSTableColumn *tCol;
    int noOfColumns = 7;
    for (int i=0; i<noOfColumns; i++)
    {
        tCol = [[NSTableColumn alloc] initWithIdentifier:[NSString stringWithFormat:@"key%d",i+1]];
        [tCol setWidth:100.0];
        [[tCol headerCell] setStringValue:[NSString stringWithFormat:@"Column %d",i+1]];
        [self.myTableView addTableColumn:tCol];
    }
    
    [self.myTableView setUsesAlternatingRowBackgroundColors:YES];
    [self.myTableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.myTableView setGridColor:[NSColor redColor]];
    [self.myTableView setRowHeight:23.0];
    [self.myTableView setDelegate:self];
    [self.myTableView setDataSource:self];
    [self.myTableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.myTableView setAutoresizesSubviews:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.myTableView];
    [tableViewPlaceholderView addSubview:scrollView];
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self createMutablearray];
    [self createTableView];
} // end awakeFromNib

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.mutableDataArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.mutableDataArray count];
    return recordCount;
}

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender{
    NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}


- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex
{
    NSString *editedtext = [self.mutableDataArray objectAtIndex:pRowIndex];
    NSString *selectedKey = [pTableColumn identifier];
    NSString *selectedItem = [[self.mutableDataArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    
    NSLog(@"Mutablearray   %@",[self.mutableDataArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@\n", selectedKey);

    NSLog(@"Mutablearray Selected  %@",selectedItem);

    NSLog(@"%d  %@\n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@\n", (NSString *)pObject);
    
    [[self.mutableDataArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    NSLog(@"Mutablearray again  %@",[self.mutableDataArray objectAtIndex:pRowIndex]);
};
- (IBAction)addAtSelectedRow:(id)pId
{
    NSInteger isel = [self.myTableView selectedRow];

    if(isel > 0) {
        mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"90001",_key1,@"90002",_key2,@"90003",_key3,
                             @"90004",_key4,@"90005",_key5,@"90006",_key6,@"90007",_key7, nil];
        [self.mutableDataArray insertObject:self.mutableDictionary atIndex:isel];
    };
    [self.myTableView reloadData];
}

- (IBAction)deleteSelectedRow:(id)pId
{
    NSInteger isel = [self.myTableView selectedRow];
    
    if(isel > 0) {
        [self.mutableDataArray removeObjectAtIndex:isel];
    };
    [self.myTableView reloadData];
}


@end
