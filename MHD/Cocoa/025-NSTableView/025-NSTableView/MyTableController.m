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

-(void)createMutablearray
{
    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"10001",@"key1",@"10002",@"key2",@"10003",@"key3",
                          @"10004",@"key4",@"10005",@"key5",@"10006",@"key6",@"10007",@"key7", nil];
    
    self.mutableDataArray = [[NSMutableArray alloc]init];    
    [self.mutableDataArray addObject:mutableDictionary];

    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"20001",@"key1",@"20002",@"key2",@"20003",@"key3",
            @"20004",@"key4",@"20005",@"key5",@"20006",@"key6",@"20007",@"key7", nil];
    [self.mutableDataArray addObject:mutableDictionary];
    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"30001",@"key1",@"30002",@"key2",@"30003",@"key3",
            @"30004",@"key4",@"30005",@"key5",@"30006",@"key6",@"30007",@"key7", nil];
    [self.mutableDataArray addObject:mutableDictionary];
    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"40001",@"key1",@"40002",@"key2",@"40003",@"key3",
            @"40004",@"key4",@"40005",@"key5",@"40006",@"key6",@"40007",@"key7", nil];
    [self.mutableDataArray addObject:mutableDictionary];
    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"50001",@"key1",@"50002",@"key2",@"50003",@"key3",
            @"50004",@"key4",@"50005",@"key5",@"50006",@"key6",@"50007",@"key7", nil];
    [self.mutableDataArray addObject:mutableDictionary];
    mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"60001",@"key1",@"60002",@"key2",@"60003",@"key3",
            @"60004",@"key4",@"60005",@"key5",@"60006",@"key6",@"60007",@"key7", nil];
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
        mutableDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"90001",@"key1",@"90002",@"key2",@"90003",@"key3",
                             @"90004",@"key4",@"90005",@"key5",@"90006",@"key6",@"90007",@"key7", nil];
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
