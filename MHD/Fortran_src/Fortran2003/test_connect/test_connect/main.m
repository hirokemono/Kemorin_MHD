//
//  main.m
//  test_connect
//
//  Created by Hiroaki Matsui on 2018/05/10.
//  Copyright © 2018年 Hiroaki Matsui. All rights reserved.
//

#import <Foundation/Foundation.h>

extern void c_read_control_sph_SGS_MHD(char *file_name);

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // insert code here...
        
        c_read_control_sph_SGS_MHD(file_name);
        
        NSLog(@"Hello, World!");
    }
    return 0;
}
