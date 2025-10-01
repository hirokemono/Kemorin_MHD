/*
//  bitonicsort.metal
//
//  Created by Hiroaki Matsui on 5/1/24.
//
//  [Using Code based off of this](https://github.com/tgymnich/MetalSort)
//  Rewritten to make it more understandable.
//
*/

#include <metal_stdlib>
using namespace metal;

kernel void bitonicSort(device float     *floats     [[ buffer(0) ]],
                        device int       *indices       [[ buffer(1) ]],
                        constant int     &p             [[ buffer(2) ]],
                        constant int     &q             [[ buffer(3) ]],
                        uint             gid         [[ thread_position_in_grid ]])
{
    int pMinusQ = p-q;
    int distance = 1 << pMinusQ;
    uint gidShiftedByP = gid >> p;
    // True: Increasing / False: Descreasing
    bool direction = (gidShiftedByP & 2) == 0;
    uint gidDistance = (gid & distance);
    bool isGidDistanceZero = (gidDistance == 0);
    uint gidPlusDistance = (gid | distance);
    bool isLowerIndexGreaterThanHigher = (floats[gid] > floats[gidPlusDistance]);
    if (isGidDistanceZero && isLowerIndexGreaterThanHigher == direction) {
        float rtmp =              floats[gid];
        floats[gid] =             floats[gidPlusDistance];
        floats[gidPlusDistance] = rtmp;
        
        int itmp =     indices[gid];
        indices[gid] = indices[gidPlusDistance];
        indices[gidPlusDistance] = itmp;
    }
}

