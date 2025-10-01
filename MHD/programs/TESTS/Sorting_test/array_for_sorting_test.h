/*
 *  array_for_sorting_test.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 05/06/24.
 *
 */

#ifndef ARRAY_FOR_SORTING_TEST_
#define ARRAY_FOR_SORTING_TEST_

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "quicksort_c.h"

/** compare for qsort **/
int desc( const void *a, const void *b );
int asc( const void *a, const void *b );
int desc_dbl( const void *a, const void *b );
int asc_dbl( const void *a, const void *b );
int desc_long( const void *a, const void *b );
int asc_long( const void *a, const void *b );

/** procedure to set randum values to sort **/
void init_Int_Array(long num, long nArray, int *iorg);
void init_Long_Array(long num, long nArray, long *lorg);
void init_Double_Array(long num, long nArray, double *dorg);
void init_Float_Array(long num, long nArray, float *org);

/** procedure to copy randum values to sort **/
void copy_Int_Array(const long num, const long nArray, 
                    const int *iorg, int *ires, long *idx);
void copy_Long_Array(const long num, const long nArray,
                     const long *lorg, long *lres, long *idx);
void copy_Double_Array(const long num, long const nArray, 
                       const double *dorg, double *dres, long *idx);
void copy_Float_Array(const long num, const long nArray, 
                      const float *org, float *res, long *idx);

/** procedure to verify sort results **/
void check_sorted_Int(long num, const int *ires);
void check_sorted_Double(long num, const double *dres);
void check_sorted_Float(long num, const float *res);

/** Output sorted lists for short array **/
void print_sorted_Int(long num, long nArray, const int *iorg,
                      const int *ires, const long *idx);
void print_sorted_Long(long num, long nArray, const long *iorg,
                       const long *ires, const long *idx);

void print_sorted_Double(long num, long nArray, const double *dorg, 
                         const double *dres, const long *idx);
void print_sorted_Float(long num, long nArray, const float *org,
                        const float *res, const long *idx);

#endif /* ARRAY_FOR_SORTING_TEST_ */
