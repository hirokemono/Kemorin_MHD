/*
 bitonic.c 

 This file contains two different implementations of the bitonic sort
        recursive  version :  rec
        imperative version :  BitonicSort_imp() 
 

 The bitonic sort is also known as Batcher Sort. 
 For a reference of the algorithm, see the article titled 
 Sorting networks and their applications by K. E. Batcher in 1968 


 The following codes take references to the codes avaiable at 

 http://www.cag.lcs.mit.edu/streamit/results/bitonic/code/c/bitonic.c

 http://www.tools-of-computing.com/tc/CS/Sorts/bitonic_sort.htm

 http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm 
 */

/* 
------- ---------------------- 
   Nikos Pitsianis, Duke CS 
-----------------------------

    modified by Antotsiou Dafni and Sourgkounis Theodosis

*/


#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <pthread.h>
#include <math.h>
#include <omp.h>

#include "quicksort_c.h"
#include "bitonic_sort_omp.h"

const int ASCENDING  = 1;
const int DESCENDING = 0;


void init(long num, long nArray, int *ires, int *org, long *idx);
void print(long num, long nArray, const int *org, 
           const int *ires, const long *idx);
void test(long num, const int *ires);

void bitonicsort_rec(long num, int *ires, long *idx);
void BitonicSort_imp(long num, int *ires, long *idx);
void recBitonicSort(long lo, long hi, int iflag_ascend,
                    int *ires, long *idx);
void bitonicMerge(long lo, long hi, int iflag_ascend,
                  int *ires, long *idx);

void bitonicsort_Pthread(int nthreads, long num, int *ires, long *idx);
void * PrecBitonicSort(void *arg);
void * PbitonicMerge(void *arg);

void * thread_work(void *arg);
int imax_array_pthreads(int nthreads, long num, int *ires);
void flip_sign_pthreads(int nthreads, long num, int *ires);


/** compare for qsort **/
int desc( const void *a, const void *b ){
    int* arg1 = (int *)a;
    int* arg2 = (int *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 > *arg2 ){ return -1;};
    return 1;
}
int asc( const void *a, const void *b ){
    int* arg1 = (int *)a;
    int* arg2 = (int *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 < *arg2 ){ return -1;};
    return 1;
}

int desc_dbl( const void *a, const void *b ){
    double* arg1 = (double *)a;
    double* arg2 = (double *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 > *arg2 ){ return -1;};
    return 1;
}
int asc_dbl( const void *a, const void *b ){
    double* arg1 = (double *)a;
    double* arg2 = (double *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 < *arg2 ){ return -1;};
    return 1;
}

int desc_long( const void *a, const void *b ){
    long* arg1 = (long *)a;
    long* arg2 = (long *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 > *arg2 ){ return -1;};
    return 1;
}
int asc_long( const void *a, const void *b ){
    long* arg1 = (long *)a;
    long* arg2 = (long *)b;
    if( *arg1 == *arg2 ){ return 0;}
    else if( *arg1 < *arg2 ){ return -1;};
    return 1;
}

/** the main program **/ 
int main( int argc, char **argv ) {
    struct timeval startwtime, endwtime;
    double seq_time1, seq_time2, seq_time3;
    double seq_time4, seq_time5;
    
    if (argc != 3 || atoi( argv[ 2 ] ) > 256 ) {
        printf("Usage: %s n t\n  where n is problem size,");
        printf(" and t is the number of threads to use.\n", argv[ 0 ] );
        exit( 1 );
    }
    
    long Narray =  atol(argv[1]);
    int nthreads = atoi(argv[2]);
//    long Narray =  1 << atoi(argv[1]);
//    int nthreads = 1 << atoi(argv[2]);
    
    int nextP2 =  1 + (int) log2((double) (Narray-1));
    long narrayP2 =  1 << nextP2;
    /*
    int nextP2n = 1 + (int) log2((double) (Narray-2));
    int nextP2p = 1 + (int) log2((double) (Narray  ));
    long narrayP2n =  1 << nextP2n;
    long narrayP2p =  1 << nextP2p;
    printf("nextP2   %d %d %d \n", nextP2n,   nextP2,   nextP2p);
    printf("narrayP2 %d %d %d \n", narrayP2n, narrayP2, narrayP2p);
    */
    
    int *ia;
    ia = (int *) malloc(narrayP2 * sizeof(int));
    int *org = (int *) malloc(narrayP2 * sizeof(int));
    long *idx = (long *) malloc(narrayP2 * sizeof(long));
    
    init(Narray, narrayP2, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    quicksort_int_c(ia, idx, 0, (Narray-1));
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Quicksort ");
    test(Narray, ia);
    print(Narray, narrayP2, org, ia, idx);
    printf("-------------------------------------\n");
    
    init(Narray, narrayP2, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_rec(narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial   recursive ");
    test(Narray, ia);
    print(Narray, narrayP2, org, ia, idx);
    printf("-------------------------------------\n");
    
    init(Narray, narrayP2, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    bitonicsort_Pthread(nthreads, narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time3 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic parallel recursive with %i threads ", nthreads);
    test(Narray, ia);
    print(Narray, narrayP2, org, ia, idx);
    printf("-------------------------------------\n");
    
    
    init(Narray, narrayP2, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    BitonicSort_imp(narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time4 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic serial  imperative ");
    test(Narray, ia);
    print(Narray, narrayP2, org, ia, idx);
    printf("-------------------------------------\n");
    
    init(Narray, narrayP2, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    OMPimp_int_BitonicSort(nthreads, narrayP2, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time5 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("OpenMP Bitonic parallel imperagive with %i threads ", nthreads);
    test(Narray, ia);
    print(Narray, narrayP2, org, ia, idx);
    printf("-------------------------------------\n\n");
    
    printf("                 Quicksort wall clock time = %f\n", seq_time1);
    printf("Bitonic serial   recursive wall clock time = %f\n", seq_time2);
    printf("Bitonic parallel recursive with %i threads\n", nthreads);
    printf("             and quicksort wall clock time = %f\n", seq_time3);
    printf("Bitonic serial  imperative wall clock time = %f\n", seq_time4);
    printf("OpenMP Bitonic parallel imperagive \n");
    printf("           with %i threads wall clock time = %f\n", nthreads,  seq_time5);
    printf("-------------------------------------\n");
    
    int imax1, imax2, imax3;
    gettimeofday( &startwtime, NULL );
    imax1 = max_int_array(Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    
    gettimeofday( &startwtime, NULL );
    imax2 = imax_array_pthreads(nthreads, Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time2 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    
    gettimeofday( &startwtime, NULL );
    imax3 = max_int_array_omp(nthreads, Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time3 = (double)( ( endwtime.tv_usec - startwtime.tv_usec )
                         / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    
    printf("max_int_array          %d\n", imax1);
    printf("imax_array_pthreads %d\n", imax2);
    printf("max_int_array_omp      %d\n", imax3);
    printf("-------------------------------------\n");
    
    printf("           Serial maximum                 wall clock time = %f\n", seq_time1);
    printf("pthreads parallel maximum with %i threads wall clock time = %f\n",
           nthreads,  seq_time2);
    printf("OpenMP   parallel maximum with %i threads wall clock time = %f\n",
           nthreads,  seq_time3);
    printf("-------------------------------------\n");
    
    
    gettimeofday( &startwtime, NULL );
    flip_int_sign(Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("         Serial flip_int_sign                   wall clock time = %f\n", seq_time1 );
    
    gettimeofday( &startwtime, NULL );
    flip_sign_pthreads(nthreads, Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("pthreads parallel flip_int_sign with %i threads wall clock time = %f\n",
           nthreads,  seq_time1 );
    
    gettimeofday( &startwtime, NULL );
    flip_int_sign_omp(nthreads, Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time1 = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                         + endwtime.tv_sec - startwtime.tv_sec );
    printf("OpenMP   parallel flip_int_sign with %i threads wall clock time = %f\n", 
            nthreads,  seq_time1);
    
    
    free(ia);
}

/** -------------- SUB-PROCEDURES  ----------------- **/ 




/** procedure test() : verify bitonicsort_rec results **/
void test(long num, const int *ires){
  int pass = 1;
  long i;
    for (i=1;i<num;i++) {
    pass &= (ires[i-1] <= ires[i]);
  }
    
  printf(" TEST %s\n",(pass) ? "PASSed" : "FAILed");
}


/** procedure init(long num, int *ires, int *org, long *idx) : initialize array "ires" with data **/
void init(long num, long nArray, int *ires, int *org, long *idx) {
    long i;
    for (i=0;i<num;i++) {
        ires[i] = rand(); // (N - i);
//        ires[i] = rand() % num; // (N - i);
        org[i] = ires[i];
        idx[i] = i;
    }
    int imax = max_int_array(num, ires);
    for (i=num;i<nArray;i++) {
        ires[i] = imax + 1;
        org[i] = ires[i];
        idx[i] = -1;
    }
}

/** procedure  print() : print array elements **/
void print(long num, long nArray, const int *org, 
           const int *ires, const long *idx) {
    int i;
    if(num > 33) return;
  for (i = 0; i < num; i++) {
        printf("%d %d: %d %d \n", i, org[i], (int) idx[i], ires[i]);
  }
  printf("\n");
  for (i = num; i < nArray; i++) {
        printf("%d %d: %d %d \n", i, org[i], (int) idx[i], ires[i]);
  }
}


/** Procedure bitonicMerge() 
   It recursively sorts a bitonic sequence in ascending order, 
   if iflag_ascend = ASCENDING, and in descending order otherwise. 
   The sequence to be sorted starts at index position lo,
   the parameter cbt is the number of elements to be sorted. 
 **/
void bitonicMerge(long lo, long hi, int iflag_ascend,
                  int *ires, long *idx) {
    long i;
    if (hi>1) {
        long k = hi/2;
        for (i=lo; i<lo+k; i++){
            if (iflag_ascend == (ires[i]>ires[i+k])){
                exchange(&ires[i], &ires[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        };
        
        bitonicMerge(lo,   k, iflag_ascend, ires, idx);
        bitonicMerge(lo+k, k, iflag_ascend, ires, idx);
    }
}



/** function recBitonicSort() 
    first produces a bitonic sequence by recursively sorting 
    its two halves in opposite sorting orders, and then
    calls bitonicMerge to make them in the same order 
 **/
void recBitonicSort(long lo, long hi, int iflag_ascend,
                    int *ires, long *idx) {
  if (hi>1) {
    long k = hi/2;
    recBitonicSort(lo, k, ASCENDING, ires, idx);
    recBitonicSort(lo+k, k, DESCENDING, ires, idx);
    bitonicMerge(lo, hi, iflag_ascend, ires, idx);
  }
}


/** function bitonicsort_rec() 
   Caller of recBitonicSort for sorting the entire array of length N 
   in ASCENDING order
 **/
void bitonicsort_rec(long num, int *ires, long *idx) {
  recBitonicSort(0, num, ASCENDING, ires, idx);
}



/*
  imperative version of bitonic sort
*/
void BitonicSort_imp(long num, int *ires, long *idx){
    long i, j, k, ij;
    
    for (k=2; k<=num; k=2*k) {
        for (j=k>>1; j>0; j=j>>1) {
            for (i=0; i<num; i++) {
                ij=i^j;
                if ((ij)>i) {
                    if ((i&k)==0 && ires[i] > ires[ij]){
                        exchange(&ires[i], &ires[ij]);
                        exchange_long(&idx[i], &idx[ij]);
                    };
                    if ((i&k)!=0 && ires[i] < ires[ij]){
                        exchange(&ires[i], &ires[ij]);
                        exchange_long(&idx[i], &idx[ij]);
                    };
                }
            }
        }
    }
}


typedef struct{
    int id;
    int nthreads;
    
    long lo;
    long hi;
    int iflag_ascend;
    int maxlayers;
    int layer;
    
    int  *ires;
    long *idx;
} sarg;

/** Procedure bitonicMerge
 *  Same as serial, but uses pthreads.
 **/
void * PbitonicMerge(void *arg){
    long i;
    
    long lo = ((sarg *) arg)->lo;
    long hi = ((sarg *) arg)->hi;
    int iflag_ascend = ((sarg *) arg)->iflag_ascend;
    int maxlayers =    ((sarg *) arg)->maxlayers;
    int layer = ((sarg *) arg)->layer;
    int *ires = ((sarg *) arg)->ires;
    long *idx = ((sarg *) arg)->idx;
    
    if( hi > 1 ){
        long k = hi / 2;
        for(i=lo;i<(lo+k);++i){
            if (iflag_ascend == (ires[i]>ires[i+k])){
                exchange(&ires[i], &ires[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        }
        if( layer <= 0 ){
            bitonicMerge(lo,     k, iflag_ascend, ires, idx);
            bitonicMerge((lo+k), k, iflag_ascend, ires, idx);
            return 0;
        }
        
        pthread_t thread1;
        sarg arg1;
        arg1.lo = lo;
        arg1.hi = k;
        arg1.iflag_ascend = iflag_ascend;
        arg1.maxlayers =    maxlayers;
        arg1.layer = layer - 1;
        arg1.ires = ires;
        arg1.idx =  idx;
        
        pthread_t thread2;
        sarg arg2;
        arg2.lo = lo + k;
        arg2.hi = k;
        arg2.iflag_ascend = iflag_ascend;
        arg2.maxlayers =    maxlayers;
        arg2.layer = layer - 1;
        arg2.ires = ires;
        arg2.idx =  idx;
        
        pthread_create( &thread1, NULL, PbitonicMerge, &arg1 );
        pthread_create( &thread2, NULL, PbitonicMerge, &arg2 );
        
        pthread_join(thread1, NULL);
        pthread_join(thread2, NULL);
    }
    return 0;
}

/** function PrecBitonicSort() 
    first produces a bitonic sequence by recursively sorting 
    its two halves in opposite sorting orders, and then
    calls bitonicMerge to make them in the same order 

    Uses pthreads
 **/

void * PrecBitonicSort(void *arg){
    long lo = ((sarg *) arg)->lo;
    long hi = ((sarg *) arg)->hi;
    int iflag_ascend = ((sarg *) arg)->iflag_ascend;
    int maxlayers =    ((sarg *) arg)->maxlayers;
    int layer = ((sarg *) arg)->layer;
    int *ires = ((sarg *) arg)->ires;
    long *idx = ((sarg *) arg)->idx;
    if ( hi > 1 ) {
        long k = hi / 2;
        if(layer >= maxlayers) {
            quicksort_int_c(&ires[lo  ], &idx[lo  ], 0, (k-1));
            
            flip_int_sign(k, &ires[lo+k]);
            quicksort_int_c(&ires[lo+k], &idx[lo+k], 0, (k-1));
            flip_int_sign(k, &ires[lo+k]);
        }
        else{
            sarg arg1;
            pthread_t thread1;
            arg1.lo = lo;
            arg1.hi = k;
            arg1.iflag_ascend = ASCENDING;
            arg1.maxlayers =    maxlayers;
            arg1.layer = layer + 1;
            arg1.ires = ires;
            arg1.idx =  idx;
            
            sarg arg2;
            pthread_t thread2;
            arg2.lo = lo + k;
            arg2.hi = k;
            arg2.iflag_ascend = DESCENDING;
            arg2.maxlayers =    maxlayers;
            arg2.layer = layer + 1;
            arg2.ires = ires;
            arg2.idx =  idx;
            
            pthread_create(&thread1, NULL, PrecBitonicSort, &arg1);
            pthread_create(&thread2, NULL, PrecBitonicSort, &arg2);
            
            pthread_join(thread1, NULL);
            pthread_join(thread2, NULL);
        }
        sarg arg3;
        arg3.lo = lo;
        arg3.hi = hi;
        arg3.iflag_ascend = iflag_ascend;
        arg3.maxlayers =    maxlayers;
        arg3.layer = maxlayers - layer;
        arg3.ires = ires;
        arg3.idx =  idx;
        PbitonicMerge(&arg3);
    }
    return 0;
}

/** function sort() 
   Caller of recBitonicSort for sorting the entire array of length N 
   in ASCENDING order
 **/
void bitonicsort_Pthread(int nthreads, long num, int *ires, long *idx){
    int threadlayers = 0;
    if(nthreads != 0 && nthreads != 1) {
        threadlayers = 1 + (int) log2((double) (nthreads-1));
    }
    
    sarg arg;
    arg.lo = 0;
    arg.hi = (int) num;
    arg.iflag_ascend = ASCENDING;
    arg.maxlayers = threadlayers;
    arg.layer = 0;
    arg.ires = ires;
    arg.idx = idx;
    
    PrecBitonicSort(&arg);
}


void * thread_work(void *args)
{
    int id = ((sarg *) args)->id;
    int nthreads = ((sarg *) args)->nthreads;
    int lo = ((sarg *) args)->lo;
    int hi = ((sarg *) args)->hi;
    int *ires = ((sarg *) args)->ires;
    long *imax = ((sarg *) args)->idx;
    
    /* Get portion of array to process. */
    int start = hi * id /     nthreads; /* Thread is in charge of [start, start+n] elements */
    int end =   hi * (id+1) / nthreads; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    imax[id] = ires[start];
    for(int i=start+1;i<end;i++){
        if(ires[i] > imax[id]){imax[id] = ires[i];};
    };
    return 0;
}

int imax_array_pthreads(int nthreads, long num, int *ires){
    int i;
    /* Allocate thread arguments. */
    sarg *args = (sarg *) malloc (nthreads * sizeof(sarg));
    if (!args) {fprintf (stderr, "Malloc failed for sarg.\n"); exit(1);}
    long *imax = (long *) malloc (nthreads * sizeof(long));
    if (!imax) {fprintf (stderr, "Malloc failed for imax.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].nthreads = nthreads;
        args[i].lo = 0;
        args[i].hi = num;
        args[i].ires = ires;
        args[i].idx =  imax;
/* Create a thread. */
//        printf ("####### CREATING THREAD id = %d\n", args[i].id);
        pthread_create(&thread_handles[i], NULL, thread_work, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {
        pthread_join(thread_handles[i], NULL);
    }
    free(args);
    
    long imax_out = imax[0];
    for (i=0;i<nthreads;i++) {
        if(imax[i] > imax_out){imax_out = imax[i];};
//        printf("imax[i] %d %d \n", i, imax[i]);
    };
    return imax_out;
}


void * flip_sign_work(void *args)
{
    int id = ((sarg *) args)->id;
    int nthreads = ((sarg *) args)->nthreads;
    int hi = ((sarg *) args)->hi;
    int *ires = ((sarg *) args)->ires;
    
    /* Get portion of array to process. */
    int start = hi * id /     nthreads; /* Thread is in charge of [start, start+n] elements */
    int end =   hi * (id+1) / nthreads; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    for(int i=start+1;i<end;i++){ires[i] = -ires[i];};
    return 0;
}

void flip_sign_pthreads(int nthreads, long num, int *ires){
    int i;
    /* Allocate thread arguments. */
    sarg *args = (sarg *) malloc (nthreads * sizeof(sarg));
    if (!args) {fprintf (stderr, "Malloc failed for sarg.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].nthreads = nthreads;
        args[i].hi = num;
        args[i].ires = ires;
/* Create a thread. */
//        printf ("####### CREATING THREAD id = %d\n", args[i].id);
        pthread_create(&thread_handles[i], NULL, flip_sign_work, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {
        pthread_join(thread_handles[i], NULL);
    }
    free(args);
    return;
}
