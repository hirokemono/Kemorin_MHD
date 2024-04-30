/*
 bitonic.c 

 This file contains two different implementations of the bitonic sort
        recursive  version :  rec
        imperative version :  impBitonicSort() 
 

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


struct timeval startwtime, endwtime;
double seq_time;


int nt;          // data array size

int threadlayers;

const int ASCENDING  = 1;
const int DESCENDING = 0;


void init(int num, int *ires, int *org, long *idx);
void print(long num, const int *org, const int *ires, const long *idx);
void test(int num, const int *ires);
//inline void exchange(int i, int j);
void exchange(int *i, int *j);
void exchange_long(long *i, long *j);
void exchange_double(double *x, double *y);

void quicksort_int_c(int *ivec, long *list, int lo, int hi);
void sort(int num, int *ires, long *idx);
void impBitonicSort(int num, int *ires, long *idx);
void recBitonicSort( int lo, int cnt, int dir, int *ires, long *idx);
void bitonicMerge( int lo, int cnt, int dir, int *ires, long *idx);

void Psort(int num, int *ires, long *idx);
void * PrecBitonicSort(void *arg);
void * PbitonicMerge(void *arg);
void OMPimpBitonicSort(int num, int *ires, long *idx);

int imax_array(long num, const int *ires);
int imax_array_omp(long num, const int *ires);
void * thread_work(void *arg);
int imax_array_pthreads(long num, int *ires);

void flip_sign(long num, int *ires);
void flip_sign_omp(long num, int *ires);
void flip_sign_pthreads(long num, int *ires);


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
    
    if (argc != 3 || atoi( argv[ 2 ] ) > 256 ) {
        printf("Usage: %s q t\n  where n=2^q is problem size (power of two),");
        printf(" and t is the number of threads, <=256, to use.\n", argv[ 0 ] );
        exit( 1 );
    }
    
    int Narray = 1 << atoi( argv[ 1 ] );
    nt = atoi( argv[ 2 ] );
    
    threadlayers = nt;
    if( nt != 0 && nt != 1 ) {
        threadlayers = nt - 1;
    }
    
    int *ia;
    ia = (int *) malloc( Narray * sizeof( int ) );
    int *org = (int *) malloc( Narray * sizeof( int ) );
    long *idx = (long *) malloc( Narray * sizeof( long ) );
    
    init(Narray, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    quicksort_int_c(ia, idx, 0, Narray);
    gettimeofday( &endwtime, NULL );
    seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf( "quicksort wall clock time = %f\n", seq_time );
    test(Narray, ia);
    print(Narray, org, ia, idx);
   
    init(Narray, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    sort(Narray, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf( "Bitonic serial recursive wall clock time = %f\n", seq_time );
    test(Narray, ia);
    print(Narray, org, ia, idx);
    
    init(Narray, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    Psort(Narray, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf( "Bitonic parallel recursive with qsort and %i threads wall clock time = %f\n", 1 << nt, seq_time );
    test(Narray, ia);
    print(Narray, org, ia, idx);
    
    init(Narray, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    impBitonicSort(Narray, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf( "Bitonic serial imperative wall clock time = %f\n", seq_time );
    test(Narray, ia);
    print(Narray, org, ia, idx);
    
    init(Narray, ia, org, idx);
    gettimeofday( &startwtime, NULL );
    OMPimpBitonicSort(Narray, ia, idx);
    gettimeofday( &endwtime, NULL );
    seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf( "OpenMP Bitonic parallel imperagive with %i threads wall clock time = %f\n", 1 << nt,  seq_time );
    test(Narray, ia);
    print(Narray, org, ia, idx);
    
    
    int imax;
    gettimeofday( &startwtime, NULL );
    imax = imax_array((long) Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("maximum wall clock time = %f\n", seq_time );
    printf("imax_array %d\n", imax);
    
    gettimeofday( &startwtime, NULL );
    imax = imax_array_pthreads((long) Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("pthreads parallel maximum with %i threads wall clock time = %f\n", 1 << nt,  seq_time );
    printf("imax_array_pthreads %d\n", imax);
    
    gettimeofday( &startwtime, NULL );
    imax = imax_array_omp((long) Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf( "OpenMP parallel maximum with %i threads wall clock time = %f\n", 1 << nt,  seq_time );
    printf("imax_array_omp %d\n", imax);
    
    
    gettimeofday( &startwtime, NULL );
    flip_sign((long) Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("flip_sign wall clock time = %f\n", seq_time );
    
    gettimeofday( &startwtime, NULL );
    flip_sign_pthreads((long) Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf("pthreads parallel flip_sign with %i threads wall clock time = %f\n", 1 << nt,  seq_time );
    
    gettimeofday( &startwtime, NULL );
    flip_sign_omp((long) Narray, ia);
    gettimeofday( &endwtime, NULL );
    seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6 + endwtime.tv_sec - startwtime.tv_sec );
    printf( "OpenMP parallel flip_sign with %i threads wall clock time = %f\n", 1 << nt,  seq_time );
    
    
    free(ia);
}

/** -------------- SUB-PROCEDURES  ----------------- **/ 




/** procedure test() : verify sort results **/
void test(int num, const int *ires){
  int pass = 1;
  int i;
  for (i = 1; i < num; i++) {
    pass &= (ires[i-1] <= ires[i]);
  }
    
  printf(" TEST %s\n",(pass) ? "PASSed" : "FAILed");
}


/** procedure init(int num, int *ires, int *org, long *idx) : initialize array "ires" with data **/
void init(int num, int *ires, int *org, long *idx) {
  int i;
  for (i = 0; i < num; i++) {
        ires[i] = rand() % num; // (N - i);
        org[i] = ires[i];
        idx[i] = i;
    }
}

/** procedure  print() : print array elements **/
void print(long num, const int *org, const int *ires, const long *idx) {
    int i;
    if(num > 33) return;
  for (i = 0; i < num; i++) {
        printf("%d %d: %d %d \n", i, org[i], (int) idx[i], ires[i]);
  }
  printf("\n");
}


/** INLINE procedure exchange() : pair swap **/
inline void exchange(int *i, int *j) {
  int t;
  t = *i;
  *i = *j;
  *j = t;
}

inline void exchange_long(long *i, long *j) {
  long t;
  t = *i;
  *i = *j;
  *j = t;
}

inline void exchange_double(double *x, double *y) {
  double t;
  t = *x;
  *x = *y;
  *y = t;
}

void quicksort_int_c(int *ivec, long *list, int lo, int hi) {
    int pivot, itmp;
    long i, j, it8;
	
	
	if(lo == hi) return; 
	i=lo; 
	j=hi;
	pivot= ivec[(lo+hi)/2]; 
	
	/* Split the array into two parts */
	do {    
		while (ivec[i] < pivot) i++; 
		while (ivec[j] > pivot) j--;
		if (i<=j) {
			itmp = ivec[i];
			ivec[i] = ivec[j];
			ivec[j] = itmp;
			it8 =    list[i];
			list[i] = list[j];
			list[j] = it8;
			i++;
			j--;
		}
	} while (i<=j);
	
	if (lo < j) quicksort_int_c(ivec, list, lo, j);
	if (i < hi) quicksort_int_c(ivec, list, i, hi);
	return;
};

/** Procedure bitonicMerge() 
   It recursively sorts a bitonic sequence in ascending order, 
   if dir = ASCENDING, and in descending order otherwise. 
   The sequence to be sorted starts at index position lo,
   the parameter cbt is the number of elements to be sorted. 
 **/
void bitonicMerge(int lo, int cnt, int dir, int *ires, long *idx) {
    if (cnt>1) {
        int k=cnt/2;
        int i;
        for (i=lo; i<lo+k; i++){
            if (dir == (ires[i]>ires[i+k])){
                exchange(&ires[i], &ires[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        };
        
        bitonicMerge(lo, k, dir, ires, idx);
        bitonicMerge(lo+k, k, dir, ires, idx);
    }
}



/** function recBitonicSort() 
    first produces a bitonic sequence by recursively sorting 
    its two halves in opposite sorting orders, and then
    calls bitonicMerge to make them in the same order 
 **/
void recBitonicSort(int lo, int cnt, int dir, int *ires, long *idx) {
  if (cnt>1) {
    int k=cnt/2;
    recBitonicSort(lo, k, ASCENDING, ires, idx);
    recBitonicSort(lo+k, k, DESCENDING, ires, idx);
    bitonicMerge(lo, cnt, dir, ires, idx);
  }
}


/** function sort() 
   Caller of recBitonicSort for sorting the entire array of length N 
   in ASCENDING order
 **/
void sort(int num, int *ires, long *idx) {
  recBitonicSort(0, num, ASCENDING, ires, idx);
}



/*
  imperative version of bitonic sort
*/
void impBitonicSort(int num, int *ires, long *idx){
    int i, j, k, ij;
    
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
    int nt;
    
    int lo;
    int cnt;
    int dir;
    int layer;
    
    int  *ires;
    long *idx;
} sarg;

/** Procedure bitonicMerge
 *  Same as serial, but uses pthreads.
 **/
void * PbitonicMerge(void *arg){
    int lo = ((sarg *) arg)->lo;
    int cnt = ((sarg *) arg)->cnt;
    int dir = ((sarg *) arg)->dir;
    int layer = ((sarg *) arg)->layer;
    int *ires = ((sarg *) arg)->ires;
    long *idx = ((sarg *) arg)->idx;
    
    if( cnt > 1 ){
        int k = cnt / 2;
        int i;
        for( i = lo; i < lo + k; ++i ){
            if (dir == (ires[i]>ires[i+k])){
                exchange(&ires[i], &ires[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        }
        if( layer <= 0 ){
            bitonicMerge(lo,     k, dir, ires, idx);
            bitonicMerge((lo+k), k, dir, ires, idx);
            return 0;
        }
        
        pthread_t thread1;
        sarg arg1;
        arg1.lo = lo;
        arg1.cnt = k;
        arg1.dir = dir;
        arg1.layer = layer - 1;
        arg1.ires = ires;
        arg1.idx =  idx;
        
        pthread_t thread2;
        sarg arg2;
        arg2.lo = lo + k;
        arg2.cnt = k;
        arg2.dir = dir;
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
    int lo = ((sarg *) arg)->lo;
    int cnt = ((sarg *) arg)->cnt;
    int dir = ((sarg *) arg)->dir;
    int layer = ((sarg *) arg)->layer;
    int *ires = ((sarg *) arg)->ires;
    long *idx = ((sarg *) arg)->idx;
    if ( cnt > 1 ) {
        int k = cnt / 2;
        if( layer >= threadlayers ) {
            qsort( &ires[lo  ], k, sizeof( int ), asc );
            qsort( &ires[lo+k], k, sizeof( int ), desc );
        }
        else{
            sarg arg1;
            pthread_t thread1;
            arg1.lo = lo;
            arg1.cnt = k;
            arg1.dir = ASCENDING;
            arg1.layer = layer + 1;
            arg1.ires = ires;
            arg1.idx =  idx;
            pthread_create( &thread1, NULL, PrecBitonicSort, &arg1 );
            
            sarg arg2;
            pthread_t thread2;
            arg2.lo = lo + k;
            arg2.cnt = k;
            arg2.dir = DESCENDING;
            arg2.layer = layer + 1;
            arg2.ires = ires;
            arg2.idx =  idx;
            pthread_create( &thread2, NULL, PrecBitonicSort, &arg2 );
            
            
            pthread_join( thread1, NULL );
            pthread_join( thread2, NULL );
        }
        sarg arg3;
        arg3.lo = lo;
        arg3.cnt = cnt;
        arg3.dir = dir;
        arg3.layer = threadlayers - layer;
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
void Psort(int num, int *ires, long *idx){
    sarg arg;
    arg.lo = 0;
    arg.cnt = num;
    arg.dir = ASCENDING;
    arg.layer = 0;
    arg.ires = ires;
    arg.idx = idx;
    
    PrecBitonicSort(&arg);
}



/*
  imperative version of bitonic sort with OpenMP
*/
void OMPimpBitonicSort(int num, int *ires, long *idx){
    int i, j, ij;
    int k=0;
    int term = (int) log2(num);
    
    omp_set_num_threads( nt );//"num" is the number of threads - arg[2]; 
    for (k = 2; k <= num; k *= 2 ) {
        for (j=k>>1; j>0; j=j>>1) {
#pragma omp parallel for private(i,ij)
            for(i=0; i<num; i++) {
                ij=i^j;
                if ((ij)>i) {
                    if ((i&k)==0 && ires[i] > ires[ij]){
                        exchange(&ires[i],&ires[ij]);
                        exchange_long(&idx[i],&idx[ij]);
                    }
                    if ((i&k)!=0 && ires[i] < ires[ij]){
                        exchange(&ires[i],&ires[ij]);
                        exchange_long(&idx[i],&idx[ij]);
                    }
                }
            }
        }
    }
} 


int imax_array(long num, const int *ires){
    int i, imax;
    imax = ires[0];
    for (i=0;i<num; i++) {
        if(ires[i] > imax){imax = ires[i];};
    };
  return imax;
}

int imax_array_omp(long num, const int *ires){
    int i, imax;
    imax = ires[0];
#pragma omp parallel for private(i) reduction(max:imax)
    for (i=0;i<num; i++) {
        if(ires[i] > imax){imax = ires[i];};
    };
  return imax;
}


void * thread_work(void *args)
{
    int id = ((sarg *) args)->id;
    int nt = ((sarg *) args)->nt;
    int lo = ((sarg *) args)->lo;
    int cnt = ((sarg *) args)->cnt;
    int *ires = ((sarg *) args)->ires;
    long *imax = ((sarg *) args)->idx;
    
    /* Get portion of array to process. */
    int start = cnt * id /     nt; /* Thread is in charge of [start, start+n] elements */
    int end =   cnt * (id+1) / nt; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    imax[id] = ires[start];
    for(int i=start+1;i<end;i++){
        if(ires[i] > imax[id]){imax[id] = ires[i];};
    };
    return 0;
}

int imax_array_pthreads(long num, int *ires){
    int i;
    /* Allocate thread arguments. */
    sarg *args = (sarg *) malloc (nt * sizeof(sarg));
    if (!args) {fprintf (stderr, "Malloc failed for sarg.\n"); exit(1);}
    long *imax = (long *) malloc (nt * sizeof(long));
    if (!imax) {fprintf (stderr, "Malloc failed for imax.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nt * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for ( i = 0; i < nt; i++ ) {
        args[i].id = i;
        args[i].nt = nt;
        args[i].lo = 0;
        args[i].cnt = num;
        args[i].ires = ires;
        args[i].idx =  imax;
/* Create a thread. */
//        printf ("####### CREATING THREAD id = %d\n", args[i].id);
        pthread_create(&thread_handles[i], NULL, thread_work, &args[i]);
    }
    
    for ( i = 1; i < nt; i++ ) {
        pthread_join(thread_handles[i], NULL);
    }
    free(args);
    
    long imax_out = imax[0];
    for (i=0;i<nt; i++) {
        if(imax[i] > imax_out){imax_out = imax[i];};
//        printf("imax[i] %d %d \n", i, imax[i]);
    };
    return imax_out;
}




void flip_sign(long num, int *ires){
    int i;
    for (i=0;i<num; i++) {ires[i] = -ires[i];};
    return;
}

void flip_sign_omp(long num, int *ires){
    int i;
#pragma omp parallel for private(i)
    for (i=0;i<num; i++) {ires[i] = -ires[i];};
    return;
}

void * flip_sign_work(void *args)
{
    int id = ((sarg *) args)->id;
    int nt = ((sarg *) args)->nt;
    int cnt = ((sarg *) args)->cnt;
    int *ires = ((sarg *) args)->ires;
    
    /* Get portion of array to process. */
    int start = cnt * id /     nt; /* Thread is in charge of [start, start+n] elements */
    int end =   cnt * (id+1) / nt; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    for(int i=start+1;i<end;i++){ires[i] = -ires[i];};
    return 0;
}

void flip_sign_pthreads(long num, int *ires){
    int i;
    /* Allocate thread arguments. */
    sarg *args = (sarg *) malloc (nt * sizeof(sarg));
    if (!args) {fprintf (stderr, "Malloc failed for sarg.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nt * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for ( i = 0; i < nt; i++ ) {
        args[i].id = i;
        args[i].nt = nt;
        args[i].cnt = num;
        args[i].ires = ires;
/* Create a thread. */
//        printf ("####### CREATING THREAD id = %d\n", args[i].id);
        pthread_create(&thread_handles[i], NULL, flip_sign_work, &args[i]);
    }
    
    for ( i = 1; i < nt; i++ ) {
        pthread_join(thread_handles[i], NULL);
    }
    free(args);
    return;
}
