
/*   quicksort_c.c */

#include "quicksort_c.h"

/*
#define maxsize 600
int A[maxsize];
int list[maxsize];
*/

void quicksort_int8_c(long *ivec, long *list, long lo, long hi) {
    long i, j, pivot, itmp;
	
	
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
			itmp =    list[i];
			list[i] = list[j];
			list[j] = itmp;
			i++;
			j--;
		}
	} while (i<=j);
	
	if (lo < j) quicksort_int8_c(ivec, list, lo, j);
	if (i < hi) quicksort_int8_c(ivec, list, i, hi);
	return;
};

void quicksort_double_c(double *dvec, long *list, long lo, long hi){
    long i, j;
	long itmp;
	double rtmp, pivot;
	
	if(lo >= hi) return; 
	i=lo; 
	j=hi;
	pivot= dvec[(lo+hi)/2]; 
	
	/* Split the array into two parts */
	do {    
		while (dvec[i] < pivot) i++; 
		while (dvec[j] > pivot) j--;
		if (i<=j) {
			rtmp = dvec[i];
			dvec[i] = dvec[j];
			dvec[j] = rtmp;
			itmp =    list[i];
			list[i] = list[j];
			list[j] = itmp;
			i++;
			j--;
		}
	} while (i<=j);
	
	if (lo < j) quicksort_double_c(dvec, list, lo, j);
	if (i < hi) quicksort_double_c(dvec, list, i, hi);
	return;
}

void quicksort_real_c(float *rvec, long *list, long lo, long hi){
    long i, j;
    long itmp;
	float rtmp, pivot;
	
	if(lo == hi) return; 
	i=lo; 
	j=hi;
	pivot= rvec[(lo+hi)/2]; 
	
	/* Split the array into two parts */
	do {    
		while (rvec[i] < pivot) i++; 
		while (rvec[j] > pivot) j--;
		if (i<=j) {
			rtmp = rvec[i];
			rvec[i] = rvec[j];
			rvec[j] = rtmp;
			itmp =    list[i];
			list[i] = list[j];
			list[j] = itmp;
			i++;
			j--;
		}
	} while (i<=j);
	
	if (lo < j) quicksort_real_c(rvec, list, lo, j);
	if (i < hi) quicksort_real_c(rvec, list, i, hi);
	return;
}

/*
void printarr(long n)
{
	int i;
	for(i=0;i<n;i++)
	{
		printf("address: %d, Value: %d \n",list[i], A[i]);
	}
	  }

main()
{
 long i,s;
	printf("enter the number of numbers to be entered \n");
	scanf("%d",&s);
	  for(i=0;i<s;i++)
	  {
	 printf("enter the number \n" );
	 scanf("%d",&A[i]);
	 }
	 printf("array before sorting \n");
	printarr(s);
	
	for(i=lo;i<hi+1;i++) list[i] = i;
  quicksort_int8_c(A, list, 0, s-1);
  printf("array after sorting\n");
  printarr(s);
}
*/
