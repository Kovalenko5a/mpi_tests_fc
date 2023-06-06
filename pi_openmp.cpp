#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
#include <iostream>
#include <iomanip>
long double f(long double y) {return(4.0/(1.0+y*y));}

int main(int argc, char *argv[]){
long double w,x,sum,pi;
long i;
long n;
int nthreads, tid;
if(argc>=2) {n = atoi(argv[1]); nthreads = atoi(argv[2]);}
else {n = 1000000000000; nthreads = 2;}
//set the number of threads
w = 1.0/n;
sum = 0.0;
omp_set_num_threads(nthreads);
double start = omp_get_wtime();
#pragma omp parallel for private(x) shared(w) reduction(+:sum)
for(i=0; i<n; i++)
	{
		x = w*(i-0.5);
		sum = sum + f(x);
	}
pi = w*sum;


double endt = omp_get_wtime();
//printf("pi = %f\n", pi);
std::cout << std::setprecision(9)  << pi  << "   " << endt - start << std::endl;

//return 0;
}
