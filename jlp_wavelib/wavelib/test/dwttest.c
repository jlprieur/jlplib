#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "../header/wavelib.h"

double absmax(double *array, int N) {
	double max;
	int i;

	max = 0.0;
	for (i = 0; i < N; ++i) {
		if (fabs(array[i]) >= max) {
			max = fabs(array[i]);
		}
	}

	return max;
}

int main() {
	wave_object obj;
	wt_object wt;
	double *inp,*out,*diff;
	int N, i,J;

	FILE *ifp;
	double temp[1200];

	char name1[32], dwt_str[16], sym_str[16], direct_str[16];

	strcpy(name1, "db4");
	strcpy(dwt_str, "dwt");
	strcpy(sym_str, "sym");
	strcpy(direct_str, "direct");

	obj = wave_init(name1);// Initialize the wavelet

	ifp = fopen("signal.txt", "r");
	i = 0;
	if (!ifp) {
		printf("Cannot Open File");
		exit(100);
	}
	while (!feof(ifp)) {
		fscanf(ifp, "%lf \n", &temp[i]);
		i++;
	}
	N = 256;

	inp = (double*)malloc(sizeof(double)* N);
	out = (double*)malloc(sizeof(double)* N);
	diff = (double*)malloc(sizeof(double)* N);
	//wmean = mean(temp, N);

	for (i = 0; i < N; ++i) {
		inp[i] = temp[i];
		//printf("%g \n",inp[i]);
	}
	J = 3;

	wt = wt_init(obj, dwt_str, N, J);// Initialize the wavelet transform object
	setDWTExtension(wt, sym_str);// Options are "per" and "sym". Symmetric is the default option
	setWTConv(wt, direct_str);
	
	dwt(wt, inp);// Perform DWT
	//DWT output can be accessed using wt->output vector. Use wt_summary to find out how to extract appx and detail coefficients
	
	for (i = 0; i < wt->outlength; ++i) {
		printf("%g ",wt->output[i]);
	}
	
	idwt(wt, out);// Perform IDWT (if needed)
	// Test Reconstruction
	for (i = 0; i < wt->siglength; ++i) {
		diff[i] = out[i] - inp[i];
	}
	
	printf("\n MAX %g \n", absmax(diff, wt->siglength)); // If Reconstruction succeeded then the output should be a small value.
	
	wt_summary(wt);// Prints the full summary.
	wave_free(obj);
	wt_free(wt);

	free(inp);
	free(out);
	free(diff);
	return 0;
}
