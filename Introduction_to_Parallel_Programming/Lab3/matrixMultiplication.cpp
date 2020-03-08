#include <iostream>
#include <omp.h>

using namespace std;

int main()
{
	cout << "Running code when outer ALL loops are parallelized: " << endl;

	const int dim = 3;

	int a[dim][dim] = { {1,2,3},
					{4,5,6},
					{7,8,9},
	};

	int b[dim][dim] = { {1,2,3},
					{4,5,6},
					{7,8,9},
	};

	int c[dim][dim];

	int i, j, k;

	double startTime = omp_get_wtime();
	#pragma omp parallel
	{
		#pragma omp critical
		if (omp_get_thread_num() == 0)cout << "Number of threads: " << omp_get_num_threads() << endl;
		
		#pragma omp for schedule(static)
		for (i = 0; i < dim; i++)
		{
			#pragma omp parallel
			{
				#pragma omp for schedule(static)
				for (j = 0; j < dim; j++)
				{
					c[i][j] = 0;
					#pragma omp parallel
					{
						#pragma omp for schedule(static)
						for (k = 0; k < dim; k++)
						{
							#pragma omp critical
							{
								cout << "I am thread " << omp_get_thread_num() << " this is my value i: " << i << ", j: " << j << ", k: " << k << endl;
							}
							c[i][j] += a[i][k] * b[k][j];
						}
					}
				}
			}
		}
	}

	cout << "Result: " << endl;
	for (int i = 0; i < dim; i++)
	{
		for (int j = 0; j < dim; j++)
		{
			cout << c[i][j] << " ";
		}
		cout << endl;
	}

	cout << endl;

	cout << "Program ended in: " << (omp_get_wtime() - startTime) << endl;
	return 0;
}