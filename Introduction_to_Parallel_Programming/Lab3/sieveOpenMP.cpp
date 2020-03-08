#include <iostream>
#include <math.h>
#include <omp.h>
#include <vector>

using namespace std;

#define LLI long long int
#define NUM_THREADS 3

LLI* seeds;
LLI seedSize;

#pragma region Computing Seeds
void computeSeeds(vector<LLI> vec, LLI maxValue)
{
	LLI seedRange = sqrt(maxValue);
	vec[0] = -1;
	LLI k = 2;

	for (LLI i = pow(k, 2) - 1; i < maxValue; i++)
	{
		for (LLI index = k; index < maxValue; index++)
		{
			if (vec[index] % k == 0)vec[index] = -1;
		}

		for (LLI j = k; j < maxValue; j++)
		{
			if (vec[j] != -1 && vec[j] > k)
			{
				k = vec[j];
				break;
			}
		}
	}

	seedSize = 0;
	for (LLI i = 0; i < seedRange; i++)
	{
		if (vec[i] != -1)seedSize++;
	}

	seeds = new  LLI[seedSize];
	seedSize = 0;

	for (LLI i = 0; i < seedRange; i++)
	{
		if (vec[i] != -1)
		{
			seeds[seedSize] = vec[i];
			seedSize++;
		}
	}
}
#pragma endregion

int main()
{
	LLI maxValue = 100;
	vector<LLI> vec;

	for (LLI i = 0; i < maxValue; i++)vec.push_back(i + 1);

	computeSeeds(vec, maxValue);

	//cout /*<< endl*/ << "Seeds: ";
	//for (LLI i = 0; i < seedSize; i++)cout << seeds[i] << " ";
	//cout << endl;

	double startTime = omp_get_wtime();
	#pragma omp parallel num_threads(NUM_THREADS)
	{
		vector<LLI> data;
		LLI seedCount = 0;
		LLI k = seeds[seedCount];
		LLI maxData;

		#pragma omp for
		for (LLI i = 0; i < maxValue; i++)
		{
			data.push_back(vec[i]);
		}

		//#pragma omp critical
		//{	
			//cout << "This is thread " << omp_get_thread_num() << " and these are the data I have: " << endl;

			//for (LLI i = 0; i < data.size(); i++)
			//{
			//	cout << data[i] << " ";
			//}
			//cout << endl;

		maxData = data.back();

		//cout << "This is the max value in my vector: " << maxData << endl << endl;

		for (LLI i = pow(k, 2); i < maxData;)
		{
			for (LLI j = 0; j < data.size(); j++)
			{
				if (data[j] % k == 0 && data[j] != k)vec[(data[j]) - 1] = -1;
			}

			k = seeds[++seedCount];
			i = k * k;
		}
		//}
	}

	vec[0] = -1;

	cout << "Prime numbers: " << endl;
	for (LLI i = 0; i < maxValue; i++)if (vec[i] != -1) cout << vec[i] << " ";
	cout << endl;

	cout << endl << "Program finished in: " << (omp_get_wtime() - startTime) << endl;
}