#include<iostream>
#include<pthread.h>
#include<math.h>

using namespace std;

pthread_mutex_t mutex;

long long int *seed;
long long int seedSize;

struct struct_of_chunks
{
    long long int *arr;
    long long int arraySize;
};

void* computePrimes(void *arg)
{
    pthread_mutex_lock(&mutex);
    struct_of_chunks* arg_struct = (struct_of_chunks*) arg;
    long long int max_value_index = arg_struct->arraySize - 1;
    long long int seedRange = arg_struct->arr[max_value_index];
    long long int j = 1;
    long long int k = 2;

     while(j < seedSize){
      k = seed[j+1];
      if(pow(k,2) <= seedRange){
	for(long long int i=0; i < arg_struct->arraySize; i++)
	{	  
	    if(arg_struct->arr[i]%k==0){
            
	      arg_struct->arr[i] =-1;
	    
	    }	    	  
	}
      }
      j++;
    }

    for(long long int i=0;i<arg_struct->arraySize;i++)
    {
        if(arg_struct->arr[i]!=-1)
        {
	  cout << arg_struct->arr[i] << " " << endl;
        }
    }

    pthread_mutex_unlock(&mutex);
    pthread_exit(0);
}

void computeSeeds(long long int userArray[], long long int maxValue)
{
    long long int seedRange = sqrt(maxValue);
    long long int k=2;

    while(pow(k,2)<=seedRange){
      for(long long int i=pow(k,2); i <= seedRange; i++)
	{
	  if(userArray[i]%k==0){
            
	    userArray[i]=-1;
	    
	  }
	}
      for(long long int index=k+1; index<maxValue; index++)
        {
	  if(userArray[index]!= -1){
	    k = index;
	    break;
	  }
        }
    }

    seedSize=0;
    for(long long int i=1;i<=seedRange;i++)
    {
        if(userArray[i]!=-1)seedSize++;
    }
    //cout << "Number of seeds: " << seedSize << endl;

    seed = new long long int[seedSize];
    seedSize=0;
    seed[0]=0;

    for(long long int i=1;i<=seedRange;i++)
    {
        if(userArray[i]!=-1)
        {
	  seedSize++;
          seed[seedSize]=userArray[i];
	  cout << userArray[i] << " " << endl;
        }
    }
}

int main()
{
    if (pthread_mutex_init(&mutex, NULL) != 0)
    {
        printf("\n mutex init failed\n");
        return 1;
    }
  
    long long int maxValue=100000;
    long long int cores=4;
  
    long long int userArray[maxValue];
    userArray[0]=0; 
    for(long long int i=1;i<=maxValue;i++)userArray[i]=i;

    computeSeeds(userArray, maxValue);

    long long int numOfChunks;
    long long int seedRange = sqrt(maxValue);
    if(cores >= maxValue)numOfChunks=1;
    else numOfChunks = (maxValue-seedRange)/cores;

    struct_of_chunks chunks[numOfChunks];

    long long int avgChunkArraySize = (maxValue-seedRange)/numOfChunks;

////    cout << "Avg chunk array size: " << avgChunkArraySize << endl;

    long long int count = sqrt(maxValue);
    for(long long int i=0;i<numOfChunks;i++)
    {
        if(i<numOfChunks-1)
        {
            chunks[i].arraySize = avgChunkArraySize;
            chunks[i].arr=new long long int[chunks[i].arraySize];
            count+=avgChunkArraySize;
        }
        else
        {
            chunks[i].arraySize = maxValue - count;
            chunks[i].arr = new long long int[chunks[i].arraySize];
        }
    }

    pthread_t tids[numOfChunks];
    count = sqrt(maxValue);

    for(long long int i=0;i<numOfChunks;i++)
    {
        for(long long int j=0;j<chunks[i].arraySize;j++)
        {
            chunks[i].arr[j] = ++count;
        }

        pthread_attr_t attr;
        pthread_attr_init(&attr);
        pthread_create(&tids[i], &attr, computePrimes, &chunks[i]);
	pthread_join(tids[i], NULL);
    }

    pthread_mutex_destroy(&mutex);
    return 0;
}
