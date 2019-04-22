    
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <mpi.h>
#include <math.h>
#include <time.h>

const int population_size=pow(2,10);
const int problem_dimension=600;
const int number_of_chromosome_to_select=40;
const int iterations=1;
const int window_size=200;

/*This function creates a possible path*/
int* create_possible_path(int dimension){
	int* is_used = (int*)NULL;
	int* possible_path=(int*)NULL;
	int i,r;

	possible_path=(int*)malloc(sizeof(int)*dimension);
	is_used=(int*)malloc(sizeof(int)*dimension);
	for(i=0;i<dimension;i++){
		is_used[i]=0;
	}
	//printf("Numbers:");
	for (i = 0; i < dimension; ++i) {

		do{
			r = rand() % (dimension*1000)/1000;
		}while(is_used[r]);
		possible_path[i] = r; /* +1 since the range begins from 1 */
		is_used[r] = 1;
		//printf("%d ",possible_path[i]);
	}
	//printf("\n");
	//free(is_used);
	return possible_path;

}

/*This function evaluates a possible path fitness , thus the total distance*/
float Evaluate(int* path, float** distance,int problem_dimension){
	int index,i,j;
	float total_distance;

	for(index=0;index<problem_dimension-1;index++){
		i=path[index];
		j=path[index+1];
		total_distance+=distance[i][j];
	}
	//printf("Evaluate:%f\n",total_distance);
	return total_distance;

}

/*This function initializes the population*/
void Initialize_Pop(const int size_population,const int problem_dimension, int ***population,float **populationFitness,float **distance_matrix){

	int index,index2;
	int* create_possible_path(int dimension);
	float Evaluate(int* path, float** distance,int problem_dimension);
	/*Allocate population*/
	(*population)=(int**)malloc(sizeof(int*)*size_population);
	for(index=0;index<size_population;index++){
		(*population)[index]=(int*)malloc(sizeof(int)*problem_dimension);
	}

	/*Build the population*/
	for(index2=0;index2<size_population;index2++){
		(*population)[index2]=create_possible_path(problem_dimension);
		(*populationFitness)[index2]=Evaluate((*population)[index2],distance_matrix,problem_dimension);
	}
}

/*This function read the distance matrix from the binary file*/
float** Read_Matrix_Distance(int n, int p,float** matrix,int *sub_graph_cities,int sub_graph_size){
	int index,index2,real_index,real_index2,temp;
	float **input_matrix;
	int tmp;
	FILE *datafile=NULL;
	
	matrix=(float**)malloc(sizeof(float*)*n);
	input_matrix=(float**)malloc(sizeof(float*)*n);
	
	for(index=0;index<n;index++){
		matrix[index]=(float*)malloc(sizeof(float)*n);
	}
	
	for(index=0;index<n;index++){
                input_matrix[index]=(float*)malloc(sizeof(float)*p);
        }

	datafile=fopen("DistanceMatrix600_3.bin","rb");
	int distanceArrayBinHolder[n][p];
	/*Get the binary matrix*/
	for(index=0;index<n;index++){
		for(index2=0;index2<n;index2++){
			fread(&tmp,sizeof(int),1,datafile);
			distanceArrayBinHolder[index][index2]=tmp;
		}
	}

	srand(time(NULL));
	/*Fill the matix allocated*/
	for(index=0;index<n;index++){
		real_index=sub_graph_cities[index];
		for(index2=0;index2<n;index2++){
			if(index==index2){
				matrix[index][index2]=0;
			}
			else{
				temp=distanceArrayBinHolder[index][index2];

				matrix[index][index2] = temp;
			}
		}
	}

	return matrix;
}

/*This function was created to debug the program with a fake matrix*/
float** Generate_Matrix_Distance(int n, int p,float** matrix){
	int index,index2;
	matrix=(float**)malloc(sizeof(float*)*n);
	for(index=0;index<n;index++){
		matrix[index]=(float*)malloc(sizeof(float)*p);
	}
	srand(time(NULL));
	for(index=0;index<n;index++){
		for(index2=0;index2<p;index2++){
			if(index==index2){
				matrix[index][index2]=0;
			}
			else{
				matrix[index][index2]=rand()%1000;
			}
		}
	}
	return matrix;
}


/*This function perform the mutation on a chromosome*/
void Mutate(int** chromosome,int length_chromosome){
	int swap_id_1=0,swap_id_2=0;
	int buf;
	
	/*Make Sure that mutation ids are not the same*/
	while(swap_id_1==swap_id_2){
		swap_id_1=rand()%length_chromosome;
		swap_id_2=rand()%length_chromosome;
	}
	
	/*swap two values*/
	buf=(*chromosome)[swap_id_1];
	*((*chromosome)+swap_id_1)=*((*chromosome)+swap_id_2);
	*((*chromosome)+swap_id_2)=buf;
}

/*This function perform the Crossover between two chromosomes*/
void Cross_Over(int *chromosome_1,int *chromosome_2,int **offspring_1,int **offspring_2,int length_chromosome){
	
	int cut_point_1=0,cut_point_2=0,max,min;
	int index,index2,index3,index4,found;
	
	/* Find Splitting Points, we impose size for the spliting 3 */
	
	cut_point_1=rand()%(length_chromosome-length_chromosome/window_size)/1;
	cut_point_2=cut_point_1+length_chromosome/window_size;
	/*Picking upper and lower bound*/ 
	min=cut_point_1;
	max=cut_point_2;
	/*Creating Offspring*/
	
	/**Order Crossover**/
	/*Picking the non changing part*/
	for(index=min;index<=max;index++){
		*((*offspring_1)+index) = *(chromosome_1+index);
		*((*offspring_2)+index) = *(chromosome_2+index);	
	}
	index3=max+1;
	index4=max+1;
	/*Doing the order selection , ignoring already present cities*/
	for(index=max+1;index<max+length_chromosome+1;index++){
		found=0;
		index2=min;
		/*Checking whether the city is already in the list*/
		while(index2<=max && found==0){
			if(*(chromosome_2+index%length_chromosome) == *((*offspring_1)+index2)){
				found=1;
			}
			index2++;
		}
		
		/*If not present, then add it*/
		if (found==0){
			*((*offspring_1)+index3%length_chromosome) = *(chromosome_2+index%length_chromosome);
			index3++;
		}
		
		/*Same but for the other one*/
		found=0;
		index2=min;
		while(index2<=max && found==0){
			if(*((*offspring_2)+index2) == *(chromosome_1+index%length_chromosome)){
				found=1;
			}
			index2++;
		}
		if (found==0){
			*((*offspring_2)+index4%length_chromosome) = *(chromosome_1+index%length_chromosome);
			index4++;
		}
		
	}
	
}

/*This function performs the breeding (crossover + mutation)*/
void Breeding(int** offspring,int * mating_pool,float **offspring_fitness, int population_size,int problem_dimension,float** distance_matrix){
	
	int index, *child_1=NULL, *child_2=NULL, *offspring_1, *offspring_2=NULL;
	void Mutate(int** chromosome,int length_chromosome);
	void Cross_Over(int *chromosome_1,int *chromosome_2,int **offspring_1,int **offspring_2,int length_chromosome);
	float Evaluate(int* path, float** distance,int problem_dimension);
	void print_mem_addresses(int* ary,int size);
	(*offspring)=(int*)malloc(sizeof(int)*population_size*problem_dimension);
	(*offspring_fitness)=(float*)malloc(sizeof(float)*population_size);
	
	/*For each chromosome issued from pool mate ( The best ones)*/
	for(index=0;index<population_size-1;index=index+2){
		
		child_1=mating_pool+index*problem_dimension;
		child_2=mating_pool+(index+1)*problem_dimension;
		offspring_1=(*offspring)+index*problem_dimension;
		offspring_2=(*offspring)+(index+1)*problem_dimension;
		/*Cross Over + Mutations to create offspring from pool mate*/
		Cross_Over(child_1,child_2,&offspring_1,&offspring_2,problem_dimension);
		
		/*Mutate*/
		Mutate(&offspring_1,problem_dimension);
		Mutate(&offspring_2,problem_dimension);
		
		
		/*store new offsprings' fitness value*/
		(*offspring_fitness)[index]=Evaluate(offspring_1,distance_matrix,problem_dimension);
		(*offspring_fitness)[index+1]=Evaluate(offspring_2,distance_matrix,problem_dimension);
		
	}
	
}

/*Perform the tournament selection to feed the mating pool*/
void Tournament_Selection(int *population, float* populationFitness,int number_of_chromosome_to_select, int population_size,int** mating_pool){
	int length_mating_pool=0,index;
	int ids,min_id,popmember;
	float competing_individual_val,min=999999999999;

	(*mating_pool)=(int*)malloc(sizeof(int)*population_size*problem_dimension);

	/*filling mating_pool with tournament selection*/
	while(length_mating_pool<population_size){
		/*One tournament => Fill the mating pool with best chromosome of random subset of population  */
		for(index=0;index<number_of_chromosome_to_select;index++){
			/*Pick a random chromosome ID==population_id*/
			ids=rand()%population_size;
			/*Pick the associated fitness value*/
			competing_individual_val=populationFitness[ids];
			if(competing_individual_val<min){
				min_id=ids;
				min=competing_individual_val;
			}/*If it is improves the best value so far --> keep it in mind*/

		}
		for(int idx = 0; idx < problem_dimension; idx++){
			popmember =population[min_id*problem_dimension+idx];
			(*mating_pool)[length_mating_pool*problem_dimension+idx]=popmember;
        }
		length_mating_pool++;
	}
}

/*Replace the current pop with the new optmized pop*/
void Insert_Offspring_in_Pop(int **population,float **population_fitness,int *offspring,float *offspring_fitness){
	
	int index;
	int *temp=NULL;
	float *temp_fit=NULL;
	
	temp=(*population);
	temp_fit=(*population_fitness);
	(*population)=offspring;
	(*population_fitness)=offspring_fitness;
}

/*Translate 2D matrix to 1D row major order matrix*/
void translate_into_row_major(int **twod, int *oned, int problem_dimension, int pop_size){
	for(int row = 0; row < pop_size; row++){
		for(int idx = 0; idx < problem_dimension; idx++){
			oned[row*problem_dimension+idx] = twod[row][idx];
		}
	}
}

/* Print an array of int*/
void print_mem_addresses(int* ary,int size){
	for(int i = 0; i < size; i++){
		printf("Index: %d, Value: %d\n",i,ary[i]);
		 if((i+1)%4==0){
        	     printf("\n");
	         }

	}
}

/* Print an array of float*/
void print_mem_addresses_float(float* ary,int size){
        for(int i = 0; i < size; i++){
                printf("Index: %d, Value: %f\n",i,ary[i]);
        if((i+1)%4==0){
         printf("\n"); 
       } 
	}
}

/* Find the min of a float array*/
float find_min(float* ary,int size){
	float min = ary[1];
        for(int i = 0; i < size; i++){
		if(min > ary[i])
			min = ary[i];
	}
	return min;
}

/*Main Function*/
int main(){

	float **matrix_distance=NULL;
	int** population=NULL;
	float *population_fitness=NULL;
	int rank, num_proc;
	int *local_pop,*local_mating_pool,*local_offspring,*population1d;
	double time1, time2,duration,global;
	float *local_pop_fit,*local_offspring_fit;
	int index,local_pop_size;
	int local_pop_len;
	int sub_cities[10] = {100, 150, 250, 50, 75, 10, 9, 98, 8,1};
	MPI_Init(NULL,NULL);
	MPI_Comm_rank(MPI_COMM_WORLD,&rank);
	MPI_Comm_size(MPI_COMM_WORLD,&num_proc);
	
	/*Malloc section*/
	local_pop_size = population_size/num_proc;
	local_pop_len = local_pop_size*problem_dimension;
	local_pop = (int*)malloc(sizeof(int)*local_pop_len*problem_dimension);
	population1d = (int*)malloc(sizeof(int)*population_size*problem_dimension);
	local_pop_fit=(float*)malloc(sizeof(float)*local_pop_size);
	local_offspring=NULL;
	local_offspring_fit=NULL;
	
	/*Read matrix section*/
	matrix_distance=Read_Matrix_Distance(600,600,matrix_distance,sub_cities,problem_dimension);
	population_fitness = (float*)malloc(sizeof(float)*population_size);
	


	time1 = MPI_Wtime();
	if(rank==0){
		printf("Start\n");
		/*Initialize population on process 0*/
		Initialize_Pop(population_size,problem_dimension,&population, &population_fitness,matrix_distance);
		translate_into_row_major(population,population1d,problem_dimension,population_size);
	}
	/*Optionnal*/
	MPI_Barrier(MPI_COMM_WORLD);
	/*Start the optimisation*/
	for(index=0;index<iterations;index++){
		if(rank==0){
			printf("\n\nIteration:%d\n",index);
		}
		/*Scatter the data on each process*/
		MPI_Scatter(population1d,local_pop_size*problem_dimension,MPI_INT,local_pop,local_pop_size*problem_dimension,MPI_INT,0,MPI_COMM_WORLD);
		MPI_Scatter(population_fitness,local_pop_size,MPI_FLOAT,local_pop_fit,local_pop_size,MPI_FLOAT,0,MPI_COMM_WORLD);
        /*Perform the tournament selection on the local process*/       
        Tournament_Selection(local_pop,local_pop_fit,number_of_chromosome_to_select,local_pop_size, &local_mating_pool);
		/*WARNING : MATRIX_DISTANCE WITH SEVERAL PROCESSOR SHOULD BE BROADCASTED FIRST OR EACH PROCESSOR READ THE FILE!!!!!*/
        	
		/*Breeding: Crossover+Mutation*/
        	Breeding(&local_offspring,local_mating_pool,&local_offspring_fit, local_pop_size,problem_dimension, matrix_distance);
        	
	
        /*In term of memory managment, the two next line are awfull I know and the memory managment is not at stake here... Sorry for that*/
		local_pop=local_offspring;
		local_pop_fit=local_offspring_fit;
        /*Gather the local computations for the next iterration*/
		MPI_Gather(local_pop,local_pop_size*problem_dimension,MPI_INT,population1d,local_pop_size*problem_dimension,MPI_INT,0,MPI_COMM_WORLD);
		MPI_Gather(local_pop_fit,local_pop_size,MPI_FLOAT,population_fitness,local_pop_size,MPI_FLOAT,0,MPI_COMM_WORLD);

    }
	/*Optionnal*/
	MPI_Barrier(MPI_COMM_WORLD);
	/*Copute the optimal solution*/
	if(rank==0){
		time2 = MPI_Wtime();
		duration = time2 - time1;
		printf("Total_time=%f\n",duration);
		printf("Values:\n");
		printf("The min fitness value is: %f\n", find_min(population_fitness, population_size));
	}
	MPI_Finalize();

  return 0;
}
