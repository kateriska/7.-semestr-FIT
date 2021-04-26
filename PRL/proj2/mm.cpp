#include <mpi.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include <algorithm>

using namespace std;

// tags for marking channels communication between processors
#define TAG_0 0
#define TAG_1 1
#define TAG_2 2
#define TAG_3 3

// debug function for showing saved values into vector
void showVectorItems(vector<int> input_vector)
{
  for (auto i = input_vector.begin(); i != input_vector.end(); ++i)
  {
    cout << *i << ' ';
  }

  cout << endl;
}

// load matrix from file
vector<int> loadMatrix(string file_name)
{
  vector<int> loaded_matrix;
  int line_id = 0;
  string line;
  string word;

  fstream file;
  file.open(file_name);

  while (getline(file, line))
  {
    if (line_id == 0) // skip header with count of rows / cols
    {
      line_id = line_id + 1;
      continue;
    }
    line_id = line_id + 1;

    stringstream stream(line);

    while (stream >> word)
    {
      int loaded_number = stoi(word);
      loaded_matrix.push_back(loaded_number);
    }

  }

  //showVectorItems(loaded_matrix);

  file.close();
  return loaded_matrix;
}

// get dimension of matrix which is on the first line of file - first matrix - count of rows, second matrix - count of cols
int getInputMatrixFileDimension(string file_name)
{
  vector<int> loaded_matrix;
  string line;
  string word;

  fstream file;
  file.open(file_name);

  getline(file, line);

  int matrix_rows_count = stoi(line);
  file.close();

  return matrix_rows_count;
}

// get 2D position of processor in mesh
vector<int> getProcessor2Dposition(int my_id, int matrix2_cols_count)
{
  vector<int> position;

  int coordinate1 = my_id / matrix2_cols_count;
  position.push_back(coordinate1);

  int coordinate2 = my_id % matrix2_cols_count;
  position.push_back(coordinate2);

  return position;
}

// get 1D position of processor in mesh or number in matrix
int get1Dposition(int matrix_cols_count, int i, int j)
{
  int position;

  position = (i * matrix_cols_count) + j;
  return position;
}

// print final output of calculated matrix to stdout
void printFinalOutput(int matrix1_rows_count, int matrix2_cols_count, vector<int> computed_matrix)
{
  cout << matrix1_rows_count << ":" << matrix2_cols_count << endl;

  int computed_matrix_cols_count = 0;
  string printed_line = "";
  int loaded_number;

  for (vector<int>::size_type i = 0; i < computed_matrix.size(); i++)
  {
    int & loaded_number = computed_matrix[i];
    string loaded_number_str = to_string(loaded_number);

    computed_matrix_cols_count = computed_matrix_cols_count + 1;

    if (computed_matrix_cols_count == matrix2_cols_count )
    {
      printed_line = printed_line + " " + loaded_number_str;
      printed_line = printed_line.substr(1, printed_line.length());
      cout << printed_line << endl;
      computed_matrix_cols_count = 0;
      printed_line = "";
      continue;
    }
    printed_line = printed_line + " " + loaded_number_str;
  }
  return;
}

int main(int argc, char *argv[])
{
  int processor_count; // count of processors
  int my_id;  // rank of my processor
  int neighbour_num; // number which carry neighbour process
  int my_num; // my number
  int neighbour_id; // id of neighbour processsor
  MPI_Status stat;

  vector<int> loaded_matrix1;
  int matrix1_rows_count = 0;
  int matrix1_cols_count = 0;

  vector<int> loaded_matrix2;
  int matrix2_rows_count = 0;
  int matrix2_cols_count = 0;

  vector<int> matrix1_first_processor;
  vector<int> matrix2_first_processor;

  vector<int> computed_matrix;

  int c = 0;
  int a = 0;
  int b = 0;

  //MPI INIT
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD, &processor_count);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_id);

  if (my_id == 0)
  {
    // load two matrix from file and get their dimensions
    loaded_matrix1 = loadMatrix("mat1");
    matrix1_rows_count = getInputMatrixFileDimension("mat1");
    matrix1_cols_count = loaded_matrix1.size() / matrix1_rows_count;

    loaded_matrix2 = loadMatrix("mat2");
    matrix2_cols_count = getInputMatrixFileDimension("mat2");
    matrix2_rows_count = loaded_matrix2.size() / matrix2_cols_count;

    // the requirement of matrix multiplication is that count of cols of first matrix has to be same as count of rows of second matrix
    if (matrix1_cols_count != matrix2_rows_count)
    {
      cerr << "Error - Count of cols of first matrix has to be equal to count of rows of second matrix!\n";
      exit(EXIT_FAILURE);
    }

    // send dimensions of each matrix to next processors in mesh
    for (int processor_id = 1; processor_id < processor_count; processor_id++)
    {
      MPI_Send(&matrix1_rows_count, 1, MPI_INT, processor_id, TAG_0, MPI_COMM_WORLD);
      MPI_Send(&matrix2_rows_count, 1, MPI_INT, processor_id, TAG_1, MPI_COMM_WORLD);
      MPI_Send(&matrix1_cols_count, 1, MPI_INT, processor_id, TAG_2, MPI_COMM_WORLD);
      MPI_Send(&matrix2_cols_count, 1, MPI_INT, processor_id, TAG_3, MPI_COMM_WORLD);
    }

  }
  else
  {
    // other processors in mesh receive dimensions of each matrix
    MPI_Recv(&neighbour_num, 1, MPI_INT, 0, TAG_0, MPI_COMM_WORLD, &stat);
    matrix1_rows_count = neighbour_num;
    MPI_Recv(&neighbour_num, 1, MPI_INT, 0, TAG_1, MPI_COMM_WORLD, &stat);
    matrix2_rows_count = neighbour_num;
    MPI_Recv(&neighbour_num, 1, MPI_INT, 0, TAG_2, MPI_COMM_WORLD, &stat);
    matrix1_cols_count = neighbour_num;
    MPI_Recv(&neighbour_num, 1, MPI_INT, 0, TAG_3, MPI_COMM_WORLD, &stat);
    matrix2_cols_count = neighbour_num;
  }

  // each processor calculates their own 2D position in mesh
  vector<int> position = getProcessor2Dposition(my_id, matrix2_cols_count);
  int & processor_coordinate1 = position[0];
  int & processor_coordinate2 = position[1];

  //showVectorItems(position);

  // send numbers of each matrix to first row / col processors
  if (my_id == 0)
  {
    for (int i = 0; i < matrix1_rows_count; i++)
    {
      for (int j = 0; j < matrix1_cols_count; j++)
      {
        int matrix_value_index = get1Dposition(matrix1_cols_count, i, j);
        my_num = loaded_matrix1[matrix_value_index];
        neighbour_id = get1Dposition(matrix2_cols_count, i, 0);
        MPI_Send(&my_num, 1, MPI_INT, neighbour_id, TAG_0, MPI_COMM_WORLD);
      }
    }

    for (int j = 0; j < matrix2_cols_count; j++)
    {
      for (int i = 0; i < matrix2_rows_count; i++)
      {
        int matrix_value_index = get1Dposition(matrix2_cols_count, i, j);
        my_num = loaded_matrix2[matrix_value_index];
        neighbour_id = get1Dposition(matrix2_cols_count, 0, j);
        MPI_Send(&my_num, 1, MPI_INT, neighbour_id, TAG_1, MPI_COMM_WORLD);
      }
    }
  }

  // first row / cols processors inserted received values into vectors
  if (processor_coordinate2 == 0)
  {
    for (int i = 0; i < matrix1_cols_count; i++)
    {
      MPI_Recv(&neighbour_num, 1, MPI_INT, 0, TAG_0, MPI_COMM_WORLD, &stat);
      matrix1_first_processor.push_back(neighbour_num);
    }

    reverse(matrix1_first_processor.begin(), matrix1_first_processor.end());
    //showVectorItems(matrix1_first_processor);
  }

  if (processor_coordinate1 == 0)
  {
    for (int i = 0; i < matrix2_rows_count; i++)
    {
      MPI_Recv(&neighbour_num, 1, MPI_INT, 0, TAG_1, MPI_COMM_WORLD, &stat);
      matrix2_first_processor.push_back(neighbour_num);
    }

    reverse(matrix2_first_processor.begin(), matrix2_first_processor.end());
    //showVectorItems(matrix2_first_processor);
  }

  // computation of own mesh multiplication algorithm
  MPI_Barrier(MPI_COMM_WORLD); /* IMPORTANT */
  double start_time = MPI_Wtime(); // start of measuring time for experiments of own algorithm and not preparation process
  for (int i = 0; i < matrix2_rows_count; i++)
  {
    if (processor_coordinate2 == 0)
    {
      b = matrix1_first_processor.back();
      matrix1_first_processor.pop_back();
    }
    else
    {
      neighbour_id = get1Dposition(matrix2_cols_count, processor_coordinate1, processor_coordinate2 - 1);
      MPI_Recv(&neighbour_num, 1, MPI_INT, neighbour_id, TAG_1, MPI_COMM_WORLD, &stat);
      b = neighbour_num;
    }

    if (processor_coordinate1 == 0)
    {
      a = matrix2_first_processor.back();
      matrix2_first_processor.pop_back();
    }
    else
    {
      neighbour_id = get1Dposition(matrix2_cols_count, processor_coordinate1 - 1, processor_coordinate2);
      MPI_Recv(&neighbour_num, 1, MPI_INT, neighbour_id, TAG_0, MPI_COMM_WORLD, &stat);
      a = neighbour_num;
    }

    c = c + (a * b);

    //cout << c << endl;

    if (processor_coordinate2 + 1 < matrix2_cols_count)
    {
      neighbour_id = get1Dposition(matrix2_cols_count, processor_coordinate1, processor_coordinate2 + 1);
      MPI_Send(&b, 1, MPI_INT, neighbour_id, TAG_1, MPI_COMM_WORLD);
    }

    if (processor_coordinate1 + 1 < matrix1_rows_count)
    {
      neighbour_id = get1Dposition(matrix2_cols_count, processor_coordinate1 + 1, processor_coordinate2);
      MPI_Send(&a, 1, MPI_INT, neighbour_id, TAG_0, MPI_COMM_WORLD);
    }
  }

  MPI_Barrier(MPI_COMM_WORLD); /* IMPORTANT */
  double end_time = MPI_Wtime(); // end of measuring time for experiments

  // processor except first processor send their computed c values to first processor
  if (my_id != 0)
  {
    MPI_Send(&c, 1, MPI_INT, 0, TAG_0, MPI_COMM_WORLD);
  }
  else
  {
    computed_matrix.push_back(c); // saving c value which computed first processor
    // receiving c values from other processors
    for (int processor_id = 1; processor_id < processor_count; processor_id++)
    {
      MPI_Recv(&neighbour_num, 1, MPI_INT, processor_id, TAG_0, MPI_COMM_WORLD, &stat);
      computed_matrix.push_back(neighbour_num);
    }

    // final print of calculated result to stdout
    printFinalOutput(matrix1_rows_count, matrix2_cols_count, computed_matrix);
  }


  MPI_Finalize();

  /*
  if(my_id == 0)
  {
        printf("Runtime=%f\n", end_time-start_time); // printing of total running time of mesh multiplication algorithm
  }
  */


  return 0;
}
