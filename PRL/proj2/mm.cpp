#include <mpi.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

using namespace std;

#define TAG_ROWS_COUNT_1 0
#define TAG_ROWS_COUNT_2 1
#define TAG_COLS_COUNT_1 2
#define TAG_COLS_COUNT_2 3

void showVectorItems(vector<int> input_vector)
{
  for (auto i = input_vector.begin(); i != input_vector.end(); ++i)
  {
    std::cout << *i << ' ';
  }

  cout << endl;
}

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

  showVectorItems(loaded_matrix);

  file.close();
  return loaded_matrix;


}

// first matrix - count of rows, second matrix - count of cols
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

vector<int> getProcessor2Dposition(int my_id, int matrix2_cols_count)
{
  vector<int> position;

  int coordinate1 = my_id / matrix2_cols_count;
  position.push_back(coordinate1);

  int coordinate2 = my_id % matrix2_cols_count;
  position.push_back(coordinate2);

  return position;
}


int main(int argc, char *argv[])
{
  int processor_count; // count of processors
  int my_id;  // rank of my processor
  int neighbour_num; // number which carry neighbour process
  int my_num; // my number
  MPI_Status stat;

  vector<int> loaded_matrix1;
  int matrix1_rows_count = 0;
  int matrix1_cols_count = 0;

  vector<int> loaded_matrix2;
  int matrix2_rows_count = 0;
  int matrix2_cols_count = 0;

  //MPI INIT
  MPI_Init(&argc,&argv);
  MPI_Comm_size(MPI_COMM_WORLD, &processor_count);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_id);

  if (my_id == 0)
  {
    loaded_matrix1 = loadMatrix("mat1");
    matrix1_rows_count = getInputMatrixFileDimension("mat1");
    cout << matrix1_rows_count << endl;
    matrix1_cols_count = loaded_matrix1.size() / matrix1_rows_count;
    cout << matrix1_cols_count << endl;

    loaded_matrix2 = loadMatrix("mat2");
    matrix2_cols_count = getInputMatrixFileDimension("mat2");
    matrix2_rows_count = loaded_matrix2.size() / matrix2_cols_count;
    cout << matrix2_rows_count << endl;
    cout << matrix2_cols_count << endl;

    for (int processor_id = 1; processor_id < processor_count; processor_id++)
    {
      MPI_Send(&matrix1_rows_count, 1, MPI_INT, processor_id, TAG_ROWS_COUNT_1, MPI_COMM_WORLD);
      MPI_Send(&matrix2_rows_count, 1, MPI_INT, processor_id, TAG_ROWS_COUNT_2, MPI_COMM_WORLD);
      MPI_Send(&matrix1_cols_count, 1, MPI_INT, processor_id, TAG_COLS_COUNT_1, MPI_COMM_WORLD);
      MPI_Send(&matrix2_cols_count, 1, MPI_INT, processor_id, TAG_COLS_COUNT_2, MPI_COMM_WORLD);
    }

  }
  else
  {
    MPI_Recv(&neighbour_num, 1, MPI_INT, 0, TAG_ROWS_COUNT_1, MPI_COMM_WORLD, &stat);
    matrix1_rows_count = neighbour_num;
    cout << "Matrix1RowsCount : " << matrix1_rows_count << endl;
    MPI_Recv(&neighbour_num, 1, MPI_INT, 0, TAG_ROWS_COUNT_2, MPI_COMM_WORLD, &stat);
    matrix2_rows_count = neighbour_num;
    cout << "Matrix2RowsCount : " << matrix2_rows_count << endl;
    MPI_Recv(&neighbour_num, 1, MPI_INT, 0, TAG_COLS_COUNT_1, MPI_COMM_WORLD, &stat);
    matrix1_cols_count = neighbour_num;
    cout << "Matrix1ColsCount : " << matrix1_cols_count << endl;
    MPI_Recv(&neighbour_num, 1, MPI_INT, 0, TAG_COLS_COUNT_2, MPI_COMM_WORLD, &stat);
    matrix2_cols_count = neighbour_num;
    cout << "Matrix2ColsCount : " << matrix2_cols_count << endl;
  }

  vector<int> position = getProcessor2Dposition(my_id, matrix2_cols_count);
  int & processor_coordinate1 = position[0];
  int & processor_coordinate2 = position[1];

  showVectorItems(position);





  MPI_Finalize();
  return 0;
}
