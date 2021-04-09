/*
Author - Katerina Fortova (xforto00)
Course - Parallel and Distributed Algorithms
Project - Pipeline Merge Sort
Academic Year - 2020 / 21
*/

#include <mpi.h>
#include <iostream>
#include <fstream>
#include <queue>
#include <cmath>

using namespace std;

#define TAG 0

// function for showing input queue to user
void showQueue(queue<int16_t> input_queue)
{
    int i = 0;

    while (input_queue.empty() == false)
    {
        if (i == 0)
        {
          cout << input_queue.front();
          input_queue.pop();
        }
        else
        {
          cout << ' ' << input_queue.front();
          input_queue.pop();
        }
        i = i + 1;
    }
    cout << '\n';
}

int main(int argc, char *argv[])
{
   int processor_count; // count of processors
   int my_id;  // rank of my processor
   int neighbour_num; // number which carry neighbour process
   int my_num; // my number
   MPI_Status stat;

   queue<int16_t> input_queue;

   //MPI INIT
   MPI_Init(&argc,&argv);
   MPI_Comm_size(MPI_COMM_WORLD, &processor_count);
   MPI_Comm_rank(MPI_COMM_WORLD, &my_id);

   // processor with id 0 read input numbers and show them on one line to stdout
   if (my_id == 0)
   {
       char input[] = "numbers";
       int16_t number;
       int invar = 0;
       fstream fin;
       fin.open(input, ios::in);

       while (fin.good())
       {
           number = fin.get();
           if (!fin.good()) break;
           input_queue.push(number);
           invar++;
       }

       fin.close();

       showQueue(input_queue); // show input number sequence on one line to user

       // pushing numbers to initial queue for sorting algorithm
       queue<int16_t> input_queue;
       while (fin.good())
       {
           number= fin.get();
           if (!fin.good()) break;
           input_queue.push(number);
           invar++;
       }

       fin.close();

       /*
       3 queues for experiment: sorted ascending/descending sequence + unsorted sequence
       input_queue.push(1);
       input_queue.push(2);
       input_queue.push(3);
       input_queue.push(4);
       input_queue.push(5);
       input_queue.push(6);
       input_queue.push(7);
       input_queue.push(8);
       input_queue.push(9);
       input_queue.push(10);
       input_queue.push(11);
       input_queue.push(12);
       input_queue.push(13);
       input_queue.push(14);
       input_queue.push(15);
       input_queue.push(16);

       input_queue.push(16);
       input_queue.push(15);
       input_queue.push(14);
       input_queue.push(13);
       input_queue.push(12);
       input_queue.push(11);
       input_queue.push(10);
       input_queue.push(9);
       input_queue.push(8);
       input_queue.push(7);
       input_queue.push(6);
       input_queue.push(5);
       input_queue.push(4);
       input_queue.push(3);
       input_queue.push(2);
       input_queue.push(1);

       input_queue.push(5);
       input_queue.push(12);
       input_queue.push(15);
       input_queue.push(1);
       input_queue.push(14);
       input_queue.push(13);
       input_queue.push(9);
       input_queue.push(16);
       input_queue.push(6);
       input_queue.push(8);
       input_queue.push(10);
       input_queue.push(11);
       input_queue.push(4);
       input_queue.push(3);
       input_queue.push(2);
       input_queue.push(7);

       showQueue(input_queue);
       */
   }

  int index = 0; // index which shows current iteration of sorting algorithm
  int used_queue = 1; // will be used first queue (1) or second queue (2)
  int pushed_queue_elements = 0; // counter for count of received number for some processor
  int compared_elements_count = 0; // counter for count of compared numbers
  int numbers_to_sort_count_first_queue = pow(2, my_id - 1);
  int numbers_to_sort_count_second_queue = pow(2, my_id - 1);
  int remove_other_element_queue;

  // two queues for processors based on schema of algorithm
  queue<int16_t> first_queue;
  queue<int16_t> second_queue;

  int sorted_numbers_order = 0; // counting index of each sorted number

  //double start_time = MPI_Wtime(); // start of measuring time for experiments

  // iterate until last processor completely ends - 16 is a fixed count of input numbers
  while (index < (16 - 1) + pow(2, processor_count - 1) + processor_count - 1)
  {
    if (my_id == 0) // first processor only load input numbers and send them to next processor
    {
      if (input_queue.empty() == false)
      {
        my_num = input_queue.front();
        input_queue.pop();
        MPI_Send(&my_num, 1, MPI_INT, my_id + 1, TAG, MPI_COMM_WORLD);
      }
    }
    else
    {
      int previous_processor_end;
      // set index when previous processor ends
      if (my_id == 1)
      {
        previous_processor_end = pow(2, processor_count - 1);
      }
      else
      {
        previous_processor_end = pow(2, processor_count - 1) + pow(2, my_id - 1) + my_id - 2;
      }

      // set index when my processor starts receiving data
      int my_processor_receive_start = pow(2, my_id - 1) + my_id - 2;

      if (index >= my_processor_receive_start && index < previous_processor_end)
      {
        if (pushed_queue_elements == pow(2, my_id - 1))
        {
          pushed_queue_elements = 0; // clear received numbers counter, because now we will work with the other queue
          // time to switch queues
          if (used_queue == 1)
          {
            used_queue = 2;
          }
          else if (used_queue == 2)
          {
            used_queue = 1;
          }
        }

        MPI_Recv(&neighbour_num, 1, MPI_INT, my_id - 1, TAG, MPI_COMM_WORLD, &stat); // receive number from previous processor

        pushed_queue_elements = pushed_queue_elements + 1; // counting of received numbers from previous processor

        // pushing numbers to correct queue
        if (used_queue == 1)
        {
          first_queue.push(neighbour_num);
        }
        else if (used_queue == 2)
        {
          second_queue.push(neighbour_num);
        }
      }

      // between these indexes my processor could compare received numbers
      int my_processor_compare_start = pow(2, my_id) + my_id - 1;
      int my_processor_compare_end = pow(2, my_id) + my_id - 1 + pow(2, processor_count - 1);

      // when it is time to compare numbers with my current processor based on indexes
      if (index >= my_processor_compare_start && index < my_processor_compare_end)
      {
        if (compared_elements_count <= pow(2, my_id) - 2)
        {
          compared_elements_count = compared_elements_count + 1; // counting number of compared numbers
          // send right compared number to next processor
          // if some queue is empty, send number of second queue right away
          if (numbers_to_sort_count_first_queue == 0)
          {
            my_num = second_queue.front();
            second_queue.pop();
          }
          else if (numbers_to_sort_count_second_queue == 0)
          {
            my_num = first_queue.front();
            first_queue.pop();
          }
          // if on there are numbers on both positions in first and second queue - compare them and send smaller number to next processor
          else if (first_queue.front() < second_queue.front())
          {
            numbers_to_sort_count_first_queue = numbers_to_sort_count_first_queue - 1; // removed number from queue, in queue is now less numbers of one
            remove_other_element_queue = 2;

            my_num = first_queue.front();
            first_queue.pop();
          }
          else
          {
            numbers_to_sort_count_second_queue = numbers_to_sort_count_second_queue - 1;
            remove_other_element_queue = 1;

            my_num = second_queue.front();
            second_queue.pop();
          }
        }

        else if (compared_elements_count > pow(2, my_id) - 2 && remove_other_element_queue == 1)
        {
          my_num = first_queue.front();
          first_queue.pop();

          numbers_to_sort_count_first_queue = pow(2, my_id - 1);
          numbers_to_sort_count_second_queue = pow(2, my_id - 1);
          compared_elements_count = 0;
        }
        else if (compared_elements_count > pow(2, my_id) - 2 && remove_other_element_queue == 2)
        {
          my_num = second_queue.front();
          second_queue.pop();

          numbers_to_sort_count_first_queue = pow(2, my_id - 1);
          numbers_to_sort_count_second_queue = pow(2, my_id - 1);
          compared_elements_count = 0;
        }

        if (my_id == processor_count - 1) // last processor only prints sorted sequence to stdout
        {
          //cout << sorted_numbers_order << ":" << my_num << endl;
          cout << my_num << endl;
          //sorted_numbers_order = sorted_numbers_order + 1;
        }
        else
        {
          MPI_Send(&my_num, 1, MPI_INT, my_id + 1, TAG, MPI_COMM_WORLD); // other processors can send their number to next processor
        }
      }
    }
    index = index + 1; // increment index of loop
  }

  //double end_time = MPI_Wtime(); // end of measuring time for experiments
  MPI_Finalize();
  /*
  if(my_id == 0)
  {
        printf("Runtime=%f\n", end_time-start_time); // printing of total running time of sorted algorithm
  }
  */
   return 0;
 }
