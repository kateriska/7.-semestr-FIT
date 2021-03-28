#include <mpi.h>
#include <iostream>
#include <fstream>
#include <queue>
#include <cmath>

using namespace std;

#define TAG 0

void showQueue(queue<int16_t> input_queue)
{
    //queue<int16_t> g = gq;
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
   int processor_count;               //pocet procesoru
   int my_id;                   //muj rank
   int neighbour_num;            //hodnota souseda
   int my_num;               //moje hodnota
   MPI_Status stat;            //struct- obsahuje kod- source, tag, error

   queue<int16_t> input_queue;
   int numbers_count = 0;

   //MPI INIT
   MPI_Init(&argc,&argv);                          // inicializace MPI
   MPI_Comm_size(MPI_COMM_WORLD, &processor_count);       // zjistĂ­me, kolik procesĹŻ bÄ›ĹľĂ­
   MPI_Comm_rank(MPI_COMM_WORLD, &my_id);           // zjistĂ­me id svĂ©ho procesu

   //NACTENI SOUBORU
   /* -proc s rankem 0 nacita vsechny hodnoty
    * -postupne rozesle jednotlive hodnoty vsem i sobe
   */
   if(my_id == 0){
       char input[]= "numbers";                          //jmeno souboru
       int16_t number;                                     //hodnota pri nacitani souboru
       int invar= 0;                                   //invariant- urcuje cislo proc, kteremu se bude posilat
       fstream fin;                                    //cteni ze souboru
       fin.open(input, ios::in);

       while(fin.good()){
           number= fin.get();
           if(!fin.good()) break;                      //nacte i eof, takze vyskocim
           //cout<<invar<<":"<<number<<endl;             //kdo dostane kere cislo
           numbers_count++;
           input_queue.push(number);
           //cout << "The input queue is : ";
           //showq(input_queue);
           //MPI_Send(&number, 1, MPI_INT, myid + 1, TAG, MPI_COMM_WORLD); //buffer,velikost,typ,rank prijemce,tag,komunikacni skupina
           invar++;
       }//while
       //showq(input_queue);
       //cout << numbers_count;
       fin.close();

       showQueue(input_queue);
       numbers_count = input_queue.size();
       queue<int16_t> input_queue;
       while(fin.good()){
           number= fin.get();
           if(!fin.good()) break;                      //nacte i eof, takze vyskocim
           //cout<<invar<<":"<<number<<endl;             //kdo dostane kere cislo
           numbers_count++;
           input_queue.push(number);
           //cout << "The input queue is : ";
           //showq(input_queue);
           //MPI_Send(&number, 1, MPI_INT, myid + 1, TAG, MPI_COMM_WORLD); //buffer,velikost,typ,rank prijemce,tag,komunikacni skupina
           invar++;
       }//while
       //showq(input_queue);
       //cout << numbers_count;
       fin.close();

       //showq(input_queue);
   }//nacteni souboru

  int index = 0;
  int used_queue = 1; // will be used first queue (1) or second queue (2)
  int pushed_queue_elements = 0;
  int compared_elements_count = 0;
  int numbers_to_sort_count_first_queue = pow(2, my_id-1);
  int numbers_to_sort_count_second_queue = pow(2, my_id - 1);
  int used_queue_pop = 1;
  queue<int16_t> first_queue;
  queue<int16_t> second_queue;
  int sorted_numbers_order = 0;

  while (index < (16 - 1) + pow(2,processor_count-1) + processor_count - 1)
  {
    if (my_id == 0)
    {
      if (input_queue.empty() == false)
      {
        my_num = input_queue.front();
        //cout << my_num << endl;
        input_queue.pop();
        MPI_Send(&my_num, 1, MPI_INT, my_id + 1, TAG, MPI_COMM_WORLD);
      }
    }
    else
    {
      int previous_processor_end;
      if (my_id == 1)
      {
        previous_processor_end = pow(2, processor_count - 1);
        //cout << previous_processor_end << endl;
      }
      else
      {
        previous_processor_end = pow(2, processor_count - 1) - 1 + pow(2, my_id - 1) + my_id - 1;
        //cout << previous_processor_end << endl;
      }

      int my_processor_receive_start = pow(2, my_id - 1) + my_id - 1 -1;

      if (index >= my_processor_receive_start && index < previous_processor_end)
      {
        if (pushed_queue_elements == pow(2, my_id - 1))
        {
          // time to switch queues
          if (used_queue == 1)
          {
            used_queue = 2;
          }
          else if (used_queue == 2)
          {
            used_queue = 1;
          }

          pushed_queue_elements = 0;
        }

        MPI_Recv(&neighbour_num, 1, MPI_INT, my_id - 1, TAG, MPI_COMM_WORLD, &stat);

        pushed_queue_elements = pushed_queue_elements + 1;

        if (used_queue == 1)
        {
          first_queue.push(neighbour_num);
        }
        else if (used_queue == 2)
        {
          second_queue.push(neighbour_num);
        }

      }

      int my_processor_compare_start = pow(2, my_id) + my_id -1;
      int my_processor_compare_end = pow(2, processor_count-1) -1 + (pow(2, my_id)) + my_id;

      if (index >= my_processor_compare_start && index < my_processor_compare_end)
      {
        if (my_id == processor_count - 1)
        {
          if (second_queue.empty() == true)
          {
            while (first_queue.empty() == false)
            {
              cout << sorted_numbers_order << ":" << first_queue.front() << endl;
              sorted_numbers_order = sorted_numbers_order + 1;
              //cout << "Last iter count second empty: " << index << endl;
              first_queue.pop();
            }
            break;
          }
          if (first_queue.empty() == true)
          {
            while (second_queue.empty() == false)
            {
              cout << sorted_numbers_order << ":" << second_queue.front() << endl;
              sorted_numbers_order = sorted_numbers_order + 1;
              //cout << "Last iter count first empty: " << index << endl;
              second_queue.pop();
            }
            break;
          }
        }

        if (compared_elements_count < pow(2, my_id) - 1)
        {
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
          else if (first_queue.front() < second_queue.front())
          {
            my_num = first_queue.front();
            first_queue.pop();
            numbers_to_sort_count_first_queue = numbers_to_sort_count_first_queue - 1;
            used_queue_pop = 2;
          }
          else
          {
            my_num = second_queue.front();
            second_queue.pop();
            numbers_to_sort_count_second_queue = numbers_to_sort_count_second_queue - 1;
            used_queue_pop = 1;
          }

          compared_elements_count = compared_elements_count + 1;

        }

        else if (compared_elements_count >= pow(2, my_id) - 1 && used_queue_pop == 1)
        {
          my_num = first_queue.front();
          first_queue.pop();

          compared_elements_count = 0;
          numbers_to_sort_count_first_queue = pow(2, my_id - 1);
          numbers_to_sort_count_second_queue = pow(2, my_id - 1);
        }
        else if (compared_elements_count >= pow(2, my_id) - 1 && used_queue_pop == 2)
        {
          my_num = second_queue.front();
          second_queue.pop();

          compared_elements_count = 0;
          numbers_to_sort_count_first_queue = pow(2, my_id - 1);
          numbers_to_sort_count_second_queue = pow(2, my_id - 1);
        }

        if (my_id == processor_count - 1)
        {
          cout << sorted_numbers_order << ":" << my_num << endl;
          sorted_numbers_order = sorted_numbers_order + 1;
        }
        else
        {
          MPI_Send(&my_num, 1, MPI_INT, my_id + 1, TAG, MPI_COMM_WORLD);
        }
      }
    }
    index = index + 1;
  }


   MPI_Finalize();
   return 0;
 }
