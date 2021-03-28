#include <mpi.h>
#include <iostream>
#include <fstream>
#include <queue>
#include <cmath>

using namespace std;

#define TAG 0

void showq(queue<int16_t> gq)
{
    queue<int16_t> g = gq;
    while (!g.empty()) {
        cout << ' ' << g.front();
        g.pop();
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

       showq(input_queue);
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
  //while (index < 2*pow(2, processor_count-1) + (processor_count -1) -1)
  {
    if (my_id == 0)
    {
      if (input_queue.empty() == false)
      {
        my_num = input_queue.front();
        cout << my_num << endl;
        input_queue.pop();
        MPI_Send(&my_num, 1, MPI_INT, my_id + 1, TAG, MPI_COMM_WORLD);
      }
    }
    else
    {

    }
  }


   MPI_Finalize();
   return 0;
 }
