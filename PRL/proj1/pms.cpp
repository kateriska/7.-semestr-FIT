#include <mpi.h>
#include <iostream>
#include <fstream>
#include <queue>

using namespace std;

#define TAG 0

void showq(queue<int16_t> gq)
{
    queue<int16_t> g = gq;
    while (!g.empty()) {
        cout << '\t' << g.front();
        g.pop();
    }
    cout << '\n';
}

int main(int argc, char *argv[])
{
   int numprocs;               //pocet procesoru
   int myid;                   //muj rank
   int neighnumber;            //hodnota souseda
   int mynumber;               //moje hodnota
   MPI_Status stat;            //struct- obsahuje kod- source, tag, error

   queue<int16_t> input_queue;

   //MPI INIT
   MPI_Init(&argc,&argv);                          // inicializace MPI
   MPI_Comm_size(MPI_COMM_WORLD, &numprocs);       // zjistĂ­me, kolik procesĹŻ bÄ›ĹľĂ­
   MPI_Comm_rank(MPI_COMM_WORLD, &myid);           // zjistĂ­me id svĂ©ho procesu

   //NACTENI SOUBORU
   /* -proc s rankem 0 nacita vsechny hodnoty
    * -postupne rozesle jednotlive hodnoty vsem i sobe
   */
   if(myid == 0){
       char input[]= "numbers";                          //jmeno souboru
       int16_t number;                                     //hodnota pri nacitani souboru
       int invar= 0;                                   //invariant- urcuje cislo proc, kteremu se bude posilat
       fstream fin;                                    //cteni ze souboru
       fin.open(input, ios::in);

       while(fin.good()){
           number= fin.get();
           if(!fin.good()) break;                      //nacte i eof, takze vyskocim
           cout<<invar<<":"<<number<<endl;             //kdo dostane kere cislo
           input_queue.push(number);
           cout << "The input queue is : ";
           showq(input_queue);
           //MPI_Send(&number, 1, MPI_INT, invar + 1, TAG, MPI_COMM_WORLD); //buffer,velikost,typ,rank prijemce,tag,komunikacni skupina
           invar++;
       }//while
       fin.close();
   }//nacteni souboru

   if (myid == 0)
   {
     if (input_queue.empty() == 0)
     {
       int first_number = input_queue.front();
       input_queue.pop();
       MPI_Send(&first_number, 1, MPI_INT, myid + 1, TAG, MPI_COMM_WORLD);
       cout << "CPU id: " << myid + 1;
     }
   }

   MPI_Finalize();
   return 0;
 }
