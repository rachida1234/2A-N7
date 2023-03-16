#include "aux.h"


void bottom_up(int nleaves, struct node **leaves, int nnodes);

int main(int argc, char **argv){
  long   t_start, t_end;
  int    nnodes, nleaves;
  struct node **leaves;

  // Command line argument: number of nodes in the tree
  if ( argc == 2 ) {
    nnodes = atoi(argv[1]); 
  } else {
    printf("Usage:\n\n ./main n\n\nwhere n is the number of nodes in the tree.\n");
    return 1;
  }

  printf("\nGenerating a tree with %d nodes\n\n",nnodes);
  generate_tree(nnodes, &leaves, &nleaves);
  
  t_start = usecs();
  bottom_up(nleaves, leaves, nnodes);
  t_end = usecs();
  
  printf("Parallel time : %8.2f msec.\n\n",((double)t_end-t_start)/1000.0);

  check_result();
  
}
  

/* You can change the number and type of arguments if needed.     */
/* Just don't forget to update the interface declaration above.   */
void bottom_up(int nleaves, struct node **leaves, int nnodes){

  int l,i,v;
  struct node *curr;
  int *visited;

  // Allocate memory to save the nodes that were already accessed (initially all zero)
  // Access visited[i] by the id of the current node
  visited = (int*)malloc(nnodes*sizeof(int));
  for(i=0; i<nnodes; i++){
    visited[i] = 0;
  }


  for(l=0; l<nleaves; l++){
    printf("Leaf: %d", l);
    curr = leaves[l];
    
    while (curr){
      printf("  ID: %d \n", curr->id);
      v = visited[curr->id-1]++;
      if(v){
        break;
      }
      process_node(curr);
      curr = curr->parent;
      printf("Parent of : %d ", l);
    }
  }

}
    




