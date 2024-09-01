#include <stdio.h>
#include <stdlib.h>
#include <stdlib_tainted.h>

#define MAX_UNSIGNED 0xFFFFFFFF
#define COMPLEX_FLOAT float _Complex

typedef struct {
    unsigned long state;
    unsigned long amplitude;
} quantum_reg_node_struct;

typedef struct {
      int width;    /* number of qubits in the qureg */
  int size;     /* number of non-zero vectors */
  int hashw;    /* width of the hash array */
	_TPtr<quantum_reg_node_struct> node;
_TPtr<int> hash;
} quantum_reg;

struct quantum_matrix_struct {
  int rows;
  int cols;
  COMPLEX_FLOAT *t;
};

typedef struct quantum_matrix_struct quantum_matrix;


quantum_reg quantum_matrix2qureg(quantum_matrix *m, int width)
{
  quantum_reg reg;
  int i, j, size=0;
  
  if(m->cols != 1)
    {
      printf("Error! Cannot convert a multi-column-matrix (%i)!\n", m->cols);
      exit(1);
    }
  reg.width = width;


  for(i=0; i<m->rows; i++)
    {
      if(m->t[i])
        size++;
    }


  reg.size = size;
  reg.hashw = width + 2;
  reg.node = t_malloc(size * sizeof(quantum_reg_node_struct));

  if(!reg.node)
    {
      printf("Not enough memory for %i-sized qubit!\n", size);
      exit(1);
    }

    reg.node = t_malloc((1 << reg.hashw)* sizeof(quantum_reg_node_struct));
  if(!reg.hash)
    {
      printf("Not enough memory for %i-sized hash!\n", 1 << reg.hashw);
      exit(1);
    }

  /* Copy the nonzero amplitudes of the vector into the quantum
     register */

  for(i=0, j=0; i<m->rows; i++)
    {
      if(m->t[i])
	{
	  reg.node[j].state = i;
	  reg.node[j].amplitude = m->t[i];
	  j++;
	}
    }
	return reg;
}
quantum_matrix
quantum_new_matrix(int cols, int rows)
{
  quantum_matrix m;

  m.rows = rows;
  m.cols = cols;

  if(!m.t)
  {
    printf("Not enogh memory for %ix%i-Matrix!",rows,cols);
    exit(1);
  }
  quantum_memman(sizeof(COMPLEX_FLOAT) * cols * rows);

  return m;
}

quantum_matrix
quantum_qureg2matrix(quantum_reg reg)
{
  quantum_matrix m;
  int i;

  m = quantum_new_matrix(1, 1 << reg.width);

  for(i=0; i<reg.size; i++)
    m.t[reg.node[i].state] = reg.node[i].amplitude;

  return m;
}

