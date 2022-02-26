// Copyright 2021 Avram Cristian-Stefan 311CA <stefanavram93@gmail.com>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MOD 10007
#define S1 sizeof(int)
#define S2 sizeof(int *)
#define S3 sizeof(int **)
#define MA malloc
#define CA calloc
#define RA realloc
#define OPTION_L loading_mat(&mat, &k, &dim)
#define OPTION_D dim_of_mat(dim, k)
#define OPTION_P print_mat(&mat, k, &dim)
#define OPTION_C redimension(&mat, &dim, k)
#define OPTION_M multiply(&mat, &k, &dim)
#define OPTION_O sort_mat(&mat, &dim, k)
#define OPTION_T transpose(&mat, &dim, k)
#define FINISH finish_program(&mat, &dim, k)
#define TMP_I (i % (tmp / 2))
#define TMP_J (j % (tmp / 2))
#define HF (tmp / 2) //HF comes from half - for shorter lines

// Function that allocs a matrix and read it
void alloc_mat_read(int *****mat, int index, int row, int col)
{   if (index == 0) {
		**mat = (int ***)MA((index + 1) * S3);
	} else {
		**mat = (int ***)RA(**mat, (index + 1) * S3);
	}

	if (!(**mat))
		fprintf(stderr, "Could not allocate list of matrix.\n");

	(**mat)[index] = (int **)MA(row * S2);

	if (!(**mat)[index])
		fprintf(stderr, "Could not allocate matrix.\n");

	for (int i = 0; i < row; i++) {
		(**mat)[index][i] = (int *)MA(col * S1);

		if (!(**mat)[index][i])
			fprintf(stderr, "Could not allocate columns for matrix.\n");

		for (int j = 0; j < col; j++) {
			int a;
			scanf("%d", &a);
			(**mat)[index][i][j] = a;
		}
	}
}

// Function that allocs the dimensions of a given matrix
void alloc_dimension(int ****dim, int index, int row, int col)
{   if (index == 0) {
		**dim = (int **)MA((index + 1) * S2);
	} else {
		**dim = (int **)RA(**dim, (index + 1) * S2);
	}

	if (!(**dim))
		fprintf(stderr, "Could not allocate 'dimensions'.\n");

	(**dim)[index] = (int *)MA(2 * S1);

	if (!(**dim)[index])
		fprintf(stderr, "Could not allocate columns for 'dimensions'.\n");

	(**dim)[index][0] = row;
	(**dim)[index][1] = col;
}

// Function that is given to command 'L' and uploads a new matrix in the list
void loading_mat(int ****mat, int *k, int ***dim)
{   int row, col;
	scanf("%d%d", &row, &col);

	alloc_mat_read(&*&mat, *k, row, col);
	alloc_dimension(&*&dim, *k, row, col);

	(*k)++;
}

// Function that is given to command 'D' and prints the dimensions of a matrix
void dim_of_mat(int **dim, int k)
{   int index;
	scanf("%d", &index);

	if (index < 0 || index >= k) {
		printf("No matrix with the given index\n");
		return;
	}

	printf("%d %d\n", dim[index][0], dim[index][1]);
}

//Function that is given to command 'P' and prints a matrix
void print_mat(int ****mat, int k, int ***dim)
{   int index;
	scanf("%d", &index);

	if (index < 0 || index >= k) {
		printf("No matrix with the given index\n");
		return;
	}

	for (int i = 0; i < (*dim)[index][0]; i++) {
		for (int j = 0; j < (*dim)[index][1]; j++)
			printf("%d ", (*mat)[index][i][j]);
		printf("\n");
	}
}

//Function that is given to command 'C' and redimension a matrix
void redimension(int ****mat, int ***dim, int k)
{   int index;
	scanf("%d", &index);

	if (index < 0 || index >= k) {
		printf("No matrix with the given index\n");
		return;
	}

	/*Variables:new_row -> number of rows after redimension
				new_col -> number of columns after redimension
				row_index -> an array that loads the indexes of
							row_elements that are needed for redimension
				col_index -> an array that loads the indexes of
							column_elements that are needes for redimension
	*/
	int new_row, new_col, *row_index, *col_index;

	scanf("%d", &new_row);
	row_index = (int *)MA(new_row * S1);

	if (!row_index)
		fprintf(stderr, "Could not allocate row_index.\n");

	for (int i = 0; i < new_row; i++)
		scanf("%d", &row_index[i]);

	scanf("%d", &new_col);
	col_index = (int *)MA(new_col * S1);

	if (!col_index)
		fprintf(stderr, "Could not allocate col_index.\n");

	for (int i = 0; i < new_col; i++)
		scanf("%d", &col_index[i]);

	int n = (*dim)[index][0];
	int m = (*dim)[index][1];

	//Realloc memory for mirroring the matrix
	for (int i  = 0; i < n; i++)
		(*mat)[index][i] = (int *)RA((*mat)[index][i], (m + new_col) * S1);

	for (int i = 0; i < new_row; i++) {
		for (int j = 0; j < new_col; j++) {
			int tmp = (*mat)[index][row_index[i]][col_index[j]];
			(*mat)[index][i][m + j] = tmp;
		}
	}

	for (int i = new_row; i < (*dim)[index][0]; i++)
		free((*mat)[index][i]);

	//Equalizing the matrix with its redimensioned form;
	for (int i = 0; i < new_row; i++) {
		for (int j = 0; j < new_col; j++)
			(*mat)[index][i][j] = (*mat)[index][i][m + j];
	}

	for (int i = 0; i < new_row; i++) {
		(*mat)[index][i] = (int *)RA((*mat)[index][i], new_col * S1);

		if (!(*mat)[index][i])
			fprintf(stderr, "Could not allocate columns for matrix.\n");
	}

	(*dim)[index][0] = new_row;
	(*dim)[index][1] = new_col;

	free(row_index);
	free(col_index);
}

//Function that calculates the modulo 1e4 + 7 of a number
int modulo_nr(int x)
{	if (x % MOD < 0)
		return x % MOD + MOD;
	return x % MOD;
}

//Function that is given to command 'M' and upiplies 2 matrices
//A new matrix will be obtained and stored into the list
void multiply(int ****mat, int *k, int ***dim)
{   int index1, index2;
	scanf("%d%d", &index1, &index2);

	if ((index1 >= (*k) || index1 < 0) || (index2 >= (*k) || index2 < 0)) {
		printf("No matrix with the given index\n");
		return;
	}

	//Verify if the column1 = row2
	if ((*dim)[index1][1] != (*dim)[index2][0]) {
		printf("Cannot perform matrix multiplication\n");
		return;
	}

	(*k)++;
	(*mat) = (int ***)RA((*mat), (*k) * S3);

	if (!*mat)
		fprintf(stderr, "Could not allocate list of matrix.\n");

	(*dim) = (int **)RA((*dim), (*k) * S2);

	if (!*dim)
		fprintf(stderr, "Could not allocate 'dimensions'.\n");

	(*dim)[(*k) - 1] = (int *)MA(2 * S1);

	if (!(*dim)[(*k) - 1])
		fprintf(stderr, "Could not allocate columns for 'dimensions'.\n");

	(*dim)[(*k) - 1][0] = (*dim)[index1][0];
	(*dim)[(*k) - 1][1] = (*dim)[index2][1];

	int new_m = (*dim)[(*k) - 1][0]; //nr of rows for new_matrix
	int new_n = (*dim)[(*k) - 1][1]; //nr of cols for new_matrix

	//Alloc space for new_matrix
	(*mat)[(*k) - 1] = (int **)MA(new_m * S2);

	if (!(*mat)[(*k) - 1])
		fprintf(stderr, "Could not allocate matrix.\n");

	for (int i = 0; i < new_m; i++) {
		(*mat)[(*k) - 1][i] = (int *)CA(new_n, S1);

		if (!(*mat)[(*k) - 1][i])
			fprintf(stderr, "Could not allocate columns for matrix.\n");
	}

	for (int i = 0; i < (*dim)[index1][0]; i++) {
		for (int j = 0; j < (*dim)[index2][1]; j++) {
			for (int p = 0; p < (*dim)[index1][1]; p++) {
				int ad = 1;

				ad *= modulo_nr((*mat)[index1][i][p]);
				ad *= modulo_nr((*mat)[index2][p][j]);
				ad = modulo_nr(ad);

				(*mat)[(*k) - 1][i][j] += ad;
				(*mat)[(*k) - 1][i][j] = modulo_nr((*mat)[(*k) - 1][i][j]);
			}
			(*mat)[(*k) - 1][i][j] = modulo_nr((*mat)[(*k) - 1][i][j]);
		}
	}
}

//Function that returns the elements' sum of a matrix
int sum_mat(int *****mat, int ****dim, int index)
{   int sum = 0;

	for (int i = 0; i < (**dim)[index][0]; i++)
		for (int j = 0; j < (**dim)[index][1]; j++) {
			sum = modulo_nr(sum);
			sum += (**mat)[index][i][j];
			sum = modulo_nr(sum);
		}
	return modulo_nr(sum);
}

/*Function used in sorting matrices. It is made for
  sorting matrices by swapping them. In function,
  we are swapping 2 matrices.
  First, it equalizes the dimensions of matrices by
  the superior dimension. For example, if the program needs
  to swap the following matrices: 2x3 and 5x2
  The matrix 1 will be 5x3 by raising 2 to 5
  and matrix 2 will be 5x3 by raising 2 to 3
*/
void swap_mat(int *****mat, int ****dim, int i1, int i2)
{   if ((**dim)[i1][0] > (**dim)[i2][0]) {
		(**mat)[i2] = (int **)RA((**mat)[i2], (**dim)[i1][0] * S2);

		if (!(**mat)[i2])
			fprintf(stderr, "Could not allocate matrix.\n");

		for (int i = (**dim)[i2][0]; i < (**dim)[i1][0]; i++) {
			(**mat)[i2][i] = (int *)MA((**dim)[i2][1] * S1);

			if (!(**mat)[i2][i])
				fprintf(stderr, "Could not allocate columns for matrix.\n");
		}
	} else {
		(**mat)[i1] = (int **)RA((**mat)[i1], (**dim)[i2][0] * S2);

		if (!(**mat)[i1])
			fprintf(stderr, "Could not allocate matrix.\n");

		for (int i = (**dim)[i1][0]; i < (**dim)[i2][0]; i++) {
			(**mat)[i1][i] = (int *)MA((**dim)[i1][1] * S1);

			if (!(**mat)[i1][i])
				fprintf(stderr, "Could not allocate columns for matrix.\n");
		}
	}
	if ((**dim)[i1][1] < (**dim)[i2][1]) {
		for (int i = 0; i < (**dim)[i1][0]; i++) {
			(**mat)[i1][i] = (int *)RA((**mat)[i1][i], (**dim)[i2][1] * S1);

			if (!(**mat)[i1][i])
				fprintf(stderr, "Could not allocate columns for matrix.\n");
		}
	} else {
		for (int i = 0; i < (**dim)[i2][0]; i++) {
			(**mat)[i2][i] = (int *)RA((**mat)[i2][i], (**dim)[i1][1] * S1);

			if (!(**mat)[i2][i])
				fprintf(stderr, "Could not allocate columns for matrix.\n");
		}
	}

	int **aux = (**mat)[i1]; //-> The actual process of swapping
	(**mat)[i1] = (**mat)[i2];
	(**mat)[i2] = aux;

	int *aux_2 = (**dim)[i1];
	(**dim)[i1] = (**dim)[i2];
	(**dim)[i2] = aux_2;

	if ((**dim)[i2][0] > (**dim)[i1][0]) { // -> Shrink the matrices by old dim
		for (int i = (**dim)[i1][0]; i < (**dim)[i2][0]; i++)
			free((**mat)[i1][i]);
		(**mat)[i1] = (int **)RA((**mat)[i1], (**dim)[i1][0] * S2);

		if (!(**mat)[i1])
			fprintf(stderr, "Could not allocate matrix.\n");
	} else {
		for (int i = (**dim)[i2][0]; i < (**dim)[i1][0]; i++)
			free((**mat)[i2][i]);
		(**mat)[i2] = (int **)RA((**mat)[i2], (**dim)[i2][0] * S2);

		if (!(**mat)[i2])
			fprintf(stderr, "Could not allocate matrix.\n");
	}
	if ((**dim)[i1][1] < (**dim)[i2][1]) {
		for (int i = 0; i < (**dim)[i1][0]; i++) {
			(**mat)[i1][i] = (int *)RA((**mat)[i1][i], (**dim)[i2][1] * S1);

			if (!(**mat)[i1][i])
				fprintf(stderr, "Could not allocate columns for matrix.\n");
		}
	} else {
		for (int i = 0; i < (**dim)[i2][0]; i++) {
			(**mat)[i2][i] = (int *)RA((**mat)[i2][i], (**dim)[i1][1] * S1);

			if (!(**mat)[i2][i])
				fprintf(stderr, "Could not allocate columns for matrix.\n");
		}
	}
}

//Function that is given to command 'O' and sort the matrices
//by their elements' sum
void sort_mat(int ****mat, int ***dim, int total)
{   for (int i = 0; i < total - 1; i++) {
		int j = i + 1;

		while (j < total) {
			if (sum_mat(&*&mat, &*&dim, i) > sum_mat(&*&mat, &*&dim, j))
				swap_mat(&*&mat, &*&dim, i, j);
			j++;
		}
	}
}

//If the command 'F' is given, this function swap the matrices
//from the given index to final. After these operations, the matrix
//that we need to delete will be stored in the last postion of the list.
void sort_mat_and_free(int ****mat, int ***dim, int total, int index)
{   for (int i = index; i < total - 1; i++)
		swap_mat(&*&mat, &*&dim, i, i + 1);
}

//Function that modify a matrix "in-place" as in final
//there will be its transpose.
void transpose(int ****mat, int ***dim, int k)
{   int nr;
	scanf("%d", &nr);

	if (nr < 0 || nr >= k) {
		printf("No matrix with the given nr\n");
		return;
	}

	int tmp = (*dim)[nr][0]; //number of rows // number of cols

	//As in swapping, the function equals the nr of row with the number of cols
	if ((*dim)[nr][0] < (*dim)[nr][1]) {
		(*mat)[nr] = (int **)RA((*mat)[nr], (*dim)[nr][1] * S2);

		if (!(*mat)[nr])
			fprintf(stderr, "Could not allocate matrix.\n");

		for (int i = (*dim)[nr][0]; i < (*dim)[nr][1]; i++) {
			(*mat)[nr][i] = (int *)CA((*dim)[nr][1], S1);

			if (!(*mat)[nr][i])
				fprintf(stderr, "Could not allocate columns for matrix.\n");
		}

		tmp = (*dim)[nr][1];
	}
	if ((*dim)[nr][0] > (*dim)[nr][1]) {
		for (int i = 0; i < (*dim)[nr][0]; i++) {
			(*mat)[nr][i] = (int *)RA((*mat)[nr][i], (*dim)[nr][0] * S1);

			if (!(*mat)[nr][i])
				fprintf(stderr, "Could not allocate columns for matrix.\n");

			for (int j = (*dim)[nr][1]; j < (*dim)[nr][0]; j++)
				(*mat)[nr][i][j] = 0;
		}
		tmp = (*dim)[nr][0];
	}

	//Calculating the transpose
	for (int i = 0; i < tmp - 1; i++) {
		for (int j = i + 1; j < tmp; j++) {
			int aux = (*mat)[nr][i][j];
			(*mat)[nr][i][j] = (*mat)[nr][j][i];
			(*mat)[nr][j][i] = aux;
		}
	}

	//Shrinking the matrix dimensions
	if ((*dim)[nr][0] < (*dim)[nr][1]) {
		for (int i = 0; i < tmp; i++) {
			(*mat)[nr][i] = (int *)RA((*mat)[nr][i], (*dim)[nr][0] * S1);

			if (!(*mat)[nr][i])
				fprintf(stderr, "Could not allocate columns for matrix.\n");
		}
	}
	if ((*dim)[nr][0] > (*dim)[nr][1]) {
		for (int i = (*dim)[nr][1]; i < (*dim)[nr][0]; i++)
			free((*mat)[nr][i]);

		(*mat)[nr] = (int **)RA((*mat)[nr], (*dim)[nr][1] * S2);

		if (!(*mat)[nr])
			fprintf(stderr, "Could not allocate matrix.\n");
	}

	(*dim)[nr][0] += (*dim)[nr][1];
	(*dim)[nr][1] = (*dim)[nr][0] - (*dim)[nr][1];
	(*dim)[nr][0] = (*dim)[nr][0] - (*dim)[nr][1];
}

//Function that free a matrix and deallocate its space.
void free_mat(int ****mat, int ***dim, int *k)
{   for (int i = 0; i < (*dim)[(*k) - 1][0]; i++)
		free((*mat)[(*k) - 1][i]);
	free((*mat)[(*k) - 1]);

	free((*dim)[(*k) - 1]);
	(*dim) = (int **)RA((*dim), (*k - 1) * S2);

	if (!(*dim))
		fprintf(stderr, "Could not allocate 'dimensions'.\n");

	(*k)--;
}

//Function used in the end. Its job is to free all the memory that
//has been allocated to the execution of this program.
void finish_program(int ****mat, int ***dim, int k)
{   for (int index = 0; index < k; index++) {
		for (int i = 0; i < (*dim)[index][0]; i++)
			free((*mat)[index][i]);
	}
	for (int i = 0; i < k; i++)
		free((*mat)[i]);
	free(*mat);

	for (int i = 0; i < k; i++)
		free((*dim)[i]);
	free(*dim);
}

//Function used to add a matrix to another
int **ad(int **x, int **y, int tmp)
{	int **aux_mat;
	aux_mat = (int **)MA(tmp * S2);
	for (int i = 0; i < tmp; i++)
		aux_mat[i] = (int *)MA(tmp * S1);
	for (int i = 0; i < tmp; i++) {
		for (int j = 0; j < tmp; j++) {
			aux_mat[i][j] = modulo_nr(modulo_nr(x[i][j]) + modulo_nr(y[i][j]));
			aux_mat[i][j] = modulo_nr(aux_mat[i][j]);
		}
	}
	return aux_mat;
}

//Function used to substract a matrix from another
int **sub(int **x, int **y, int tmp)
{	int **aux_mat;
	aux_mat = (int **)MA(tmp * S2);
	for (int i = 0; i < tmp; i++)
		aux_mat[i] = (int *)MA(tmp * S1);
	for (int i = 0; i < tmp; i++) {
		for (int j = 0; j < tmp; j++) {
			aux_mat[i][j] = modulo_nr(modulo_nr(x[i][j]) - modulo_nr(y[i][j]));
			aux_mat[i][j] = modulo_nr(aux_mat[i][j]);
		}
	}
	return aux_mat;
}

//Function used to multiply 2 matrices of size 2x2
int **multiply_2nd(int **x, int **y, int tmp, int ok1, int ok2)
{	int **aux_mat;
	aux_mat = (int **)MA(tmp * S2);
	for (int i = 0; i < tmp; i++)
		aux_mat[i] = (int *)MA(tmp * S1);
	int p1, p2, p3, p4, p5, p6, p7;
	int a11 = modulo_nr(x[0][0]);
	int a12 = modulo_nr(x[0][1]);
	int a21 = modulo_nr(x[1][0]);
	int a22 = modulo_nr(x[1][1]);
	int b11 = modulo_nr(y[0][0]);
	int b12 = modulo_nr(y[0][1]);
	int b21 = modulo_nr(y[1][0]);
	int b22 = modulo_nr(y[1][1]);

	p1 = modulo_nr(modulo_nr(a11 + a22) * modulo_nr(b11 + b22));
	p2 = modulo_nr(modulo_nr(a21 + a22) * b11);
	p3 = modulo_nr(a11 * modulo_nr(b12 - b22 + MOD));
	p4 = modulo_nr(a22 * modulo_nr(b21 - b11 + MOD));
	p5 = modulo_nr(modulo_nr(a11 + a12) * b22);
	p6 = modulo_nr(modulo_nr(a21 - a11 + MOD) * modulo_nr(b11 + b12));
	p7 = modulo_nr(modulo_nr(a12 - a22 + MOD) * modulo_nr(b21 + b22));

	aux_mat[0][0] = modulo_nr(p1 + p4);
	aux_mat[0][0] = modulo_nr(aux_mat[0][0] - p5 + MOD);
	aux_mat[0][0] = modulo_nr(aux_mat[0][0] + p7);
	aux_mat[0][1] = modulo_nr(p3 + p5);
	aux_mat[1][0] = modulo_nr(p2 + p4);
	aux_mat[1][1] = modulo_nr(p1 - p2 + MOD);
	aux_mat[1][1] = modulo_nr(aux_mat[1][1] + p3);
	aux_mat[1][1] = modulo_nr(aux_mat[1][1] + p6);

	for (int i = 0; i < tmp; i++) {
		if (ok1)
			free(x[i]);
		if (ok2)
			free(y[i]);
	}
	if (ok1)
		free(x);
	if (ok2)
		free(y);

	return aux_mat;
}

//Function used to equalize the submatrix a11, a12, a21, a22, b11, b12, b21
//and b22 with the quarters of the matrices that we want to multiply
//by the Strassen algorithm.
void equalize(int tmp, int ***x11, int ***x12, int ***x21, int ***x22, int **m)
{	for (int i = 0; i < tmp; i++) {
		int ci = i;
		for (int j = 0; j < tmp; j++) {
			int cj = j;
			if (i < HF && j < HF)
				(*x11)[ci][cj] = m[i][j];
			if (i < HF && j >= HF) {
				cj %= HF;
				(*x12)[ci][cj] = m[i][j];
			}
			if (i >= HF && j < HF) {
				ci %= HF;
				(*x21)[ci][cj] = m[i][j];
			}
			if (i >= HF && j >= HF) {
				cj %= HF;
				ci %= HF;
				(*x22)[ci][cj] = m[i][j];
			}
		}
	}
}

//Function used to alloc memory for submatrix and aux_mat (when ok = 1)
void alloc_utils(int ***x1, int ***x2, int ***x3, int tmp, int ok)
{	if (ok)
		*x1 = (int **)MA(tmp * S2);
	else
		*x1 = (int **)MA((HF) * S2);
	*x2 = (int **)MA((HF) * S2);
	*x3 = (int **)MA((HF) * S2);
}

//Function used to alloc memory for submatrix rows and aux_mat (when ok = 1)
void alloc2_utils(int ***x1, int ***x2, int ***x3, int tmp, int ok)
{	for (int i = 0; i < HF; i++) {
		if (ok) {
			(*x1)[i] = (int *)MA(tmp * S1);
			(*x1)[HF + i] = (int *)MA(tmp * S1);
		} else {
			(*x1)[i] = (int *)MA((HF) * S1);
		}
		(*x2)[i] = (int *)MA((HF) * S1);
		(*x3)[i] = (int *)MA((HF) * S1);
	}
}

//Function for free the auxiliary matrices
void free_utils(int ***x1, int ***x2, int ***x3, int tmp)
{	for (int i = 0; i < HF; i++) {
		free((*x1)[i]);
		free((*x2)[i]);
		free((*x3)[i]);
	}
	free(*x1);
	free(*x2);
	free(*x3);
}

//Strassen algorithm
int **s_m(int **copy_mat_1, int **copy_mat_2, int tmp, int ok1, int ok2)
{	int **a11, **a12, **a21, **a22, **aux_mat;
	int **b11, **b12, **b21, **b22;
	int **p1, **p2, **p3, **p4, **p5, **p6, **p7;
	alloc_utils(&aux_mat, &a11, &a12, tmp, 1);
	alloc_utils(&a21, &a22, &b11, tmp, 0);
	alloc_utils(&b12, &b21, &b22, tmp, 0);
	alloc2_utils(&aux_mat, &a11, &a12, tmp, 1);
	alloc2_utils(&a21, &a22, &b11, tmp, 0);
	alloc2_utils(&b12, &b21, &b22, tmp, 0);
	equalize(tmp, &a11, &a12, &a21, &a22, copy_mat_1);
	equalize(tmp, &b11, &b12, &b21, &b22, copy_mat_2);

	if (tmp == 4) {
		p1 = multiply_2nd((ad(a11, a22, HF)), (ad(b11, b22, HF)), HF, 1, 1);
		p2 = multiply_2nd(ad(a21, a22, HF), b11, HF, 1, 0);
		p3 = multiply_2nd(a11, sub(b12, b22, HF), HF, 0, 1);
		p4 = multiply_2nd(a22, sub(b21, b11, HF), HF, 0, 1);
		p5 = multiply_2nd(ad(a11, a12, HF), b22, HF, 1, 0);
		p6 = multiply_2nd(sub(a21, a11, HF), ad(b11, b12, HF), HF, 1, 1);
		p7 = multiply_2nd(sub(a12, a22, HF), ad(b21, b22, HF), HF, 1, 1);
	} else {
		p1 = s_m((ad(a11, a22, HF)), (ad(b11, b22, HF)), HF, 1, 1);
		p2 = s_m(ad(a21, a22, HF), b11, HF, 1, 0);
		p3 = s_m(a11, sub(b12, b22, HF), HF, 0, 1);
		p4 = s_m(a22, sub(b21, b11, HF), HF, 0, 1);
		p5 = s_m(ad(a11, a12, HF), b22, HF, 1, 0);
		p6 = s_m(sub(a21, a11, HF), ad(b11, b12, HF), HF, 1, 1);
		p7 = s_m(sub(a12, a22, HF), ad(b21, b22, HF), HF, 1, 1);
	}
	for (int i = 0; i < tmp; i++) {
		for (int j = 0; j < tmp; j++) {
			if (i < HF && j < HF) {
				aux_mat[i][j] = modulo_nr(p1[TMP_I][TMP_J]);
				aux_mat[i][j] += modulo_nr(p4[TMP_I][TMP_J]);
				aux_mat[i][j] = modulo_nr(aux_mat[i][j]);
				aux_mat[i][j] = aux_mat[i][j] - modulo_nr(p5[TMP_I][TMP_J]);
				aux_mat[i][j] += MOD;
				aux_mat[i][j] = modulo_nr(aux_mat[i][j]);
				aux_mat[i][j] = aux_mat[i][j] + modulo_nr(p7[TMP_I][TMP_J]);
				aux_mat[i][j] = modulo_nr(aux_mat[i][j]);
			}
			if (i < HF && j >= HF) {
				aux_mat[i][j] = modulo_nr(p3[TMP_I][TMP_J]);
				aux_mat[i][j] += modulo_nr(p5[TMP_I][TMP_J]);
				aux_mat[i][j] = modulo_nr(aux_mat[i][j]);
			}
			if (i >= HF && j < HF) {
				aux_mat[i][j] = modulo_nr(p2[TMP_I][TMP_J]);
				aux_mat[i][j] += modulo_nr(p4[TMP_I][TMP_J]);
				aux_mat[i][j] = modulo_nr(aux_mat[i][j]);
			}
			if (i >= HF && j >= HF) {
				aux_mat[i][j] = modulo_nr(p1[TMP_I][TMP_J]);
				aux_mat[i][j] -= modulo_nr(p2[TMP_I][TMP_J]) + MOD;
				aux_mat[i][j] = modulo_nr(aux_mat[i][j]);
				aux_mat[i][j] = aux_mat[i][j] + modulo_nr(p3[TMP_I][TMP_J]);
				aux_mat[i][j] = modulo_nr(aux_mat[i][j]);
				aux_mat[i][j] = aux_mat[i][j] + modulo_nr(p6[TMP_I][TMP_J]);
				aux_mat[i][j] = modulo_nr(aux_mat[i][j]);
			}
		}
	}
	for (int i = 0; i < tmp; i++) {
		if (ok1)
			free(copy_mat_1[i]);
		if (ok2)
			free(copy_mat_2[i]);
	}
	if (ok1)
		free(copy_mat_1);
	if (ok2)
		free(copy_mat_2);

	free_utils(&p1, &p2, &p3, tmp);
	free_utils(&p4, &p5, &p6, tmp);
	free_utils(&p7, &a11, &a12, tmp);
	free_utils(&a21, &a22, &b11, tmp);
	free_utils(&b12, &b21, &b22, tmp);
	return aux_mat;
}

//Main function for Strassen
void strassen(int ****mat, int ***dim, int *total)
{	int i1, i2;
	scanf("%d%d", &i1, &i2);

	if ((i1 < 0 || i1 >= *total) || (i2 < 0 || i2 >= *total)) {
		printf("No matrix with the given index\n");
		return;
	}
	int ok = 1;
	if ((*dim)[i1][0] != (*dim)[i1][1])
		ok = 0;
	if ((*dim)[i2][0] != (*dim)[i2][1])
		ok = 0;
	if ((*dim)[i1][0] != (*dim)[i2][1])
		ok = 0;
	if (ok == 0) {
		printf("Cannot perform matrix multiplication\n");
		return;
	}

	*mat = (int ***)RA(*mat, ((*total) + 1) * S3);

	*dim = (int **)RA(*dim, ((*total) + 1) * S2);
	(*dim)[*total] = (int *)MA(2 * S1);
	(*dim)[*total][0] = (*dim)[i1][0];
	(*dim)[*total][1] = (*dim)[i1][0];

	int tmp = (*dim)[*total][0];

	int **copy_mat_1, **copy_mat_2;
	copy_mat_1 = (int **)MA(tmp * S2);
	copy_mat_2 = (int **)MA(tmp * S2);
	for (int i = 0; i < tmp; i++) {
		copy_mat_1[i] = (int *)MA(tmp * S1);
		copy_mat_2[i] = (int *)MA(tmp * S1);
	}

	for (int i = 0; i < tmp; i++)
		for (int j = 0; j < tmp; j++) {
			copy_mat_1[i][j] = (*mat)[i1][i][j];
			copy_mat_2[i][j] = (*mat)[i2][i][j];
		}
	ok = 1;
	if (tmp > 2) {
		(*mat)[*total] = s_m(copy_mat_1, copy_mat_2, tmp, 1, 1);
	} else {
		(*mat)[*total] = multiply_2nd(copy_mat_1, copy_mat_2, tmp, 1, 1);
		ok = 0;
	}
	(*total)++;
}

int main(void)
{   int ***mat, **dim, k = 0; //mat -> list_of_matrix
							//dim -> a matrix of dimensions (rows & columns)
							//k -> the number of matrices from the list
	/* dim[a][b] -> a = index of the matrix
					b = 0 || the 'row' dimension of a matrix (nr of rows)
					b = 1 || the 'col' dimension of a matrix (nr of columns)
	    example -> dim[3][0] = number of rows stored in matrix NR. 3
					dim[8][1] = number of columns sotred in matrix NR. 8
	*/

	while (1) {
		char command;
		scanf(" %c", &command);
		if (command == 'L') {
			OPTION_L;
		} else if (command == 'D') {
			OPTION_D;
		} else if (command == 'P') {
			OPTION_P;
		} else if (command == 'C') {
			OPTION_C;
		} else if (command == 'M') {
			OPTION_M;
		} else if (command == 'O') {
			OPTION_O;
		} else if (command == 'T') {
			OPTION_T;
		} else if (command == 'F') {
			int index;
			scanf("%d", &index);

			if (index < 0 || index >= k) {
				printf("No matrix with the given index\n");
			} else {
				if (k > 1) {
					sort_mat_and_free(&mat, &dim, k, index);
					free_mat(&mat, &dim, &k);
				} else {
					finish_program(&mat, &dim, k);
					k--;
				}
			}
		} else if (command == 'Q') {
			break;
	    } else if (command == 'S') {
			strassen(&mat, &dim, &k);
		} else {
			int search = (int)command;
			char digits[] = "0123456789";
			char *letter = strchr(digits, search);

			if (!letter)
				printf("Unrecognized command\n");
		}
	}

	//Free the memory
	if (k > 0)
		FINISH;

	return 0;
}

