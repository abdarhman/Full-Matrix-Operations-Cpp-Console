#include <iostream>
#include "string"
#include "cstdlib"
#include "sstream"
#include "math.h"
#define MAX 100
using namespace std;
float d = 0;
void Normalrender(float outputmatrix[100][100], int i , int j, int C1, int R1)
{
	if (i == 0 && j == 0)cout << "[";

	if (j != C1 - 1)
	{ cout << outputmatrix[i][j] << " "; }
	else
	{ cout << outputmatrix[i][j]; }
	if (j == C1 - 1 && i != R1 - 1)
	{ cout << ";"; }
	if (i == R1 - 1 && j == C1 - 1)cout << "]";
}
float determ(float a[MAX][MAX], int n) {
	int h, k, i, j, p;
	float det = 0.0 ;
	float temp[100][100];
	if (n == 1) {
		return a[0][0];
	}
	else if (n == 2) {
		det = (a[0][0] * a[1][1] - a[0][1] * a[1][0]);
		return det;
	}
	else {
		for (p = 0; p<n; p++) {
			h = 0;
			k = 0;
			for (i = 1; i<n; i++) {
				for (j = 0; j<n; j++) {
					if (j == p) {
						continue;
					}
					temp[h][k] = a[i][j];
					k++;
					if (k == n - 1) {
						h++;
						k = 0;
					}
				}
			}
			float q = determ(temp, n - 1);
			det = det + a[0][p] * pow(-1, p)*q;
		}
		return det;
	}
} // new one
void inverse(float rmatrix[MAX][MAX], float inversedmatrix[MAX][MAX],int R1, bool yesorno)
{
	float deto = determ(rmatrix, R1);
	float temp[100][100];
	float CoFactorMatrix[100][100];
	for (int j = 0; j<R1; j++) {
		for (int i = 0; i<R1; i++) {

			/* Form the adjoint a_ij */
			int i1 = 0;
			for (int ii = 0; ii<R1; ii++) {
				if (ii == i)
					continue;
				int j1 = 0;
				for (int jj = 0; jj<R1; jj++) {
					if (jj == j)
						continue;
					temp[i1][j1] = rmatrix[ii][jj];
					j1++;
				}
				i1++;
			}

			/* Calculate the determinate */
			float deto2 = determ(temp, R1 - 1);

			/* Fill in the elements of the cofactor */
			CoFactorMatrix[i][j] = pow(-1.0, i + j + 2.0) * deto2;
		}
	}
	for (int i = 0; i<R1; i++)
	{
		for (int j = 0; j<R1; j++)
		{ temp[i][j] = CoFactorMatrix[j][i]; }
	}
	for (int x = 0; x < R1; x++)
	{
		for (int y = 0; y < R1; y++)
		{ inversedmatrix[x][y] = temp[x][y] * (1 / deto); 
		if (yesorno) Normalrender(inversedmatrix, x, y, R1, R1);}
	}
}
void multiply(float first[MAX][MAX], float second[MAX][MAX], float multimatrix[MAX][MAX],int C1, int R2,int C2, int R1 , bool yesorno)
{
	if (C1 != R2)
	{ cout << "ERROR"; exit(0); } 
	else 
	{
		for (int i = 0; i < R1; i++)
		{
			for (int j = 0; j < C2; j++)
			{
				multimatrix[i][j] = 0;
				for (int q = 0; q < R2; q++)
				{
					multimatrix[i][j] = multimatrix[i][j] + first[i][q] * second[q][j];
					if ( (multimatrix[i][j] > 0) && (multimatrix[i][j] < 0.0001))multimatrix[i][j] = 0;
				}
			}
		}
		if (yesorno) {
			for (int i = 0; i < R1; i++)
			{
				for (int j = 0; j < C2; j++)
				{
					if (i == 0 && j == 0)cout << "[";

					if (j != C2 - 1)
					{
						cout << multimatrix[i][j] << " ";
					}
					else
					{
						cout << multimatrix[i][j];
					}
					if (j == C2 - 1 && i != R1 - 1)
					{
						cout << ";";
					}
					if (i == R1 - 1 && j == C2 - 1)cout << "]";
				}

			}
		}
	}
	
}
void matrixpower(float rmatrix[MAX][MAX],  int Power, int R1, int C1)
{
	float accumulativematrix[MAX][MAX];
	float helpermatrix[MAX][MAX];
	for (int i = 0; i < R1;i++) { for ( int j = 0; j < C1 ; j++){ helpermatrix[i][j] = rmatrix[i][j]; } }
	if (Power<0)
	{ cout << "ERROR"; exit(0); }
	if (R1 != C1)
	{ cout << "ERROR"; exit(0); }
	/*if (Power == 2){
		for (int i = 0; i < R1; i++)
		{
			for (int j = 0; j < C1; j++)
			{
				multimatrix[i][j] = 0;
				for (int q = 0; q < R1; q++){ multimatrix[i][j] = multimatrix[i][j] + helpermatrix[i][q] * helpermatrix[q][j]; }
			}
		}
	}*/
	for (int i = 1; i < Power; i++)
	{
		if (i == Power - 1)multiply(helpermatrix, rmatrix, accumulativematrix, C1, R1, C1, R1, true);
		multiply(helpermatrix, rmatrix, accumulativematrix, C1, R1, C1, R1, false);
		for (int k = 0; k < R1; k++) { for (int j = 0; j < C1; j++){ helpermatrix[k][j]=accumulativematrix[k][j] ; } }
	}
}
int main()
{
	//all variables like x11 are indexes in the upcoming for loops
	
	int m11, k11, sum = 0, sps = 0, i11, i, j, k, len, space = 0;
 	string crc, matrix, c1, c2 = "", c3, c4, matrix1[1000], matrix3[1000], rrmatrix[100][100];
	float  rmatrix[100][100], multimatrix[100][100];
	getline(cin, matrix);
	len = matrix.length();
	string ch1 = matrix.substr(0, 1);
	string ch2 = matrix.substr(len - 1, 1);
	for (i = 0; i<len; i++)
	{
		c4 = matrix.substr(i, 1);
		if (c4 == " ")
		{
			space = space + 1;
		}
	}
	if (len<4)
	{
		cout << "ERROR";
		exit(0);
	}

	if (ch1 != "[" || ch2 != "]")
	{
		cout << "ERROR";
		exit(0);

	}
	for (i = 0; i<len; i++)  ///to put the numbers in array without spaces
	{
		c1 = matrix.substr(i, 1);
		if (c1 != " "&&c1 != "["&&c1 != "]"&&c1 != ";")
		{
			c2 = c2 + c1;
		}
		else
		{
			matrix1[i] = c2;
			c2 = "";
			continue;
		}
	}

	for (j = 0; j<len; j++)  ///to put the numbers in a new array without empty spaces
	{
		for (i = 0; i<len; i++)
		{
			c3 = matrix1[i];
			if (c3 != "")
			{
				matrix3[j] = c3;
				matrix1[i] = "";
				c3 = "";
				break;
			}
			else
			{
				c3 = "";
				continue;
			}
		}
	}

	for (k = 0; k<len; k++) ///to know the true length of the numbers
	{
		if (matrix3[k] != "")	{continue;}
		else{break;}
	}
	for (i11 = 0; i11<len; i11++) ///to count the number of rows
	{
		crc = matrix.substr(i11, 1);
		if (crc == ";"){sum = sum + 1;}

	}
	for (k11 = 0; k11<len; k11++)  ///to know the place of first semicoloumn
	{
		crc = matrix.substr(k11, 1);
		if (crc == ";"){ break;}
	}
	for (m11 = 0; m11<k11; m11++) ///to count the spaces then add 1 to find the no of coloumn
	{
		crc = matrix.substr(m11, 1);
		if (crc == " ")	{sps = sps + 1;	}
	}
	int R1 = sum + 1; // sum calculate the number of semicolomns (rows)
	int C1 = sps + 1; // number of colomns (spaces)
	if (matrix3[C1*R1] != ""){ cout << "ERROR";  exit(0); }
	int truelength = 0;
	for (int u = 0; u < len; u++){ if (matrix3[u] != ""){ truelength++; } else { break; } }
	if (truelength%C1 != 0){ cout << "ERROR"; exit(0); }
	if (truelength%R1 != 0){ cout << "ERROR"; exit(0); }
	int s = 0;
	for (i = 0; i<R1; i++)      ///rrmatrix
	{
		for (j = 0; j<C1; j++)
		{
			for (s = 0; s<k; s++) // k number of true elements in 1D array
			{
				if (matrix3[s] != "")
				{
					rrmatrix[i][j] = matrix3[s];
					matrix3[s] = "";
					break;
				}
				else continue;
			}
		}
	}

	for (i = 0; i<R1; i++)   ///conversion of rmatrix from string to float
	{
		for (j = 0; j<C1; j++)
		{
			rmatrix[i][j] = atof(rrmatrix[i][j].c_str());
		}
	}
	for (int z = 0; z < R1; z++)
	{ if (rrmatrix[z][C1 - 1] == "") { cout << "ERROR"; exit(0); } }

	string opert;
	getline(cin, opert);

	
	if (opert == "^")
	{
		int power;
		cin >> power;
		matrixpower(rmatrix, power, R1, C1);

		/*for (i = 0; i<R1; i++)
		{
			for (j = 0; j<C1; j++)
			{
				if (i == 0 && j == 0)cout << "[";

				if (j != C1 - 1)
				{
					cout << multimatrix[i][j] << " ";
				}
				else
				{
					cout << multimatrix[i][j];
				}
				if (j == C1 - 1 && i != R1 - 1)
				{
					cout << ";";
				}
				if (i == R1 - 1 && j == C1 - 1)cout << "]";
			}

		}*/
		system("pause");
		exit(0);
	}
	float inversedmatrix[MAX][MAX]; 
	if (opert == "I")
	{
		if (R1 != C1){ cout << "ERROR"; exit(0); }
		inverse(rmatrix,inversedmatrix,R1,true);
		//float temp[100][100];
		//float InversedMatrix[100][100];
		//float CoFactorMatrix[100][100];
		//for (int j = 0; j<R1; j++) {
		//	for (int i = 0; i<R1; i++) {

		//		/* Form the adjoint a_ij */
		//		int i1 = 0;
		//		for (int ii = 0; ii<R1; ii++) {
		//			if (ii == i)
		//				continue;
		//			int j1 = 0;
		//			for (int jj = 0; jj<R1; jj++) {
		//				if (jj == j)
		//					continue;
		//				temp[i1][j1] = rmatrix[ii][jj];
		//				j1++;
		//			}
		//			i1++;
		//		}

		//		/* Calculate the determinate */
		//		float deto2 = determ(temp, R1 - 1);

		//		/* Fill in the elements of the cofactor */
		//		CoFactorMatrix[i][j] = pow(-1.0, i + j + 2.0) * deto2;
		//	}
		//}
		//for (i = 0; i<C1; i++)
		//{
		//	for (j = 0; j<R1; j++)
		//	{
		//		transmatrix[i][j] = CoFactorMatrix[j][i];
		//	}
		//}
		//for (int x = 0; x < R1; x++)
		//{
		//	for (int y = 0; y < R1; y++)
		//	{
		//		InversedMatrix[x][y] = transmatrix[x][y] * (1 / deto);
		//		
		//		Normalrender(InversedMatrix, x, y, R1, R1);
		//	}
		//}
		system("pause");
		exit(0);

	}
	
	if (opert == "T")
	{
		for (i = 0; i<C1; i++)
		{
			for (j = 0; j<R1; j++)
			{
				inversedmatrix[i][j] = rmatrix[j][i];
			}
		}
		for (i = 0; i<C1; i++)
		{
			for (j = 0; j<R1; j++)
			{
				Normalrender(inversedmatrix, i, j,R1,C1) ;
			}
		}
		exit(0);
	}
	if (opert == "D")
	{
		if (R1 != C1)
		{ cout << "ERROR ";	} 
		else cout << determ(rmatrix, R1);
		system("pause");
		exit(0);
	}
	int MM1MM, KK1KK, SSUMM = 0, SSpSS = 0, II, iii, jjj, kkk, lleen, spac = 0;
	string CCrCC, MATRIIX, CC1, CC2 = "", CC3, CC4, TRIX1[1000], TRIX3[1000], RTRIX[100][100];
	float RRTRIX[100][100]; //TTTRIX[100][100];
	getline(cin, MATRIIX);
	lleen = MATRIIX.length();
	string CH123 = MATRIIX.substr(0, 1);
	string CH321 = MATRIIX.substr(lleen - 1, 1);
	for (iii = 0; iii<lleen; iii++)
	{
		CC4 = MATRIIX.substr(iii, 1);
		if (CC4 == " ")
		{
			spac = spac + 1;
		}
	}
	if (lleen<4)
	{
		cout << "ERROR";
		exit(0);
	}

	if (CH123 != "[" || CH321 != "]")
	{
		cout << "ERROR";
		exit(0);

	}
	for (iii = 0; iii<lleen; iii++)  ///to put the numbers in array without spaces
	{
		CC1 = MATRIIX.substr(iii, 1);
		if (CC1 != " "&&CC1 != "["&&CC1 != "]"&&CC1 != ";")
		{
			CC2 = CC2 + CC1;
		}
		else
		{
			TRIX1[iii] = CC2;
			CC2 = "";
			continue;
		}
	}

	for (jjj = 0; jjj<lleen; jjj++)  ///to put the numbers in a new array without empty spaces
	{
		for (iii = 0; iii<lleen; iii++)
		{
			CC3 = TRIX1[iii];
			if (CC3 != "")
			{
				TRIX3[jjj] = CC3;
				TRIX1[iii] = "";
				CC3 = "";
				break;
			}
			else
			{
				CC3 = "";
				continue;
			}
		}
	}

	for (kkk = 0; kkk<lleen; kkk++) ///to know the true length of the numbers
	{
		if (TRIX3[kkk] != "")
		{
			continue;
		}
		else
		{
			break;
		}
	}
	for (II = 0; II<lleen; II++) ///to count the number of rows
	{
		CCrCC = MATRIIX.substr(II, 1);
		if (CCrCC == ";")
		{
			SSUMM = SSUMM + 1;
		}

	}
	for (KK1KK = 0; KK1KK<lleen; KK1KK++)  ///to know the place of first semicoloumn
	{
		CCrCC = MATRIIX.substr(KK1KK, 1);
		if (CCrCC == ";")
		{
			break;
		}
	}
	for (MM1MM = 0; MM1MM<KK1KK; MM1MM++) ///to count the spaces then add 1 to find the no of coloumn
	{
		CCrCC = MATRIIX.substr(MM1MM, 1);
		if (CCrCC == " ")
		{
			SSpSS = SSpSS + 1;
		}
	}
	int R2 = SSUMM + 1;
	int C2 = SSpSS + 1;
	truelength = 0;
	for (int u = 0; u < len; u++){ if (TRIX3[u] != ""){ truelength++; } else { break; } }
	if (truelength%C2 != 0){ cout << "ERROR"; exit(0); }
	if (truelength%R2 != 0){ cout << "ERROR"; exit(0); }
	int s11 = 0;
	for (iii = 0; iii<R2; iii++)      ///rrmatrix
	{
		for (jjj = 0; jjj<C2; jjj++)
		{
			for (s11 = 0; s11<kkk; s11++)
			{
				if (TRIX3[s11] != "")
				{
					RTRIX[iii][jjj] = TRIX3[s11];
					TRIX3[s11] = "";
					break;
				}
				else continue;
			}
		}
	}

	for (int i = 0; i<R2; i++)   ///conversion RRTRIX
	{
		for (jjj = 0; jjj<C2; jjj++)
		{
			RRTRIX[i][jjj] = atof(RTRIX[i][jjj].c_str());
		}
	}


	if (opert == "+")  ///PLUS
	{
		if (R1 != R2 || C1 != C2)
		{ cout << "ERROR";	exit(0);}
		for (i = 0; i<R1; i++)
		{
			for (j = 0; j<C1; j++)
			{multimatrix[i][j] = RRTRIX[i][j] + rmatrix[i][j]; Normalrender(multimatrix, i, j, C1, R1);	}
		}
	}
	if (opert == "-") ///MINUS
	{
		if (R1 != R2 || C1 != C2)
		{cout << "ERROR";	exit(0);}
		for (i = 0; i<R1; i++)
		{
			for (j = 0; j<C1; j++)
			{ multimatrix[i][j] = rmatrix[i][j] - RRTRIX[i][j]; Normalrender(multimatrix, i, j, C1, R1);}
		}
	}

	if (opert == "*") ///times
	{
		multiply(rmatrix, RRTRIX, multimatrix,C1, R2, C2, R1,true);
		/*if (C1 != R2)
		{cout << "ERROR"; exit(0); }
		for (i = 0; i<R1; i++)
		{
			for (j = 0; j<C2; j++)
			{
				multimatrix[i][j] = 0;
				for (int q = 0; q<R2; q++)
				{
					multimatrix[i][j] = multimatrix[i][j] + rmatrix[i][q] * RRTRIX[q][j];
				}
			}
		}
		for (i = 0; i<R1; i++)
		{
			for (j = 0; j<C2; j++)
			{
				if (i == 0 && j == 0)cout << "[";

				if (j != C2 - 1)
				{
					cout << multimatrix[i][j] << " ";
				}
				else
				{
					cout << multimatrix[i][j];
				}
				if (j == C2 - 1 && i != R1 - 1)
				{
					cout << ";";
				}
				if (i == R1 - 1 && j == C2 - 1)cout << "]";
			}

		}*/
	}
	if (opert == "/")
	{
		inverse(RRTRIX, inversedmatrix, R2, false);
		multiply(rmatrix, inversedmatrix, multimatrix,C1, R2, C2, R1,true);
	}









}
