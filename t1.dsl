A := Matrix[[1.23; 0.45; 2.11; -1.05; 0.88; 3.14];[0.76; -0.34; 1.29; 0.47; -2.13; 0.66];[2.00; 1.01; -0.98; 0.55; 0.00; -1.21];[-0.12; 0.44; 1.87; 2.33; -0.77; 0.12];[1.95; -1.65; 0.33; 0.99; 1.25; -0.49]]
b := Vector[1.0, 2.0, 3.0, 4.0, 5.0] 

A_T := trans_mat(A)
A_TA := A_T * A

d := det_mat(A_TA)
if(d = 0)then{Print("MatrixNotInvertible")}else{A_TA_inv := inv_mat(A_TA)}
if(d = 0)then{Print("MatrixNotInvertible")}else{A_Tb := A_T * b}
if(d = 0)then{Print("MatrixNotInvertible")}else{theta := A_TA_inv * A_Tb}
if(d = 0)then{Print("MatrixNotInvertible")}else{Print(theta)}
