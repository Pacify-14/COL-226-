A := Matrix[[2.0; 4.0]; [1.0; 2.0]] 
d := det_mat(A)
if(d = 0)then{Print("MatrixNotInvertible")}else{A_inv := inv_mat(A)}
if(d = 0)then{Print("MatrixNotInvertible")}else{Print(A_inv)}
