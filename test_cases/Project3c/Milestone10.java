class Factorial {
    public static void main(String[] a){
	System.out.println(new Fac().BadLoop());
    }
}

class Fac {
    public int BadLoop(){
        int num_aux;
        int i;
        num_aux = 0;
        i = 0;
        while(i < 10000) {
            num_aux = num_aux + 0;
            num_aux = num_aux - 0;
            num_aux = 0 + num_aux;
            num_aux = num_aux * 1;
            num_aux = 1 * num_aux;
            i = i + 1;
        }
	return 1 ;
    }
}