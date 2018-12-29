class Factorial {
    public static void main(String[] a){
	System.out.println(new Fac().Init(10) + 10);
    }
}

class Fac {
    int[] number ;
    int size ;
    
    // Print array of integers
    public int Print(){
	int j ;

	j = 0 ;
	while (j < (size)) {
	    System.out.println(number[j]);
	    j = j + 1 ;
	}
	return 0 ;
    }
    
    
    public int Init(int sz){
	int j ;
	int k ;
	int aux01 ;
	int aux02 ;

	size = sz ;
	number = new int[sz] ;
	
	j = 0 ;
	k = size + 1 ;
	while (j < (size)) {
	    number[j] = j ;
	    j = j + 1 ;
	    k = k - 1 ;
	}
        
	return this.Print() ;	
    }
}