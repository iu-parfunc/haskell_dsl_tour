
Obsidian
======== 



Key aspects of implementation
-----------------------------

  * Small core: The Exp and Program GADTs, the AST (or deeply embedded part) 
    of Obsidian is relatively small.  
  
  * Arrays are implemented as a library on top of deep embedding. 
    pull arrays: 
     data Pull s a = Pull s (W32 -> a)  
     data Push t s a = Push s ((W32 -> a -> Program Thread ()) -> Program t ())
    
    The shallow/deep approach. 
  
   
  * In a sense the current approach to "how to program the hierarchy" 
    is also just a library on top of the deep embedding. 
    The deep embedded (Program type) is restricted to describe programs 
    that we can compile to CUDA. But the currently used API exposed to the 
    user can be changed if we like. 

  * Simple (naive) monad reification (Observe structure of monad program). 
    Bjorn & Benny reification. 


The thoughts behind Obsidian
---------------------------- 
  
  * Started out as the question: can a lava-style of programming 
    be applied to GPUs. 

    Originally: Write a high-level program in lava style and 
    automatically decompose it to fit the GPU 
    In order to do that we must first understand how to program GPUs. So 
    let us expose all the GPU low-level details needed for performance. 
    (This is where we are still at). 
   
  * Implementation-wise I was long reluctant to use too many "extensions" 
    and tried to do as much as possible in as simple as possible Haskell. 
    Over the years, even I learned (to some degree) how to use some of the 
    fancier aspects of Haskell and they crept in. 

  
			     
  


