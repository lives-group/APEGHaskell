# APEGLitleToy

This project purpuse is to serve as a toy for experiments with APEG interpreter and Type System implementations.
For now olny simple syntax extensions are permited and the APEG expression language is reduce to it's
simplest form allowing only string, map and language values, as weel as operations over these values. 


The MicroSugar example is functional and and can be executed with the `runMS :: FilePath -> IO ()` function.
To compile the Sample1.txt file just invoke the GHCi on MicroSugar Module and run 

`runMS "Sample1.txt"`

The result will be a tuple containing the value and type enviroments, the consumed string and the remaing sting.


