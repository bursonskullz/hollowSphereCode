# MaterialModelDriver

## Description
This project contains the `MaterialModelDriver` executable, which is used to process data for the hollowSPhere problem

## Instructions before compiling 
Please change the following path to the path your script is located (or data files you read in) 

    character(*), parameter :: path1 = 'C:/Users/Roy Burson/Documents/Math research/Resarch on Ductile Rapture/'
    character(*), parameter :: path2 = 'HS-GLPD/glpdMaster/Data/' 
    character(*), parameter :: path = path1//path2

## Installation
To compile the project, run the following command:
```sh
make
./MaterialModelDriver.exe
