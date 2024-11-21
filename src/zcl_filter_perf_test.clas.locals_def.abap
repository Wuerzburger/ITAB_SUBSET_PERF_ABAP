*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
types: BEGIN OF ts_small
     , element1 type i
     , element2 type char40
     , element3 type char1
     , element4 type numc5
     , end of ts_small.

 types tt_small type SORTED TABLE OF ts_small WITH NON-UNIQUE key element3.
