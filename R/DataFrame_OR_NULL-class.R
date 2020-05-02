### =========================================================================
### The DataFrame_OR_NULL class
### -------------------------------------------------------------------------

### At this point the DataFrame class is not defined yet so we cannot
### include it in the DataFrame_OR_NULL union. We'll add it later with
### setIs() (see DataFrame-class.R).
### The reason we need to define DataFrame_OR_NULL so early (i.e. before
### DataFrame) is because we use it in the definition of the Vector class
### (for the specification of the elementMetadata slot) so it needs to be
### defined **before** Vector. However DataFrame extends Vector (via
### SimpleList and List) so needs to be defined **after** Vector.

setClassUnion("DataFrame_OR_NULL", "NULL")

