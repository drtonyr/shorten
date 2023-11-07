/******************************************************************************
*                                                                             *
*       Copyright (C) 1992-1995 Tony Robinson                                 *
*                                                                             *
*       See the file LICENSE for conditions on distribution and usage         *
*                                                                             *
******************************************************************************/

# include <stdio.h>
# include <stdlib.h>
# include "shorten.h"

int main(argc, argv) int argc; char **argv; {
  return(shorten(stdin, stdout, argc, argv));
}
