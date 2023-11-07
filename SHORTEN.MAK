# Microsoft Visual C++ generated build script - Do not modify

PROJ = SHORTEN
DEBUG = 0
PROGTYPE = 6
CALLER = 
ARGS = 
DLLS = 
D_RCDEFINES = -d_DEBUG
R_RCDEFINES = -dNDEBUG
ORIGIN = MSVC
ORIGIN_VER = 1.00
PROJPATH = D:\SOFTSO~1\SHORTEN\SHORTE~1.3\
USEMFC = 0
CC = cl
CPP = cl
CXX = cl
CCREATEPCHFLAG = 
CPPCREATEPCHFLAG = 
CUSEPCHFLAG = 
CPPUSEPCHFLAG = 
FIRSTC = ARRAY.C     
FIRSTCPP =             
RC = rc
CFLAGS_D_DEXE = /nologo /G2 /W3 /Zi /AM /Od /D "_DEBUG" /D "_DOS" /FR /Fd"SHORTEN.PDB"
CFLAGS_R_DEXE = /nologo /Gs /G3 /W3 /AM /O2 /Ox /D "NDEBUG" /D "_DOS" /D "MSDOS" /FR 
LFLAGS_D_DEXE = /NOLOGO /ONERROR:NOEXE /NOI /CO /STACK:5120
LFLAGS_R_DEXE = /NOLOGO /ONERROR:NOEXE /NOI /STACK:5120
LIBS_D_DEXE = oldnames mlibce
LIBS_R_DEXE = oldnames mlibce
RCFLAGS = /nologo
RESFLAGS = /nologo
RUNFLAGS = 
OBJS_EXT = 
LIBS_EXT = 
!if "$(DEBUG)" == "1"
CFLAGS = $(CFLAGS_D_DEXE)
LFLAGS = $(LFLAGS_D_DEXE)
LIBS = $(LIBS_D_DEXE)
MAPFILE = nul
RCDEFINES = $(D_RCDEFINES)
!else
CFLAGS = $(CFLAGS_R_DEXE)
LFLAGS = $(LFLAGS_R_DEXE)
LIBS = $(LIBS_R_DEXE)
MAPFILE = nul
RCDEFINES = $(R_RCDEFINES)
!endif
!if [if exist MSVC.BND del MSVC.BND]
!endif
SBRS = ARRAY.SBR \
		DUPFINFO.SBR \
		EXIT.SBR \
		HSGETOPT.SBR \
		LPC.SBR \
		MAIN.SBR \
		POLY.SBR \
		SHORTEN.SBR \
		SULAWALA.SBR \
		VARIO.SBR \
		FIXIO.SBR \
		LICENSE.SBR \
		RIFFWAVE.SBR


ARRAY_DEP = d:\softso~1\shorten\shorte~1.3\shorten.h


DUPFINFO_DEP = d:\softso~1\shorten\shorte~1.3\shorten.h


EXIT_DEP = d:\softso~1\shorten\shorte~1.3\shorten.h


HSGETOPT_DEP = d:\softso~1\shorten\shorte~1.3\shorten.h


LPC_DEP = d:\softso~1\shorten\shorte~1.3\shorten.h


MAIN_DEP = d:\softso~1\shorten\shorte~1.3\shorten.h


POLY_DEP = d:\softso~1\shorten\shorte~1.3\shorten.h


SHORTEN_DEP = d:\softso~1\shorten\shorte~1.3\shorten.h


SULAWALA_DEP = 

VARIO_DEP = d:\softso~1\shorten\shorte~1.3\shorten.h


FIXIO_DEP = d:\softso~1\shorten\shorte~1.3\shorten.h \
	d:\softso~1\shorten\shorte~1.3\bitshift.h


LICENSE_DEP = 

RIFFWAVE_DEP = d:\softso~1\shorten\shorte~1.3\shorten.h


all:	$(PROJ).EXE $(PROJ).BSC

ARRAY.OBJ:	ARRAY.C $(ARRAY_DEP)
	$(CC) $(CFLAGS) $(CCREATEPCHFLAG) /c ARRAY.C

DUPFINFO.OBJ:	DUPFINFO.C $(DUPFINFO_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c DUPFINFO.C

EXIT.OBJ:	EXIT.C $(EXIT_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c EXIT.C

HSGETOPT.OBJ:	HSGETOPT.C $(HSGETOPT_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c HSGETOPT.C

LPC.OBJ:	LPC.C $(LPC_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LPC.C

MAIN.OBJ:	MAIN.C $(MAIN_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c MAIN.C

POLY.OBJ:	POLY.C $(POLY_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c POLY.C

SHORTEN.OBJ:	SHORTEN.C $(SHORTEN_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c SHORTEN.C

SULAWALA.OBJ:	SULAWALA.C $(SULAWALA_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c SULAWALA.C

VARIO.OBJ:	VARIO.C $(VARIO_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c VARIO.C

FIXIO.OBJ:	FIXIO.C $(FIXIO_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c FIXIO.C

LICENSE.OBJ:	LICENSE.C $(LICENSE_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c LICENSE.C

RIFFWAVE.OBJ:	RIFFWAVE.C $(RIFFWAVE_DEP)
	$(CC) $(CFLAGS) $(CUSEPCHFLAG) /c RIFFWAVE.C

$(PROJ).EXE::	ARRAY.OBJ DUPFINFO.OBJ EXIT.OBJ HSGETOPT.OBJ LPC.OBJ MAIN.OBJ POLY.OBJ \
	SHORTEN.OBJ SULAWALA.OBJ VARIO.OBJ FIXIO.OBJ LICENSE.OBJ RIFFWAVE.OBJ $(OBJS_EXT) $(DEFFILE)
	echo >NUL @<<$(PROJ).CRF
ARRAY.OBJ +
DUPFINFO.OBJ +
EXIT.OBJ +
HSGETOPT.OBJ +
LPC.OBJ +
MAIN.OBJ +
POLY.OBJ +
SHORTEN.OBJ +
SULAWALA.OBJ +
VARIO.OBJ +
FIXIO.OBJ +
LICENSE.OBJ +
RIFFWAVE.OBJ +
$(OBJS_EXT)
$(PROJ).EXE
$(MAPFILE)
f:\msvc\lib\+
f:\msvc\mfc\lib\+
$(LIBS)
$(DEFFILE);
<<
	link $(LFLAGS) @$(PROJ).CRF

run: $(PROJ).EXE
	$(PROJ) $(RUNFLAGS)


$(PROJ).BSC: $(SBRS)
	bscmake @<<
/o$@ $(SBRS)
<<
