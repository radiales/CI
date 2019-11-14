#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define FAIL -1
#define OK 0
#define MAXKEYWORDCLASSLENGTH 3
#define NOKEYWORD -1

static char zvek[] = {
/*         0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
/*0 */     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
/*10*/     7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
/*20*/     7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/*30*/     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
/*40*/     0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
/*50*/     2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0,
/*60*/     0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
/*70*/     2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0
};

static char* keyWords[] =
        {
                "BEGIN",    "CALL",     "CONST",    "DO",   "END",  "IF",
                "ODD",      "PROCEDURE","THEN",     "VAR",  "WHILE"
        };

static int keywordClasses[][MAXKEYWORDCLASSLENGTH] =
        {
/*0*/    {-1, -1, -1},
/*1*/   {-1, -1, -1},
/*2*/   { 5,  3, -1},
/*3*/   { 4,  6,  9},
/*4*/   { 1,  8, -1},
/*5*/   { 0,  2, 10},
/*6*/   {-1, -1, -1},
/*7*/   {-1, -1, -1},
/*8*/   {-1, -1, -1},
/*9*/   { 7, -1, -1}
        };

static void        fl          (void);
static void        fb          (void);
static void        fgl         (void);
static void        fsl         (void);
static void        fslb        (void);

typedef void (*FX)(void);

static FX vfx[] = {fl, fb, fgl, fsl, fslb};

typedef enum T_Fx {ifl=0x0, ifb=0x10, ifgl=0x20, ifsl=0x30, ifslb=0x40} tFx;
typedef enum T_MC{mcEmpty, mcSymb, mcNum, mcIdent, mEOF} tMC;
typedef enum T_ZS{
    zNIL,
    zERG = 128, zLE, zGE,
    zBGN, zCLL, zCST, zDO, zEND, zIF, zODD, zPRC, zTHN, zVAR, zWHL
}tZS;

typedef struct{
    tMC MC;                     //Morphemcode
    int PosLine;                //Line
    int PosCol;                 //Column
    union VAL{
        long Num;
        char* pStr;
        int Symb;
    }Val;
    int MLen;

} tMorph;

int initLex(char* fname);
tMorph* Lex(void);


/*
 * Z0: Anfangszustand
 * Z1: Buchstabe
 * Z2: Ziffer
 * Z3: Doppelpunkt
 * Z4: <
 * Z5: >
 * Z6: Folgezustand Doppelpunkt
 * Z7: Folgezustand <
 * Z8: Folgezustand >
*/

static char flgZstMatrix[][8] =
        {
/*       So  Zi  Bu  :  =  <  >  Space */
/* 0 */  0,  2,  1,  3, 0, 4, 5, 0,
/* 1 */  0,  2,  1,  0, 0, 0, 0, 0,
/* 2 */  0,  2,  0,  0, 0, 0, 0, 0,
/* 3 */  0,  0,  0,  0, 6, 0, 0, 0,
/* 4 */  0,  0,  0,  0, 7, 0, 0, 0,
/* 5 */  0,  0,  0,  0, 8, 0, 0, 0,
/* 6 */  0,  0,  0,  0, 0, 0, 0, 0,
/* 7 */  0,  0,  0,  0, 0, 0, 0, 0,
/* 8 */  0,  0,  0,  0, 0, 0, 0, 0,
        };

//static FX vfx[] = {fl = 0, fb = 1, fgl = 2, fsl = 3, fslb = 4};

static char vSMatrix[][8]=
/*      So Zi Bu :  =  <  >  Space
/*--------------------------------------------*/
/* 0 */{4, 3, 2, 3, 0, 4, 4, 0,
/* 1 */ 0, 3, 2, 0, 0, 0, 0, 0,
/* 2 */ 0, 3, 0, 0, 0, 0, 0, 0,
/* 3 */ 0, 0, 0, 0, 3, 0, 0, 0,
/* 4 */ 0, 0, 0, 0, 3, 0, 0, 0,
/* 5 */ 0, 0, 0, 0, 3, 0, 0, 0,
/* 6 */ 0, 0, 0, 0, 0, 0, 0, 0,
/* 7 */ 0, 0, 0, 0, 0, 0, 0, 0,
/* 8 */ 0, 0, 0, 0, 0, 0, 0, 0
        };

//USED TO BE CHAR
static int vFMatrix[][8]=
/*        So      Zi      Bu       :                =      '<'     '>'      Space
/*--------------------------------------------------------------*/
/* 0 */{ifslb   ,ifsl   ,ifgl       ,ifsl       ,ifslb      ,ifsl ,ifsl     ,ifb,
/* 1 */ ifb     ,ifsl   ,ifgl       ,ifb        ,ifb        ,ifb  ,ifb      ,ifb,
/* 2 */ ifb     ,ifsl   ,ifb        ,ifb        ,ifb        ,ifb  ,ifb      ,ifb,
/* 3 */ ifb     ,ifb    ,ifb        ,ifb        ,ifsl       ,ifb  ,ifb      ,ifb,
/* 4 */ ifb     ,ifb    ,ifb        ,ifb        ,ifsl       ,ifb  ,ifb      ,ifb,
/* 5 */ ifb     ,ifb    ,ifb        ,ifb        ,ifsl       ,ifb  ,ifb      ,ifb,
/* 6 */ ifb     ,ifb    ,ifb        ,ifb        ,ifb        ,ifb  ,ifb      ,ifb,
/* 7 */ ifb     ,ifb    ,ifb        ,ifb        ,ifb        ,ifb  ,ifb      ,ifb,
/* 8 */ ifb     ,ifb    ,ifb        ,ifb        ,ifb        ,ifb  ,ifb      ,ifb
        };

static FILE* pIF;
static tMorph MorphInit;
tMorph Morph;
static int X, Z, lastZ;
static char vBuf[1024+1];
static char* pBuf;
static int line, col;

int initLex(char* fname){
    char vName[128+1];

    Z=0;

    strcpy(vName, fname);
    if(strstr(vName, ".pl0")==NULL) strcat(vName, ".pl0");

    pIF = fopen(vName, "r+t");
    if(pIF!=NULL) {X=fgetc(pIF); return OK;}
    return FAIL;
}

tMorph* Lex(void){
    int zx;
    Z = lastZ = 0;
    Morph = MorphInit;
    Morph.PosLine = line;
    Morph.PosCol = col;
    Morph.MLen = 0;
    pBuf = vBuf;

    //Check for EOF (temporary)
    if(X == EOF){
        Morph.Val.Symb = mEOF;
        return &Morph;
    }

    do{
        printf("Zeichen: %c\n", X);

        zx = vSMatrix[Z][zvek[X]];
        vfx[zx]();
        lastZ = Z;
        Z = flgZstMatrix[Z][zvek[X]];
        //Z = zx;
    }while(Z != 0);

    fb();

    return &Morph;
}

/*---- lesen ----*/
static void fl  (void){
    X=fgetc(pIF);
    if (X=='\n') line++,col=0;
    else col++;
}

/*---- schreiben als Grossbuchstabe, lesen ----*/
static void fgl (void){
    *pBuf=(char)toupper(X);// oder  *Buf=(char)X&0xdf;
    *(++pBuf)=0;
    Morph.MLen++;
    fl();
}

/*---- schreiben, lesen ----*/
static void fsl (void){
    *pBuf=(char)X;
    *(++pBuf)=0;
    fl();
}

/*---- schreiben, lesen, beenden ----*/
static void fslb(void){
    fsl();fb();
}

static void fb  (){
    int i,j;
    switch (lastZ){
        //Identifier
        case 1:
            Morph.MC = mcSymb;
            if(Morph.Val.pStr == NULL) Morph.Val.pStr = vBuf;
            break;
            // Doppelpunkt
        case 3:
            Morph.Val.Symb = vBuf[0];
            Morph.MC = mcSymb;
            break;
            //Kleiner
        case 4:
            Morph.Val.Symb = vBuf[0];
            Morph.MC = mcSymb;
            break;
            //Groesser
        case 5:
            Morph.Val.Symb = vBuf[0];
            Morph.MC = mcSymb;
            break;
        case 0:
            Morph.Val.Symb=vBuf[0];
            Morph.MC =mcSymb;
            break;
            //Zahl
        case 2: Morph.Val.Num=atol(vBuf);
            Morph.MC =mcNum;
            break;
            //Ergibtzeichen
        case 6: Morph.Val.Symb=(long)zERG;
            Morph.MC =mcSymb;
            break;
            //KleinerGleich
        case 7: Morph.Val.Symb=(long)zLE;
            Morph.MC =mcSymb;
            break;
            //GroesserGleich
        case 8: Morph.Val.Symb=(long)zGE;
            Morph.MC =mcSymb;
            break;
    }
    Z = 0;
}

int checkKeyword(tMorph m){
    int c, idx;
    for(c = 0; c < MAXKEYWORDCLASSLENGTH; c++){
        if((idx = keywordClasses[m.MLen][c]) > 0 && !strcmp(m.Val.pStr, keyWords[idx])){
            return idx;
        }
    }
    return NOKEYWORD;
}

int main(int argc, char* argv[]){

    printf("Running Lexer.\n");

    tMorph* tmp;
    int debugCounter = 0;

    printf("%s\n", (initLex(argv[1]))?"InitLex failed.\n":"");

    //DEBUG, DELETE!
    tMorph* morphs[256];

    do{
        tmp = Lex();

        //DEBUG, DELETE!
        morphs[debugCounter] = tmp;

        if(tmp != NULL &&(*tmp).Val.pStr != NULL){
            //...
        }
        debugCounter++;
    } while((tmp == NULL || tmp->Val.Symb != mEOF) && debugCounter <= 150);

    if(debugCounter == 50)
        printf("Exceeded debug counter.\n");

/*
    tMorph testM;
    char testKeyword[] = "PROCEDURE";
    testM.Val.pStr = testKeyword;
    testM.MLen = 9;

    char* res = keyWords[checkKeyword(testM)];
*/
    return 0;
}

