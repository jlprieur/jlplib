/**********************************************************************
* Routines used for minimizing non-linear problems
*
* Contains:
* jlp_minimize calls DCV_LBFGS or frprmn (Numerical recipees)
* frprmn with all routines needed
**********************************************************************/
#include "jlp_numeric.h"

/* In "../math/lbfgs_bcm/dcv_lbfgs.c" */
#define DCV_LBFGS RENAME_(dcv_lbfgs)
#define DCV_LBFGS_FULL RENAME_(dcv_lbfgs_full)
int DCV_LBFGS(int *npts, double *pp, int *positive, double *f,
              double *ftol, int *pp_dim, double (*func)(double *),
              void (*dfunc)(double *, double *));
int DCV_LBFGS_FULL(int *npts, double *pp, double *lbound, double *ubound,
                   int *nbound, double *f, double *ftol, int *pp_dim,
                   double (*func)(double *), 
                   void (*dfunc)(double *, double *));

/* Contained here */
int jlp_minimize(double *pp, int npts, int pp_dim, double ftol, int *iter,
                 int iter_max, double *fret, double (*func)(double *),
                 void (*dfunc)(double *, double *), int positive, int lbfgs);
static int frprmn(double pp[], int npts, int pp_dim, double ftol, int *iter, 
                  int iter_max, double *fret, double (*func)(double []), 
                  void (*dfunc)(double [], double []));

/**********************************************************************
* jlp_minimize
* to find the location of the minimum of a given function
*
* INPUT:
* pp: first guess of parameters 
*    (starting at index 1 to be compatible with Numerical Recipees)
* npts = 1 to n points used for minimisation (npts = pp_dim if all points)
* pp_dim = dimension of pp
* ftol = tolerance to exit from minimization loop
* *func: pointer to routine that computes the function value:
*        function_value = func(double pp[])
* *dfunc: pointer to routine that computes the gradient vector:
*        func(double pp[], double grad_vector[])
* positive: set to one if positive constraint
* lbfgs: flag set to one if LBFGS is wanted
*
* OUTPUT:
* pp: set of parameters corresponding to the minimum (starting at index 1!)
* iter: nber of iterations
* fret: value of minimum
*
**********************************************************************/
int jlp_minimize(double *pp, int npts, int pp_dim, double ftol, int *iter,
                 int iter_max, double *fret, double (*func)(double *),
                 void (*dfunc)(double *, double *), int positive, int lbfgs)
{
int i, istat;

if(lbfgs)
/* Transfer to "C" arrays starting at 0.
*
* WARNING: indices of pp array should belong to [1,n] ! 
* (to be compatible with Numerical Recipees) : */
  {
  for(i = 0; i < npts; i++) pp[i] = pp[i+1];
  istat = DCV_LBFGS(&npts, pp, &positive, fret, &ftol, &pp_dim, func, dfunc);
  for(i = npts-1; i > 0; i--) pp[i] = pp[i-1];
  *iter = 0;
  }
else
  istat = frprmn(pp, npts, pp_dim, ftol, iter, iter_max, fret, func, dfunc);

return(istat);
}

/***************************************************************************
********* Following part was derived from numerical recipees **************
***************************************************************************/

/*
#define NRANSI
#include "nrutil.h"
*/
#define FLOAT double

#define TOL 2.0e-4
#define EPS 1.0e-10
#define GOLD 1.618034
#define GLIMIT 100.0
#define TINY 1.0e-20
#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);
#define ITMAX_BRENT 100
#define CGOLD 0.3819660
#define ZEPS 1.0e-10
#define MOV3(a,b,c, d,e,f) (a)=(d);(b)=(e);(c)=(f);
#define MAXI(x,y) ((x) > (y) ? (x) : (y))
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))

/* JLP2002: to solve memory problems... */
static FLOAT *vector(int istart, int iend);
static void free_all(FLOAT *xi, FLOAT *h, FLOAT *g, FLOAT *xt,
                     FLOAT *dxt, FLOAT *pp0, FLOAT *xicom, FLOAT *pcom);

static int ncom;
static FLOAT *pcom, *xicom, *xt, *dxt;
static FLOAT (*nrfunc)(FLOAT []);
static void (*nrdfun)(FLOAT [], FLOAT []);

/* The routines linmin or dlinmin are called to perform line minimizations.
* define LINMIN if linmin is wanted, otherwise dlinmin will be used*/
/*
#define LINMIN
*/
#ifdef LINMIN
static void linmin(FLOAT p[], FLOAT xi[], int n, FLOAT *fret,
            FLOAT (*func)(FLOAT []));
static FLOAT brent(FLOAT ax, FLOAT bx, FLOAT cx, FLOAT (*f)(FLOAT),
                   FLOAT tol, FLOAT *xmin);
#else
static void dlinmin(FLOAT p[], FLOAT xi[], int n, FLOAT *fret,
                    FLOAT (*func)(FLOAT []), void (*dfunc)(FLOAT [], FLOAT []));
static FLOAT dbrent(FLOAT ax, FLOAT bx, FLOAT cx, FLOAT (*f)(FLOAT),
                    FLOAT (*df)(FLOAT), FLOAT tol, FLOAT *xmin);
#endif

static void mnbrak(FLOAT *ax, FLOAT *bx, FLOAT *cx, FLOAT *fa, FLOAT *fb, 
                   FLOAT *fc, FLOAT (*func)(FLOAT));
static FLOAT f1dim(FLOAT x);
static FLOAT df1dim(FLOAT x);

/*********************************************************
* Flechter-Reeves-Polak-Ribiere minimization
* Cf "Numerical recipees" Sect. 10.6 (in C p 423, in F77 p 416)
*
* INPUT:
* pp: first guess
* npts = 1 to npts points used for minimisation 
*        (NOTA: npts = dimension of p if all points)
* pp_dim = dimension of pp
* ftol = tolerance to exit from minimization loop
*
* OUTPUT:
* pp: set of parameters corresponding to the minimum of the function
* iter: nber of iterations that were performed
* fret: minimum value found for the function
*
* Given a starting point p that is a vector of length n
* Flechter-Reeves-Polak-Ribiere minimization is performed on
* a function func, using its gradient as calculated by a routine dfunc.
* The convergence tolerance on the function value is input as ftol.
* The routines linmin or dlinmin are called to perform line minimizations.
*********************************************************/
static int frprmn(FLOAT pp[], int npts, int pp_dim, FLOAT ftol, int *iter, 
                  int iter_max, FLOAT *fret, FLOAT (*func)(FLOAT []), 
                  void (*dfunc)(FLOAT [], FLOAT []))
{
int j, its;
FLOAT gg, gam, fp, dgg, crit_fp, crit_pp;
double sum;
FLOAT *g, *h, *xi, *pp0;

/* JLP2002: I add: 
* pp0: previous solution
*/
        pp0=vector(1,pp_dim);
        xt=vector(1,pp_dim);
        dxt=vector(1,pp_dim);
        pcom=vector(1,pp_dim);
        xicom=vector(1,pp_dim);
	g=vector(1,pp_dim);
	h=vector(1,pp_dim);
	xi=vector(1,pp_dim);
        for(j = 1; j <= pp_dim; j++) {
          pp0[j] = 0.;
          xt[j] = 0.;
          dxt[j] = 0.;
          pcom[j] = 0.;
          xicom[j] = 0.;
          g[j] = 0.;
          h[j] = 0.;
          xi[j] = 0.;
          }
/* When npts != pp_dim, set to zero all points between nn and pp_dim: */
        for(j = npts+1; j <= pp_dim; j++) pp[j] = 0.;

/* Initialize fp and xi with the values of the function and its gradient
* at the starting point of pp: 
*/
	fp=(*func)(pp);
	(*dfunc)(pp,xi);
	for (j=1; j<=npts; j++) {
		g[j] = -xi[j];
		xi[j]=h[j]=g[j];
	}

/* Main loop: */
	for (its=1; its <= iter_max; its++) {
		*iter=its;
/* Copy the previous solution: */
	    for (j=1; j<=npts; j++) pp0[j] = pp[j];
#ifdef LINMIN
		linmin(pp,xi,npts,fret,func);
#else
		dlinmin(pp,xi,npts,fret,func,dfunc);
#endif
/* Distance to the previous solution measured with crit_pp 
* (JLP 2002 and 2006): */
            sum = 0.;
	    for (j = 1; j <= npts; j++) 
                sum += SQUARE(pp0[j] - pp[j])/ (SQUARE(pp0[j]) + EPS);
            crit_pp = sum; 
/* Next statement is normal return:
! Exit criterium is 2 * |fret-fp| < ftol * (|fret| + |fp| + eps)
! i.e. relative variation of fp is small:
! 2 * |fret-fp| / (|fret| + |fp| + eps) < ftol
!  which is nearly: |fret-fp| / |fret| < ftol
!        if(2.*abs(fret-fp).le.ftol*(abs(fret)+abs(fp)+eps))return
! JLP 2001
! I also put a constraint on the variations of pp
*/
                crit_fp = 2.0*fabs(*fret-fp) / (fabs(*fret)+fabs(fp)+EPS);
                printf("frprmn/iter=%d crit_pp=%e, crit_fp=%e sumsq=%e\n", 
                        *iter, crit_pp, crit_fp, *fret);
		if ((crit_fp <= ftol) || (crit_pp <= ftol)) {
			free_all(xi, h, g, xt, dxt, pp0, xicom, pcom);
			return(0);
		}
		fp=(*func)(pp);
		(*dfunc)(pp,xi);
		dgg=gg=0.0;
		for (j=1;j<=npts;j++) {
			gg += g[j]*g[j];
/*
! Flechter-Reeves:
!         dgg=dgg+xi(j)**2
! Polak-Ribiere: (better)
!         dgg=dgg+(xi(j)+g(j))*xi(j)
*/
			dgg += (xi[j]+g[j])*xi[j];
		}
/* Unlikely: if gradient is exactly zero, then we are already done.
*/
		if (gg == 0.0) {
			free_all(xi, h, g, xt, dxt, pp0, xicom, pcom);
			return(0);
		}
		gam=dgg/gg;
		for (j=1;j<=npts;j++) {
			g[j] = -xi[j];
			xi[j]=h[j]=g[j]+gam*h[j];
		}
	}
printf("frprmn/Fatal: too many iterations: iter=%d ftol=%e\n", 
        *iter, ftol);
free_all(xi, h, g, xt, dxt, pp0, xicom, pcom);
return(-1);
}

/*********************************************************
*
*********************************************************/
static FLOAT df1dim(FLOAT x)
{
	int j;
	FLOAT df1=0.0;
	for (j=1;j<=ncom;j++) xt[j]=pcom[j]+x*xicom[j];
	(*nrdfun)(xt,dxt);
	for (j=1;j<=ncom;j++) df1 += dxt[j]*xicom[j];
	return df1;
}

/*********************************************************
*
*********************************************************/
static FLOAT f1dim(FLOAT x)
{
	int j;
	FLOAT f;

	for (j=1;j<=ncom;j++) xt[j]=pcom[j]+x*xicom[j];
	f=(*nrfunc)(xt);
	return f;
}
/*********************************************************
*
*********************************************************/
static void mnbrak(FLOAT *ax, FLOAT *bx, FLOAT *cx, FLOAT *fa, FLOAT *fb, 
                   FLOAT *fc, FLOAT (*func)(FLOAT))
{
	FLOAT ulim,u,r,q,fu,dum;

	*fa=(*func)(*ax);
	*fb=(*func)(*bx);
	if (*fb > *fa) {
		SHFT(dum,*ax,*bx,dum)
		SHFT(dum,*fb,*fa,dum)
	}
	*cx=(*bx)+GOLD*(*bx-*ax);
	*fc=(*func)(*cx);
	while (*fb > *fc) {
		r=(*bx-*ax)*(*fb-*fc);
		q=(*bx-*cx)*(*fb-*fa);
		u=(*bx)-((*bx-*cx)*q-(*bx-*ax)*r)/
			(2.0*SIGN(MAXI(fabs(q-r),TINY),q-r));
		ulim=(*bx)+GLIMIT*(*cx-*bx);
		if ((*bx-u)*(u-*cx) > 0.0) {
			fu=(*func)(u);
			if (fu < *fc) {
				*ax=(*bx);
				*bx=u;
				*fa=(*fb);
				*fb=fu;
				return;
			} else if (fu > *fb) {
				*cx=u;
				*fc=fu;
				return;
			}
			u=(*cx)+GOLD*(*cx-*bx);
			fu=(*func)(u);
		} else if ((*cx-u)*(u-ulim) > 0.0) {
			fu=(*func)(u);
			if (fu < *fc) {
				SHFT(*bx,*cx,u,*cx+GOLD*(*cx-*bx))
				SHFT(*fb,*fc,fu,(*func)(u))
			}
		} else if ((u-ulim)*(ulim-*cx) >= 0.0) {
			u=ulim;
			fu=(*func)(u);
		} else {
			u=(*cx)+GOLD*(*cx-*bx);
			fu=(*func)(u);
		}
		SHFT(*ax,*bx,*cx,u)
		SHFT(*fa,*fb,*fc,fu)
	}
}
/********************************************************************
* Minimization along a line
*********************************************************************/
#ifdef LINMIN
static void linmin(FLOAT pp[], FLOAT xi[], int npts, FLOAT *fret, 
                   FLOAT (*func)(FLOAT []))
{
int j;
FLOAT xx,xmin,fx,fb,fa,bx,ax;

  ncom=npts;
  nrfunc=func;
  for (j=1;j<=npts;j++) {
	pcom[j]=pp[j];
	xicom[j]=xi[j];
  }
  ax=0.0;
  xx=1.0;
  mnbrak(&ax,&xx,&bx,&fa,&fx,&fb,f1dim);
  *fret=brent(ax,xx,bx,f1dim,TOL,&xmin);
  for (j=1;j<=npts;j++) {
	xi[j] *= xmin;
	pp[j] += xi[j];
  }
return;
}
/**********************************************************
* Inverse parabolic interpolation for one-dimension minimization
* (Brent's method, p 402) 
***********************************************************/
static FLOAT brent(FLOAT ax, FLOAT bx, FLOAT cx, FLOAT (*f)(FLOAT),
                   FLOAT tol, FLOAT *xmin)
{
	int iter;
	FLOAT a,b,d,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm;
        FLOAT e=0.0;

/* a and b must be in ascending order: */
	a=(ax < cx ? ax : cx);
	b=(ax > cx ? ax : cx);
	x=w=v=bx;
	fw=fv=fx=(*f)(x);
	for (iter=1; iter<=ITMAX_BRENT; iter++) {
		xm=0.5*(a+b);
		tol1=tol*fabs(x)+ZEPS;
		tol2=2.0*tol1;
/* Exit when criterium has been reached: */
		if (fabs(x-xm) <= (tol2-0.5*(b-a))) {
			*xmin=x;
			return fx;
		}
/* Construct a trial parabolic fit: */
		if (fabs(e) > tol1) {
			r = (x - w) * (fx - fv);
			q = (x - v) * (fx - fw);
                        p = (x - v) * q - (x - w) * r; 
                        q = 2.0 * (q - r);
                        if (q > 0.0) p = -p;
                        q = fabs(q);
			etemp=e;
			e=d;
/* conditions which determine the acceptability of the parabolic fit: */
			if(fabs(p) >= fabs(0.5 * q * etemp)
                           || p <= q * (a - x) || p >= q * (b - x)) {
                             e = (x >= xm ? a-x : b-x);
                             d = CGOLD * e;
                           }
                         else {
                             d = p / q;
                             u = x + d;
                             if(u - a < tol2 || b - u < tol2)
                                d = SIGN(tol1, xm - x); 
                           }
		} else {
                   e = (x >= xm ? a-x : b-x);
                   d = CGOLD * e;
		}
                u = (fabs(d) >= tol1 ? (x + d) : (x + SIGN(tol1, d)));
/* This is the one function evaluation per iteration: */
                fu = (*f)(u);
                if (fu <= fx) {
                  if (u >= x) a = x; else b = x;
                  SHFT(v, w, x, u);
                  SHFT(fv, fw, fx, fu);
                } else {
                  if (u < x) a = u; else b = u;
                  if(fu <= fw || w == x) {
                     v = w;
                     w = u;
                     fv = fw;
                     fw = fu;
                  } else if (fu <= fv || v == x || v == w) {
                     v = u;
                     fv = fu;
                  }
                }
	}
	printf("brent/Fatal: too many iterations\n");
        exit(-1);
/*
        *xmin = x;
	return fx;
*/
}
// ifndef LINMIN:
#else
/********************************************************************
* line minimization
*********************************************************************/
static void dlinmin(FLOAT pp[], FLOAT xi[], int npts, FLOAT *fret, 
                    FLOAT (*func)(FLOAT []), void (*dfunc)(FLOAT [], FLOAT []))
{
	int j;
	FLOAT xx,xmin,fx,fb,fa,bx,ax;

	ncom=npts;
	nrfunc=func;
	nrdfun=dfunc;
	for (j=1;j<=npts;j++) {
		pcom[j]=pp[j];
		xicom[j]=xi[j];
	}
	ax=0.0;
	xx=1.0;
	mnbrak(&ax,&xx,&bx,&fa,&fx,&fb,f1dim);
	*fret=dbrent(ax,xx,bx,f1dim,df1dim,TOL,&xmin);
	for (j=1;j<=npts;j++) {
		xi[j] *= xmin;
		pp[j] += xi[j];
	}
return;
}
/**********************************************************
* Inverse parabolic interpolation for one-dimension minimization
* (Brent's method with first derivative, p 405) 
***********************************************************/
static FLOAT dbrent(FLOAT ax, FLOAT bx, FLOAT cx, FLOAT (*f)(FLOAT),
                    FLOAT (*df)(FLOAT), FLOAT tol, FLOAT *xmin)
{
	int iter,ok1,ok2;
	FLOAT a,b,d,d1,d2,du,dv,dw,dx,e=0.0;
	FLOAT fu,fv,fw,fx,olde,tol1,tol2,u,u1,u2,v,w,x,xm;

	a=(ax < cx ? ax : cx);
	b=(ax > cx ? ax : cx);
	x=w=v=bx;
	fw=fv=fx=(*f)(x);
	dw=dv=dx=(*df)(x);
	for (iter=1;iter<=ITMAX_BRENT;iter++) {
		xm=0.5*(a+b);
		tol1=tol*fabs(x)+ZEPS;
		tol2=2.0*tol1;
		if (fabs(x-xm) <= (tol2-0.5*(b-a))) {
			*xmin=x;
			return fx;
		}
		if (fabs(e) > tol1) {
			d1=2.0*(b-a);
			d2=d1;
			if (dw != dx) d1=(w-x)*dx/(dx-dw);
			if (dv != dx) d2=(v-x)*dx/(dx-dv);
			u1=x+d1;
			u2=x+d2;
			ok1 = (a-u1)*(u1-b) > 0.0 && dx*d1 <= 0.0;
			ok2 = (a-u2)*(u2-b) > 0.0 && dx*d2 <= 0.0;
			olde=e;
			e=d;
			if (ok1 || ok2) {
				if (ok1 && ok2)
					d=(fabs(d1) < fabs(d2) ? d1 : d2);
				else if (ok1)
					d=d1;
				else
					d=d2;
				if (fabs(d) <= fabs(0.5*olde)) {
					u=x+d;
					if (u-a < tol2 || b-u < tol2)
						d=SIGN(tol1,xm-x);
				} else {
					d=0.5*(e=(dx >= 0.0 ? a-x : b-x));
				}
			} else {
				d=0.5*(e=(dx >= 0.0 ? a-x : b-x));
			}
		} else {
			d=0.5*(e=(dx >= 0.0 ? a-x : b-x));
		}
		if (fabs(d) >= tol1) {
			u=x+d;
			fu=(*f)(u);
		} else {
			u=x+SIGN(tol1,d);
			fu=(*f)(u);
			if (fu > fx) {
				*xmin=x;
				return fx;
			}
		}
		du=(*df)(u);
		if (fu <= fx) {
			if (u >= x) a=x; else b=x;
			MOV3(v,fv,dv, w,fw,dw)
			MOV3(w,fw,dw, x,fx,dx)
			MOV3(x,fx,dx, u,fu,du)
		} else {
			if (u < x) a=u; else b=u;
			if (fu <= fw || w == x) {
				MOV3(v,fv,dv, w,fw,dw)
				MOV3(w,fw,dw, u,fu,du)
			} else if (fu < fv || v == x || v == w) {
				MOV3(v,fv,dv, u,fu,du)
			}
		}
	}
	printf("dbrent/Fatal: too many iterations\n");
        exit(-1);
}
#endif  //ifndef LINMIN
/***********************************************************
* Routine to replace "vector" of nrutil.c
*
***********************************************************/
static FLOAT *vector(int istart, int iend)
{
FLOAT *pntr;

pntr = (FLOAT *) malloc( (iend-istart+2+2000) * sizeof(FLOAT));
if(pntr == NULL)
  {
  printf("vector/Fatal error allocating memory space: istart=%d iend=%d\n",
         istart, iend);
  exit(-1);
  }
return(pntr);
}
/***********************************************************
* Routine to replace "free_vector" of nrutil.c
*
***********************************************************/
static void free_all(FLOAT *xi, FLOAT *h, FLOAT *g, FLOAT *xt,
                     FLOAT *dxt, FLOAT *pp0, FLOAT *xicom, FLOAT *pcom)
{
free(xi);
free(h);
free(g);
free(xt);
free(dxt);
free(pp0); 
free(xicom); 
free(pcom);
}
#undef ITMAX_BRENT
#undef ZEPS
#undef MOV3
#undef TOL
#undef EPS
#undef NRANSI
#undef GOLD
#undef GLIMIT
#undef TINY
#undef SHFT
#undef NRANSI
/* (C) Copr. 1986-92 Numerical Recipes Software *$!$!6)$. 
(Modified by JLP) */
