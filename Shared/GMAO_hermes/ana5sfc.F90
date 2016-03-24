#ifdef PROTEX
!BOI

!  !TITLE: Calculation of Surface Analysis Diagnostics in GEOS-5 DAS

!  !AUTHORS: Arlindo da Silva

!  !AFFILIATION: Global Modeling and Assimilation Office, NASA/GSFC, Greenbelt, MD 20771

!  !DATE: July 2005, Revised September 2005

!  !INTRODUCTION: Algorithm Overview


This document describes the calculation of {\em after analysis} near
surface parameters in GEOS-5 DAS. This algorithm is implemented in a
command line utility called {\tt ana5sfc.x}, and produces the
so-called {\tt ana.sfc} files. More specifically, given upper-air
analysis and background fields ($u, v, \theta, q$) and background near
surface instantaneous diagnostics ($u_{\mbox{10m}}$, $v_{\mbox{10m}}$,
etc.), {\em after analysis} of the following diagnostics are produced: 
\begin{itemize}
        \item temperature at 2m and 10m
        \item specific humidity at 2m and 10m
        \item wind at 2m and 10m
        \item wind stress
       \item  sensible and latent hear fluxes
       \item total column water vapor, ozone and cloud condensate
 \end{itemize}
The computation of near surface meteorology is described in 
section~\ref{sec:met}, while the estimation of surface fluxes of heat
and momentum is described in section~\ref{sec:fluxes}. The running of
utility {\tt ana5sfc.x} is discussed in section~\ref{sec:util}.

%....................................................................

\subsection{Notation}
%           ---------
\label{sec:notation}

\begin{tabular}{cl}

$u_{\mbox{2m}}$ & zonal wind component at 2 m above the surface\\
$v_{\mbox{2m}}$ & zonal wind component at 2 m above the surface\\
$\theta_{\mbox{2m}}$ & potential temperature at 2 m above the surface\\
$T_{\mbox{2m}}$ & temperature at 2 m above the surface\\
${q_V}_{\mbox{2m}}$ & specific humidity at 2 m above the surface\\
\mbox{}\\
$u_{\mbox{10m}}$ & zonal wind component at 10 m above the surface\\
$v_{\mbox{10m}}$ & zonal wind component at 10 m above the surface\\
$\theta_{\mbox{10m}}$ & potential temperature at 10 m above the surface\\
$T_{\mbox{10m}}$ & temperature at 10 m above the surface\\
${q_V}_{\mbox{10m}}$ & specific humidity at 10 m above the surface\\
\mbox{}\\
$u_{\mbox{b}}$ & zonal wind component at the model bottom layer\\
$v_{\mbox{b}}$ & zonal wind component at the model bottom layer\\
$\theta_{\mbox{b}}$ & potential temperature at the model bottom layer\\
$T_{\mbox{b}}$ & temperature at the model bottom layer\\
${q_V}_{\mbox{b}}$ & specific humidity at the model bottom layer\\
\mbox{}\\
$\tau_x$ & eastward wind stress \\
$\tau_y$ & northward wind stress \\
$Q_L$    & latent heat flux \\
$Q_S$    & sensible heat flux \\
\mbox{}\\
$G$      & gustiness\\
$V$      & surface ventilation velocity, defined as\\
         & $ V^2 = u^2_{\mbox{b}} + v^2_{\mbox{b}} + G^2$ \\
\end{tabular}


\subsection{Near Surface Meteorology}
%           ---------------------
\label{sec:met}

We assume that the same surface layer conditions derived from the
background fields are still valid for the analysis. The following
similarity relationships are adopted:
%
\bea
{ u^f(z) \over u^f(z_b) } = \lambda_u(z) \\
{ q^f(z) - q_s \over q^f(z_b) - q_s } = \lambda_q(z) \\
{ \theta^f(z) - \theta_s \over \theta^f(z_b) - \theta_s } = \lambda_q(z) 
\eea
%
where $z$ is the desired height, either 2 or 10 meters. The following near surface approximation is used for converting potential temperature to temperature:
\be
\theta \approx T + \gamma z, \qquad \gamma = 0.01 \mbox(K/m)
\ee
so that
\be
\theta - \theta_s  \approx T-T_s + \gamma z
\ee
In order to ensure algorithm stability we require that $\lambda\in[0,1]$, viz.
\be
\lambda := \min\( 0, \max(1,\lambda) \)
\ee
From the expressions above we get:
%
\bea
 u^a(z) & = &  \lambda_u(z) u^f(z_b) \\
 q^a(z) & = & (1-\lambda_q) q_s + \lambda_q(z) q^a(z_b) \\
 \theta^a(z) & = & (1-\lambda_q(z)) \theta_s + \lambda_q(z) \theta^a(z_b) 
\eea
%
Notice that each one of these near surface quantities are in between
their value at the model bottom layer $z_b$ and their surface value.

\subsection{Surface fluxes of heat and momentum}
\label{sec:fluxes}
%           ---------------------

The before and after analysis ventilation velocity are
defined as
\begin{eqnarray}
V^f & = & \max\(1,\sqrt{ {u^f_b}^2 + (v^f_b)^2 + (G^f)^2 }\) \\
V^a & = & \max\(1,\sqrt{ {u^a_b}^2 + {v^a_b}^2 + (G^f)^2 }\) 
\end{eqnarray}
where the supscripts $a$/$f$ refer to analysis/first guess
quantities. The lower bound of ``1'' is for consistency with the
GEOS-5 AGCM. Notice that the first guess gustiness $G^f$ is used to
estimate the after analysis ventilation velocity. Define the ratio:
\be
\alpha = { V^a \over V^f}
\ee
The after analysis wind stress is estimated by correcting the
ventilation velocity and using the analyzed winds at the bottom layer, viz.
\be
{\bf \tau} = C_M^f \cdot \alpha \cdot {\bf u}^a_b\label{eq:tau}
\ee
Inspection of the near surface gradients of temperature 
and moisture coming from the
analysis ($\theta^a_b-\theta^a_s$, $q^a_b - q^a_s$, respectively)
indicate that these quantities are not very reliable for estimating
sensible and latent heat fluxes.
The main reason is that the analysis increments of bottom
layer temperature and skin temperature are essentially decoupled in
the GSI analysis, leading to unrealistically large near surface
gradients. For this reason, we only apply a correction of the
ventilation velocity for estimating the after analysis heat fluxes:
\begin{eqnarray}
Q^a_L &=& \alpha \cdot Q^f_L \\
Q^a_S &=& \alpha \cdot Q^f_S 
\end{eqnarray}

\subsubsection*{Notes}
\begin{enumerate}
\item In the code, an upper bound of 5 is imposed on $\alpha$ to
  avoild very large values of the fluxes in isolated gridpoints; about
  0.3\% of the grid points exercise this safeguard.
\item The expression (\ref{eq:tau}) does not strictly apply when using
  fields from the {\tt bkg.sfc} file. This is because of the
  semi-implicit nature of the GEOS-5 surface formulation. The stress
  in this file is computed with winds 1 timestep away from the time on
  file. 
\item Rather that using the ventilation velocity on file, we
  recompute $V^f$ here to ensure consistency with the computation of $V^a$.
\item The input upper level winds are given on a D-grid. Therefore, a
conversion of the winds to the A-grid is performed beform carrying out
the calculations outlined above.

\item The GEOS-5 input files have longitudes in the range
$[-180,180)$. For consistency with GEOS-4, the upper level fields are
flipped so that it has longitudes in the range $[0,360)$; this
operation is performed inside routine {\tt dyn\_get()}. Analogously,
this utility also flips the 2D background diagnostics to have
longitudes in the range $[0,360)$; a command line option is given to
prevent this. (Unlike {\tt dyn\_get()} no query is made of the
longitudes, so a flip is always performed in 2D bkg.sfc fields 
unless the user disables it.)

\end{enumerate}

\subsection{Using {\tt ana5sfc.x}}
\label{sec:util}
%           ---------------------

\begin{verbatim}

 USAGE:

  ana5sfc.x [-v]  [-o ana.sfc]  ana.eta  bkg.eta bkg.sfc

 where

  anal.eta    input ana.eta file name
  bkg.eta     input bkg.eta file name
  bkg.sfc     input nkg.sfc file name
  -o ana.sfc  optional output ana.sfc file name; default is
              $expid.ana.sfc.$time.nc4, where $expid and
              $time are derived from the input "ana.eta" file

  -noFlip     does not flip longitude of diag files; default
              is to do so.

  -v        verbose mode

\end{verbatim}

Notice that when the input {\tt ana.eta} and {\tt bkg.eta} file names
are the same no flux calculation is performed and the background
values are returned. Although the algorithm above would give
essentially the same results, roundoff errors would cause a
distracting non-zero difference.


!EOI
#endif

      program Ana5sfc

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!noBOP
!
! !ROUTINE: Ana5sfc:  Produces 2D Analysis Diagnostics Near Surface
!
! !INTERFACE:
!
!     See usage()
!
! !USES:
!
      use  m_dyn          ! dynamics state vector
      use  m_insitu       ! simulator vector
      use  m_const        ! physical constants
      use  m_die          ! error handling
      use  m_zeit         ! timer
      use  m_FileResolv   ! file resolver
      use  m_StrTemplate  ! string template expansion

      implicit NONE

! !DESCRIPTION: Given an analyzed {\em dynamics vector} file and a 2D
!               diagnostic file, this application produces estimates
!  of the following near surface diagnostics:
!  \bn
!        \item temperature at 2m and 10m
!        \item specific humidity at 2m and 10m
!        \item wind at 2m and 10m
!        \item wind stress
!        \item sensible and latent hear fluxes
!  \en
!
! !REVISION HISTORY:
!
!  03Jan2002  da Silva  Initial code.
!  04Feb2002  da Silva  Further development
!  09Apr2002  Owens     Fixed output (-o option) problem
!  25Nov2002  Lucchesi  Reset fill values in TROPP, TROPT, and TROPQ
!  22May2005  da Silva  Adapted for GEOS-5 from ana2sfc.
!  24Oct2006  Todling   Bug fix in point to tto3 (when writing file)
!  02Nov2006  Todling   Added 50 m fields and displacement height
!  09Nov2006  Todling   Bug fixes: disph for ana; reversed qti/qtl
!
!EOP
!-----------------------------------------------------------------------

      character(len=*), parameter :: myname = 'ana5sfc'

!     Dynamics vectors
!     ----------------
      type (dyn_vect) :: w_a               ! analysis   state vector
      type (dyn_vect) :: w_f               ! background state vector


!    Near sfc diagnostics
!    --------------------
     type bundle 

!      ana.eta/bkg.eta derived quantities
!      ----------------------------------
       real, pointer ::  pb(:,:)  ! pressure at bottom layer
       real, pointer ::  zb(:,:)  ! height of bottom layer
       real, pointer ::  tb(:,:)  ! temperature at bottom layer
       real, pointer ::  qb(:,:)  ! specific hum. at bottom layer
       real, pointer ::  ua(:,:)  ! zonal wind at bottom layer
       real, pointer ::  va(:,:)  ! meridional wind at bottom layer
       real, pointer ::  ub(:,:)  ! zonal wind at bottom layer
       real, pointer ::  vb(:,:)  ! meridional wind at bottom layer
       real, pointer ::  ts(:,:)  ! skin tempeature
       real, pointer ::  qs(:,:)  ! saturation q at sfc: q_s(ts)
       real, pointer ::  ps(:,:)  ! surface pressure
       real, pointer ::  slp(:,:) ! sea level pressure
       real, pointer ::  zs(:,:)  ! surface topography
       real, pointer ::  phis(:,:)  ! surface topography
       real, pointer ::  lwi(:,:) ! land-ice-water mask

       real, pointer ::  that(:,:)  ! skin tempeature (turb)
       real, pointer ::  qhat(:,:)  ! skin moisture (turb)
       real, pointer ::  gust(:,:) ! gustiness (m/s)
       real, pointer ::  vent(:,:) ! ventilation (m/s): sqrt(u^2 + gust^2)
       real, pointer ::  ct(:,:)   ! exchange coeff (kg/m2/s)
       real, pointer ::  cq(:,:)   ! exchange coeff (kg/m2/s)
       real, pointer ::  cm(:,:)   ! exchange coeff (kg/m2/s)
       real, pointer ::  disph(:,:)! displacement height (m)


!      bkg.sfc/ana.sfc related quantities
!      ----------------------------------
       real, pointer ::  u2m(:,:),  v2m(:,:),  t2m(:,:),  q2m(:,:)  
       real, pointer ::  u10m(:,:), v10m(:,:), t10m(:,:), q10m(:,:)  
       real, pointer ::  u50m(:,:), v50m(:,:), t50m(:,:), q50m(:,:)  
       real, pointer ::  taux(:,:), tauy(:,:), shf(:,:),  lhf(:,:)
       real, pointer ::  tropP(:,:), tropT(:,:), tropQ(:,:), tto3(:,:)
       real, pointer ::  tpw(:,:), lwp(:,:), iwp(:,:), to3(:,:)

    end type bundle

    type(bundle) :: x_f
    type(bundle) :: x_a


!  Metadata for output GFIO file
!  -----------------------------
   integer, parameter :: NVARS = 28 ! 12 ana.sfc + slp,ps,ts.phis,lwi,trop*
   character(len=255)              :: title, source, contact, levunits
   character(len=255), pointer     :: vname(:), vtitle(:), vunits(:)
   real,    pointer :: lat(:) => null(), lon(:)=>null(), lev(:)=>null()
   real,    pointer :: coslon(:)=>null(), sinlon(:)=>null()
   real,    pointer :: valid_range(:,:), packing_range(:,:)
   integer, pointer :: kmvar(:)
   type field
       real, pointer :: data(:,:)
   end type field
   type(field) var(NVARS)

   real    :: p, ptop

!   Local variables
!   ---------------
   integer :: rc
   integer :: nymd, nhms, im, jm, km
   integer  :: READ_ONLY=1  

   integer, parameter :: iH2O=1, iO3=2, iIWP=3, iLWP=4 ! for now.
 
   character(len=255) :: bkg_eta_fn         ! background file name (in)
   character(len=255) :: ana_eta_fn         ! analysis   file name (in)
   character(len=255) :: bkg_sfc_tn, bkg_sfc_fn ! 2D diag  file name (in)
   character(len=255) :: ana_sfc_tn, ana_sfc_fn ! near surface analysis file name (out)
   
   logical debug, first, has_ana, lon_flip
   integer fid, ntimes, nvars_a, ngatts, timeinc, itime
   integer vectype

!.......................................................................

  first = .true.

  call zeit_ci ( 'ana5sfc' )

! Parse command line
! ------------------
  call Init_()

! Whether an analysis is present
! ------------------------------
  if ( trim(bkg_eta_fn) .ne. trim(ana_eta_fn) ) then
       has_ana = .true.
  else
       has_ana = .false. 
  end if

! Determine how many time levels on file
! --------------------------------------
  call GFIO_Open ( ana_eta_fn, READ_ONLY, fid, rc )
  if ( rc .ne. 0 ) then
     call die(myname,'cannot open GFIO file '//trim(ana_eta_fn))
  end if
  call GFIO_DimInquire ( fid, im, jm, km, ntimes, nvars_a, ngatts, rc)
  if ( rc .ne. 0 ) then
     call die(myname,'problems getting dimensions' )
  end if
  call GFIO_Close ( fid, rc )

! For each time on file...
! ------------------------
  do itime = 1, ntimes


!    Get background data for this time
!    ---------------------------------
     call dyn_get ( bkg_eta_fn, nymd, nhms, w_f, rc, &
                    timidx=itime, freq=timeinc, vectype=vectype )
     if ( rc .ne. 0 ) then
         call die(myname,'cannot read analysis file')
     else
         call fix_names_()
     end if


!     Allocate necessary memory
!     -------------------------
      if ( first ) then
           call malloc_(x_f,im,jm)
           call malloc_(x_a,im,jm)
      end if

!     Set 2D fields from bkg.eta fields
!     ---------------------------------
      call eta_set_( w_f, x_f )

!     Load background (diagnostic) fields
!     -----------------------------------
      call sfc_set_ ( bkg_sfc_fn, nymd, nhms, x_f )

!     Initialize metadata
!     -------------------
      if ( first ) then
         if ( has_ana ) then
              call meta_init_ ( w_f, x_a )
         else
              call meta_init_ ( w_f, x_f )
         end if
      end if

      call dyn_clean ( w_f )     ! no longer needed

!     Only do this calculation if there is an analysis
!     ------------------------------------------------
      if ( has_ana ) then

!        Get analysis data for this time
!        --------------------------
         call dyn_get ( ana_eta_fn, nymd, nhms, w_a, rc, &
                       timidx=itime, freq=timeinc, vectype=vectype )
         if ( rc .ne. 0 ) then
            call die(myname,'cannot read analysis file')
         end if

!        Set 2D fields ana bkg.eta fields
!        --------------------------------
         call eta_set_( w_a, x_a )

         call dyn_clean ( w_a )     ! no longer needed

         call stat_ ( 'INPUT:', x_f )

!        Compute near surface meteorology
!        --------------------------------
         call  uvtq_set_ ( im, jm, 2.0, &
                           x_f%u2m, x_f%v2m, x_f%q2m, x_f%t2m, &
                           x_a%u2m, x_a%v2m, x_a%q2m, x_a%t2m )
         call  uvtq_set_ ( im, jm, 10.0, &
                           x_f%u10m, x_f%v10m, x_f%q10m, x_f%t10m, &
                           x_a%u10m, x_a%v10m, x_a%q10m, x_a%t10m )
         x_f%t50m  = x_f%t10m   ! for now gcm does not output t50m/q50m
         x_f%q50m  = x_f%q10m   ! for now gcm does not output t50m/q50m
         call  uvtq_set_ ( im, jm, 50.0, &
                           x_f%u50m, x_f%v50m, x_f%q50m, x_f%t50m, &
                           x_a%u50m, x_a%v50m, x_a%q50m, x_a%t50m )

         x_a%disph = x_f%disph  ! for now, displacement height of ana=bkg

!        Compute heat and momentum fluxes
!        --------------------------------
         call fluxes_set_ ( im, jm, x_f, x_a )

!     Take values from background if ana.eta = bkg.eta
!     ------------------------------------------------
      else

         if ( debug ) print *, ' [] Deriving all from bkg...'

      end if

!     Output ana.sfc quantities
!     --------------------------
      if ( has_ana ) then
         call fix_units_( x_a )
         call stat_ ( 'OUTPUT:', x_a )
      else
         call fix_units_( x_f )
         call stat_ ( 'OUTPUT:', x_f )
      end if
      call nsfc_put_ ( nymd, nhms ) ! arrays ptr set during meta_init_()

      first = .false.

   end do

   call meta_clean_()

!  Free memory
!  -----------
!   call fremem_(x_f)
!   call fremem_(x_a)


  call exit(0)

!............................................................................

      CONTAINS

subroutine malloc_ ( x, im, jm )
  implicit NONE
  type(bundle), intent(inout) :: x
  integer, intent(in) :: im, jm ! horizontal dimensions
  integer ios  
  if ( debug ) print *, ' [] allocating memory'
  allocate ( x%pb(im,jm), x%zb(im,jm), x%tb(im,jm), x%qb(im,jm),  & 
             x%ub(im,jm), x%vb(im,jm), x%ts(im,jm), x%ps(im,jm), x%slp(im,jm), &
             x%zs(im,jm), x%phis(im,jm), x%lwi(im,jm), x%qs(im,jm), &
             stat = ios )
  if ( ios .ne. 0 ) & 
       call die(myname,'cannot allocate analysis 2D fields' )
  
  allocate ( x%u2m(im,jm),  x%v2m(im,jm),  x%t2m(im,jm),  x%q2m(im,jm), &  
             x%u10m(im,jm), x%v10m(im,jm), x%t10m(im,jm), x%q10m(im,jm), &  
             x%u50m(im,jm), x%v50m(im,jm), x%t50m(im,jm), x%q50m(im,jm), &  
             x%taux(im,jm), x%tauy(im,jm), x%shf(im,jm),  x%lhf(im,jm), &
             x%tropP(im,jm), x%tropT(im,jm), x%tropQ(im,jm), &
             x%tpw(im,jm), x%lwp(im,jm), x%iwp(im,jm), x%to3(im,jm), & 
             x%tto3(im,jm), x%that(im,jm), x%qhat(im,jm), x%gust(im,jm), &
             x%vent(im,jm), x%ct(im,jm),  x%cq(im,jm),  x%cm(im,jm), x%disph(im,jm), &
             x%ua(im,jm), x%va(im,jm), &
             stat = ios )
  if ( ios .ne. 0 ) & 
       call die(myname,'cannot allocate ana.sfc 2D fields' )
  
  if ( .not. associated(lat) ) then

     if ( debug ) print *, ' [] allocating memory for metadata'
     allocate ( lat(jm), lon(im), lev(km), coslon(im), sinlon(im),                                &
          vname(nvars), vunits(nvars), vtitle(nvars), kmvar(nvars), &
          valid_range(2,nvars), packing_range(2,nvars),             &
          stat=rc )
     if ( rc/=0 ) call die(myname,'cannot allocate mem for metadata')
     
  end if

 end subroutine malloc_

#ifdef NEVER 
subroutine fremem_()
  implicit NONE
  if ( debug ) print *, ' [] deallocating memory'
  deallocate ( pb, zb, tb, qb,  & 
       ub, vb, ts, ps, slp, &
       zs, phis, lwi )
  deallocate ( u2m,  v2m,  t2m,  q2m, &  
               u10m, v10m, t10m, q10m, &  
               u50m, v50m, t50m, q50m, &  
               taux, tauy, shf,  lhf, &
               tropP, tropT, tropQ, tpw, lwp, iwp, to3,
               tto3, that, qhat, gust, &
               vent, ct,  cq,  cm, disph, &
               ua, va)
end subroutine fremem_
#endif


subroutine sfc_set_ ( bkg_sfc_fn, nymd, nhms, x ) ! read needed background fields
  implicit NONE
  character(len=*), intent(in) :: bkg_sfc_fn       ! diagnostic file name
  integer, intent(in)          :: nymd, nhms   ! date/time
  type(bundle), intent(inout) :: x
!         
  integer :: fid, rc
  
  if ( debug ) print *, ' [] Reading ', trim(bkg_sfc_fn)

  call GFIO_Open ( bkg_sfc_fn, READ_ONLY, fid, rc ) 
  if ( rc .ne. 0 ) call die(myname,'cannot open diag file '//trim(bkg_sfc_fn))

  call GFIO_GetVarFlip ( fid, 'QS', nymd, nhms, im, jm, 0, 1, x%qs, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read QS' )
 
  call GFIO_GetVarFlip ( fid, 'UA', nymd, nhms, im, jm, 0, 1, x%UA, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read UA' )
 
  call GFIO_GetVarFlip ( fid, 'VA', nymd, nhms, im, jm, 0, 1, x%VA, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read VA' )
 
  call GFIO_GetVarFlip ( fid, 'U2M', nymd, nhms, im, jm, 0, 1, x%U2M, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read U2M' )
 
  call GFIO_GetVarFlip ( fid, 'V2M', nymd, nhms, im, jm, 0, 1, x%V2M, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read V2M' )
 
  call GFIO_GetVarFlip ( fid, 'T2M', nymd, nhms, im, jm, 0, 1, x%T2M, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read T2M' )

  call GFIO_GetVarFlip ( fid, 'Q2M', nymd, nhms, im, jm, 0, 1, x%Q2M, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read Q2M' )
 
  call GFIO_GetVarFlip ( fid, 'U10M', nymd, nhms, im, jm, 0, 1, x%U10M, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read U10M' )
 
  call GFIO_GetVarFlip ( fid, 'V10M', nymd, nhms, im, jm, 0, 1, x%V10M, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read V10M' )
 
  call GFIO_GetVarFlip ( fid, 'T10M', nymd, nhms, im, jm, 0, 1, x%T10M, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read T10M' )
 
  call GFIO_GetVarFlip ( fid, 'Q10M', nymd, nhms, im, jm, 0, 1, x%Q10M, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read Q10M' )

  call GFIO_GetVarFlip ( fid, 'U50M', nymd, nhms, im, jm, 0, 1, x%U50M, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read U50M' )

  call GFIO_GetVarFlip ( fid, 'V50M', nymd, nhms, im, jm, 0, 1, x%V50M, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read V50M' )

! call GFIO_GetVarFlip ( fid, 'T50M', nymd, nhms, im, jm, 0, 1, x%T50M, rc )
! if ( rc .ne. 0 ) call die(myname,'cannot read T50M' )

! call GFIO_GetVarFlip ( fid, 'Q50M', nymd, nhms, im, jm, 0, 1, x%Q50M, rc )
! if ( rc .ne. 0 ) call die(myname,'cannot read Q50M' )
 
  call GFIO_GetVarFlip ( fid, 'UFLUX', nymd, nhms, im, jm, 0, 1, x%TAUX, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read TAUX' )
 
  call GFIO_GetVarFlip ( fid, 'VFLUX', nymd, nhms, im, jm, 0, 1, x%TAUY, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read TAUY' )
 
  call GFIO_GetVarFlip ( fid, 'HFLUX', nymd, nhms, im, jm, 0, 1, x%SHF, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read SHF' )
 
  call GFIO_GetVarFlip ( fid, 'EFLUX', nymd, nhms, im, jm, 0, 1, x%LHF, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read LHF' )

  call GFIO_GetVarFlip ( fid, 'THAT', nymd, nhms, im, jm, 0, 1, x%THAT, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read THAT' )
  
  call GFIO_GetVarFlip ( fid, 'QHAT', nymd, nhms, im, jm, 0, 1, x%QHAT, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read QHAT' )
  
  call GFIO_GetVarFlip ( fid, 'GUST', nymd, nhms, im, jm, 0, 1, x%GUST, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read GUST' )
  
  call GFIO_GetVarFlip ( fid, 'VENT', nymd, nhms, im, jm, 0, 1, x%VENT, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read VENT' )
  
  call GFIO_GetVarFlip ( fid, 'CT', nymd, nhms, im, jm, 0, 1, x%CT, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read CT' )
  
  call GFIO_GetVarFlip ( fid, 'CQ', nymd, nhms, im, jm, 0, 1, x%CQ, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read CQ' )
  
  call GFIO_GetVarFlip ( fid, 'CM', nymd, nhms, im, jm, 0, 1, x%CM, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read CM' )

  call GFIO_GetVarFlip ( fid, 'DISPH', nymd, nhms, im, jm, 0, 1, x%DISPH, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot read DISPH' )

  call GFIO_Close ( fid, rc )
  if ( rc .ne. 0 ) call die(myname,'cannot close diag file '//trim(bkg_sfc_fn))

end subroutine sfc_set_

  subroutine hflip2_ ( q,im,jm )
      implicit none
      integer  im,jm,i,j
      real, intent(inout) :: q(im,jm)
      real, allocatable   :: dum(:)
      allocate ( dum(im) )
      do j=1,jm
      do i=1,im/2
         dum(i) = q(i+im/2,j)
         dum(i+im/2) = q(i,j)
      enddo
         q(:,j) = dum(:)
      enddo
      deallocate ( dum )
  end subroutine hflip2_

      subroutine GFIO_GetVarFlip ( fid, vname, yyyymmdd, hhmmss, &
                                   im, jm, kbeg, kount, grid, rc)
!
      Implicit NONE
!
! !INPUT PARAMETERS:
!
      integer        fid              ! File handle
      character*(*)  vname            ! Variable name
      integer        yyyymmdd         ! Year-month-day, e.g., 19971003
      integer          hhmmss         ! Hour-minute-second, e.g., 120000
      integer         im              ! size of longitudinal dimension
      integer         jm              ! size of latitudinal  dimension
      integer         kbeg            ! first level to read; if 2-D grid
                                      !  set kbeg = 0.
      integer         kount           ! number of levels to read
      logical         cyclic          ! whether time dimension is periodic
      real         grid(im,jm)        ! Gridded data read for this time
      integer         rc              ! Error return code:
!
!EOP

      cyclic = .false.
      call GFIO_GetVar1 ( fid, vname, yyyymmdd, hhmmss, &
                              im, jm, kbeg, kount, grid, &
                              cyclic, rc )

      if ( lon_flip .and. debug ) &
           print *, '    <> Flipping longitudes for ', trim(vname)
      if ( lon_flip ) call  hflip2_ ( grid, im, jm )

      return
 
   end subroutine GFIO_GetVarFlip

subroutine eta_set_(w,x)      ! set needed quantities from analysis
   implicit NONE
   type(dyn_vect), intent(inout) :: w
   type(bundle), intent(inout)   :: x
   type(sim_vect) :: w_s
   integer        :: rc, nobs, i, j, k, kb, ncount
   real :: zmin, alpha, ptop, pi, d2r
   if ( debug ) print *, ' [] Setting analysis fields...'
    x%tpw = 0.0 
    x%lwp = 0.0
    x%iwp = 0.0
    x%to3 = 0.0
    ncount = 0
    call Insitu_Init ( w, w_s, rc )
    if ( rc .ne. 0 ) call die ( myname, 'cannot create simulator vector' )
    do j = 1, jm
       do i = 1, im                        ! determine bottom layer
          zmin = 10. ! at least 10m above sfc (used to be zpd+z0m)
          do k = km, 1, -1 
             kb = k

             x%zb(i,j) = (w_s%ze(i,k,j) - w_s%ze(i,k+1,j))/2.
             if ( x% zb(i,j) .ge. zmin ) then
                  exit ! avoid very thin shaved layers
             endif
          end do
          if ( kb /= km ) then
               ncount = ncount + 1
          end if

          x%pb(i,j)  = 100 * ( w_s%pe(i,kb,j) + w_s%pe(i,kb+1,j) ) / 2
          x%tb(i,j)  = w_s%q3d(i,j,kb,iqt)
          x%qb(i,j)  = w_s%q3d(i,j,kb,iqq) / 1000. ! g/kg -> kg/kg
          x%ub(i,j)  = w_s%q3d(i,j,kb,iqu)
          x%vb(i,j)  = w_s%q3d(i,j,kb,iqv)
          x%ts(i,j)  = w_s%q2d(i,j,iqts)
          x%ps(i,j)  = 100 * w_s%q2d(i,j,iqps)
          x%zs(i,j)  = w_s%q2d(i,j,iqzs)
          x%phis(i,j)  = grav * x%zs(i,j)
          x%lwi(i,j) = w_s%q2d(i,j,iqlwi)
          x%slp(i,j) = w_s%q2d(i,j,iqslp)

          do k = 1, km
             x%tpw(i,j) = x%tpw(i,j) + w%delp(i,j,k) * w%q(i,j,k,iH2O)/grav
             x%to3(i,j) = x%to3(i,j) + w%delp(i,j,k) * w%q(i,j,k,iO3)/grav
             x%lwp(i,j) = x%lwp(i,j) + w%delp(i,j,k) * w%q(i,j,k,iLWP)/grav
             x%iwp(i,j) = x%iwp(i,j) + w%delp(i,j,k) * w%q(i,j,k,iIWP)/grav
          end do
       end do
    end do

    ! not sure if the kb logic above is still needed
    if ( ncount > 0 ) then
       print *, " <> number of times kb /= km: ", ncount
    end if

!   From D to A
!   -----------
    pi = 4. * atan(1.)
    d2r = pi / 180.
    coslon = cos(d2r * lon)
    sinlon = sin(d2r * lon)
    if ( debug ) print *, ' D Grid: u_b = ', minval(x%ub), maxval(x%ub)  
    if ( debug ) print *, ' D Grid: v_b = ', minval(x%vb), maxval(x%vb)  
    call d2a ( x%ub, x%vb, im, jm, 1, jm, coslon, sinlon )
    if ( debug ) print *, ' A Grid: u_b = ', minval(x%ub), maxval(x%ub)  
    if ( debug ) print *, ' A Grid: v_b = ', minval(x%vb), maxval(x%vb)  

    alpha = 0.03 ! empirical parameter
    ptop = w%grid%ptop
    if ( debug ) print *, ' [] Setting tropopause fields...'
    call tropopause ( alpha, im, jm, km, w_s%pe, w_s%pm, &
                      Ptop, x%TropP, &
                      w_s%q3d(1:im,1:jm,1:km,iqt), x%TropT, &
                      w_s%q3d(1:im,1:jm,1:km,iqq), x%TropQ, &
                      w%q(1:im,1:jm,1:km,iO3),   x%Tto3,  &
                      w%delp(1:im,1:jm,1:km), undef, grav )

    call Insitu_Clean ( w_s )

end subroutine eta_set_

subroutine uvtq_set_ ( im, jm, z, uz_f, vz_f, qz_f, tz_f, &
                                  uz_a, vz_a, qz_a, tz_a )
!
! Computes u,v,tq, at 2 and 10m (any level z, really)
!
   integer, intent(in) :: im, jm
   real, intent(in)                    ::  z ! 2 or 10
   real, intent(in),  dimension(im,jm) ::  uz_f, vz_f, qz_f, tz_f
   real, intent(out), dimension(im,jm) ::  uz_a, vz_a, qz_a, tz_a

   real eps, gamma, factor, thz_f, thz_a, thb_f, thb_a
   integer i, j
   integer :: nu0, nt0, nq0, ntb
   integer :: nu1, nt1, nq1

   x_a%qs = x_f%qs ! uses bkg q_s as analysis does modify this

   eps = tiny( x_f%u10m(1,1) )
   gamma = 0.01 ! K/m
   nu0 = 0
   nt0 = 0
   nq0 = 0
   nu1 = 0
   nt1 = 0
   nq1 = 0
   ntb = 0

   do j = 1, jm
      do i = 1, im

!        Winds
!        -----
         factor = ( sqrt(uz_f(i,j)**2 + vz_f(i,j)**2) + eps ) / &
                  ( sqrt(x_f%ub(i,j)**2 + x_f%vb(i,j)**2) + eps ) 
         factor = max ( 0.0, min ( 1.0, factor ) ) ! in [0,1]
         uz_a(i,j) = factor * x_a%ub(i,j)
         vz_a(i,j) = factor * x_a%vb(i,j)

         if ( factor .eq. 0.0 ) nu0 = nu0 + 1
         if ( factor .eq. 1.0 ) nu1 = nu1 + 1

!        Temperature (calculation is in terms of theta ~= T + gamma * z)
!        ---------------------------------------------------------------
         thz_f = tz_f(i,j)   + gamma * z
         thb_f = x_f%tb(i,j) + gamma * x_f%zb(i,j)
         factor = ( thz_f - x_f%ts(i,j) + eps ) / ( thb_f - x_f%ts(i,j) + eps )
         factor = max ( 0.0, min ( 1.0, factor ) ) ! in [0,1]
         thb_a = x_a%tb(i,j) + gamma * x_a%zb(i,j)
         thz_a = (1.0 - factor ) * x_a%ts(i,j) + factor * thb_a
         tz_a(i,j) = thz_a - gamma * x_a%zb(i,j)

         if ( factor .eq. 0.0 ) nt0 = nt0 + 1
         if ( factor .eq. 1.0 ) nt1 = nt1 + 1

!        Humidity 
!        --------
         factor = ( qz_f(i,j) - x_f%qs(i,j) + eps ) / &
                  ( x_f%qb(i,j) - x_f%qs(i,j) + eps ) 
         factor = max ( 0.0, min ( 1.0, factor ) ) ! in [0,1]
         qz_a(i,j) = ( 1.0 - factor ) * x_a%qs(i,j) + factor * x_a%qb(i,j)

         if ( factor .eq. 0.0 ) nq0 = nq0 + 1
         if ( factor .eq. 1.0 ) nq1 = nq1 + 1

      end do
   end do

#ifdef NEVER
   if ( debug ) then

      factor = 100. / float ( im*jm )
      print *
      print *, ' [] uvtq: nu = ', nu0, nu1, nint(factor*nu0), nint(factor*nu1)
      print *, ' [] uvtq: nq = ', nq0, nq1, nint(factor*nq0), nint(factor*nq1)
      print *, ' [] uvtq: nt = ', nt0, nt1, nint(factor*nt0), nint(factor*nt1)
      print *, ' [] uvtq: ntb= ', ntb

   end if
#endif

 end subroutine uvtq_set_

 subroutine fluxes_set_ ( im, jm, x_f, x_a )
   implicit NONE
   integer, intent(in) :: im, jm
   type(bundle), intent(inout)  :: x_f
   type(bundle), intent(inout) :: x_a
 !                ---

   real :: alpha, vent_a, vent_f
   real :: alpha_min=+undef, alpha_max=-undef

   integer :: i, j, nfix = 0


   do j = 1, jm
      do i = 1, im

      vent_a = sqrt( x_a%ub(i,j)**2 + x_a%ub(i,j)**2 + x_f%gust(i,j)**2 ) 
      vent_a = max ( 1.0, vent_a )

      vent_f = sqrt( x_f%ub(i,j)**2 + x_f%ub(i,j)**2 + x_f%gust(i,j)**2 ) 
      vent_f = max ( 1.0, vent_f )

      alpha = vent_a / vent_f

      alpha = min ( alpha, 5.0 ) ! safeguard

      if ( alpha .eq. 5.0 ) nfix = nfix + 1

      alpha_min = min ( alpha_min, alpha )  
      alpha_max = max ( alpha_max, alpha )  

#ifdef WITHOUT_EXCHANGE_COEFFS
      x_a%taux(i,j) = alpha * x_f%taux(i,j)
      x_a%tauy(i,j) = alpha * x_f%tauy(i,j)
#else
      x_a%taux(i,j) =  x_f%cm(i,j) * alpha * x_a%ub(i,j)
      x_a%tauy(i,j) =  x_f%cm(i,j) * alpha * x_a%vb(i,j)
#endif
      x_a%shf(i,j)  = alpha * x_f%shf(i,j)
      x_a%lhf(i,j)  = alpha * x_f%lhf(i,j)

      end do
   end do

   if ( debug ) then

      print *
      print *, ' [] alpha = ', alpha_min, alpha_max, & 
                 (100*nfix/float(im*jm))

   end if

 end subroutine fluxes_set_

subroutine fix_units_(x)

   implicit NONE
   type(bundle), intent(inout) :: x
   real mu, Mair, Vd, factor
   x%ps = x%ps / 100.
   x%q2m  = x%q2m  * 1000.   ! kg/kg -> g/kg
   x%q10m = x%q10m * 1000.   ! kg/kg -> g/kg

   mu = 22.4136 ! m3/kmol
   Mair = 28.9  ! kg/kmol
   Vd = 1.0e-5  ! m3
   factor = (1.0E-6) * mu / ( Mair * Vd )  ! mg/m2 -> Dobson Units

   where (  x%to3 /= undef )  x%to3 = factor *  x%to3
   where ( x%tto3 /= undef ) x%tto3 = factor * x%tto3

end subroutine fix_units_

subroutine stat_(what,x)
   character(len=*), intent(in) :: what
   type(bundle), intent(inout) :: x
   if ( debug ) then
         print *
         print *, '--------------------------------------------------'
         print *, trim(what)
         print *, ' <>  pb  = ', minval(x%pb), maxval(x%pb)
         print *, ' <>  tb  = ', minval(x%tb), maxval(x%tb)
         print *, ' <>  qb  = ', minval(x%qb), maxval(x%qb)
         print *, ' <>  ub  = ', minval(x%ub), maxval(x%ub)
         print *, ' <>  vb  = ', minval(x%vb), maxval(x%vb)
         print *, ' <>  zb  = ', minval(x%zb), maxval(x%zb)
         print *
         print *, ' <>  phs = ', minval(x%phis), maxval(x%phis)
         print *, ' <>  ts  = ', minval(x%ts), maxval(x%ts)
         print *, ' <>  qs  = ', minval(x%qs), maxval(x%qs)
         print *, ' <>  ps  = ', minval(x%ps), maxval(x%ps)
         print *, ' <>  slp = ', minval(x%slp), maxval(x%slp)
         print *, ' <>  lwi = ', minval(x%lwi), maxval(x%lwi)
         print *
         print *, ' <>  shf = ', minval(x%shf), maxval(x%shf)
         print *, ' <>  lhf = ', minval(x%lhf), maxval(x%lhf)
         print *, ' <>  tx  = ', minval(x%taux), maxval(x%taux)
         print *, ' <>  ty  = ', minval(x%tauy), maxval(x%tauy)
         print *
         print *, ' <>  t2  = ', minval(x%t2m), maxval(x%t2m)
         print *, ' <>  t10 = ', minval(x%t10m), maxval(x%t10m)
!        print *, ' <>  t50 = ', minval(x%t50m), maxval(x%t50m)
         print *, ' <>  q2  = ', minval(x%q2m), maxval(x%q2m)
         print *, ' <>  q10 = ', minval(x%q10m), maxval(x%q10m)
!        print *, ' <>  q50 = ', minval(x%q50m), maxval(x%q50m)
         print *, ' <>  u2  = ', minval(x%u2m), maxval(x%u2m)
         print *, ' <>  u10 = ', minval(x%u10m), maxval(x%u10m)
         print *, ' <>  u50 = ', minval(x%u50m), maxval(x%u50m)
         print *, ' <>  v2  = ', minval(x%v2m), maxval(x%v2m)
         print *, ' <>  v10 = ', minval(x%v10m), maxval(x%v10m)
         print *, ' <>  v50 = ', minval(x%v50m), maxval(x%v50m)
         print *
         print *, ' <>  trP = ', minval(x%tropP), maxval(x%tropP)
         print *, ' <>  trT = ', minval(x%tropT), maxval(x%tropT)
         print *, ' <>  trQ = ', minval(x%tropQ), maxval(x%tropQ)
         print *, ' <> tqv  = ', minval(x%tpw), maxval(x%tpw)
         print *, ' <> to3  = ', minval(x%to3), maxval(x%to3)
         print *, ' <> tto3 = ', minval(x%tto3), maxval(x%tto3)
         print *, ' <> tql  = ', minval(x%lwp), maxval(x%lwp)
         print *, ' <> tqi  = ', minval(x%iwp), maxval(x%iwp)
         print *
         print *, ' <> disph  = ', minval(x%disph), maxval(x%disph)
         print *, '--------------------------------------------------'

    end if

  end subroutine stat_

subroutine nsfc_put_ ( nymd, nhms )    ! writes out GFIO file
  implicit NONE
  integer, intent(in)          :: nymd, nhms   ! date/time
!                              ---
  integer, parameter :: READ_WRITE = 0
  logical :: fexists  
  integer :: i, fid, rc, rcs(NVARS), prec=0
!
! NOTE: array pointers have already been set in metadata
!  
  inquire ( file=trim(ana_sfc_fn), exist=fexists )  
  if ( fexists ) then
      if ( debug ) print *, ' [] Writing to ', trim(ana_sfc_fn)
     call GFIO_Open ( ana_sfc_fn, READ_WRITE, fid, rc ) ! open existing file
     if ( rc/=0 ) call die(myname,'cannot open GFIO file '//trim(ana_sfc_fn) )  
  else
    if ( debug ) print *, ' [] Creating ', trim(ana_sfc_fn)
    call GFIO_Create ( ana_sfc_fn, title, source, contact, undef,       &
                       im, jm, km, lon, lat, lev, levunits,         &
                       nymd, nhms, timeinc,                         &
                       nvars, vname, vtitle, vunits, kmvar,         &
                       valid_range, packing_range, prec,            &
                       fid, rc )
    if ( rc/=0 ) call die(myname,'cannot create GFIO file '//trim(ana_sfc_fn) )
  end if
  rcs = 0
  do i = 1, NVARS
      if ( debug ) print *, '       o writing ', trim(vname(i))
      call GFIO_PutVar ( fid, trim(vname(i)), nymd, nhms, im, jm, 0, 1, &
                         var(i)%data, rcs(i) )
  end do
  if ( any(rcs .ne. 0) ) call die(myname,'cannot write fields')
  call GFIO_Close ( fid, rc )
end subroutine nsfc_put_

subroutine meta_init_ ( w, x )       ! allocates local memory
  type(dyn_vect), intent(in) :: w
  type(bundle), intent(inout) :: x
  integer rc, i, j, k
  real dpref
  dpref(k) = ( w%grid%ak(k+1) - w%grid%ak(k) ) + &
             ( w%grid%bk(k+1) - w%grid%bk(k) ) * 98400.
  lat(:) = w%grid%lat(:)
  lon(:) = w%grid%lon(:)
  ptop = w%grid%ptop
  p = ptop + 0.5 * dpref(1)
  lev(1) = p
  do k = 2, km ! NOTE: verical coords nor really needed here
     p = p + 0.5 * ( dpref(k-1) + dpref(k) )
     lev(k) = p
  end do
  lev(1:km) = lev(1:km) / 100.
  levunits = 'hPa'
  title = 'fvDAS Near Surface Analysis Diagnostics'
  source = 'Global Modeling and Assimilation Office, NASA/GSFC'
  contact = 'data@gmao.gsfc.nasa.gov'
  kmvar(1:nvars)  = 0  ! all 2D quantites
  valid_range = undef
  packing_range = undef
  i = 0
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'PHIS'
  vtitle(i) = 'Surface Geopotential Height'
  vunits(i) = 'm2/s2'
  var(i)%data => x%phis
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'PS' 
  vtitle(i) = 'Surface Pressure'
  vunits(i) = 'hPa'
  var(i)%data => x%ps
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'SLP'
  vtitle(i) = 'Sea Level Pressure'
  vunits(i) = 'hPa'
  var(i)%data => x%slp
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'LWI' 
  vtitle(i) = 'Surface Type (0=water, 1=land, 2=ice)'
  vunits(i) = 'non-dimensional'
  var(i)%data => x%lwi
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'TSKIN' 
  vtitle(i) = 'surface skin temperature'
  vunits(i) = 'K'
  var(i)%data => x%ts
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'T2M'
  vtitle(i) = 'Temperature at 2 m Above Surface'  
  vunits(i) = 'K'
  var(i)%data => x%t2m
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'T10M'
  vtitle(i) = 'Temperature at 10 m Above Surface'  
  vunits(i) = 'K'
  var(i)%data => x%t10m
!
! i = i + 1; if (i>NVARS) call die(myname,'too many variables')
! vname(i)  = 'T50M'
! vtitle(i) = 'Temperature at 50 m Above Surface'  
! vunits(i) = 'K'
! var(i)%data => x%t50m
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'Q2M'
  vtitle(i) = 'Specific Humidity at 2 m Above Surface'  
  vunits(i) = 'g/kg'
  var(i)%data => x%q2m
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'Q10M'
  vtitle(i) = 'Specific Humidity at 10 m Above Surface'  
  vunits(i) = 'k/kg'
  var(i)%data => x%q10m
!
! i = i + 1; if (i>NVARS) call die(myname,'too many variables')
! vname(i)  = 'Q50M'
! vtitle(i) = 'Specific Humidity at 50 m Above Surface'  
! vunits(i) = 'k/kg'
! var(i)%data => x%q50m
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'U2M'
  vtitle(i) = 'Zonal Wind at 2 m Above Surface'  
  vunits(i) = 'm/s'
  var(i)%data => x%u2m
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'U10M'
  vtitle(i) = 'Zonal Wind at 10 m Above Surface'  
  vunits(i) = 'm/s'
  var(i)%data => x%u10m
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'U50M'
  vtitle(i) = 'Zonal Wind at 50 m Above Surface'  
  vunits(i) = 'm/s'
  var(i)%data => x%u50m
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'V2M'
  vtitle(i) = 'Meridional Wind at 2 m Above Surface'  
  vunits(i) = 'm/s'
  var(i)%data => x%v2m
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'V10M'
  vtitle(i) = 'Meridional Wind at 10 m Above Surface'  
  vunits(i) = 'm/s'
  var(i)%data => x%v10m
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'V50M'
  vtitle(i) = 'Meridional Wind at 50 m Above Surface'  
  vunits(i) = 'm/s'
  var(i)%data => x%v50m
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'HFLUX'
  vtitle(i) = 'Sensible Heat Flux at Surface'
  vunits(i) = 'W/m2'
  var(i)%data => x%shf
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'EFLUX'
  vtitle(i) = 'Latent Heat Flux at Surface'
  vunits(i) = 'W/m2'
  var(i)%data => x%lhf
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'UFLUX'
  vtitle(i) = 'Zonal Wind Stress at Surface'
  vunits(i) = 'N/m2'
  var(i)%data => x%taux
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'VFLUX'
  vtitle(i) = 'Meridional Wind Stress at Surface'
  vunits(i) = 'N/m2'
  var(i)%data => x%tauy
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'TROPP'
  vtitle(i) = 'Tropopause Pressure'
  vunits(i) = 'hPa'
  var(i)%data => x%tropP
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'TROPT'
  vtitle(i) = 'Tropopause Temperature'
  vunits(i) = 'K'
  var(i)%data => x%tropT
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'TROPQ'
  vtitle(i) = 'Tropopause Specific Humidity'
  vunits(i) = 'g/kg'
  var(i)%data => x%tropQ
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'TQV'
  vtitle(i) = 'Total Precipitable Water'
  vunits(i) = 'kg/m2'
  var(i)%data => x%tpw
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'TO3'
  vtitle(i) = 'Total Column Ozone'
  vunits(i) = 'dobson'
  var(i)%data => x%to3
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'TTO3'
  vtitle(i) = 'Tropospheric Ozone Column'
  vunits(i) = 'dobson'
  var(i)%data => x%tto3
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'TQL'
  vtitle(i) = 'Cloud Liquid Water Path'
  vunits(i) = 'kg/m2'
  var(i)%data => x%lwp
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'TQI'
  vtitle(i) = 'Cloud Ice Water Path'
  vunits(i) = 'kg/m2'
  var(i)%data => x%iwp
!
  i = i + 1; if (i>NVARS) call die(myname,'too many variables')
  vname(i)  = 'DISPH'
  vtitle(i) = 'Displacement Height'
  vunits(i) = 'm'
  var(i)%data => x%disph
!
  if ( i /= NVARS ) then
     print *, myname//': actual/declared NVARS: ', i, nvars
     call die(myname, 'less variables than NVARS have been set' )
  end if

end subroutine meta_init_

subroutine meta_clean_()             ! de-allocates local memory
     integer rc
     deallocate ( lat, lon, lev,           &
             vname, vunits, vtitle, kmvar, &
             valid_range, packing_range,   &
             stat=rc )
     if ( rc/=0 ) call die(myname,'cannot deallocate mem for metadata')
end subroutine meta_clean_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!noBOP
! !IROUTINE: Init_ --- Initialize Sbc
!
! !INTERFACE:
!
      subroutine Init_ ()

! !USES:

      use m_die
      implicit NONE


! !DESCRIPTION: Parses command line.
!
! !REVISION HISTORY:
!
!     31Jan02   da Silva   Initial code.
!     31Jan2014 Todling    Update for MERRA-2.
!
!noEOP
!-----------------------------------------------------------------------

      character(len=*), parameter :: myname = 'init'

      integer i, iarg, argc, iargc, n
      character(len=255) argv

      print *
      print *, '     -------------------------------------------------------'
      print *, '        ana5sfc - near surface analysis field calculator'
      print *, '     -------------------------------------------------------'
      print *


      ana_sfc_tn = 'ana5sfc.nc4'
      debug = .false.
      lon_flip = .true.   ! flip longitudes by default
      vectype = 4         ! GEOS-4-like (also MERRA-1)

!     Parse command line
!     ------------------
      argc =  iargc()
      if ( argc .lt. 3 ) call usage()

      iarg = 0
      n = 0
      do i = 1, 32767
         iarg = iarg + 1
         if ( iarg .gt. argc ) then
              exit
         endif
         call GetArg ( iArg, argv )
         if (index(argv,'-v' ) .gt. 0 ) then
             debug = .true.
         else if (index(argv,'-noFlip' ) .gt. 0 ) then
             lon_flip = .false.
         else if (index(argv,'-g5') .gt. 0 ) then
               vectype = 5
         else if (index(argv,'-o' ) .gt. 0 ) then
            if ( iarg+1 .gt. argc ) call usage()
            iarg = iarg + 1
            call GetArg ( iArg, ana_sfc_tn )  ! not ana_sfc_fn
         else
            n = n + 1
            if ( n .eq. 1 ) then
                 ana_eta_fn = trim(argv)
            else if ( n .eq. 2 ) then
                 bkg_eta_fn = trim(argv)
            else if ( n .eq. 3 ) then
                 bkg_sfc_tn = trim(argv)
            else
             call usage()
          end if
       end if
      end do

      if ( trim(ana_sfc_tn) .eq. 'ana5sfc.nc4' ) then
         i = index ( ana_eta_fn, '.eta.' )
         if ( i .gt. 1 ) then
            ana_sfc_tn = ana_eta_fn(1:i-5) // '.ana.sfc.' // ana_eta_fn(i+5:)
         end if
      end if

      if ( n .eq. 0 ) then
         call warn ( myname, 'missing first guess file name' )
         call usage()
      end if

      end subroutine Init_

subroutine fix_names_()
      implicit NONE
      integer :: i, rc
      character(len=255) :: prefix
      i = index ( ana_eta_fn, '.' ) - 1
      if ( i .gt. 0 ) then
         prefix = ana_eta_fn(1:i)
      else
         prefix = 'unknown'
      end if
      call FileResolv (trim(prefix), nymd, nhms, bkg_sfc_tn, bkg_sfc_fn )
      call strTemplate ( ana_sfc_fn, trim(ana_sfc_tn), 'GRADS', trim(prefix), &
                         nymd, nhms, rc )
      if ( rc /= 0 ) call die ( myname, 'cannot form sfc file name' )
      if ( first ) then
      print *
      print *, '------------------------------------------------------------------'
      print *, '     Background Eta file: ', trim(bkg_eta_fn)
      print *, '       Analysis Eta file: ', trim(ana_eta_fn)
      print *, '   Surface Analysis file: ', trim(ana_sfc_fn)
      end if
      print *, '     Surface Diagnostics: ', trim(bkg_sfc_fn), ' at ', nymd, nhms
!!!      print *, '-------------------------------------------------------------------'
!!!      print *
end subroutine fix_names_

    end program Ana5sfc

!.......................................................................

      subroutine usage()
      use m_die
      print *
      print *, 'USAGE:  '
      print *
      print *, ' ana5sfc.x [-v]  [-o ana.sfc]  ana.eta  bkg.eta bkg.sfc'
      print *
      print *, 'where'
      print *
      print *, ' anal.eta    input ana.eta file name'
      print *, ' bkg.eta     input bkg.eta file name'
      print *, ' bkg.sfc     input nkg.sfc file name'
      print *, ' -g5         needed to handle geos-5 bkg/ana.eta files'
      print *, ' -o ana.sfc  optional output ana.sfc file name; default is'
      print *, '             $expid.ana.sfc.$time.nc4, where $expid and'
      print *, '             $time are derived from the input "ana.eta" file'
      print *
      print *, ' -noFlip     does not flip longitude of diag files; default'
      print *, '             is to do so. '
      print *
      print *, ' -v        verbose mode'
      print *
      call die('ana5sfc','not enough arguments' )
      end subroutine usage

!.......................................................................


      subroutine tropopause (alpha,im,jm,levels,ple,pl, &
                             ptop,TropP,tmpu,TropT,sphu,TropQ, &
                             O3, Tto3, delp, undef, grav )

!********************************************************************
!!                     Subroutine tropopause
!********************************************************************
!
!!ROUTINE:  tropopause
!
!!DESCRIPTION:
!
!     This routine finds the tropopause pressure and temperature from
!     model temperature profiles.  It finds these values for a
!     given 3-dimensional grid.  The algorithm is based on the
!     temperature profile only.  It is similar to visually inspecting
!     a Skew-T Log P diagram for the left-most kink in the temperature
!     profile.  In a standard Skew-T Log P diagram the isotherms
!     intersect the Log P scale at a 45 degree angle.  To more
!     effectively isolate the tropopause,  this angle can be adjusted.
!     ThatÕs how this program works.  It adjusts the angle at which
!     the isotherms intersect the Log P axis via the coeffiecient
!     Òalpha.Ó It simply looks for the furthest point to the left on
!     the profile.  The routine defines the leftward position of each
!     temperature point as xfact:
!
!         xfact = (alpha * temperature) - log (pressure)
!
!     The tropopause for a given profile is where xfact is a minimum.
!     Uncertainty can occur when the kink is not very distinct.  This
!     is where the selection of alpha becomes important.  Optimal
!     values for alpha appear to be between .02 and .04.  For lower
!     values of alpha, the tropopause selection will favor lower
!     levels (higher P).  For higher values of alpha, the selection
!     will favor higher levels (lower P).  A value of alpha = .03
!     appears to be optimal in generating tropopause values within
!     the range indicated in the Handbook of Geophysics and the Space
!     Environment, AFGL, chapt 14, 1985.
!
!!INPUT PARAMETERS:
!
!     alpha   = see discussion above ((log mb)/deg K)
!     im      = number of longitude grid points
!     jm      = number of latitude grid points
!     levels  = number of model levels
!     ple     = pressures at model edges
!     pl      = pressures at model mid-layers
!     Ptop    = pressure at the top model edge (mb)
!     TmpU    = 3-d array of gridded temperature on model level (deg K)
!
!!OUTPUT PARAMETERS:
!
!     TropP = array of gridded tropopause pressures (pl units)
!     TropT = array of gridded tropopause temperatures (deg K)
!     TropQ = array of gridded tropopause specific humidity (sphu units)
!
!!REVISION HISTORY:
!
!     Created 25 Jun 97 by Jim Stobie
!     Mods:   23 Jul 97 by L.Takacs
!          1) Send in Ps-Ptop (rather than Ps)
!          2) Use  log10 (average pressure) rather than average 
!             (log10(pressure) )
!
!             05 Feb 2002 by da Silva
!             Eliminated getcon() dependecy; ps_top not needed here
!             Change order of pl/ple indices (i,j,k)->i,k,j)
!
!             18 Feb 2002 by S. Nebuda
!             Added support for pressure input in pascals
!
!********************************************************************

      implicit none

      integer    levmax
      parameter (levmax=1000)


! Passed Variables

      integer im
      integer jm
      integer levels

      real alpha
      real Ptop
      real pl   (im,levels,jm)    ! Pressure at model mid-layers
      real ple  (im,levels+1,jm)  ! Pressure at model edges
      real TmpU (im,jm,levels)
      real, intent(out) :: TropP(im,jm)
      real, intent(out) :: TropT(im,jm)
      real Sphu(im,jm,levels)
      real, intent(out) :: TropQ(im,jm)
      real O3(im,jm,levels)
      real delp(im,jm,levels)
      real, intent(out) :: Tto3(im,jm)
!!!   integer, intent(out), OPTIONAL :: TropK(im,jm) ! tropopause index

! Local Variables

      integer i,j,k          !loop variables
      integer kend           !end index for model level search
      integer kstart         !start index for model level search
      integer ktrop          !index for tropopause level

      real phigh             !highest pressure for search
      real plow              !lowest pressure for search
      real cfac              !if input is in pascals, conversion to mb
      real undef             !value for undefined variables
      real grav
      real xfacmn            !minimum x-factor, see prologue
      real xfact(levmax)     !x-factor, see prologue

      integer kk
      real mu, Mair, Vd, factor

!----------------------------------------------------------------
! Get value for undefined variables from funciton getcon
!---------------------------------------------------------------

!!!      undef = getcon('UNDEF')
!!!       undef = 1.E25

!----------------------------------------------------------------
! Set vertical limits on search.  Tropopause search will be
! limited to the range between plow and phigh (mb).
! According to Handbook of Geophysics and the Space Environment,
! AFGL, 1985, pg 14-6, the lowest tropopause values are near 8 km
! in the polar winter (approx 350 mb) and the highest near 18 km
! in the tropics (approx 80 mb).
!----------------------------------------------------------------

        plow  = 40.
        phigh = 550.
        cfac = 1.

        if (pl(1,levels,1) .gt. 2000.) then	! input in pascals
          plow  = plow * 100.
          phigh = phigh * 100.
          cfac = 0.01
        endif

!----------------------------------------------------------------
! If this particular run does not go up to plow, then write a
! warning message and fill TropT and TropP with undefined data
! values.
!----------------------------------------------------------------

      if (plow.LT.Ptop) then

!!!	write(6,1000) Ptop,plow

        do j = 1, jm
	 do i = 1, im
!!!	    if ( present(tropk) ) TropK(i,j) = -1
	    TropT(i,j) = undef
	    TropP(i,j) = undef
            TropQ(i,j) = undef

        end do
        end do

      else

!--------------------
! Loop over lat/lon Grid
!--------------------

        do j = 1, jm
        do i = 1, im

!-------------------------------------------------------
! Find pressure range for search.  Search will begin
! at first model level edge above phigh, and end
! at the first model level edge below plow.
!-------------------------------------------------------

               kend = levels
            do while (ple(i,kend,j).GE.phigh)
               kend = kend-1
            enddo

               kstart = 1
            do while (ple(i,kstart,j).le.plow)
               kstart = kstart+1
            enddo

!-----------------------------------------------------
! Calculate pressure of the model layer midpoints.
! Then calculate xfact for these points.  See prologue
! for description of xfact.
!-----------------------------------------------------

            do k = kstart, kend
              xfact(k) = alpha * TmpU(i,j,k) - log10(pl(i,k,j)*cfac)
            end do

!-----------------------------------------------
! Tropopause is level for which xfact is minimum
!-----------------------------------------------

            xfacmn = 100000.

            do k = kstart, kend

              if (xfact(k).LT.xfacmn) then
                xfacmn = xfact(k)
                ktrop = k
              end if

            end do
!-------------------------------------------------------
! If the minimum value of xfact is at the upper or lower
! boundary of the search, then the tropopause has not
! been sucessfully isolated.  In this case a warning
! message is printed and the grid point value filled
! with the undefined value.
!-------------------------------------------------------

            if(ktrop.EQ.kstart.or.ktrop.EQ.kend) then

!             write(6,1100) i,j
              !!! if ( present(tropk) ) TropK(i,j) = -1
              tropp(i,j) = undef
              tropt(i,j) = undef
              TropQ(i,j) = undef
              tto3(i,j) = undef

            else

!------------------------------------------------------
! If the tropopause has been successfully isolated
!     store tropopause pressure    in TropP
! and store tropopause temperature in TropT.
!------------------------------------------------------

              !!! if ( present(tropk) ) tropk(i,j) =  ktrop
              tropp(i,j) =   pl(i,ktrop,j)
              tropt(i,j) = TmpU(i,j,ktrop)
              TropQ(i,j) = SphU(i,j,ktrop)

              tto3(i,j) = 0.0 ! tropospheric O3 (column)
              do kk = ktrop, levels
                 tto3(i,j) = tto3(i,j) + O3(i,j,kk) * delp(i,j,kk) / grav
              end do

            end if

          end do

        end do

      end if

      return

 1000 format("WARNING.  Can't find tropopause.  Top of model", &
            " is too low (",f4.0," mb).  Tropfind requires", &
            " model top to be ",f4.0," mb")

 1100 format("WARNING.  Can't find tropopause at gridpoint:", &
            "  i = ",i4,"  j = ",i4)

    end subroutine tropopause

      subroutine d2a(u,v,im,jm,jfirst,jlast,coslon,sinlon)
! This is primarily for turbulence package designed for A-grid.
! Also used for output to A-grid.
! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1; removed fvcore.h
! WS 99.07.26 : Added jfirst and jlast as arguments

      implicit none
      integer im, jm, jfirst, jlast
! WS 99.07.26 : u must be ghosted N2S1
      real u(im,jm),v(im,jm),ua(im,jm),va(im,jm),coslon(im),sinlon(im)

      integer   imh
      real r16
      parameter (r16 = 1./16.)

      integer i, j, js, jn, im1
      real un, vn, us, vs

! Convert D-grid winds to A-grid
! u --> ua, v --> va

      real utmp(im,jm),vtmp(im,jm)

      imh = im/2

      js = 3
      jn = jm - js + 1
      im1 = im-1

      do 30 j=2,js-1
      do 30 i=1,im1
30    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 35 j=2,js-1
35    vtmp(im,j) = 0.5*(v(im,j) + v(1,j))

      do 45 j=jn+1,jm-1
      do 45 i=1,im1
45    vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))

      do 50 j=jn+1,jm-1
50    vtmp(im,j) = 0.5*(v(im,j) + v(1,j))

      do 60 j=js,jn
      do 60 i=2,im-2
      vtmp(i,j) = ( 9.*(v(i,  j) + v(i+1,j)) - &
                       (v(i-1,j) + v(i+2,j))  ) * r16
60    continue

      do 70 j=js,jn
      vtmp(1,j) = ( 9.*(v(1,j) + v(2,j)) - &
                       (v(im,j) + v(3,j))  ) * r16
      vtmp(im,j) = ( 9.*(v(im,j) + v(1,j)) - &
                       (v(im1,j) + v(2,j))  ) * r16
      vtmp(im1,j) = ( 9.*(v(im1,  j) + v(im,j)) - &
                       (v(im-2,j) + v(1,j))  ) * r16
70    continue

! WS 990726 :  Moved loop 25 down here for clarity
      do j=3,jm-2
      do i=1,im
      utmp(i,j) = ( 9.*(u(i,j+1)+u(i,j)) - &
                       (u(i,j+2)+u(i,j-1)) ) * r16
      enddo
      enddo

! WS 990726 :  Added condition to decide if poles are on this processor

      IF ( jfirst .EQ. 1 ) THEN
! Projection at  SP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,2) = 0.5*(u(i,2) + u(i,3))
      enddo

      us = 0.
      vs = 0.
      do i=1,imh
      us = us + (utmp(i+imh,2)-utmp(i,2))*sinlon(i) &
              + (vtmp(i,2)-vtmp(i+imh,2))*coslon(i)
      vs = vs + (utmp(i+imh,2)-utmp(i,2))*coslon(i) &
              + (vtmp(i+imh,2)-vtmp(i,2))*sinlon(i)
      enddo

! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1
      us = us/im
      vs = vs/im

      do i=1,imh
      ua(i,1)   = -us*sinlon(i) - vs*coslon(i)
      va(i,1)   =  us*coslon(i) - vs*sinlon(i)
      ua(i+imh,1)   = -ua(i,1)
      va(i+imh,1)   = -va(i,1)
      enddo

      ENDIF

      IF ( jlast .EQ. jm ) THEN
! Projection at  NP
! WS 990726 :  Moved utmp SP treatment to SP section
      do i=1,im
      utmp(i,jm-1) = 0.5*(u(i,jm-1) + u(i,jm))
      enddo

      un = 0.
      vn = 0.
      do i=1,imh
      un = un + (utmp(i+imh,jm-1)-utmp(i,jm-1))*sinlon(i) &
              + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*coslon(i)
      vn = vn + (utmp(i,jm-1)-utmp(i+imh,jm-1))*coslon(i) &
              + (vtmp(i+imh,jm-1)-vtmp(i,jm-1))*sinlon(i)
      enddo

! WS 99.05.25 : Replaced IMR by IM, JMR by JM-1
      un = un/im
      vn = vn/im

      do i=1,imh
      ua(i,jm) = -un*sinlon(i) + vn*coslon(i)
      va(i,jm) = -un*coslon(i) - vn*sinlon(i)
      ua(i+imh,jm) = -ua(i,jm)
      va(i+imh,jm) = -va(i,jm)
      enddo

      ENDIF

      do 100 j=2,jm-1
      do 100 i=1,im
      ua(i,j) = utmp(i,j)
100   va(i,j) = vtmp(i,j)

      u = ua
      v = va

      return
      end subroutine d2a




