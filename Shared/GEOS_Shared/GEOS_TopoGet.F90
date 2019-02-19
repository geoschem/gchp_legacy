! $Id$
! _VERIFY and _RETURN macros for error handling

#include "MAPL_Generic.h"

!BOP

! !MODULE: GEOS_TopoGetMod -- The topography computation

! !INTERFACE:

module GEOS_TopoGetMod

! !USES:

  use ESMF
  use MAPL_Mod
  
  implicit none
  private

! !PUBLIC ROUTINES:

  public GEOS_TopoGet

! !DESCRIPTION:

!  This module computes the Earth's topography associated with
!  an input ESMF grid.  The mean height topography was averaged
!  from the 30"x30" GTOPO30 dataset obtained from:
!
!          http://edcdaac.usgs.gov/gtopo30/gtopo30.html 
!
!  using a box stencil with gaussian weights, retaining scales >= 100 km.
!  In addition to mean heights, the routine can also return isotropic and
!  directional variances associated with Gravity-Wave-Drag scales (10-100 km) 
!  and turbulence scales (0-10 km).


!EOP

contains

!==========================================================================

!BOP

! !IROUTINE GEOS_TopoGet -- Gets Topographic Variables

! !INTERFACE

  subroutine GEOS_TopoGet ( cf,                          &
                            MEAN,                        &
                            GWDVAR,   GWDVARX,  GWDVARY, &
                            GWDVARXY, GWDVARYX, TRBVAR, RC)

! !ARGUMENTS

    type(ESMF_Config)                            :: cf
    type(ESMF_Field),    optional, intent(INOUT) :: MEAN
    type(ESMF_Field),    optional, intent(INOUT) :: GWDVAR
    type(ESMF_Field),    optional, intent(INOUT) :: GWDVARX
    type(ESMF_Field),    optional, intent(INOUT) :: GWDVARY
    type(ESMF_Field),    optional, intent(INOUT) :: GWDVARXY
    type(ESMF_Field),    optional, intent(INOUT) :: GWDVARYX
    type(ESMF_Field),    optional, intent(INOUT) :: TRBVAR
    integer,             optional, intent(OUT)   :: RC

! !DESCRIPTION

!  This subroutine creates topographic data associated with an input
!  ESMF grid.  The available topographic data types are:  MEAN, GWDVAR,
!  GWDVARX, GWDVARY, GWDVARXY, GWDVARYX, and TRBVAR.  The raw data
!  for each of these types has been pre-processed and stored at 2.5'x2.5'
!  resolution.  The resulting gridded data will be binned-averaged on the
!  input ESMF grid from the 2.5'x2.5' data.
!  The arguments are:
!
! \begin{description}
!   \item[GRID]
!                   The ESMF GRID which contains information about 
!                   horizontal grid structure.
!   \item[MEAN]
!                   The mean values of topography with scales >= 100 km.
!   \item[GWDVAR]
!                   The isotropic variance of the GWD topography data,
!                   (scales 10-100 km).
!   \item[GWDVARX]
!                   The variance of the GWD topography data in the
!                   East - West direction.
!   \item[GWDVARY]
!                   The variance of the GWD topography data in the
!                   North - South direction.
!   \item[GWDVARXY]
!                   The variance of the GWD topography data in the
!                   South_West - North_East direction.
!   \item[GWDVARYX]
!                   The variance of the GWD topography data in the
!                   North_West - South_East direction.
!   \item[TRBVAR]
!                   The isotropic variance of the Turbulence topography data,
!                   (scales 1-10 km).
!   \item[RC]
!                   Return code
! \end{description}
!

!EOP

! Locals

    character(len=ESMF_MAXSTR), parameter :: IAm = "TopoGet"
    character(len=ESMF_MAXSTR)            :: filename(7)

    real, pointer :: ptr(:,:)
    integer       :: STATUS
    integer       :: unit
    real          :: GWDFAC
    real          :: GWDFACX
    real          :: GWDFACY
    real          :: GWDFACXY
    real          :: GWDFACYX
    real          :: TRBFAC

    type(MAPL_MetaComp) :: MAPLOBJ

! Initialize CONFIG File into MAPL Object
! ---------------------------------------
    call MAPL_Set (MAPLOBJ, name='DUMMY', cf=cf, rc=STATUS )
    _VERIFY(STATUS)

! Get filenames for Get_Topo utility
! ----------------------------------
    call ESMF_ConfigGetAttribute ( cf, value=filename(1), label ='TOPO_MEAN_FILE:',     &
                                   default='hmean.2.5x2.5min.data', rc=status )
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute ( cf, value=filename(2), label ='TOPO_GWDVAR_FILE:',   &
                                   default='hgrav_var.2.5x2.5min.data', rc=status )
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute ( cf, value=filename(3), label ='TOPO_GWDVARX_FILE:',  &
                                   default='hgrav_varx.2.5x2.5min.data', rc=status )
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute ( cf, value=filename(4), label ='TOPO_GWDVARY_FILE:',  &
                                   default='hgrav_vary.2.5x2.5min.data', rc=status )
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute ( cf, value=filename(5), label ='TOPO_GWDVARXY_FILE:', &
                                   default='hgrav_varxy.2.5x2.5min.data', rc=status )
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute ( cf, value=filename(6), label ='TOPO_GWDVARYX_FILE:', &
                                   default='hgrav_varyx.2.5x2.5min.data', rc=status )
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute ( cf, value=filename(7), label ='TOPO_TRBVAR_FILE:',   &
                                   default='hturb_var.2.5x2.5min.data', rc=status )
    _VERIFY(STATUS)

  if( present(MEAN)  ) then
! -------------------------
       UNIT = GETFILE  ( filename(1),form="unformatted" )
       call MAPL_VarRead (UNIT,MEAN)
       CALL FREE_FILE    (UNIT)
       call ESMF_FieldGet(MEAN, 0, PTR, rc=status)
       ptr = ptr*MAPL_GRAV
  endif

  if( present(GWDVAR) ) then
! --------------------------
       call MAPL_GetResource( MAPLOBJ, GWDFAC, label="GWDVAR_FACTOR:",  default = 1.0, RC=STATUS )
       _VERIFY(STATUS)
       UNIT = GETFILE  (filename(2), form="unformatted")
       call MAPL_VarRead (UNIT,GWDVAR)
       CALL FREE_FILE    (UNIT)
       call ESMF_FieldGet (GWDVAR, 0, PTR, rc = status)
       ptr = sqrt( max(gwdfac*ptr,0.0) )
  endif

  if( present(GWDVARX) ) then
! ---------------------------
       call MAPL_GetResource( MAPLOBJ, GWDFACX, label="GWDVARX_FACTOR:",  default = 1.0, RC=STATUS )
       _VERIFY(STATUS)
       UNIT = GETFILE  (filename(3), form="unformatted")
       call MAPL_VarRead (UNIT,GWDVARX)
       CALL FREE_FILE    (UNIT)
       call ESMF_FieldGet (GWDVARX, 0, PTR, rc = status)
       ptr = sqrt( max(gwdfacx*ptr,0.0) )
  endif

  if( present(GWDVARY) ) then
! ---------------------------
       call MAPL_GetResource( MAPLOBJ, GWDFACY, label="GWDVARY_FACTOR:",  default = 1.0, RC=STATUS )
       _VERIFY(STATUS)
       UNIT = GETFILE  (filename(4), form="unformatted")
       call MAPL_VarRead (UNIT,GWDVARY)
       CALL FREE_FILE    (UNIT)
       call ESMF_FieldGet (GWDVARY, 0, PTR, rc = status)
       ptr = sqrt( max(gwdfacy*ptr,0.0) )
  endif

  if( present(GWDVARXY) ) then
! ----------------------------
       call MAPL_GetResource( MAPLOBJ, GWDFACXY, label="GWDVARXY_FACTOR:",  default = 1.0, RC=STATUS )
       _VERIFY(STATUS)
       UNIT = GETFILE  (filename(5), form="unformatted")
       call MAPL_VarRead (UNIT,GWDVARXY)
       CALL FREE_FILE    (UNIT)
       call ESMF_FieldGet (GWDVARXY, 0, PTR, rc = status)
       ptr = sqrt( max(gwdfacxy*ptr,0.0) )
  endif

  if( present(GWDVARYX) ) then
! ----------------------------
       call MAPL_GetResource( MAPLOBJ, GWDFACYX, label="GWDVARYX_FACTOR:",  default = 1.0, RC=STATUS )
       _VERIFY(STATUS)
       UNIT = GETFILE  (filename(6), form="unformatted")
       call MAPL_VarRead (UNIT,GWDVARYX)
       CALL FREE_FILE    (UNIT)
       call ESMF_FieldGet (GWDVARYX, 0, PTR, rc = status)
       ptr = sqrt( max(gwdfacyx*ptr,0.0) )
  endif

  if( present(TRBVAR) ) then
! --------------------------
       call MAPL_GetResource( MAPLOBJ, TRBFAC, label="TRBVAR_FACTOR:",  default = 1.0, RC=STATUS )
       _VERIFY(STATUS)
       UNIT = GETFILE  (filename(7), form="unformatted")
       call MAPL_VarRead (UNIT,TRBVAR)
       CALL FREE_FILE    (UNIT)
       call ESMF_FieldGet (TRBVAR, 0, PTR, rc = status)
       ptr = max(trbfac*ptr,0.0)
  endif

    return
  end subroutine GEOS_TopoGet

end module GEOS_TopoGetMod
