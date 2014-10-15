!===============================================================================
! CVS: $Id: shr_kind_mod.F90,v 1.3 2008-04-13 20:26:51 wsawyer Exp $
! CVS: $Source: /cvsroot/esma/esma/src/Shared/GMAO_pilgrim/shr_kind_mod.F90,v $
! CVS: $Name: jk-G40-GEOSctm-advcore-update3 $
!===============================================================================

MODULE shr_kind_mod

   !----------------------------------------------------------------------------
   ! precision/kind constants add data public
   !----------------------------------------------------------------------------
   public
   integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real
   integer,parameter :: SHR_KIND_R4 = selected_real_kind( 6) ! 4 byte real
   integer,parameter :: SHR_KIND_RN = kind(1.0)              ! native real
   integer,parameter :: SHR_KIND_I8 = selected_int_kind (13) ! 8 byte integer
   integer,parameter :: SHR_KIND_I4 = selected_int_kind ( 6) ! 4 byte integer
   integer,parameter :: SHR_KIND_IN = kind(1)                ! native integer

END MODULE shr_kind_mod
