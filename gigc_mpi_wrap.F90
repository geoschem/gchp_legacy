gal/#if defined( ESMF_ )
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: gigc_mpi_wrap
!
! !DESCRIPTION: Module GIGC\_MPI\_WRAP is the module containing MPI-based
!  routines used for the ESMF interface to the Grid-Independent
!  GEOS-Chem (aka "GIGC").
!\\
!\\
! !INTERFACE:
!
MODULE GIGC_Mpi_Wrap
!
! !USES:
!
  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: GIGC_Input_Bcast
  PUBLIC :: GIGC_Idt_Bcast
  PUBLIC :: GIGC_Reader_Bcast
  PUBLIC :: GIGC_Readchem_Bcast
  PUBLIC :: GIGC_Bcast_Char 
  PUBLIC :: GIGC_Bcast_Int
  PUBLIC :: GIGC_Bcast_Real8
  PUBLIC :: mpiComm
!
! !REMARKS:
!  These routines are needed to broadcast values read in from ASCII input
!  (which are read only on the root CPU) to all other CPUs.
!                                                                             .
!  NOTE: If you add values to a derived type object (e.g. Input_Opt), then
!  you will also have to add the proper calls so that the extra fields will
!  get broadcasted to other CPUs.
!                                                                             .
!  NOTE: The SMVGEAR init functions READER and READCHEM touch several
!  variables and arrays in a very convoluted manner.  It may be difficult
!  to try to call these on the root CPU and then broadcast to all other
!  CPUs.  For now just call READER and READCHEM on all CPUs (bmy, 3/7/13)
!
! !REVISION HISTORY:
!  03 Jan 2013 - M. Long     - Initial version
!  07 Mar 2013 - R. Yantosca - Added more ProTeX headers + comments
!EOP
!------------------------------------------------------------------------------
!BOC
  INTEGER, SAVE :: mpiComm

CONTAINS
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_input_bcast
!
! !DESCRIPTION: Routine GIGC\_INPUT\_BCAST broadcasts contents of the Input_Opt
!  read in from the input.geos\_\_\_.rc input file from the Root process to all
!  processes in the MPI_COMM_WORLD.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Gigc_Input_Bcast( am_I_Root, Input_Opt, RC )
!
! !USES:
!
    USE CMN_SIZE_Mod
    USE Drydep_Mod
    USE Input_Opt_Mod,       ONLY : OptInput
    USE ErrCode_Mod,         ONLY : GC_SUCCESS
    USE M_MPIF
!
! !INPUT PARAMETERS:
!
    LOGICAL,        INTENT(IN)    :: am_I_Root   ! Are we on the root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(OptInput), INTENT(INOUT) :: Input_Opt   ! Input Options object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT)   :: RC          ! Success or failure
!
! !REMARKS:
!  In lieu of using MPI_Type_Struct in order to broadcast a derived type
!  object, which would be simpler, the choice was made to individually
!  parse out individual elements of the Input_Opt object. In this way
!  it is more straight forward and easily interpreted by future GIGC
!  users without much MPI experience. The intent of this routine should
!  be self-evident without reference to MPI dicumentation.
!  MSL - 04 Jan 2013
!
!  NOTE: Don't do an MPI Broadcast on Input_Opt%myCpu, as this will be
!  set directly from the GEOSCHEMchem_GridCompMod.F90 Initialize, Run
!  and Finalize routines.
!
! !REVISION HISTORY:
!  04 Jan 2013 - M. Long     - Initial version
!  28 Feb 2013 - R. Yantosca - Now MPI BCast the Input_Opt%haveImpRst field
!  18 Mar 2013 - R. Yantosca - Mow MPI Bcast the Input_Opt%LINOZ_* fields
!  11 Apr 2013 - R. Yantosca - Now MPI Bcast extra fields in Input_Opt
!  03 Jun 2013 - R. Yantosca - Now MPI Bcast the GAMMA_HO2 field of Input_Opt
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER :: COUNT
!
! mpi_bcast (buffer, count, datatype, root, comm, ier)
!
    RC = GC_SUCCESS

    !----------------------------------------
    ! General Runtime & Distributed Comp Info
    !----------------------------------------
    ! none yet.

    !----------------------------------------
    ! SIZE PARAMETER fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%MAX_DIAG, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%MAX_FAM,  1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%MAX_SPC,  1, mpi_integer, 0, mpiComm, RC )

    !----------------------------------------
    ! SIMULATION MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%NYMDb, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NHMSb, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NYMDe, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NHMSe, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%RUN_DIR, len(INPUT_OPT%RUN_DIR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%IN_RST_FILE, len(INPUT_OPT%IN_RST_FILE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%DATA_DIR, len(INPUT_OPT%DATA_DIR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%CHEM_INPUTS_DIR, len(INPUT_OPT%CHEM_INPUTS_DIR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%RES_DIR, len(INPUT_OPT%RES_DIR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%GCAP_DIR, len(INPUT_OPT%GCAP_DIR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%GEOS_4_DIR, len(INPUT_OPT%GEOS_4_DIR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%GEOS_5_DIR, len(INPUT_OPT%GEOS_5_DIR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%GEOS_FP_DIR, len(INPUT_OPT%GEOS_FP_DIR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%MERRA_DIR, len(INPUT_OPT%MERRA_DIR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%DATA_DIR_1x1, len(INPUT_OPT%DATA_DIR_1x1), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TEMP_DIR, len(INPUT_OPT%TEMP_DIR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LUNZIP, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LWAIT, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LVARTROP, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NESTED_I0, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NESTED_J0, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%HcoConfigFile, len(INPUT_OPT%HcoConfigFile), mpi_character, 0, mpiComm, RC )

    !----------------------------------------
    ! TRACER MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%N_ADVECT, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%AdvectSpc_Name(:), (255)*INPUT_OPT%MAX_SPC, mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSPLIT, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_A_RnPbBe_SIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_A_FULLCHEM_SIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_A_HCN_SIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_A_TAGCO_SIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_A_C2H6_SIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_A_CH4_SIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_AN_AEROSOL_SIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_A_MERCURY_SIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_A_CO2_SIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_A_H2HD_SIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_A_POPS_SIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ITS_NOT_COPARAM_OR_CH4, 1, mpi_logical, 0, mpiComm, RC )

    !----------------------------------------
    ! AEROSOL MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%LSULF, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LCRYST, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LCARB, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSOA, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LDUST, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LDEAD, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSSALT, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LDICARB, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%SALA_REDGE_um(:), 2, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%SALC_REDGE_um(:), 2, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LGRAVSTRAT, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSOLIDPSC,  1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LHOMNUCNAT, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%T_NAT_SUPERCOOL, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%P_ICE_SUPERSAT,  1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LPSCCHEM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSTRATOD, 1, mpi_logical, 0, mpiComm, RC )

    !----------------------------------------
    ! EMISSIONS MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%LEMIS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TS_EMIS, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LBIOFUEL, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LOTDLOC,  1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSOILNOX, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LWARWICK_VSLS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSSABr2, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LFIX_PBL_BRO, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LCH4EMIS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LCH4SBC, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LOCSEMIS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LCFCEMIS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LCLEMIS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LBREMIS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LN2OEMIS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LBASICEMIS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSETH2O, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSETCH4, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSETOCS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSETCFC, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSETCL, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LBRGCCM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSETBR, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSETBRSTRAT, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSETNOYSTRAT, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSETN2O, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSETH2SO4, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%CFCYEAR, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LFUTURECFC, 1, mpi_logical, 0, mpiComm, RC )


    !----------------------------------------
    ! CO2 MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%LCHEMCO2, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LBIODIURNAL, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LBIONETCLIM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LPLANE, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LFFBKGRD, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LBIOSPHTAG, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LFOSSILTAG, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSHIPTAG, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LPLANETAG, 1, mpi_logical, 0, mpiComm, RC )


    !----------------------------------------
    ! FUTURE MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%LFUTURE, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%FUTURE_YEAR, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%FUTURE_SCEN, len(INPUT_OPT%FUTURE_SCEN), mpi_character, 0, mpiComm, RC )

    !----------------------------------------
    ! CHEMISTRY MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%LCHEM,     1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSCHEM,    1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LLINOZ,    1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TS_CHEM,   1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_BCast( INPUT_OPT%GAMMA_HO2, 1, mpi_real8,   0, mpicomm, RC )
    CALL MPI_Bcast( INPUT_OPT%LUCX,      1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LCH4CHEM,  1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LACTIVEH2O, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LO3FJX,     1, mpi_logical, 0, mpiComm, RC )

    !----------------------------------------
    ! TRANSPORT MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%LTRAN, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LFILL, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TPCORE_IORD, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TPCORE_JORD, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TPCORE_KORD, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TS_DYN, 1, mpi_integer, 0, mpiComm, RC )

    !----------------------------------------
    ! CONVECTION MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%LCONV, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LTURB, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LNLPBL, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TS_CONV, 1, mpi_integer, 0, mpiComm, RC )

    !----------------------------------------
    ! DEPOSITION MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%PBL_DRYDEP, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LDRYD, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LWETD, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%USE_OLSON_2001, 1, mpi_logical, 0, mpiComm, RC )

    !----------------------------------------
    ! GAMAP MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%GAMAP_DIAGINFO, len(INPUT_OPT%GAMAP_DIAGINFO), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%GAMAP_TRACERINFO, len(INPUT_OPT%GAMAP_TRACERINFO), mpi_character, 0, mpiComm, RC )

    !----------------------------------------
    ! OUTPUT MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%NJDAY(:), 366, mpi_integer, 0, mpiComm, RC )
    
    !----------------------------------------
    ! DIAGNOSTIC MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%ND01, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD01, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND02, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD02, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND03, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD03, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND04, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD04, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND05, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD05, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND06, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD06, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND07, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD07, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND08, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD08, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND09, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD09, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND10, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD10, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND11, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD11, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND12, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD12, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND13, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD13, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND14, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD14, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND15, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD15, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND16, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD16, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND17, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD17, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND18, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD18, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND19, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD19, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND20, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD20, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND21, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD21, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND22, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD22, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND23, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD23, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND24, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD24, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND25, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD25, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND26, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD26, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND27, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD27, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND28, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD28, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND29, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD29, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND30, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD30, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND31, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD31, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND32, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD32, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND33, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD33, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND34, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD34, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND35, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD35, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND36, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD36, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND37, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD37, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND38, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD38, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND39, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD39, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND40, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD40, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND41, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD41, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND42, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD42, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND43, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD43, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND44, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD44, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND45, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD45, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND46, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD46, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND47, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD47, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND48, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD48, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND49, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD49, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND50, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD50, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD51, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND52, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD52, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND53, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD53, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND54, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD54, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND55, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD55, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND56, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD56, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND57, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD57, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND58, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD58, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND59, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD59, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND60, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD60, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND61, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD61, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND62, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD62, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND63, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD63, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND64, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD64, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND66, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD66, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND67, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD67, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND68, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD68, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND69, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD69, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND70, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD70, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LPRT, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TINDEX(:,:), INPUT_OPT%MAX_DIAG*INPUT_OPT%MAX_SPC, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TCOUNT(:)  , INPUT_OPT%MAX_DIAG, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TMAX(:)    , INPUT_OPT%MAX_DIAG, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%DO_DIAG_WRITE, 1, mpi_logical, 0, mpiComm, RC )

    !----------------------------------------
    ! PLANEFLIGHT MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%DO_PF, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%PF_IFILE, len(INPUT_OPT%PF_IFILE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%PF_OFILE, len(INPUT_OPT%PF_OFILE), mpi_character, 0, mpiComm, RC )

    !----------------------------------------
    ! ND48 MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%DO_ND48, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND48_FILE, len(INPUT_OPT%ND48_FILE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND48_FREQ, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND48_N_STA, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND48_IARR(:), INPUT_OPT%MAX_SPC, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND48_JARR(:), INPUT_OPT%MAX_SPC, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND48_LARR(:), INPUT_OPT%MAX_SPC, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND48_NARR(:), INPUT_OPT%MAX_SPC, mpi_integer, 0, mpiComm, RC )

    !----------------------------------------
    ! ND49 MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%DO_ND49, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND49_FILE, len(INPUT_OPT%ND49_FILE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND49_TRACERS(:), INPUT_OPT%MAX_SPC, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND49_FREQ, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND49_IMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND49_IMAX, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND49_JMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND49_JMAX, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND49_LMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND49_LMAX, 1, mpi_integer, 0, mpiComm, RC )

    !----------------------------------------
    ! ND50 MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%DO_ND50, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND50_FILE, len(INPUT_OPT%ND50_FILE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LND50_HDF, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND50_TRACERS(:), INPUT_OPT%MAX_SPC, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND50_IMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND50_IMAX, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND50_JMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND50_JMAX, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND50_LMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND50_LMAX, 1, mpi_integer, 0, mpiComm, RC )

    !----------------------------------------
    ! ND51 MENU fields
     !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%DO_ND51, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51_FILE, len(INPUT_OPT%ND51_FILE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LND51_HDF, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51_TRACERS(:), INPUT_OPT%MAX_SPC, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51_HR_WRITE, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51_HR1, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51_HR2, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51_IMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51_IMAX, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51_JMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51_JMAX, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51_LMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51_LMAX, 1, mpi_integer, 0, mpiComm, RC )

    !----------------------------------------
    ! ND51b MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%DO_ND51b, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51b_FILE, len(INPUT_OPT%ND51b_FILE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LND51b_HDF, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51b_TRACERS(:), INPUT_OPT%MAX_SPC, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51b_HR_WRITE, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51b_HR1, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51b_HR2, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51b_IMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51b_IMAX, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51b_JMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51b_JMAX, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51b_LMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND51b_LMAX, 1, mpi_integer, 0, mpiComm, RC )

    !----------------------------------------
    ! ND63 MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%DO_ND63, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND63_FILE, len(INPUT_OPT%ND63_FILE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND63_FREQ, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND63_IMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND63_IMAX, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND63_JMIN, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND63_JMAX, 1, mpi_integer, 0, mpiComm, RC )

    !----------------------------------------
    ! PROD LOSS MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%DO_SAVE_PL, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND65, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LD65, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%DO_SAVE_O3, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NFAM, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%FAM_NAME, len(INPUT_OPT%FAM_NAME)*INPUT_OPT%MAX_FAM, mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%FAM_TYPE, len(INPUT_OPT%FAM_TYPE)*INPUT_OPT%MAX_FAM, mpi_character, 0, mpiComm, RC )

    !----------------------------------------
    ! UNIX CMDS fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%BACKGROUND, len(INPUT_OPT%BACKGROUND), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%REDIRECT, len(INPUT_OPT%REDIRECT), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%REMOVE_CMD, len(INPUT_OPT%REMOVE_CMD), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%SEPARATOR, len(INPUT_OPT%SEPARATOR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%WILD_CARD, len(INPUT_OPT%WILD_CARD), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%UNZIP_CMD, len(INPUT_OPT%UNZIP_CMD), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ZIP_SUFFIX, len(INPUT_OPT%ZIP_SUFFIX), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%SPACE, len(INPUT_OPT%SPACE), mpi_character, 0, mpiComm, RC )

    !----------------------------------------
    ! NESTED GRID MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%LWINDO, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LWINDO2x25, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LWINDO_NA, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TPBC_DIR_NA, len(INPUT_OPT%TPBC_DIR_NA), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LWINDO_EU, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TPBC_DIR_EU, len(INPUT_OPT%TPBC_DIR_EU), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LWINDO_CH, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TPBC_DIR_CH, len(INPUT_OPT%TPBC_DIR_CH), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LWINDO_CU, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%TPBC_DIR, len(INPUT_OPT%TPBC_DIR), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NESTED_TS, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NESTED_I1, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NESTED_J1, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NESTED_I2, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NESTED_J2, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NESTED_I0W, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%NESTED_J0W, 1, mpi_integer, 0, mpiComm, RC )

    !----------------------------------------
    ! BENCHMARK MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%LSTDRUN, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%STDRUN_INIT_FILE, len(INPUT_OPT%STDRUN_INIT_FILE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%STDRUN_FINAL_FILE, len(INPUT_OPT%STDRUN_FINAL_FILE), mpi_character, 0, mpiComm, RC )

    !----------------------------------------
    ! MERCURY MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%ANTHRO_Hg_YEAR, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%HG_SCENARIO, len(INPUT_OPT%Hg_SCENARIO), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%USE_CHECKS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LDYNOCEAN, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LPREINDHG, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LGTMM, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%GTMM_RST_FILE, len(INPUT_OPT%GTMM_RST_FILE), mpi_character, 0, mpiComm, RC )

    !----------------------------------------
    ! CH4 MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%LCH4BUD, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LGAO, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LCOL, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LLIV, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LWAST, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LBFCH4, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LRICE, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LOTANT, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LBMCH4, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LWETL, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LSOABS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LOTNAT, 1, mpi_logical, 0, mpiComm, RC )

    !----------------------------------------
    ! POPS MENU fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%POP_TYPE, 3, mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%CHEM_PROCESS, 1, mpi_logical, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%POP_XMW, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%POP_KOA, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%POP_KBC, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%POP_K_POPG_OH, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%POP_K_POPP_O3A, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%POP_K_POPP_O3B, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%POP_HSTAR, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%POP_DEL_H, 1, mpi_real8, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%POP_DEL_Hw, 1, mpi_real8, 0, mpiComm, RC )

    !----------------------------------------
    ! DRYDEP and DUST fields from input.geos
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%N_DUST_BINS, 1, mpi_integer,   0, mpiComm, RC )
 !   CALL MPI_Bcast( INPUT_OPT%NUMDEP,   1,         mpi_integer,   0, mpiComm, RC )
 !   CALL MPI_Bcast( INPUT_OPT%NDVZIND,  Input_Opt%MAX_DEP,   mpi_integer,   0, mpiComm, RC )

    !----------------------------------------
    ! GEOS-5 GCM INTERFACE fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%haveImpRst, 1, mpi_logical, 0, mpiComm, RC )

    !----------------------------------------
    ! LINOZ fields
    !----------------------------------------
   
    ! LINOZ array dimensions
    CALL MPI_Bcast( INPUT_OPT%LINOZ_NLEVELS, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LINOZ_NLAT,    1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LINOZ_NMONTHS, 1, mpi_integer, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%LINOZ_NFIELDS, 1, mpi_integer, 0, mpiComm, RC )

    ! LINOZ_TPARM array
    COUNT = INPUT_OPT%LINOZ_NLEVELS &
          * INPUT_OPT%LINOZ_NLAT    &
          * INPUT_OPT%LINOZ_NMONTHS & 
          * INPUT_OPT%LINOZ_NFIELDS

    CALL MPI_Bcast( INPUT_OPT%LINOZ_TPARM, COUNT, mpi_real8, 0, mpiComm, RC )

    !----------------------------------------
    ! Overhead O3 fields
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%USE_O3_FROM_MET, 1, mpi_logical, 0, mpiComm, RC )

    !----------------------------------------
    ! Diagnostics collection number
    !----------------------------------------
    CALL MPI_Bcast( INPUT_OPT%DIAG_COLLECTION,  1,                               mpi_integer,   0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%GC_RST_COLLECTION,  1,                             mpi_integer,   0, mpiComm, RC )
#if defined( NC_DIAG )
    CALL MPI_Bcast( INPUT_OPT%TRANSPORT_OUTPUT_TYPE, LEN(INPUT_OPT%TRANSPORT_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%WETSCAV_OUTPUT_TYPE, LEN(INPUT_OPT%WETSCAV_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%DRYDEP_OUTPUT_TYPE, LEN(INPUT_OPT%DRYDEP_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
!    CALL MPI_Bcast( INPUT_OPT%TRACER_CONC_OUTPUT_TYPE, LEN(INPUT_OPT%TRACER_CONC_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
!    CALL MPI_Bcast( INPUT_OPT%TRACER_EMIS_OUTPUT_TYPE, LEN(INPUT_OPT%TRACER_EMIS_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%MET_OUTPUT_TYPE, LEN(INPUT_OPT%MET_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND01_OUTPUT_TYPE, LEN(INPUT_OPT%ND01_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND02_OUTPUT_TYPE, LEN(INPUT_OPT%ND02_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND12_OUTPUT_TYPE, LEN(INPUT_OPT%ND12_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND14_OUTPUT_TYPE, LEN(INPUT_OPT%ND14_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND15_OUTPUT_TYPE, LEN(INPUT_OPT%ND15_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND16_OUTPUT_TYPE, LEN(INPUT_OPT%ND16_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND17_OUTPUT_TYPE, LEN(INPUT_OPT%ND17_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND18_OUTPUT_TYPE, LEN(INPUT_OPT%ND18_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND19_OUTPUT_TYPE, LEN(INPUT_OPT%ND19_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
    CALL MPI_Bcast( INPUT_OPT%ND30_OUTPUT_TYPE, LEN(INPUT_OPT%ND30_OUTPUT_TYPE), mpi_character, 0, mpiComm, RC )
#endif

  END SUBROUTINE GIGC_Input_Bcast
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_idt_bcast
!
! !DESCRIPTION: Routine GIGC\_IDT_BCAST broadcasts the tracer flags (IDTxxxx)
!  and species flags (IDxxxx), etc., that are set by the routines in
!  Headers/species_mod.F & Headers/species_index_mod.F.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Idt_Bcast( am_I_Root, Input_Opt, RC )
!
! !USES:
!
    USE Drydep_Mod
    USE Errcode_Mod
    USE Input_Opt_Mod, ONLY : OptInput
    USE M_MPIF
!
! !INPUT PARAMETERS:
!
    LOGICAL,        INTENT(IN)  :: am_I_Root   ! Are we on the root CPU?
    TYPE(OptInput), INTENT(IN)  :: Input_Opt   ! Input Options object
!
!
! !OUTPUT PARAMETERS:
!
    INTEGER,        INTENT(OUT) :: RC          ! Success or failure
!
! !REMARKS:
!
! !REVISION HISTORY:
!  04 Jan 2013 - M. Long     - Initial version
!  07 Mar 2013 - R. Yantosca - Added more ProTeX headers + comments
!   7 Mar 2013 - R. Yantosca - Reordered for clarity + cosmetic changes

    ! Return success
    RC = GC_SUCCESS

  END SUBROUTINE GIGC_Idt_Bcast
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_reader_bcast
!
! !DESCRIPTION: Routine GIGC\_READER\_BCAST performs an MPI broadcast for 
!  all of the namelist data that is read from the "mglob.dat" file
!  by routine GeosCore/reader.F.  This is one of the SMVGEAR input files.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Reader_Bcast( RC )
!
! !USES:
!
    USE Errcode_Mod
    USE M_MPIF
!
! !OUTPUT PARAMETERS:
!
    INTEGER, INTENT(OUT) :: RC   ! Success or failure
!
! !REMARKS:
!  NOTE: The READER (GeosCore/reader.F) subroutine is a tangled mess.  It not 
!  only reads values from mglob.dat but it also sets up other arrays used 
!  elsewhere in the SMVGEAR code.  It may just be simpler to run READER on
!  all CPUs so as not to have to worry about doing the MPI broadcast properly.
!  (bmy, 3/7/13)
!
! !REVISION HISTORY:
!  04 Jan 2013 - M. Long     - Initial version
!  07 Mar 2013 - R. Yantosca - Added ProTex header
!EOP
!------------------------------------------------------------------------------
!BOC

    ! Return success
    RC = GC_SUCCESS

  END SUBROUTINE GIGC_Reader_Bcast
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_readchem_bcast
!
! !DESCRIPTION: Routine GIGC\_READCHEM\_BCAST performs an MPI broadcast for 
!  data that is read from the SMVGEAR "globchem.dat" input file.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_ReadChem_Bcast( C1,    CSTRAT, CTROPL, CTROPS, CURBAN, &
                                  ININT, IORD,   NCOF,   RC              )
!
! !USES:
!
    USE Errcode_Mod
    USE M_MPIF
!
! !INPUT/OUTPUT PARAMETERS:
!
    REAL*8,  INTENT(INOUT) :: C1, CSTRAT, CTROPL, CTROPS, CURBAN
    INTEGER, INTENT(INOUT) :: ININT(10), IORD, NCOF
!
! !OUPTUT PARAMETERS:
!
    INTEGER, INTENT(OUT)   :: RC          ! Success or failure
!
! !REMARKS:
!  NOTE: The READCHEM (GeosCore/readchem.F) subroutine is a tangled mess.  
!  It not only reads values from mglob.dat but it also sets up other arrays 
!  used elsewhere in the SMVGEAR code.  It may just be simpler to run READER 
!  on all CPUs so as not to have to worry about doing the MPI broadcast 
!  properly. (bmy, 3/7/13)
!
! !REVISION HISTORY:
!  04 Jan 2013 - M. Long     - Initial version
!  07 Mar 2013 - R. Yantosca - Added ProTeX headers
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    ! Globchem.dat
    CALL MPI_Bcast( C1,     1,        mpi_real8,     0, mpiComm, RC ) ! IN
    CALL MPI_Bcast( CSTRAT, 1,        mpi_real8,     0, mpiComm, RC ) ! IN
    CALL MPI_Bcast( CTROPL, 1,        mpi_real8,     0, mpiComm, RC ) ! IN
    CALL MPI_Bcast( CTROPS, 1,        mpi_real8,     0, mpiComm, RC ) ! IN
    CALL MPI_Bcast( CURBAN, 1,        mpi_real8,     0, mpiComm, RC ) ! IN
    CALL MPI_Bcast( ININT,  10,       mpi_integer,   0, mpiComm, RC ) ! IN
    CALL MPI_Bcast( IORD,   1,        mpi_integer,   0, mpiComm, RC ) ! IN
    CALL MPI_Bcast( NCOF,   1,        mpi_integer,   0, mpiComm, RC ) ! IN

    RC = GC_SUCCESS
    
  END SUBROUTINE GIGC_Readchem_Bcast
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_bcast_char
!
! !DESCRIPTION: Wrapper routine to do an MPI broadcast operation on
!  a CHARACTER string variable.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Bcast_Char( VAL, SIZE, RC )
!
! !USES:
!
    USE Errcode_Mod
    USE M_MPIF
!
! !INPUT PARAMETERS:
!
    INTEGER,      INTENT(IN)    :: SIZE     ! # of characters
!
! !INPUT/OUTPUT PARAMETERS:
!
    CHARACTER(*), INTENT(INOUT) :: VAL(:)   ! Character string

!
! !OUTPUT PARAMETERS:
!
    INTEGER,      INTENT(OUT)   :: RC       ! Success or failure?
!
! !REMARKS:
!  Mostly experimental.
!
! !REVISION HISTORY:
!  04 Jan 2013 - M. Long     - Initial version
!  07 Mar 2013 - R. Yantosca - Added ProTeX header
!EOP
!------------------------------------------------------------------------------
!BOC
    
    ! Assume success
    RC = GC_SUCCESS

    ! Do MPI broadcast
    CALL MPI_Bcast( VAL, SIZE, mpi_character, 0, mpiComm, RC )
    
  END SUBROUTINE GIGC_Bcast_Char
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_bcast_int
!
! !DESCRIPTION: Wrapper routine to do an MPI broadcast operation on
!  an INTEGER variable.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Bcast_Int( VAL, SIZE, RC )
!
! !USES:
!
    USE Errcode_Mod
    USE M_MPIF
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN)    :: SIZE        ! Size of variable to be broadcasted
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER, INTENT(INOUT) :: VAL(SIZE)   ! Variable to be broadcasted
!
! !OUTPUT PARAMETERS:
!
    INTEGER, INTENT(OUT)   :: RC          ! Success or failure?
!
! !REMARKS:
!  Mostly experimental
!
! !REVISION HISTORY:
!  04 Jan 2013 - M. Long     - Initial version
!  07 Mar 2013 - R. Yantosca - Added ProTeX header
!EOP
!------------------------------------------------------------------------------
!BOC

    RC = GC_SUCCESS
    
    CALL MPI_Bcast( VAL, SIZE, mpi_integer, 0, mpiComm, RC )
    
  END SUBROUTINE GIGC_Bcast_Int
!EOC
!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: gigc_bcast_real8
!
! !DESCRIPTION: Wrapper routine to do an MPI broadcast operation on
!  an REAL*8 variable.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE GIGC_Bcast_Real8( VAL, SIZE, RC )
!
! !USES:
!
    USE Errcode_Mod
    USE M_MPIF
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN)    :: SIZE        ! Size of variable to be broadcast
! 
! !INPUT/OUTPUT PARAMETERS:
!
    REAL*8,  INTENT(INOUT) :: VAL(SIZE)   ! Variable to be broadcast
!
! !OUTPUT PARAMETERS:
!
    INTEGER, INTENT(OUT)   :: RC          ! Success or failure?
!
! !REMARKS:
!  Mostly experimental
!
! !REVISION HISTORY:
!  04 Jan 2013 - M. Long     - Initial version
!  07 Mar 2013 - R. Yantosca - Added ProTeX header
!EOP
!------------------------------------------------------------------------------
!BOC
    
    RC = GC_SUCCESS
    
    CALL MPI_Bcast( VAL, SIZE, mpi_real8, 0, mpiComm, RC )
    
  END SUBROUTINE GIGC_Bcast_Real8
!EOC
END MODULE GIGC_Mpi_Wrap
#endif
