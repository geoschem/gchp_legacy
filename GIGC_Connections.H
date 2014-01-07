    call MAPL_AddConnectivity ( GC,                                &
	 SHORT_NAME  = (/'TRACERS'/),                              &
         DST_ID      = DYN,                                        &
         SRC_ID      = CHEM,                                       &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddConnectivity ( GC,                                &
	 SHORT_NAME  = (/'EMISSIONS'/),                            &
         DST_ID      = CHEM,                                       &
         SRC_ID      = EMIS,                                       &
                                                            __RC__  )
