    call MAPL_AddConnectivity ( GC,                                &
	 SHORT_NAME  = (/'TRACERS'/),                              &
         DST_ID      = DYN,                                        &
         SRC_ID      = CHEM,                                       &
                                                        RC=STATUS  )
    VERIFY_(STATUS)
