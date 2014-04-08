    call MAPL_AddConnectivity ( GC,                                   &
	 SHORT_NAME  = (/'BOXHEIGHT', 'AIRVOL'/),                     &
         DST_ID      = CHEM,                                          &
         SRC_ID      = DYN,                                           &
                                                               __RC__  )

    call MAPL_AddConnectivity ( GC,                                   &
	 SHORT_NAME  = (/'AD', 'DELP', 'AIRDEN'/),                    &
         DST_ID      = CHEM,                                          &
         SRC_ID      = DYN,                                           &
                                                               __RC__  )

    call MAPL_AddConnectivity ( GC,                                 &
	 SHORT_NAME  = (/'PCENTER', 'PEDGE', 'BOXHEIGHT', 'AREA'/), &
         DST_ID      = EMIS,                                        &
         SRC_ID      = DYN,                                         &
                                                            __RC__   )

    call MAPL_AddConnectivity ( GC,                                &
	 SHORT_NAME  = (/'TRACERS'/),                              &
         DST_ID      = DYN,                                        &
         SRC_ID      = CHEM,                                       &
                                                            __RC__  )

    call MAPL_AddConnectivity ( GC,                                &
	 SHORT_NAME  = (/'TRACERS' /),                             &
         DST_ID      = EMIS,                                       &
         SRC_ID      = CHEM,                                       &
                                                            __RC__  )

    call MAPL_AddConnectivity ( GC,                                &
	 SHORT_NAME  = (/'EMISSIONS'/),                            &
         DST_ID      = CHEM,                                       &
         SRC_ID      = EMIS,                                       &
                                                            __RC__  )
